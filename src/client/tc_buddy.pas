{ TorChat - TBuddy, this component is implememting a TorChat buddy

  Copyright (C) 2012 Bernd Kreuss <prof7bit@gmail.com>

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 3 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}
unit tc_buddy;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  syncobjs,
  lNet,
  fpjson,
  tc_interface,
  tc_misc,
  tc_conn,
  tc_msgqueue;


type
  { TBuddy }
  TBuddy = class(TInterfacedObject, IBuddy)
  strict private
    FOnCbNetOutFinishedEvent: TSimpleEvent;
    FID: String;
    FClient: IClient;
    FOwnCookie: String;
    FFriendlyName: String;
    FStatus: TTorchatStatus;
    FLastDisconnect: TDateTime;
    FReconnectInterval: Integer;
    FConnIncoming: IHiddenConnection;
    FConnOutgoing: IHiddenConnection;
    FMustSendPong: Boolean;
    FReceivedCookie: String;
    FConnecting: Boolean;
    FLastActivity: TDateTime;
    FLastStatusSent: TDateTime;
    procedure InitiateConnect;
    procedure CbNetOut(ATCPStream: TStream; E: Exception);
    function CanUseThisName(AName: String): Boolean;
    procedure CallFromMainThread(AMethod: TMethodOfObject);
  public
    constructor Create(AClient: IClient); reintroduce;
    destructor Destroy; override;
    procedure CheckState;
    function IsFullyConnected: Boolean;
    function AsJsonObject: TJSONObject;
    procedure InitFromJsonObect(AObject: TJSONObject);
    function InitID(AID: String): Boolean;
    procedure OnOutgoingConnection;
    procedure OnOutgoingConnectionFail;
    procedure OnIncomingConnection;
    procedure OnIncomingConnectionFail;
    procedure MustSendPong(ACookie: String);
    procedure ForgetLastPing;
    procedure ResetConnectInterval;
    procedure ResetTimeout;
    procedure DoDisconnect;
    procedure RemoveYourself;
    function Client: IClient;
    function ID: String;
    function Cookie: String;
    function FriendlyName: String;
    function ConnIncoming: IHiddenConnection;
    function ConnOutgoing: IHiddenConnection;
    function Status: TTorchatStatus;
    procedure SetIncoming(AConn: IHiddenConnection);
    procedure SetOutgoing(AConn: IHiddenConnection);
    procedure SetStatus(AStatus: TTorchatStatus);
    procedure SetFriendlyName(AName: String);
    procedure SendPong;
    procedure SendAddMe;
    procedure SendStatus;
  end;

implementation
uses
  tc_prot_ping,
  tc_prot_pong,
  tc_prot_add_me,
  tc_prot_status,
  tc_prot_remove_me,
  tc_config;

{ TBuddy }

procedure TBuddy.InitiateConnect;
begin
  WriteLn('TBuddy.InitiateConnect() ' + ID);
  FOnCbNetOutFinishedEvent.ResetEvent;
  FClient.LNetClient.Connect(FClient.TorHost, FClient.TorPort);
  //FConnectThread := FClient.Network.ConnectAsync(ID + '.onion', 11009, @CbNetOut);
end;

procedure TBuddy.CbNetOut(ATCPStream: TStream; E: Exception);
begin
  //FConnectThread := nil;
  if assigned(ATCPStream) then begin
    SetOutgoing(THiddenConnection.Create(FClient, ATCPStream, Self));
  end
  else begin
    WriteLn(E.Message);
    FLastDisconnect := Now;
    FReconnectInterval := Round(FReconnectInterval * RECONNECT_SLOWDOWN);
    WriteLn(_F('%s next connection attempt in %d seconds',
      [ID, FReconnectInterval]));
  end;
  FOnCbNetOutFinishedEvent.SetEvent;
end;

constructor TBuddy.Create(AClient: IClient);
var
  GUID: TGuid;
begin
  inherited Create;
  FOnCbNetOutFinishedEvent := TSimpleEvent.Create;
  FConnecting := False;
  FClient := AClient;
  FLastDisconnect := 0;
  FLastActivity := Now;
  FLastStatusSent := Now;
  FMustSendPong := False;
  FReceivedCookie := '';
  FStatus := TORCHAT_OFFLINE;
  CreateGUID(GUID);
  FOwnCookie := GUIDToString(GUID);
  ResetConnectInterval;
  WriteLn('TBuddy.Create() created random cookie: ' + FOwnCookie);
end;

destructor TBuddy.Destroy;
begin
  writeln('TBuddy.Destroy() ' + ID);
  //if Assigned(FConnectThread) then begin
  //  writeln('TBuddy.Destroy() ' + ID + ' must terminate ongoing connection attempt');
  //  FConnectThread.Terminate;
  //  FOnCbNetOutFinishedEvent.WaitFor(INFINITE);
  //end;
  FOnCbNetOutFinishedEvent.Free;
  writeln('TBuddy.Destroy() ' + ID + ' finished');
  inherited Destroy;
end;

procedure TBuddy.CheckState;
begin
  if not (Assigned(ConnOutgoing) or FConnecting) then begin
    if SecondsSince(FLastDisconnect) > FReconnectInterval then begin
      InitiateConnect;
    end;
  end;

  if Assigned(ConnIncoming) and Assigned(ConnOutgoing) then begin
    if SecondsSince(FLastStatusSent) > 120 then begin
      SendStatus;
    end;
  end;

  if Assigned(ConnOutgoing) and (SecondsSince(FLastActivity) > 240) then begin
    WriteLn(_F('I TBuddy.CheckState() %s timeout, disconnecting',
      [FID]));
    DoDisconnect;
  end;
end;

function TBuddy.AsJsonObject: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('id', TJSONString.Create(FID));
  Result.Add('friendlyname', TJSONString.Create(FFriendlyName));
end;

procedure TBuddy.InitFromJsonObect(AObject: TJSONObject);
begin
  // these fields are mandatory, failing will raise exception
  FID := AObject.Strings['id'];
  if not CanUseThisName(FID) then begin
    writeln('E cannot use this ID: ' + FID);
    raise Exception.Create('cannot use this id');
  end;
  FFriendlyName := AObject.Strings['friendlyname'];

  // the following fields are optional (backwards compatibility)
  // they will be tried in excatly this order from oldest fields
  // first to newest last and failing at any point will be ignored
  try
    // nothing yet
  except
    // ignore from here on
  end;
end;

function TBuddy.InitID(AID: String): Boolean;
begin
  if CanUseThisName(AID) then begin
    FID := AID;
    Result := True;
  end
  else
    Result := False;
end;

function TBuddy.CanUseThisName(AName: String): Boolean;
begin
  Result := True;
  if not IsValidOnionName(AName) then exit(False);
  if Assigned(Client.TempList.ByID(AName)) then exit(False);
  if Assigned(Client.Roster.ByID(AName)) then exit(False);
end;

procedure TBuddy.CallFromMainThread(AMethod: TMethodOfObject);
var
  Msg: IMessage;
begin
  if ThreadID = Client.MainThread then
    AMethod()
  else begin
    Msg := TMsgCallMethod.Create(AMethod);
    Client.Queue.Put(Msg);
  end
end;

procedure TBuddy.OnOutgoingConnection;
var
  Msg: IProtocolMessage;
begin
  WriteLn('TBuddy.OnOutgoingConnection() ' + ID);
  Msg := TMsgPing.Create(Self, FOwnCookie);
  Msg.Send;

  // the other end has connected aleady and already
  // sent us a ping, so we can answer with pong
  if FMustSendPong then
    SendPong;
end;

procedure TBuddy.OnOutgoingConnectionFail;
begin
  WriteLn('TBuddy.OnOutgoingConnectionFail() ' + ID);
  FLastDisconnect := Now;
  ResetConnectInterval;
  if Assigned(ConnIncoming) then
    ConnIncoming.Disconnect;
  SetStatus(TORCHAT_OFFLINE);
end;

procedure TBuddy.OnIncomingConnection;
begin
  Writeln('TBuddy.OnIncomingConnection() ' + ID);
  SetStatus(TORCHAT_AVAILABLE);
end;

procedure TBuddy.OnIncomingConnectionFail;
begin
  Writeln('TBuddy.OnIncomingConnectionFail() ' + ID);
  FLastDisconnect := Now;
  ResetConnectInterval;
  if Assigned(ConnOutgoing) then
    ConnOutgoing.Disconnect;
  SetStatus(TORCHAT_OFFLINE);
end;

procedure TBuddy.MustSendPong(ACookie: String);
begin
  FReceivedCookie := ACookie;
  FMustSendPong := True;

  // if we are connected already we can send it
  // immediately, otherwise it will happen on connect
  if Assigned(ConnOutgoing) then
    SendPong;
end;

procedure TBuddy.ForgetLastPing;
begin
  FReceivedCookie := '';
  FMustSendPong := False;
end;

procedure TBuddy.ResetConnectInterval;
begin
  FReconnectInterval := SECONDS_INITIAL_RECONNECT;
end;

procedure TBuddy.ResetTimeout;
begin
  FLastActivity := Now;
end;

procedure TBuddy.DoDisconnect;
begin
  if Assigned(ConnIncoming) or Assigned(ConnOutgoing) then begin
    if Assigned(ConnIncoming) then ConnIncoming.Disconnect;
    if Assigned(ConnOutgoing) then ConnOutgoing.Disconnect;
  end;
end;

procedure TBuddy.RemoveYourself; // called by the GUI
var
  RemoveMe: IProtocolMessage;
begin
  Client.TempList.AddBuddy(Self);
  Client.Roster.RemoveBuddyNoCallback(Self);
  if IsFullyConnected then begin
    RemoveMe := TMsgRemoveMe.Create(Self);
    RemoveMe.Send;
  end;
end;

function TBuddy.Client: IClient;
begin
  Result := FClient;
end;

function TBuddy.ID: String;
begin
  Result := FID;
end;

function TBuddy.Cookie: String;
begin
  Result := FOwnCookie;
end;

function TBuddy.FriendlyName: String;
begin
  Result := FFriendlyName;
end;

function TBuddy.ConnIncoming: IHiddenConnection;
begin
  Result := FConnIncoming;
end;

function TBuddy.ConnOutgoing: IHiddenConnection;
begin
  Result := FConnOutgoing;
end;

function TBuddy.IsFullyConnected: Boolean;
begin
  Result := Assigned(ConnIncoming) and Assigned(ConnOutgoing);
end;

function TBuddy.Status: TTorchatStatus;
begin
  Result := FStatus;
end;

procedure TBuddy.SetIncoming(AConn: IHiddenConnection);
begin
  if AConn <> FConnIncoming then begin
    if Assigned(AConn) then begin
      FConnIncoming := AConn;
      AConn.SetBuddy(Self);
      Client.UnregisterAnonConnection(AConn);
      ResetTimeout;
      CallFromMainThread(@OnIncomingConnection);
    end
    else begin
      FConnIncoming := nil;
      CallFromMainThread(@OnIncomingConnectionFail);
    end;
  end;
end;

procedure TBuddy.SetOutgoing(AConn: IHiddenConnection);
begin
  if AConn <> FConnOutgoing then begin
    if Assigned(AConn) then begin
      FConnOutgoing := AConn;
      AConn.SetBuddy(Self);
      ResetTimeout;
      CallFromMainThread(@OnOutgoingConnection);
    end
    else begin
      FConnOutgoing := nil;
      CallFromMainThread(@OnOutgoingConnectionFail);
    end;
  end;
end;

procedure TBuddy.SetStatus(AStatus: TTorchatStatus);
begin
  FStatus := AStatus;
  if Self in Client.Roster then
    Client.OnBuddyStatusChange(Self);
end;

procedure TBuddy.SetFriendlyName(AName: String);
begin
  FFriendlyName := AName;
  Client.Roster.Save;
end;

procedure TBuddy.SendPong;
var
  Pong: IProtocolMessage;
begin
  Pong := TMsgPong.Create(Self, FReceivedCookie);
  Pong.Send;
  if Self in Client.Roster then
    SendAddMe;
  SendStatus;
  FMustSendPong := False;
  FReceivedCookie := '';
end;

procedure TBuddy.SendAddMe;
var
  AddMe: IProtocolMessage;
begin
  AddMe := TMsgAddMe.Create(Self);
  AddMe.Send;
end;

procedure TBuddy.SendStatus;
var
  Stat : IProtocolMessage;
begin
  Stat := TMsgStatus.Create(Self);
  Stat.Send;
  FLastStatusSent := Now;
end;

end.

