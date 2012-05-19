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
  fpjson,
  tc_interface,
  tc_prot_ping,
  tc_prot_pong,
  tc_prot_add_me,
  tc_misc,
  tc_conn,
  tc_sock,
  tc_msgqueue;


type
  { TBuddy }
  TBuddy = class(TInterfacedObject, IBuddy)
  strict protected
    FOnCbNetOutFinishedEvent: PRTLEvent;
    FID: String;
    FClient: IClient;
    FOwnCookie: String;
    FFriendlyName: String;
    FStatus: TTorchatStatus;
    FLastDisconnect: TDateTime;
    FConnIncoming: IHiddenConnection;
    FConnOutgoing: IHiddenConnection;
    FMustSendPong: Boolean;
    FReceivedCookie: String;
    FConnectThread: TAsyncConnectThread;
    procedure InitiateConnect; virtual;
    procedure CbNetOut(ATCPStream: TTCPStream; E: Exception); virtual;
    procedure SendPong; virtual;
    function IsFullyConnected: Boolean;
    procedure CallFromMainThread(AMethod: TMethodOfObject); virtual;
  public
    constructor Create(AClient: IClient); reintroduce;
    destructor Destroy; override;
    procedure CheckState; virtual;
    function AsJsonObject: TJSONObject; virtual;
    procedure InitFromJsonObect(AObject: TJSONObject); virtual;
    procedure InitID(AID: String); virtual;
    procedure OnOutgoingConnection; virtual;
    procedure OnOutgoingConnectionFail; virtual;
    procedure OnIncomingConnection; virtual;
    procedure OnIncomingConnectionFail; virtual;
    procedure MustSendPong(ACookie: String); virtual;
    procedure DoDisconnect; virtual;
    function Client: IClient;
    function ID: String;
    function Cookie: String;
    function FriendlyName: String;
    function ConnIncoming: IHiddenConnection;
    function ConnOutgoing: IHiddenConnection;
    function Status: TTorchatStatus;
    procedure SetIncoming(AConn: IHiddenConnection); virtual;
    procedure SetOutgoing(AConn: IHiddenConnection); virtual;
    procedure SetStatus(AStatus: TTorchatStatus); virtual;
    procedure SetFriendlyName(AName: String);
  end;

implementation

{ TBuddy }

procedure TBuddy.InitiateConnect;
begin
  WriteLn('TBuddy.InitiateConnect() ' + ID);
  RTLeventResetEvent(FOnCbNetOutFinishedEvent);
  FConnectThread := FClient.Network.ConnectAsync(ID + '.onion', 11009, @CbNetOut);
end;

procedure TBuddy.CbNetOut(ATCPStream: TTCPStream; E: Exception);
begin
  WriteLn('TBuddy.CbNetOut() ' + ID);
  FConnectThread := nil;
  if assigned(ATCPStream) then begin
    SetOutgoing(THiddenConnection.Create(FClient, ATCPStream, Self));
  end
  else begin
    WriteLn(E.Message);
    FLastDisconnect := Now;
    SetOutgoing(nil);
  end;
  WriteLn('TBuddy.CbNetOut() ' + ID + ' setting event');
  RTLeventSetEvent(FOnCbNetOutFinishedEvent);
end;

constructor TBuddy.Create(AClient: IClient);
var
  GUID: TGuid;
begin
  inherited Create;
  FOnCbNetOutFinishedEvent := RTLEventCreate;
  FConnectThread := nil;
  FClient := AClient;
  FLastDisconnect := 0;
  FMustSendPong := False;
  FReceivedCookie := '';
  FStatus := TORCHAT_OFFLINE;
  CreateGUID(GUID);
  FOwnCookie := GUIDToString(GUID);
  WriteLn('TBuddy.Create() created random cookie: ' + FOwnCookie);
end;

destructor TBuddy.Destroy;
begin
  writeln('TBuddy.Destroy() ' + ID);
  if Assigned(FConnectThread) then begin
    writeln('TBuddy.Destroy() ' + ID + ' must terminate ongoing connection attempt');
    FConnectThread.Terminate;
    writeln('TBuddy.Destroy() ' + ID + ' waiting event');
    RTLeventWaitFor(FOnCbNetOutFinishedEvent);
    writeln('TBuddy.Destroy() ' + ID + ' got event');
  end;
  RTLeventdestroy(FOnCbNetOutFinishedEvent);
  inherited Destroy;
end;

procedure TBuddy.CheckState;
begin
  if not (Assigned(ConnOutgoing) or Assigned(FConnectThread)) then begin
    if SecondsSince(FLastDisconnect) > 10 then begin
      InitiateConnect;
    end;
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

procedure TBuddy.InitID(AID: String);
begin
  FID := AID;
end;

procedure TBuddy.SendPong;
var
  Message: IProtocolMessage;
begin
  Message := TMsgPong.Create(Self, FReceivedCookie);
  Message.Send;
  Message := TMsgAddMe.Create(Self);
  Message.Send;
  FMustSendPong := False;
  FReceivedCookie := '';
end;

function TBuddy.IsFullyConnected: Boolean;
begin
  Result := Assigned(ConnOutgoing) and Assigned(ConnIncoming);
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

procedure TBuddy.DoDisconnect;
begin
  if Assigned(ConnIncoming) or Assigned(ConnOutgoing) then begin
    if Assigned(ConnIncoming) then ConnIncoming.Disconnect;
    if Assigned(ConnOutgoing) then ConnOutgoing.Disconnect;
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

function TBuddy.Status: TTorchatStatus;
begin
  Result := FStatus;
end;

procedure TBuddy.SetIncoming(AConn: IHiddenConnection);
begin
  if AConn <> FConnIncoming then begin
    FConnIncoming := AConn;
    if Assigned(AConn) then begin
      AConn.SetBuddy(Self);
      CallFromMainThread(@OnIncomingConnection);
    end
    else
      CallFromMainThread(@OnIncomingConnectionFail);
  end;
end;

procedure TBuddy.SetOutgoing(AConn: IHiddenConnection);
begin
  if AConn <> FConnOutgoing then begin
    FConnOutgoing := AConn;
    if Assigned(AConn) then begin
      AConn.SetBuddy(Self);
      CallFromMainThread(@OnOutgoingConnection);
    end
    else
      CallFromMainThread(@OnOutgoingConnectionFail);
  end;
end;

procedure TBuddy.SetStatus(AStatus: TTorchatStatus);
begin
  if AStatus <> FStatus then begin
    FStatus := AStatus;
    if Self in Client.Roster then
      Client.OnBuddyStatusChange(Self);
  end;
end;

procedure TBuddy.SetFriendlyName(AName: String);
begin
  if AName <> FFriendlyName then begin
    FFriendlyName := AName;
    Client.Roster.Save;
  end;
end;

end.

