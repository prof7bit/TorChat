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
  Sockets,
  lNet,
  lEvents,
  fpjson,
  tc_interface,
  tc_misc,
  tc_conn,
  tc_msgqueue;


type
  { TBuddy }
  TBuddy = class(TInterfacedObject, IBuddy)
  strict private
    FID: String;
    FClient: IClient;
    FOwnCookie: String;
    FFriendlyName: String;
    FSoftware: String;
    FSoftwareVersion: String;
    FAvatarData: String;
    FAvatarAlphaData: String;
    FProfileName: String;
    FProfileText: String;
    FStatus: TTorchatStatus;
    FReconnectInterval: Double;
    FConnIncoming: IHiddenConnection;
    FConnOutgoing: IHiddenConnection;
    FMustSendPong: Boolean;
    FReceivedCookie: String;
    FConnecting: Boolean;
    FTimeCreated: TDateTime;
    FLastDisconnect: TDateTime;
    FLastActivity: TDateTime;
    FLastStatusSent: TDateTime;
    FLnetClient: TLTcp;
    procedure OnProxyConnect(ASocket: TLSocket);
    procedure OnProxyConnectFailed;
    procedure OnProxyReceive(ASocket: TLSocket);
    procedure OnProxyDisconect(ASocket: TLSocket);
    procedure OnProxyError(const Error: String; ASocket: TLSocket);
    procedure InitiateConnect;
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
    procedure ResetTimeCreated;
    procedure DoDisconnect;
    procedure RemoveYourself;
    function Client: IClient;
    function ID: String;
    function Cookie: String;
    function FriendlyName: String;
    function ConnIncoming: IHiddenConnection;
    function ConnOutgoing: IHiddenConnection;
    function Status: TTorchatStatus;
    function Software: String;
    function SoftwareVersion: String;
    function AvatarData: String;
    function AvatarAlphaData: String;
    function ProfileName: String;
    function ProfileText: String;
    procedure SetIncoming(AConn: IHiddenConnection);
    procedure SetOutgoing(AConn: IHiddenConnection);
    procedure SetStatus(AStatus: TTorchatStatus);
    procedure SetFriendlyName(AName: String);
    procedure SetSoftware(ASoftware: String);
    procedure SetSoftwareVersion(AVersion: String);
    procedure SetAvatarData(ABitmap: String);
    procedure SetAvatarAlphaData(ABitmap: String);
    procedure SetProfileName(AName: String);
    procedure SetProfileText(AText: String);
    function SendIM(AText: String): Boolean;
    procedure SendPong;
    procedure SendAddMe;
    procedure SendStatus;
    procedure SendAvatar;
  end;

implementation
uses
  tc_prot_ping,
  tc_prot_pong,
  tc_prot_client,
  tc_prot_version,
  tc_prot_add_me,
  tc_prot_status,
  tc_prot_remove_me,
  tc_prot_message,
  tc_prot_profile_avatar_alpha,
  tc_prot_profile_avatar,
  tc_prot_profile_text,
  tc_prot_profile_name,
  tc_const;

{ TBuddy }

constructor TBuddy.Create(AClient: IClient);
var
  GUID: TGuid;
begin
  inherited Create;
  FConnecting := False;
  FClient := AClient;
  FLastActivity := Now;
  FTimeCreated := Now;
  FLastStatusSent := Now;
  FMustSendPong := False;
  FReceivedCookie := '';
  FStatus := TORCHAT_OFFLINE;
  CreateGUID(GUID);
  FOwnCookie := GUIDToString(GUID);
  FLnetClient := TLTcp.Create(nil);
  FLnetClient.Eventer := Client.LNetEventer;
  // move the last disconnect a few seconds into the past
  FLastDisconnect := Now - (SECONDS_INITIAL_RECONNECT - SECONDS_FIRST_CONNECT) / SecsPerDay;
  ResetConnectInterval;
  WriteLn('TBuddy.Create() created random cookie: ' + FOwnCookie);
end;

destructor TBuddy.Destroy;
begin
  WriteLn('TBuddy.Destroy() ' + ID);

  // make sure the handle that might still be stuck in select() will
  // not try to fire any late Events after the buddy is freed
  FLnetClient.IterReset;
  if Assigned(FLnetClient.Iterator) then begin
    with FLnetClient.Iterator do begin // the root handle of the TLTcp
      IgnoreRead := True;
      IgnoreWrite := True;
      IgnoreError := True;
    end;
  end;
  FLnetClient.Disconnect();
  FLnetClient.Free;
  WriteLn('TBuddy.Destroy() ' + ID + ' finished');
  inherited Destroy;
end;

procedure TBuddy.InitiateConnect;
begin
  WriteLn(_F('TBuddy.InitiateConnect() (%s) connecting to Tor: %s:%d',
    [ID, Client.TorHost, Client.TorPort]));
  with FLnetClient do begin
    OnConnect := @Self.OnProxyConnect;
    OnReceive := @Self.OnProxyReceive;
    OnDisconnect := @Self.OnProxyDisconect;
    OnError := @Self.OnProxyError;
  end;
  FLnetClient.Connect(Client.TorHost, Client.TorPort);
  FConnecting := True;
end;

procedure TBuddy.OnProxyConnect(ASocket: TLSocket);
var
  Req: String;
begin
  WriteLn('~~~~> ', ID, ' connected to Tor, sending Socks4a request');
  SetLength(Req, 8);
  Req[1] := #4; // Socks 4
  Req[2] := #1; // CONNECT command
  PWord(@Req[3])^ := ShortHostToNet(11009); // TorChat port
  PDWord(@Req[5])^ := HostToNet(1); // address '0.0.0.1' means: Socks 4a
  Req := Req + 'TorChat' + #0;
  Req := Req + ID + '.onion' + #0;
  ASocket.Send(Req[1], Length(Req));
end;

procedure TBuddy.OnProxyConnectFailed;
var
  Slowdown: Double;
begin
  FConnecting := False;
  FLastDisconnect := Now;
  Slowdown := 1 + Random(100)/200; // [1 .. 1.5]
  FReconnectInterval := FReconnectInterval * Slowdown;
  WriteLn(_F('%s next connection attempt in %g seconds',
    [ID, FReconnectInterval]));
end;

procedure TBuddy.OnProxyReceive(ASocket: TLSocket);
var
  Ans: String;
  Num: Integer;
  Err: String;
  C  : IHiddenConnection;
begin
  Num := ASocket.GetMessage(Ans);
  if (Num = 8) and (Ans[2] = #90) then begin
    //WriteLn('TBuddy.OnProxyReceive() ', ID, ' socks4a connection established');

    // remove the event methods, THiddenConnection will install its own
    FLnetClient.OnReceive := nil;
    FLnetClient.OnDisconnect := nil;
    FLnetClient.OnError := nil;
    C := THiddenConnection.Create(Client, ASocket, Self);
    SetOutgoing(C);
    FConnecting := False;
  end
  else begin
    if Num = 8 then
      Err := IntToStr(Ord((Ans[2])))
    else
      Err := 'wrong answer from proxy (' + IntToStr(Num) + ' bytes)';
    WriteLn('~~/~> ', ID, ' Socks4a connection failed: ', Err);
    ASocket.Disconnect();
  end;
end;

procedure TBuddy.OnProxyDisconect(ASocket: TLSocket);
begin
  //WriteLn('TBuddy.OnProxyDisconnect() ', ID);
  OnProxyConnectFailed;
end;

procedure TBuddy.OnProxyError(const Error: String; ASocket: TLSocket);
begin
  WriteLn('~~?~> ', ID, ' Tor connection error: ', Error);
  OnProxyConnectFailed;
end;

procedure TBuddy.CheckState;
begin
  _AddRef;

  // Connect
  if not (Assigned(ConnOutgoing) or FConnecting) then begin
    if TimeSince(FLastDisconnect) > FReconnectInterval then begin
      InitiateConnect;
    end;
  end;

  // send keep-alive
  if Assigned(ConnIncoming) and Assigned(ConnOutgoing) then begin
    if TimeSince(FLastStatusSent) > SECONDS_SEND_KEEPLIVE then begin
      SendStatus;
    end;
  end;

  // check keep-alive timeout and disconect
  if Assigned(ConnOutgoing)
  and (TimeSince(FLastActivity) > SECONDS_WAIT_KEEPALIVE) then begin
    WriteLn(_F('I TBuddy.CheckState() %s timeout, disconnecting',
      [FID]));
    DoDisconnect;
  end;

  // check max time on templist and remove from there
  if TimeSince(FTimeCreated) > SECONDS_KEEP_ON_TEMPLIST then begin
    if Self in Client.TempList then begin
      WriteLn(_F('I TBuddy.CheckState() %s timeout, removing temporary buddy',
        [FID]));
      DoDisconnect;
      Client.TempList.RemoveBuddy(Self); // should now free on _Release;
    end;
  end;
  _Release;
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
    WriteLn('E cannot use this ID: ' + FID);
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
  Msg := TMsgPing.Create(Self, FOwnCookie);
  Msg.Send;

  // the other end has connected aleady and already
  // sent us a ping, so we can answer with pong
  if FMustSendPong then
    SendPong;
end;

procedure TBuddy.OnOutgoingConnectionFail;
begin
  FLastDisconnect := Now;
  ResetConnectInterval;
  if Assigned(ConnIncoming) then
    ConnIncoming.Disconnect;
  SetStatus(TORCHAT_OFFLINE);
end;

procedure TBuddy.OnIncomingConnection;
begin
  WriteLn('<==OK incomig connection authenticated ', ID);
end;

procedure TBuddy.OnIncomingConnectionFail;
begin
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

procedure TBuddy.ResetTimeCreated;
begin
  FTimeCreated := Now;
end;

procedure TBuddy.DoDisconnect;
var
  C1, C2: IHiddenConnection;
begin
  // we assign them to local variables first to make
  // sure the ref count is incremented and it can not
  // pull away the objects from under our feet.

  C1 := ConnIncoming;
  if Assigned(C1) then C1.Disconnect;

  C2 := ConnOutgoing;
  if Assigned(C2) then C2.Disconnect;

  // there also might just be an ongoing connection attempt
  FLnetClient.Disconnect();

  // C1 and C2 will free now when going out of scope
  WriteLn('TBuddy.DoDisconnect() leaving, connections will free now');
end;

procedure TBuddy.RemoveYourself; // called by the GUI
var
  RemoveMe: IProtocolMessage;
begin
  if IsFullyConnected then begin
    RemoveMe := TMsgRemoveMe.Create(Self);
    RemoveMe.Send;
  end;
  ResetTimeCreated;
  Client.TempList.AddBuddy(Self);
  Client.Roster.RemoveBuddyNoCallback(Self);
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

function TBuddy.Software: String;
begin
  Result := FSoftware;
end;

function TBuddy.SoftwareVersion: String;
begin
  Result := FSoftwareVersion;
end;

function TBuddy.AvatarData: String;
begin
  Result := FAvatarData;
end;

function TBuddy.AvatarAlphaData: String;
begin
  Result := FAvatarAlphaData;
end;

function TBuddy.ProfileName: String;
begin
  Result := FProfileName;
end;

function TBuddy.ProfileText: String;
begin
  Result := FProfileText;
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

procedure TBuddy.SetSoftware(ASoftware: String);
begin
  FSoftware := ASoftware;
end;

procedure TBuddy.SetSoftwareVersion(AVersion: String);
begin
  FSoftwareVersion := AVersion;
end;

procedure TBuddy.SetAvatarData(ABitmap: String);
begin
  FAvatarData := ABitmap;
  if Self in Client.Roster then
    Client.OnBuddyAvatarChange(Self);
end;

procedure TBuddy.SetAvatarAlphaData(ABitmap: String);
begin
  FAvatarAlphaData := ABitmap;
end;

procedure TBuddy.SetProfileName(AName: String);
begin
  FProfileName := AName;
end;

procedure TBuddy.SetProfileText(AText: String);
begin
  FProfileText := AText;
end;

function TBuddy.SendIM(AText: String): Boolean;
var
  Msg: IProtocolMessage;
begin
  if IsFullyConnected then begin
    Msg := TMsgMessage.Create(Self, AText);
    Msg.Send;
    Result := True;
  end
  else
    Result := False;
end;

procedure TBuddy.SendPong;
var
  Msg: IProtocolMessage;
begin
  WriteLn('TBuddy.SendPong() ', ID, ' sending pong and status');
  Msg := TMsgPong.Create(Self, FReceivedCookie);
  Msg.Send;
  Msg := TMsgClient.Create(Self, SOFTWARE_NAME);
  Msg.Send;
  Msg := TMsgVersion.Create(Self, SOFTWARE_VERSION);
  Msg.Send;
  SendAvatar;
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
  WriteLn('TBuddy.SendAddMe() ', ID, ' sending add_me');
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

procedure TBuddy.SendAvatar;
var
  Msg: IProtocolMessage;
  RGB: String;
  Alpha: String;
begin
  RGB := Client.Config.GetString('AvatarData', True);
  Alpha := Client.Config.GetString('AvatarAlphaData', True);
  if Length(RGB) = 12288 then begin
    if Length(Alpha) = 4096 then
      Msg := TMsgProfileAvatarAlpha.Create(Self, Alpha)
    else
      Msg := TMsgProfileAvatarAlpha.Create(Self, '');
    Msg.Send;
    Msg := TMsgProfileAvatar.Create(Self, RGB);
    Msg.Send;
  end
  else begin // send empty avatar
    Msg := TMsgProfileAvatarAlpha.Create(Self, '');
    Msg.Send;
    Msg := TMsgProfileAvatar.Create(Self, '');
    Msg.Send;
  end;
end;

end.

