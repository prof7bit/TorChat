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
{$modeswitch autoderef}

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
    FLocalAlias: String;
    FSoftware: String;
    FSoftwareVersion: String;
    FAvatarData: String;
    FAvatarAlphaData: String;
    FProfileName: String;
    FProfileText: String;
    FStatus: TTorchatStatus;
    FConnIncoming: IHiddenConnection;
    FConnOutgoing: IHiddenConnection;
    FMustSendPong: Boolean;
    FPongAlreadySent: Boolean;
    FReceivedCookie: String;
    FMustSendMultipleConnWarning: Boolean;
    FConnecting: Boolean;
    FReconnectInterval: Double;
    FTimeCreated: TDateTime;
    FTimeLastDisconnect: TDateTime;
    FTimeLastStatusReceived: TDateTime;
    FTimeLastPingReceived: TDateTime;
    FTimeLastStatusSent: TDateTime;
    FLnetClient: TLTcp;
    procedure OnProxyConnect(ASocket: TLSocket);
    procedure OnProxyConnectFailed;
    procedure OnProxyReceive(ASocket: TLSocket);
    procedure OnProxyDisconect(ASocket: TLSocket);
    procedure OnProxyError(const Error: String; ASocket: TLSocket);
    procedure InitiateConnect;
    function CanUseThisName(AName: String): Boolean;
    procedure CallFromMainThread(AMethod: TMethodOfObject);
    procedure CalcConnectInterval;
    procedure CalcConnectIntervalAfterPing;
  public
    constructor Create(AClient: IClient); reintroduce;
    destructor Destroy; override;
    procedure CheckState;
    function IsFullyConnected: Boolean;
    function MaySendText: Boolean;
    function AsJsonObject: TJSONObject;
    procedure InitFromJsonObect(AObject: TJSONObject);
    function InitID(AID: String): Boolean;
    procedure OnOutgoingConnection;
    procedure OnOutgoingConnectionFail;
    procedure OnIncomingConnection;
    procedure OnIncomingConnectionFail;
    procedure MustSendPong(ACookie: String);
    procedure ForgetLastPing;
    procedure ResetKeepaliveTimeout;
    procedure ResetAllTimes;
    procedure DoDisconnect;
    procedure RemoveYourself;
    function Client: IClient;
    function ID: String;
    function Cookie: String;
    function LocalAlias: String;
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
    procedure SetLocalAlias(AName: String);
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
    procedure SendProfile;
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
  FMustSendPong := False;
  FStatus := TORCHAT_OFFLINE;
  CreateGUID(GUID);
  FOwnCookie := GUIDToString(GUID);
  FLnetClient := TLTcp.Create(nil);
  FLnetClient.Eventer := Client.LNetEventer;
  ResetAllTimes;
  CalcConnectInterval;
  WriteLn('TBuddy.Create() created random cookie: ' + FOwnCookie);
end;

destructor TBuddy.Destroy;
var
  S: TLSocket;
begin
  WriteLn('TBuddy.Destroy() ' + ID);

  // make sure the handle that might still be stuck in select() will
  // not try to fire any late Events after the buddy is freed
  FLnetClient.IterReset;
  S := FLnetClient.Iterator; // the root handle of the TLTcp
  if Assigned(S) then begin
    S.OnRead := @Client.DummySocketEvent;
    S.OnWrite := @Client.DummySocketEvent;
    S.OnError := @Client.DummySocketError;
  end;
  FLnetClient.Disconnect;
  FLnetClient.Free;
  WriteLn('TBuddy.Destroy() ' + ID + ' finished');
  inherited Destroy;
end;

procedure TBuddy.InitiateConnect;
begin
  WriteLnF('TBuddy.InitiateConnect() (%s) connecting to Tor: %s:%d',
    [ID, Client.TorHost, Client.TorPort]);
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
type
  TSocksReqestHeader = packed record
    Typ   : Byte;
    Cmd   : Byte;
    Port  : Word;
    Addr4 : DWord;
  end;
var
  ReqHeader: TSocksReqestHeader;
  Req: String;
begin
  WriteLn('<~~~~ ', ID, ' connected to Tor, sending Socks4a request');
  with ReqHeader do begin
    Typ := 4; // Socks 4
    Cmd := 1; // CONNECT
    Port := ShortHostToNet(11009); // TorChat port
    Addr4 := HostToNet(1); // address '0.0.0.1' means: Socks 4a
  end;
  SetLength(Req, SizeOf(TSocksReqestHeader));
  Move(ReqHeader, Req[1], SizeOf(TSocksReqestHeader));
  Req := Req + 'TorChat' + #0;
  Req := Req + ID + '.onion' + #0;
  ASocket.Send(Req[1], Length(Req));
end;

procedure TBuddy.OnProxyConnectFailed;
begin
  FConnecting := False;
  CalcConnectInterval;
end;

procedure TBuddy.OnProxyReceive(ASocket: TLSocket);
type
  PSocks4Response = ^TSocks4Response;
  TSocks4Response = packed record
    NullByte : Byte;
    Status   : Byte;
    Port     : Word;
    Addr     : DWord;
  end;
var
  Buf: String;
  Response: PSocks4Response;
  RightSize: Boolean;
  Num: Integer;
  Err: String;
  C  : IHiddenConnection;
begin
  Num := ASocket.GetMessage(Buf);
  Response := Pointer(Buf);
  RightSize := (Num = SizeOf(TSocks4Response));
  if RightSize and (Response.Status = 90) then begin
    // remove the event methods, THiddenConnection will install its own
    FLnetClient.OnReceive := nil;
    FLnetClient.OnDisconnect := nil;
    FLnetClient.OnError := nil;
    C := THiddenConnection.Create(Client, ASocket, Self);
    SetOutgoing(C);
    FConnecting := False;
  end
  else begin
    if RightSize then
      Err := SF('%d', [Response.Status])
    else
      Err := SF('wrong answer from proxy (%d bytes)', [Num]);
    WriteLn('<~/~~ ', ID, ' Socks4a connection failed: ', Err);
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
  WriteLn('<~?~~ ', ID, ' Tor connection error: ', Error);
  OnProxyConnectFailed;
end;

procedure TBuddy.CheckState;
begin
  _AddRef;

  // Connect
  if not (Assigned(ConnOutgoing) or FConnecting) then begin
    if TimeSince(FTimeLastDisconnect) > FReconnectInterval then begin
      InitiateConnect;
    end;
  end;

  // send keep-alive
  if Assigned(ConnIncoming) and Assigned(ConnOutgoing) then begin
    if TimeSince(FTimeLastStatusSent) > SECONDS_SEND_KEEPLIVE then begin
      SendStatus;
    end;
  end;

  // check keep-alive timeout and disconect
  if Assigned(ConnOutgoing)
  and (TimeSince(FTimeLastStatusReceived) > SECONDS_WAIT_KEEPALIVE) then begin
    WriteLnF('TBuddy.CheckState() %s timeout, disconnecting',
      [FID]);
    DoDisconnect;
  end;

  // check max time on templist and remove from there
  if TimeSince(FTimeCreated) > SECONDS_KEEP_ON_TEMPLIST then begin
    if Self in Client.TempList then begin
      WriteLnF('TBuddy.CheckState() %s timeout, removing temporary buddy',
        [FID]);
      DoDisconnect;
      Client.TempList.RemoveBuddy(Self); // should now free on _Release;
    end;
  end;
  _Release;
end;

function TBuddy.AsJsonObject: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('ID', TJSONString.Create(FID));
  Result.Add('Alias', TJSONString.Create(FLocalAlias));
end;

procedure TBuddy.InitFromJsonObect(AObject: TJSONObject);
begin
  // this field is mandatory, failing will raise exception
  FID := AObject.Strings['ID'];

  if not CanUseThisName(FID) then begin
    WriteLn('E cannot use this ID: ' + FID);
    raise Exception.Create('cannot use this ID');
  end;

  // the following fields are optional (backwards compatibility)
  // they will be tried in excatly this order from oldest fields
  // first to newest last and failing at any point will be ignored
  try
    FLocalAlias := AObject.Strings['Alias'];
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
    Msg := TMsgCallMethod.Create(Self, AMethod);
    Client.Queue.Put(Msg);
  end
end;

procedure TBuddy.CalcConnectInterval;
var
  Minutes: Double;

  { this function takes the minutes since last online and
    returns the seconds to wait between connect attempts.}
  function GetInterval(M: Double): Double;
  begin
    Result := M * 20; // after 3 hours we reach maximum waiting time
    if Result > 3600 then
      Result := 3600;
    Result := Result + Random(Round(Result))/20;
  end;

begin
  FTimeLastDisconnect := Now;
  Minutes := TimeSince(FTimeLastStatusReceived) / SecsPerMin;
  FReconnectInterval := GetInterval(Minutes);
  WriteLnF('%s next connect attempt in %g seconds',
    [ID, FReconnectInterval]);
end;

procedure TBuddy.CalcConnectIntervalAfterPing;
begin
  WriteLnF('%s got ping, resetting connect timers', [ID]);
  ResetAllTimes;
  if not FConnecting then
    CalcConnectInterval
  else
    WriteLnF('%s is already trying to connect', [ID]);
end;

procedure TBuddy.OnOutgoingConnection;
var
  Msg: IProtocolMessage;
begin
  ResetKeepaliveTimeout;
  Msg := TMsgPing.Create(Self, FOwnCookie);
  Msg.Send;
  // the other end has connected already and already
  // sent us a ping, so we can answer with pong
  if FMustSendPong then
    SendPong;
  SetStatus(TORCHAT_CONNECTING); // "connecting" means handshake started
end;

procedure TBuddy.OnOutgoingConnectionFail;
begin
  if Assigned(ConnIncoming) then
    ConnIncoming.Disconnect;
  SetStatus(TORCHAT_OFFLINE);
  CalcConnectInterval;
end;

procedure TBuddy.OnIncomingConnection;
begin
  WriteLn('OK==> incoming connection authenticated ', ID);
  ResetAllTimes;
end;

procedure TBuddy.OnIncomingConnectionFail;
begin
  if Assigned(ConnOutgoing) then
    ConnOutgoing.Disconnect;
  SetStatus(TORCHAT_OFFLINE);
  CalcConnectInterval;
end;

procedure TBuddy.MustSendPong(ACookie: String);
begin
  // the client maintains a global list of incoming ping cookies
  // so we can detect if there are more then one for the same buddy.
  // this function will only warn once for every newly added cookie.
  // Connection will remove its cookie from list in its Destructor.
  FMustSendMultipleConnWarning := Client.CookieList.Add(ID, ACookie);
  FReceivedCookie := ACookie;

  // if we are connected already we can send it
  // immediately, otherwise it will happen on connect
  if Assigned(ConnOutgoing) then
    SendPong
  else
    CalcConnectIntervalAfterPing;
end;

procedure TBuddy.ForgetLastPing;
begin
  FReceivedCookie := '';
  FMustSendPong := False;
end;

procedure TBuddy.ResetKeepaliveTimeout;
begin
  FTimeLastStatusReceived := Now;
end;

procedure TBuddy.ResetAllTimes;
begin
  FTimeCreated := Now;
  FTimeLastStatusSent := Now;
  FTimeLastDisconnect := Now;
  FTimeLastStatusReceived := Now;
  FTimeLastPingReceived := Now;
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
  ResetAllTimes;
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

function TBuddy.LocalAlias: String;
begin
  Result := FLocalAlias;
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

function TBuddy.MaySendText: Boolean;
begin
  Result := IsFullyConnected and FPongAlreadySent;
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
  FPongAlreadySent := False;
  if AConn <> FConnOutgoing then begin
    if Assigned(AConn) then begin
      FConnOutgoing := AConn;
      AConn.SetBuddy(Self);
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
  ResetKeepaliveTimeout;
  if Self in Client.Roster then
    if (AStatus in [TORCHAT_OFFLINE, TORCHAT_CONNECTING])
    or MaySendText then
      Client.OnBuddyStatusChange(Self); // only notify the GUI if we can send
end;

procedure TBuddy.SetLocalAlias(AName: String);
begin
  FLocalAlias := AName;
  Client.Roster.Save;
  Client.OnBuddyAliasChange(Self);
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
  if (LocalAlias = '') and (ProfileName <> '') then begin
    SetLocalAlias(FProfileName);
  end;
end;

procedure TBuddy.SetProfileText(AText: String);
begin
  FProfileText := AText;
end;

function TBuddy.SendIM(AText: String): Boolean;
var
  Msg: IProtocolMessage;
begin
  if MaySendText then begin
    Msg := TMsgMessage.Create(Self, AText);
    Msg.Send;
    Result := True;
  end
  else begin
    WriteLnF('W could not send IM because %s is not fully connected',
      [ID]);
    Result := False;
  end;
end;

procedure TBuddy.SendPong;
var
  Msg: IProtocolMessage;
  NumCookies: Integer;
  InRoster: Boolean;
begin
  // NumCookies should be 1. If it is more then the other
  // side has multiple clients running with the same ID, in
  // this case we need to send pong for each different ping
  // each time (not applying the pong-already-sent logic)
  NumCookies := Client.CookieList.CountByID(ID);
  if FPongAlreadySent  and (NumCookies = 1) then begin
    WriteLnF('TBuddy.SendPong() %s NOT sending another pong over same connection',
      [ID]);
  end
  else begin
    WriteLnF('TBuddy.SendPong() %s sending pong and status %s',
      [ID, FReceivedCookie]);
    Msg := TMsgPong.Create(Self, FReceivedCookie);
    Msg.Send;

    // send another ping. No infinite loop because we
    // do this only for the very first pong, even if
    // we are in the received-multiple-cookies mode
    if not FPongAlreadySent then begin
      Msg := TMsgPing.Create(Self, FOwnCookie);
      Msg.Send;
    end;

    Msg := TMsgClient.Create(Self, SOFTWARE_NAME);
    Msg.Send;
    Msg := TMsgVersion.Create(Self, SOFTWARE_VERSION);
    Msg.Send;

    // send the huge stuff only if curretly no multiple cookie
    // problem because we want to avoid unnecessary traffic
    if NumCookies = 1 then begin
      SendAvatar;
      SendProfile;
    end;

    InRoster := (Self in Client.Roster);
    if InRoster then
      SendAddMe;

    SendStatus;

    if InRoster then begin
      if FMustSendMultipleConnWarning then begin
        WriteLnF('W sending warning to %s (multiple connections)', [ID]);
        Msg := TMsgMessage.Create(Self, SF(
          '[autogenerated warning] received %d separate connections' +
          ' from your ID. Do you have more than one process running' +
          ' with the same TorChat ID? Maybe on a separate computer?' +
          ' Try using task manager if you can''t find it.',
          [NumCookies]
        ));
        Msg.Send;
      end;
    end
    else
      if FMustSendMultipleConnWarning then
        WriteLnF('I could not send warning bcause %s is not on roster', [ID]);

    FMustSendPong := False;
    FReceivedCookie := '';
    FPongAlreadySent := True;
    FMustSendMultipleConnWarning := False;
  end;
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
  FTimeLastStatusSent := Now;
end;

procedure TBuddy.SendAvatar;
var
  Msg: IProtocolMessage;
  RGB: String;
  Alpha: String;
begin
  RGB := Client.Config.GetString('Avatar', True);
  Alpha := Client.Config.GetString('AvatarAlpha', True);
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

procedure TBuddy.SendProfile;
var
  AName, AText: String;
  Msg: IProtocolMessage;
begin
  AName := Client.Config.GetString('ProfileName');
  AText := Client.Config.GetString('ProfileText');
  if AName <> '' then begin
    Msg := TMsgProfileName.Create(Self, AName);
    Msg.Send;
  end;
  if AText <> '' then begin
    Msg := TMsgProfileText.Create(Self, AText);
    Msg.Send;
  end;
end;

end.

