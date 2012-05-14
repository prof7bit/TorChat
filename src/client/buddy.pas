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
unit buddy;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpjson,
  torchatabstract,
  protocol_ping,
  protocol_pong,
  miscfunc,
  connection,
  networking,
  internalmessage;

type
  TMethod = procedure of object;

  { TBuddy }

  TBuddy = class(TABuddy)
  strict protected
    FMustSendPong: Boolean;
    FReceivedCookie: String;
    FConnectThread: TAsyncConnectThread;
    procedure InitiateConnect; virtual;
    procedure CbNetOut(ATCPStream: TTCPStream; E: Exception); virtual;
    procedure SendPong; virtual;
    function IsFullyConnected: Boolean;
    procedure CallFromMainThread(AMethod: TMethod); virtual;
  public
    constructor Create(AOwner: TAClient); reintroduce;
    destructor Destroy; override;
    procedure CheckState; override;
    function AsJsonObject: TJSONObject; override;
    procedure InitFromJsonObect(AObject: TJSONObject); override;
    procedure InitID(AID: String); override;
    procedure SetIncoming(AConn: TAHiddenConnection); override;
    procedure SetOutgoing(AConn: TAHiddenConnection); override;
    procedure SetStatus(AStatus: TTorchatStatus); override;
    procedure OnOutgoingConnection; override;
    procedure OnOutgoingConnectionFail; override;
    procedure OnIncomingConnection; override;
    procedure OnIncomingConnectionFail; override;
    procedure MustSendPong(ACookie: String); override;
    procedure DoDisconnect; override;
  end;

implementation

{ TBuddy }

procedure TBuddy.InitiateConnect;
begin
  WriteLn('TBuddy.InitiateConnect() ' + ID);
  FConnectThread := FClient.Network.ConnectAsync(self.ID + '.onion', 11009, @self.CbNetOut);
end;

procedure TBuddy.CbNetOut(ATCPStream: TTCPStream; E: Exception);
begin
  WriteLn(MilliTime, ' TBuddy.CbNetOut() ' + ID);
  FConnectThread := nil;
  if assigned(ATCPStream) then begin
    ConnOutgoing := THiddenConnection.Create(FClient, ATCPStream, Self);
  end
  else begin
    WriteLn(E.Message);
    FLastDisconnect := Now;
    ConnOutgoing := nil;
  end;
end;

constructor TBuddy.Create(AOwner: TAClient);
var
  GUID: TGuid;
begin
  inherited Create(AOwner);
  FConnectThread := nil;
  FClient := AOwner;
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
  if Assigned(FConnectThread) then begin
    WriteLn(MilliTime, ' TBuddy.Destroy() ' + ID + ' there is an ongoing connection attempt, terminating it.');
    FConnectThread.Terminate;
    WriteLn(MilliTime, ' TBuddy.Destroy() ' + ID + ' connection attempt terminated.');
  end;
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

procedure TBuddy.SetIncoming(AConn: TAHiddenConnection);
begin
  if AConn <> FConnIncoming then begin
    FConnIncoming := AConn;
    if Assigned(AConn) then begin
      AConn.Buddy := self;
      CallFromMainThread(@OnIncomingConnection);
    end
    else
      CallFromMainThread(@OnIncomingConnectionFail);
  end;
end;

procedure TBuddy.SetOutgoing(AConn: TAHiddenConnection);
begin
  if AConn <> FConnOutgoing then begin
    FConnOutgoing := AConn;
    if Assigned(AConn) then begin
      AConn.Buddy := self;
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
    Client.OnBuddyStatusChange(self);
  end;
end;

procedure TBuddy.SendPong;
var
  Pong: TMsgPong;
begin
  Pong := TMsgPong.Create(Self, FReceivedCookie);
  Pong.Send;
  FMustSendPong := False;
  FReceivedCookie := '';
end;

function TBuddy.IsFullyConnected: Boolean;
begin
  Result := Assigned(ConnOutgoing) and Assigned(ConnIncoming);
end;

procedure TBuddy.CallFromMainThread(AMethod: TMethod);
var
  Msg: TMsgCallMethod;
begin
  if ThreadID = Client.MainThread then
    AMethod()
  else begin
    Msg := TMsgCallMethod.Create;
    Msg.Method := AMethod;
    Client.Enqueue(Msg);
  end
end;

procedure TBuddy.OnOutgoingConnection;
var
  Msg: TMsgPing;
begin
  WriteLn('TBuddy.OnOutgoingConnection() ' + ID);
  Msg := TMsgPing.Create(self, FOwnCookie);
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
    ConnIncoming.DoClose;
  Status := TORCHAT_OFFLINE;
end;

procedure TBuddy.OnIncomingConnection;
begin
  Writeln('TBuddy.OnIncomingConnection() ' + ID);
  Status := TORCHAT_AVAILABLE;
end;

procedure TBuddy.OnIncomingConnectionFail;
begin
  Writeln('TBuddy.OnIncomingConnectionFail() ' + ID);
  FLastDisconnect := Now;
  if Assigned(ConnOutgoing) then
    ConnOutgoing.DoClose;
  Status := TORCHAT_OFFLINE;
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
    if Assigned(ConnIncoming) then ConnIncoming.DoClose;
    if Assigned(ConnOutgoing) then ConnOutgoing.DoClose;
  end;
end;

end.

