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
  torchatprotocol_ping,
  torchatprotocol_pong,
  miscfunc,
  connection,
  networking;

type
  { TBuddy }

  TBuddy = class(TABuddy)
  strict protected
    FMustSendPong: Boolean;
    FReceivedCookie: String;
    procedure InitiateConnect; virtual;
    procedure CbNetOut(ATCPStream: TTCPStream; E: Exception); virtual;
    procedure SendPong; virtual;
    function IsFullyConnected: Boolean;
  public
    constructor Create(AOwner: TAClient); reintroduce;
    procedure CheckState; override;
    function AsJsonObject: TJSONObject; override;
    procedure InitFromJsonObect(AObject: TJSONObject); override;
    procedure InitID(AID: String); override;
    procedure SetIncoming(AConn: TAHiddenConnection); override;
    procedure SetOutgoing(AConn: TAHiddenConnection); override;
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
  WriteLn(ID + '.InitiateConnect()');
  FClient.Network.ConnectAsync(self.ID + '.onion', 11009, @self.CbNetOut);
  FStateOut := STATE_TRYING;
end;

procedure TBuddy.CbNetOut(ATCPStream: TTCPStream; E: Exception);
begin
  Output := FClient.StandardOut; // make writeln redirect work in this thread
  WriteLn(ID + '.CbNetOut()');
  if assigned(ATCPStream) then begin
    SetOutgoing(THiddenConnection.Create(FClient, ATCPStream));
    OnOutgoingConnection;
  end
  else begin
    WriteLn(E.Message);
    FLastDisconnect := Now;
    SetOutgoing(nil);
  end;
end;

constructor TBuddy.Create(AOwner: TAClient);
var
  GUID: TGuid;
begin
  inherited Create(AOwner);
  FClient := AOwner;
  FLastDisconnect := 0;
  FMustSendPong := False;
  FReceivedCookie := '';
  FStatus := TORCHAT_OFFLINE;
  FStateOut := STATE_DISCONNECTED;
  FStateIn := STATE_DISCONNECTED;
  CreateGUID(GUID);
  FOwnCookie := GUIDToString(GUID);
  WriteLn('created GUID for buddy ' + FOwnCookie);
end;

procedure TBuddy.CheckState;
begin
  if FStateOut = STATE_DISCONNECTED then begin
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
    // ignore from here on (this all followig fields) because it
    // was created by an older TorChat. Fields have resonable defaults.
  end;
end;

procedure TBuddy.InitID(AID: String);
begin
  FID := AID;
end;

procedure TBuddy.SetIncoming(AConn: TAHiddenConnection);
begin
  FConnIncoming := AConn;
  if Assigned(AConn) then begin
    AConn.Buddy := self;
    FStateIn := STATE_CONNECTED;
  end
  else
    FStateIn := STATE_DISCONNECTED;
end;

procedure TBuddy.SetOutgoing(AConn: TAHiddenConnection);
begin
  FConnOutgoing := AConn;
  if Assigned(AConn) then begin
    AConn.Buddy := self;
    FStateOut := STATE_CONNECTED;
  end
  else
    FStateOut := STATE_DISCONNECTED;
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

procedure TBuddy.OnOutgoingConnection;
var
  Msg: TMsgPing;
begin
  WriteLn(ID + '.OnOutgoingConnection()');
  Msg := TMsgPing.Create(self, FOwnCookie);
  Msg.Send;

  // the other end has connected aleady and already
  // sent us a ping, so we can answer with pong
  if FMustSendPong then
    SendPong;
end;

procedure TBuddy.OnOutgoingConnectionFail;
begin
  WriteLn(ID + '.OnOutgoingConnectionFail()');
  SetOutgoing(nil);
  if Assigned(ConnIncoming) then
    ConnIncoming.Stream.DoClose;
  FLastDisconnect := Now;
end;

procedure TBuddy.OnIncomingConnection;
begin
  Writeln(ID + '.OnIncomingConnection()');
end;

procedure TBuddy.OnIncomingConnectionFail;
begin
  Writeln(ID + '.OnIncomingConnectionFail()');
  SetIncoming(nil);
  if Assigned(ConnOutgoing) then
    ConnOutgoing.Stream.DoClose;
  FLastDisconnect := Now;
end;

procedure TBuddy.MustSendPong(ACookie: String);
begin
  FReceivedCookie := ACookie;
  FMustSendPong := True;

  // if we are connected already we can send it
  // immediately, otherwise it will happen on connect
  if FStateOut = STATE_CONNECTED then
    SendPong;
end;

procedure TBuddy.DoDisconnect;
begin
  if Assigned(ConnIncoming) then ConnIncoming.Stream.DoClose;
  if Assigned(ConnOutgoing) then ConnOutgoing.Stream.DoClose;
end;

end.

