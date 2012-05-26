{ TorChat - Connection class, representing a connection via Tor

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
unit tc_conn;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  syncobjs,
  tc_interface,
  tc_conn_rcv,
  tc_sock;

type

  { THiddenConnection }
  THiddenConnection = class(TInterfacedObject, IHiddenConnection)
  strict private
    FPingBuddyID: String;
    FKnownBuddyID: String; // used for debug output
    FTimeCreated: TDateTime;
    FTCPStream: TTCPStream;
    FClient: IClient;
    FBuddy: IBuddy;
    FIsOutgoing: Boolean;
    FReceiver: TAReceiver;
    FCallbackDone: TSimpleEvent;
  public
    constructor Create(AClient: IClient; AStream: TTCPStream; ABuddy: IBuddy);
    destructor Destroy; override;
    procedure Disconnect;
    procedure Send(AData: String);
    procedure SendLine(AEncodedLine: String);
    procedure OnTCPFail;
    procedure SetPingBuddyID(AID: String);
    procedure SetBuddy(ABuddy: IBuddy);
    function IsOutgoing: Boolean;
    function DebugInfo: String;
    function Buddy: IBuddy;
    function Client: IClient;
    function Stream: TTCPStream;
    function TimeCreated: TDateTime;
    function PingBuddyID: String;
  end;

implementation

{ THiddenConnection }

constructor THiddenConnection.Create(AClient: IClient; AStream: TTCPStream; ABuddy: IBuddy);
begin
  FCallbackDone := TSimpleEvent.Create();
  FPingBuddyID := '';
  FTimeCreated := Now;
  FTCPStream := AStream;
  FClient := AClient;
  FBuddy := ABuddy;
  FIsOutgoing := Assigned(ABuddy);
  FReceiver := TReceiver.Create(Self);
  WriteLn('THiddenConnection.Create() ' + DebugInfo);
  // This object will free all its stuff in OnTCPFail().
  // Never call Connection.Free() directly. There is only one way
  // to get rid of a THiddenConnection object: call the method
  // Connection.Disconnect(); This will trigger the same events
  // that also happen when the TCP connection fails: It will call the
  // buddie's OnXxxxFail method and *wait* until the callbacks are
  // all done and then let it free automatically.
end;

destructor THiddenConnection.Destroy;
var
  Info: String;
begin
  // FReceiver has FreeOnTerminate
  // so we don't need to free it manually.
  Info := DebugInfo;
  FTCPStream.Free;
  FCallbackDone.Free;
  inherited Destroy;
  WriteLn('THiddenConnection.Destroy() finished ' + Info);
end;

procedure THiddenConnection.Disconnect;
begin
  // this will trigger the OnTCPFail callback which will
  // remove all references between connection and buddy,
  // the reference counting will then free the object.
  FTCPStream.DoClose;
  FCallbackDone.WaitFor(INFINITE);
end;

procedure THiddenConnection.Send(AData: String);
begin
  FTCPStream.Write(AData[1], Length(AData));
end;

procedure THiddenConnection.SendLine(AEncodedLine: String);
begin
  Send(AEncodedLine + #10);
end;

procedure THiddenConnection.OnTCPFail;
begin
  WriteLn('THiddenConnection.OnTCPFail()' + DebugInfo);
  Client.UnregisterAnonConnection(Self);
  if Assigned(FBuddy) then begin
    if IsOutgoing then
      FBuddy.SetOutgoing(nil)
    else
      FBuddy.SetIncoming(nil);
    FBuddy := nil;
  end;
  FCallbackDone.SetEvent;
  // it will free itself when the reference counter is zero.
end;

procedure THiddenConnection.SetPingBuddyID(AID: String);
begin
  FPingBuddyID := AID;
end;

procedure THiddenConnection.SetBuddy(ABuddy: IBuddy);
begin
  FBuddy := ABuddy;
  FKnownBuddyID := ABuddy.ID;
end;

function THiddenConnection.IsOutgoing: Boolean;
begin
  Result := FIsOutgoing;
end;

function THiddenConnection.DebugInfo: String;
var
  ID : String;
begin
  // seems complicated and serves no other pupose than producing
  // some meaningful debug messages but has helped a lot already.
  if Assigned(FBuddy) then
    ID := FBuddy.ID
  else
    if FKnownBuddyID <> '' then
      ID := FKnownBuddyID
    else
      if FPingBuddyID <> '' then
        ID := 'allegedly from ' + FPingBuddyID
      else
        ID := 'unknown';
  Result := ' (' + ID;
  if IsOutgoing then
    Result := Result + ' outgoing)'
  else
    Result := Result + ' incoming)';
end;

function THiddenConnection.Buddy: IBuddy;
begin
  Result := FBuddy;
end;

function THiddenConnection.Client: IClient;
begin
  Result := FClient;
end;

function THiddenConnection.Stream: TTCPStream;
begin
  Result := FTCPStream;
end;

function THiddenConnection.TimeCreated: TDateTime;
begin
  Result := FTimeCreated;
end;

function THiddenConnection.PingBuddyID: String;
begin
  Result := FPingBuddyID;
end;

end.

