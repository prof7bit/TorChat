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
  Sockets,
  lNet,
  lEvents,
  tc_interface,
  tc_protocol;

type

  { THiddenConnection }
  THiddenConnection = class(TInterfacedObject, IHiddenConnection)
  strict private
    FPingBuddyID: String;
    FKnownBuddyID: String; // used for debug output
    FTimeCreated: TDateTime;
    FSocket: TLSocket;
    FClient: IClient;
    FBuddy: IBuddy;
    FIsOutgoing: Boolean;
    FReceiveBuffer: String;
    procedure OnTCPFail(ASocket: TLHandle; const Error: String);
    procedure OnReceive(ASocket: TLHandle);
    procedure OnReceivedLine(EncodedLine: String);
  public
    constructor Create(AClient: IClient; ASocket: TLSocket; ABuddy: IBuddy);
    destructor Destroy; override;
    procedure Disconnect;
    procedure Send(AData: String);
    procedure SendLine(AEncodedLine: String);
    procedure SetPingBuddyID(AID: String);
    procedure SetBuddy(ABuddy: IBuddy);
    function IsOutgoing: Boolean;
    function DebugInfo: String;
    function Buddy: IBuddy;
    function Client: IClient;
    function TimeCreated: TDateTime;
    function PingBuddyID: String;
  end;

implementation
uses
  tc_misc;

{ THiddenConnection }

constructor THiddenConnection.Create(AClient: IClient; ASocket: TLSocket; ABuddy: IBuddy);
begin
  FPingBuddyID := '';
  FTimeCreated := Now;
  FSocket := ASocket;
  FSocket.OnRead := @Self.OnReceive;
  FSocket.OnError := @Self.OnTCPFail;
  FClient := AClient;
  FBuddy := ABuddy;
  FIsOutgoing := Assigned(ABuddy);
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
  Info := DebugInfo;
  FSocket.Free;
  inherited Destroy;
  WriteLn('THiddenConnection.Destroy() finished ' + Info);
end;

procedure THiddenConnection.OnTCPFail(ASocket: TLHandle; const Error: String);
begin
  WriteLn('THiddenConnection.OnTCPFail()' + DebugInfo + ' ' + Error);

  //no more callbacks
  ASocket.OnRead := nil;
  ASocket.OnError := nil;

  // remove all references
  if Assigned(FBuddy) then begin
    if IsOutgoing then
      FBuddy.SetOutgoing(nil)
    else
      FBuddy.SetIncoming(nil);
    FBuddy := nil;
  end;
  Client.UnregisterAnonConnection(Self);

  // it will free itself when the reference counter is zero.
end;

procedure THiddenConnection.OnReceive(ASocket: TLHandle);
var
  B: array[0..1023] of Byte;
  N: Integer;
  P: Integer;
  L: String;
begin
  writeln('THiddenConnection.OnReceive() ');
  //writeln('THiddenConnection.OnReceive() ' + DebugInfo);
  N := fprecv(ASocket.Handle, @B, SizeOf(B)-1, 0);
  if N > 0 then begin
    B[N] := 0;
    FReceiveBuffer := FReceiveBuffer + String(PChar(@B));
    P := Pos(#10, FReceiveBuffer);
    repeat
      L := LeftStr(FReceiveBuffer, P - 1);
      FReceiveBuffer := RightStr(FReceiveBuffer, Length(FReceiveBuffer) - P);
      if L <> '' then
        OnReceivedLine(L);
      P := Pos(#10, FReceiveBuffer);
    until P = 0;
  end
  else begin
    // connection closed on the other side
    OnTCPFail(ASocket, 'recv() returned no data');
  end;
end;

procedure THiddenConnection.Disconnect;
begin
  //FSocket.Disconnect();
  OnTCPFail(FSocket, 'forced');
end;

procedure THiddenConnection.OnReceivedLine(EncodedLine: String);
var
  Command: String;
  Msg: IProtocolMessage;
begin
  try
    Command := PopFirstWord(EncodedLine);
  except
    // reached the end of the line without finding a
    // space, so this is a command without arguments.
    Command := Trim(EncodedLine);
    EncodedLine := '';
  end;

  Msg := GetMsgClassFromCommand(Command).Create(
    Self, Command, EncodedLine);
  try
    Msg.Parse;
    Client.Queue.Put(Msg);
  except
    on Ex: Exception do begin
      WriteLn(_F('E error while parsing protocol command ''%s'': %s %s',
        [Command, Ex.ToString, Ex.Message]));
    end;
  end;
end;

procedure THiddenConnection.Send(AData: String);
begin
  FSocket.Send(AData[1], Length(AData));
end;

procedure THiddenConnection.SendLine(AEncodedLine: String);
begin
  Send(AEncodedLine + #10);
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

function THiddenConnection.TimeCreated: TDateTime;
begin
  Result := FTimeCreated;
end;

function THiddenConnection.PingBuddyID: String;
begin
  Result := FPingBuddyID;
end;

end.

