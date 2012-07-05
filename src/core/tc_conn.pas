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
  lNet,
  lEvents,
  tc_interface,
  tc_protocol;

type

  { THiddenConnection }
  THiddenConnection = class(TInterfacedObject, IHiddenConnection)
  strict private
    FPingBuddyID: String;
    FPingCookie: String;
    FKnownBuddyID: String; // used for debug output
    FTimeCreated: TDateTime;
    FSocket: TLSocket;
    FClient: IClient;
    FBuddy: IBuddy;
    FIsOutgoing: Boolean;
    FReceiveBuffer: String;
    FShuttingDown: Boolean;
    FDisconnectLock: TCriticalSection;
    procedure OnTCPFail(ASocket: TLHandle; const Error: String);
    procedure OnReceive(ASocket: TLHandle);
    procedure OnReceivedLine(EncodedLine: String);
  public
    constructor Create(AClient: IClient; ASocket: TLSocket; ABuddy: IBuddy);
    destructor Destroy; override;
    procedure Disconnect;
    procedure Send(AData: String);
    procedure SendLine(AEncodedLine: String);
    procedure SetPingData(AID: String; ACookie: String);
    procedure SetBuddy(ABuddy: IBuddy);
    function IsOutgoing: Boolean;
    function DebugInfo: String;
    function Buddy: IBuddy;
    function Client: IClient;
    function TimeCreated: TDateTime;
    function PingBuddyID: String;
    function PingCookie: String;
  end;

implementation
uses
  tc_misc;

{ THiddenConnection }

constructor THiddenConnection.Create(AClient: IClient; ASocket: TLSocket; ABuddy: IBuddy);
begin
  FDisconnectLock := TCriticalSection.Create;
  FPingBuddyID := '';
  FTimeCreated := Now;
  FSocket := ASocket;
  FSocket.OnRead := @Self.OnReceive;
  FClient := AClient;
  FBuddy := ABuddy;
  FIsOutgoing := Assigned(ABuddy);
  FShuttingDown := False;
  if IsOutgoing then
    WriteLn('<==== connected to', DebugInfo , ' ', ASocket.Handle)
  else
    WriteLn('====> connected from', DebugInfo , ' ', ASocket.Handle);

  // THiddenConnection is reference counted (use it only with
  // variables of type IHiddenConnection and never call
  // Free() directly. There is only one way to get rid of a
  // THiddenConnection object: call the method Disconnect();
  // This will trigger the same events that also happen when
  // the TCP connection fails: It will call the buddie's
  // OnXxxxFail method and not return before the callbacks are
  // all done and then let it free automatically when refcount
  // reaches zero.
end;

destructor THiddenConnection.Destroy;
var
  Info: String;
begin
  Info := DebugInfo;
  FDisconnectLock.Free;
  Client.CookieList.Remove(FPingCookie);
  inherited Destroy;
  WriteLn('THiddenConnection.Destroy() finished ' + Info);
end;

procedure THiddenConnection.OnTCPFail(ASocket: TLHandle; const Error: String);
begin
  // I'm wrapping this method into _AddRef/_Release so that the
  // destruction due to refcount=0 won't catch us somewhere in
  // the middle of the following code, I am delaying this until
  // the very end of this method.
  Self._AddRef;
  FDisconnectLock.Acquire;
  if not FShuttingDown then begin
    FShuttingDown := True;
    WriteLn('<=/=> disconnect ' + DebugInfo + ' ' + Error);

    //no more callbacks
    FSocket.OnRead := @Client.DummySocketEvent;
    FSocket.OnWrite := @Client.DummySocketEvent;
    FSocket.OnError := @Client.DummySocketError;
    FSocket.Disconnect();

    // remove references to the connection in all other objects.
    if Assigned(FBuddy) then begin
      if IsOutgoing then
        FBuddy.SetOutgoing(nil)
      else begin
        FBuddy.SetIncoming(nil);
      end;
      FBuddy := nil;
    end
    else
      FClient.UnregisterAnonConnection(Self);
  end;

  // it will free itself when the reference counter is zero.
  // This is the case when the conection closes itself because
  // the other side hung up. If we are doing the disconnect
  // ourselves then it will happen a little later when the
  // local variables in TBuddy.DoDisconect go out of scope.
  FDisconnectLock.Release;
  Self._Release;
end;

procedure THiddenConnection.Disconnect;
begin
  FDisconnectLock.Acquire;
  if not FShuttingDown then begin
    OnTCPFail(FSocket, 'forced');
  end;
  FDisconnectLock.Release;
end;

procedure THiddenConnection.OnReceive(ASocket: TLHandle);
var
  B: String;
  N: Integer;
  P: Integer;
  L: String;
begin
  _AddRef;
  FSocket.SetState(ssCanReceive); // hack-around! Why is this necessary?
  N := FSocket.GetMessage(B);
  //writeln(_F('THiddenConnection.OnReceive() %s %d Bytes', [DebugInfo, N]));
  if N > 0 then begin
    FReceiveBuffer := FReceiveBuffer + B;
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
  _Release;
end;

procedure THiddenConnection.OnReceivedLine(EncodedLine: String);
var
  Command: String;
  MsgClass: TMsgClass;
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

  MsgClass := GetMsgClassFromCommand(Command);
  if IsOutgoing = MsgClass.ReceiveOnOutgoing then begin
    Msg := MsgClass.Create(Self, Command, EncodedLine);
    try
      Msg.Parse;
      Client.Queue.Put(Msg);
    except
      on Ex: Exception do begin
        WriteLnF('E error while parsing protocol command ''%s'': %s %s',
          [Command, Ex.ToString, Ex.Message]);
      end;
    end;
  end
  else begin
    WriteLnF('W received %s on wrong connection %s, ignoring.',
      [Command, DebugInfo]);
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

procedure THiddenConnection.SetPingData(AID: String; ACookie: String);
begin
  FPingBuddyID := AID;
  FPingCookie := ACookie;
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

function THiddenConnection.PingCookie: String;
begin
  Result := FPingCookie;
end;

end.

