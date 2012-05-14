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
unit connection;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  torchatabstract,
  receiver,
  networking;

type

  { THiddenConnection }
  THiddenConnection = class(TInterfacedObject, TAHiddenConnection)
  strict protected
    FTCPStream: TTCPStream;
    FClient: TAClient;
    FBuddy: IBuddy;
    FReceiver: TAReceiver;
    FDebugInfoDefault: String;
    procedure NotifyOthersAboutDeath; virtual;
  public
    constructor Create(AClient: TAClient; AStream: TTCPStream; ABuddy: IBuddy);
    destructor Destroy; override;
    procedure Send(AData: String); virtual;
    procedure SendLine(AEncodedLine: String); virtual;
    procedure OnTCPFail; virtual;
    procedure DoClose; virtual;
    procedure SetBuddy(ABuddy: IBuddy); virtual;
    function IsOutgoing: Boolean; virtual;
    function DebugInfo: String; virtual;
    function Buddy: IBuddy;
    function Client: TAClient;
    function Stream: TTCPStream;
  end;

implementation
uses
  miscfunc;

{ THiddenConnection }

procedure THiddenConnection.NotifyOthersAboutDeath;
begin
  Client.UnregisterConnection(self);
  if Assigned(FBuddy) then begin
    if IsOutgoing then
      FBuddy.SetOutgoing(nil)
    else
      FBuddy.SetIncoming(nil);
  end;
end;

constructor THiddenConnection.Create(AClient: TAClient; AStream: TTCPStream; ABuddy: IBuddy);
begin
  FTCPStream := AStream;
  FClient := AClient;
  FBuddy := ABuddy;
  FReceiver := TReceiver.Create(Self); // this will have FreeOnTerminate=True
  FDebugInfoDefault := '(unknown incoming)';
  WriteLn('THiddenConnection.Create() ' + DebugInfo);
  // This object will free all its stuff in OnTCPFail().
  // Never call Connection.Free() directly. There is only one way
  // to get rid of a THiddenConnection object: call the method
  // Connection.DoClose(); This will trigger the same events
  // that also happen when the TCP connection fails: It will call the
  // buddie's OnXxxxFail method and then free itself automatically.
end;

destructor THiddenConnection.Destroy;
begin
  WriteLn(MilliTime, ' THiddenConnection.Destroy()');
  inherited Destroy;
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
  NotifyOthersAboutDeath;
  FTCPStream.Free;
end;

procedure THiddenConnection.DoClose;
begin
  WriteLn(MilliTime, ' THiddenConnection.DoClose()' + DebugInfo);
  FDebugInfoDefault := DebugInfo;

  // we are here because the main thread wants to
  // forcefully close and destroy this connection.
  // To avoid any race conditions we will not rely
  // on the receiver thread do the notifications
  // eventually once it detects the TCP connection
  // end while the buddy itself might be gone already,
  // instead we are doing this here ourselves
  // immediately and *before* we actually disconnect.
  NotifyOthersAboutDeath;
  FBuddy := nil;

  // Now since there are no references between buddy
  // and connection anymore we can now safely trigger
  // the shutdown and the receiver thread may then do
  // what it wants because it can not call any TBuddy
  // methods anymore.
  Stream.DoClose;
end;

procedure THiddenConnection.SetBuddy(ABuddy: IBuddy);
begin
  FBuddy := ABuddy;
end;

function THiddenConnection.IsOutgoing: Boolean;
begin
  if not Assigned(FBuddy) then
    Exit(False);
  if FBuddy.ConnOutgoing = TAHiddenConnection(self) then
    Exit(True);
  if (FBuddy.ConnIncoming = nil) and (FBuddy.ConnOutgoing = nil) then
    Exit(True);
  Result := False;
end;

function THiddenConnection.DebugInfo: String;
begin
  if Assigned(FBuddy) then begin
    Result := ' (' + FBuddy.ID;
    if IsOutgoing then
      Result := Result + ' outgoing)'
    else
      Result := Result + ' incoming)';
  end
  else
    Result := FDebugInfoDefault;
end;

function THiddenConnection.Buddy: IBuddy;
begin
  Result := FBuddy;
end;

function THiddenConnection.Client: TAClient;
begin
  Result := FClient;
end;

function THiddenConnection.Stream: TTCPStream;
begin
  Result := FTCPStream;
end;

end.

