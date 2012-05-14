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
  THiddenConnection = class(TAHiddenConnection)
  strict protected
    FDebugInfoDefault: String;
    procedure NotifyOthersAboutDeath; virtual;
  public
    constructor Create(AClient: TAClient; AStream: TTCPStream; ABuddy: TABuddy);
    procedure Send(AData: String); override;
    procedure SendLine(AEncodedLine: String); override;
    procedure OnTCPFail; override;
    procedure DoClose; override;
    procedure SetBuddy(ABuddy: TABuddy); override;
    function IsOutgoing: Boolean; override;
    function DebugInfo: String; override;
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
      FBuddy.ConnOutgoing := nil
    else
      FBuddy.ConnIncoming := nil;
  end;
end;

constructor THiddenConnection.Create(AClient: TAClient; AStream: TTCPStream; ABuddy: TABuddy);
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
  Self.Free;
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
  // end, instead we are doing it here ourselves
  // immeditely and only *after* this is completed
  // we initiate the actual disconnect.
  NotifyOthersAboutDeath;

  // now we remove the reference to the buddy and
  // initiate disconnect. Now since there are no
  // references between buddy and connection anymore
  // the receiver thread can't interfere with anything
  // anymore, it will soon terminate and free itself
  // and this object silently in the background.
  FBuddy := nil;
  Stream.DoClose;
end;

procedure THiddenConnection.SetBuddy(ABuddy: TABuddy);
begin
  FBuddy := ABuddy;
end;

function THiddenConnection.IsOutgoing: Boolean;
begin
  if not Assigned(FBuddy) then
    Exit(False);
  if FBuddy.ConnOutgoing = self then
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

end.

