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
  Classes, SysUtils, torchatabstract, receiver, networking;

type

  { THiddenConnection }
  THiddenConnection = class(TAHiddenConnection)
    constructor Create(AClient: TAClient; AStream: TTCPStream);
    procedure Send(AData: String); override;
    procedure SendLine(ALine: String); override;
    procedure OnTCPFail; override;
    procedure SetBuddy(ABuddy: TABuddy); override;
    function IsOutgoing: Boolean; override;
  strict private
    function DebugInfo: String;
  end;

implementation

{ THiddenConnection }

constructor THiddenConnection.Create(AClient: TAClient; AStream: TTCPStream);
begin
  FTCPStream := AStream;
  FClient := AClient;
  FBuddy := nil; // will be set later when connection is assigned to a buddy
  FReceiver := TReceiver.Create(Self); // this will have FreeOnTerminate=True
  WriteLn('created connection');
  // This object will free all its stuff in OnTCPFail().
  // Never call Connection.Free() directly. There is only one method
  // to get rid of a THiddenConnection object: call the method
  // Connection.Stream.DoClose(); This will trigger the same events
  // that also happen when the TCP connection fails: It will call the
  // buddie's OnXxxxFail method and then free itself automatically.
end;

procedure THiddenConnection.Send(AData: String);
begin
  FTCPStream.Write(AData[1], Length(AData));
end;

procedure THiddenConnection.SendLine(ALine: String);
begin
  Send(ALine + #10);
end;

procedure THiddenConnection.OnTCPFail;
begin
  WriteLn('THiddenConnection.OnTCPFail()' + DebugInfo);
  if Assigned(FBuddy) then begin
    if IsOutgoing then
      FBuddy.OnOutgoingConnectionFail
    else
      FBuddy.OnIncomingConnectionFail;
  end;
  FTCPStream.Free;
  Self.Free;
end;

procedure THiddenConnection.SetBuddy(ABuddy: TABuddy);
begin
  FBuddy := ABuddy;
end;

function THiddenConnection.IsOutgoing: Boolean;
begin
  Result := Assigned(FBuddy) and (FBuddy.ConnOutgoing = self);
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
    Result := ' (unknown incoming)';
end;

end.

