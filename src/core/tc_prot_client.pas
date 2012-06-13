{ TorChat - Protocol message 'client' (name of the client software)

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
unit tc_prot_client;

{$mode objfpc}{$H+}

interface

uses
  tc_interface,
  tc_protocol;

type
  { TMsgClient

    This message is sent after the ping/pong handshake to
    tell the other side the name of the client software.
    This message is optional but all clients usually
    implement it. It has the following structure

      client <name>

    <name>  is the name of the client software

    When receiving this message we store the client name
    temporarily (we don't persist it) and the GUI may
    show it as additional information for each buddy
    in a mouse-over hint window or in a separate info-
    dialog.
  }
  TMsgClient = class(TMsg)
  strict protected
    FSoftwareName: String;
    procedure Serialize; override;
  public
    constructor Create(ABuddy: IBuddy; ASoftwareName: String); reintroduce;
    class function GetCommand: String; override;
    procedure Parse; override;
    procedure Execute; override;
  end;


implementation

{ TMsgClient }

class function TMsgClient.GetCommand: String;
begin
  Result := 'client';
end;

procedure TMsgClient.Serialize;
begin
  FBinaryContent := FSoftwareName;
end;

constructor TMsgClient.Create(ABuddy: IBuddy; ASoftwareName: String);
begin
  inherited Create(ABuddy);
  FSoftwareName := ASoftwareName;
end;

procedure TMsgClient.Parse;
begin
  FSoftwareName := FBinaryContent;
end;

procedure TMsgClient.Execute;
var
  Buddy: IBuddy;
begin
  Buddy := FConnection.Buddy;
  if not Assigned(Buddy) then
    LogWarningAndIgnore
  else
    Buddy.SetSoftware(FSoftwareName);
end;

begin
  RegisterMessageClass(TMsgClient);
end.

