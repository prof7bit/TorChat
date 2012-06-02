{ TorChat - Protocol message 'version' (version of the client software)

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
unit tc_prot_version;

{$mode objfpc}{$H+}

interface

uses
  tc_interface,
  tc_protocol;

type
  { TMsgVersion

    This message is sent after the ping/pong handshake
    and after the 'client' message to tell the other
    side the version of the client software. This
    message is optional but all clients usually
    implement it. It has the following structure:

      version <versionnumber>

    <versionnumber>  a string with the version number

    When receiving this message we store the version
    number temporarily (we don't persist it) and the
    GUI may show it as additional information along
    with the client software name for each buddy in
    a mouse-over hint window or in a separate info-
    dialog.
  }
  TMsgVersion = class(TMsg)
  strict protected
    FSoftwareVersion: String;
    procedure Serialize; override;
  public
    constructor Create(ABuddy: IBuddy; ASoftwareVersion: String); reintroduce;
    class function GetCommand: String; override;
    procedure Parse; override;
    procedure Execute; override;
  end;


implementation

{ TMsgVersion }

class function TMsgVersion.GetCommand: String;
begin
  Result := 'version';
end;

procedure TMsgVersion.Serialize;
begin
  FBinaryContent := FSoftwareVersion;
end;

constructor TMsgVersion.Create(ABuddy: IBuddy; ASoftwareVersion: String);
begin
  inherited Create(ABuddy);
  FSoftwareVersion := ASoftwareVersion;
end;

procedure TMsgVersion.Parse;
begin
  FSoftwareVersion := FBinaryContent;
end;

procedure TMsgVersion.Execute;
var
  Buddy: IBuddy;
begin
  Buddy := FConnection.Buddy;
  if not Assigned(Buddy) then
    LogWarningAndIgnore
  else begin
    WriteLn('TMsgVersion.Execute() version ' + FSoftwareVersion + ' from ' + Buddy.ID);
    Buddy.SetSoftwareVersion(FSoftwareVersion);
  end;
end;

begin
  RegisterMessageClass(TMsgVersion);
end.

