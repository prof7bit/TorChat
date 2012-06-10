{ TorChat - Protocol message 'not_implemented' (response to unknown command)

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
unit tc_prot_not_implemented;

{$mode objfpc}{$H+}

interface

uses
  tc_interface,
  tc_protocol;

type
  { TMsgNotImplemented

    This message is sent as a response to any unknown
    protocol message. It has the following structure:

      not_implemented <unknowncommand>

    <unknowncommand>  is the name of the command that was
                      not recognized.

    When receiving an unknown command we reply with
    not_implemented, (this is automatically done by
    TMsgDefault) and when receiving the not_implemented
    we do nothing and log a message to the debug log.
  }
  TMsgNotImplemented = class(TMsg)
  strict protected
    FUnknownCommand: String;
    procedure Serialize; override;
  public
    class function GetCommand: String; override;
    constructor Create(ABuddy: IBuddy; ACommand: String); reintroduce;
    procedure Parse; override;
    procedure Execute; override;
  end;


implementation
uses
  tc_misc;

{ TMsgNotImplemented }

class function TMsgNotImplemented.GetCommand: String;
begin
  Result := 'not_implemented';
end;

constructor TMsgNotImplemented.Create(ABuddy: IBuddy; ACommand: String);
begin
  inherited Create(ABuddy);
  FUnknownCommand := ACommand;
end;

procedure TMsgNotImplemented.Serialize;
begin
  FBinaryContent := FUnknownCommand;
end;

procedure TMsgNotImplemented.Parse;
begin
  FUnknownCommand := FBinaryContent;
end;

procedure TMsgNotImplemented.Execute;
begin
  if not Assigned(FConnection.Buddy) then
    LogWarningAndIgnore(FUnknownCommand)
end;

begin
  RegisterMessageClass(TMsgNotImplemented);
end.

