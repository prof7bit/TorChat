{ TorChat - Protocol message 'profile_name' (user defined alias)

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
unit tc_prot_profile_name;

{$mode objfpc}{$H+}

interface

uses
  tc_interface,
  tc_protocol;

type
  { TMsgProfileName

  This message is sent after the ping/pong handshake to
  transport a user defined profile alias. It has the
  following structure:

    profile_name <data>

  <data>  a short utf8 string of the buddie's name

  When receiving this message and the user has not
  yet gotten an alias assigned then we set the alias
  to this name. It will also be shown in the userinfo
  popup as additional information for this buddy.
  }
  TMsgProfileName = class(TMsg)
  strict protected
    FName: String;
    procedure Serialize; override;
    procedure ExecuteWithBuddy; override;
  public
    constructor Create(ABuddy: IBuddy; AText: String); reintroduce;
    class function GetCommand: String; override;
    procedure Parse; override;
  end;


implementation
uses
  tc_misc;

{ TMsgProfileName }

class function TMsgProfileName.GetCommand: String;
begin
  Result := 'profile_name';
end;

procedure TMsgProfileName.Serialize;
begin
  FBinaryContent := LineBreaksAnyToSpace(FName);
end;

constructor TMsgProfileName.Create(ABuddy: IBuddy; AText: String);
begin
  inherited Create(ABuddy);
  FName := AText;
end;

procedure TMsgProfileName.Parse;
begin
  FName := LineBreaksAnyToSpace(FBinaryContent);
end;

procedure TMsgProfileName.ExecuteWithBuddy;
begin
  FBuddy.SetProfileName(FName);
end;

begin
  RegisterMessageClass(TMsgProfileName);
end.

