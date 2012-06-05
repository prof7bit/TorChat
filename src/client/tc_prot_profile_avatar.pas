{ TorChat - Protocol message 'profile_avatar' (user defined buddy icon)

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
unit tc_prot_profile_avatar;

{$mode objfpc}{$H+}

interface

uses
  tc_interface,
  tc_protocol;

type
  { TMsgProfileAvatar

    This message is sent after the ping/pong handshake to
    transport a bitmp of a customized buddy icon. It has
    the following structure:

      pofile_avatar <data>

    <data>  12288 bytes binary uncompressed RGB bitmap

    This message is sent after 'profile_avatar_alpha',
    these two messages are always sent together. If the
    icon has no alpha channel then the profile_avatar_alpha
    message will be empty. If there is no buddy icon at
    all then no avatar messages are sent at all.

    The size of the bitmap is always the same, therefore
    this message always has 12288 bytes. No other sizes
    allowed.
  }
  TMsgProfileAvatar = class(TMsg)
  strict protected
    FBitmap: String;
    procedure Serialize; override;
  public
    constructor Create(ABuddy: IBuddy; AAvatarData: String); reintroduce;
    class function GetCommand: String; override;
    procedure Parse; override;
    procedure Execute; override;
  end;


implementation
uses
  tc_misc;

{ TMsgProfileAvatar }

class function TMsgProfileAvatar.GetCommand: String;
begin
  Result := 'profile_avatar';
end;

procedure TMsgProfileAvatar.Serialize;
begin
  FBinaryContent := FBitmap;
end;

constructor TMsgProfileAvatar.Create(ABuddy: IBuddy; AAvatarData: String);
begin
  inherited Create(ABuddy);
  FBitmap := AAvatarData;
end;

procedure TMsgProfileAvatar.Parse;
begin
  if Length(FBinaryContent) = 12288 then
    FBitmap := FBinaryContent
  else begin
    FBitmap := '';
    FParseError := _F(
      'invalid avatar data (%d bytes)',
      [Length(FBinaryContent)]
    );
  end;
end;

procedure TMsgProfileAvatar.Execute;
var
  Buddy: IBuddy;
begin
  Buddy := FConnection.Buddy;
  if not Assigned(Buddy) then
    LogWarningAndIgnore
  else begin
    WriteLn('TMsgProfileAvatar.Execute() profile_avatar from ' + Buddy.ID);
    if FParseError <> '' then
      WriteLn('W ', FParseError, ' from ', Buddy.ID);
    Buddy.SetAvatarData(FBitmap);
  end;
end;

begin
  RegisterMessageClass(TMsgProfileAvatar);
end.

