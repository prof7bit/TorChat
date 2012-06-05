{ TorChat - Protocol message 'avatar_alpha' (alpha channel for 'profile_avatar')

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
unit tc_prot_profile_avatar_alpha;

{$mode objfpc}{$H+}

interface

uses
  tc_interface,
  tc_protocol;

type
  { TMsgProfileAvatarAlpha

  This message is sent after the ping/pong handshake to
  transport a bitmp of a customized buddy icon. It has
  the following structure:

    profile_avatar_alpha <data>

  <data>  4096 bytes binary uncompressed alpha bitmap
          or empty (0 bytes) if no alpha channel

  This message is sent before the 'profile_avatar'
  message. If the icon has no alpha channel then the
  data must be empty (0 bytes, message without payload),
  but the message may not be omitted. Either the
  'avatar_alpha' and 'avatar' are both sent or none
  of them at all.

  The size of the bitmap is always the same, therefore
  this message either has 4096 bytes or 0 bytes, no
  other sizes allowed.
  }
  TMsgProfileAvatarAlpha = class(TMsg)
  strict protected
    FBitmap: String;
    procedure Serialize; override;
  public
    constructor Create(ABuddy: IBuddy; AAvatarAlpha: String); reintroduce;
    class function GetCommand: String; override;
    procedure Parse; override;
    procedure Execute; override;
  end;


implementation
uses
  tc_misc;

{ TMsgProfileAvatarAlpha }

class function TMsgProfileAvatarAlpha.GetCommand: String;
begin
  Result := 'profile_avatar_alpha';
end;

procedure TMsgProfileAvatarAlpha.Serialize;
begin
  FBinaryContent := FBitmap;
end;

constructor TMsgProfileAvatarAlpha.Create(ABuddy: IBuddy; AAvatarAlpha: String);
begin
  inherited Create(ABuddy);
  FBitmap := AAvatarAlpha;
end;

procedure TMsgProfileAvatarAlpha.Parse;
begin
  if Length(FBinaryContent) = 4096 then
    FBitmap := FBinaryContent
  else begin
    FBitmap := '';
    if Length(FBinaryContent) <> 0 then begin // 0 is also allowed
      FParseError := _F(
        'invalid avatar alpha data (%d bytes)',
        [Length(FBinaryContent)]
      );
    end;
  end;
end;

procedure TMsgProfileAvatarAlpha.Execute;
var
  Buddy: IBuddy;
begin
  Buddy := FConnection.Buddy;
  if not Assigned(Buddy) then
    LogWarningAndIgnore
  else begin
    WriteLn('TMsgProfileAvatarAlpha.Execute() profile_avatar_alpha from ' + Buddy.ID);
    if FParseError <> '' then
      WriteLn('W ', FParseError, ' from ', Buddy.ID);
    Buddy.SetAvatarAlphaData(FBitmap);
  end;
end;

begin
  RegisterMessageClass(TMsgProfileAvatarAlpha);
end.

