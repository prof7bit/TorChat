{ TorChat - Protocol message 'pong' (response to ping message)

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
unit torchatprotocol_pong;

{$mode objfpc}{$H+}

interface

uses
  torchatabstract,
  torchatprotocol;

type
  { TMsgPong }

  TMsgPong = class(TMsg)
  strict protected
    FCookie: String;
    procedure Serialize; override;
  public
    constructor Create(ABuddy: TABuddy; ACookie: String); reintroduce;
    procedure Parse; override;
    procedure Execute; override;
    class function GetCommand: String; override;
  end;


implementation

{ TMsgPong }

constructor TMsgPong.Create(ABuddy: TABuddy; ACookie: String);
begin
  inherited Create(ABuddy);
  FCookie := ACookie;
end;

procedure TMsgPong.Serialize;
begin
  FBinaryContent := FCookie;
end;

procedure TMsgPong.Parse;
begin
  FCookie := FBinaryContent;
end;

procedure TMsgPong.Execute;
var
  ABuddy: TABuddy;
begin
  WriteLn('TMsgPong.Execute');
  WriteLn(FCookie);
  ABuddy := Client.BuddyList.FindBuddyByCookie(FCookie);
  if Assigned(ABuddy) then begin
    ABuddy.SetIncoming(FConnection);
    ABuddy.OnIncomingConnection;
  end;
end;

class function TMsgPong.GetCommand: String;
begin
  Result := 'pong';
end;

begin
  RegisterMessageClass(TMsgPong);
end.

