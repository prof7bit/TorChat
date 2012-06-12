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
unit tc_prot_pong;

{$mode objfpc}{$H+}

interface

uses
  tc_interface,
  tc_protocol;

type
  { TMsgPong

    The pong message must be sent as a response to the ping message.
    It is the second message of the handshake. It has the following
    structure:

      pong <cookie>

    <cookie>  is the exact same string that has been received in
              the ping message.

    When the pong message is received then we search through all
    our buddies to find the one that has previously sent out a
    ping with the same cookie. When we find it then we know beyond
    any doubt that this incoming connection belongs to this buddy,
    we assign the incoming connection to this buddy and the buddy
    should now have both connections (in and out) established, the
    handshake is completed, the buddy status is "online".

    When the cookie does not match any of our buddies or something
    else is wrong (buddy has already ConnIncoming assigned, etc.)
    then we ignore the message and close the connection.
  }
  TMsgPong = class(TMsg)
  strict protected
    FCookie: String;
    procedure Serialize; override;
  public
    class function GetCommand: String; override;
    constructor Create(ABuddy: IBuddy; ACookie: String); reintroduce;
    procedure Parse; override;
    procedure Execute; override;
  end;


implementation

{ TMsgPong }

class function TMsgPong.GetCommand: String;
begin
  Result := 'pong';
end;

constructor TMsgPong.Create(ABuddy: IBuddy; ACookie: String);
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
  Buddy: IBuddy;
begin
  Buddy := FClient.Roster.ByCookie(FCookie);
  if not Assigned(Buddy) then
    Buddy := FClient.TempList.ByCookie(FCookie);
  if Assigned(Buddy) then
    Buddy.SetIncoming(FConnection)
  else
    LogWarningAndIgnore('unknown cookie');
end;

begin
  RegisterMessageClass(TMsgPong);
end.

