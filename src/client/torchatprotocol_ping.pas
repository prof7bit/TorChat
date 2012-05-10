{ TorChat - Protocol message 'ping' (first message after connection)

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

{ The ping message is the first message that is sent when an outgoing
connection is established. It has the following structure:

ping <sender_id> <random_cookie>

The receiver must connect back to <sender_id> (TorChat always uses two
connections) and respond with a pong message containing the exact same
cookie (See also TMsgPong). The ping/pong mechanism with sending a random
cookie on the outgoing connection and waiting for the same cookie to
return on one of the incoming connections is used to identify the incoming
connections (Incoming connections on Tor are anonymous but both ends must
find a way to make sure that they are really connected to the right person
and this is a simple but effective way to ensure this).
}
unit torchatprotocol_ping;

{$mode objfpc}{$H+}

interface

uses
  torchatabstract,
  torchatprotocol;

type
  { TMsgPing }

  TMsgPing = class(TMsg)
  strict protected
    FID: String;
    FCookie: String;
    procedure Serialize; override;
  public
    class function GetCommand: String; override;
    constructor Create(ABuddy: TABuddy; ACookie: String); reintroduce;
    procedure Parse; override;
    procedure Execute; override;
  end;

implementation

{ TMsgPing }

class function TMsgPing.GetCommand: String;
begin
  Result := 'ping';
end;

constructor TMsgPing.Create(ABuddy: TABuddy; ACookie: String);
begin
  inherited Create(ABuddy);
  FCookie := ACookie;
end;

procedure TMsgPing.Parse;
begin
  FID := PopFirstWord(FBinaryContent);
  FCookie := FBinaryContent;
end;

procedure TMsgPing.Serialize;
begin
  FBinaryContent := FBuddy.ID + ' ' + FCookie;
end;

procedure TMsgPing.Execute;
var
  ABuddy: TABuddy;
begin
  WriteLn('TMsgPing.Execute()');
  Writeln('ID=' + FID + ' cookie=' + FCookie);

  ABuddy := Client.BuddyList.FindBuddy(FID);
  if Assigned(ABuddy) then
    ABuddy.MustSendPong(FCookie)
  else begin
    Writeln('I got Ping from unknown Buddy: ' + FID);
  end;
end;

begin
  RegisterMessageClass(TMsgPing);
end.

