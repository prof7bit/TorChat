{ TorChat - Protocol message 'add_me' (request being added to the list)

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
unit tc_prot_add_me;

{$mode objfpc}{$H+}

interface

uses
  tc_interface,
  tc_protocol;

type
  { TMsgAddMe

    This message is sent after the ping/pong handshake to request
    being added to the other client's buddy list. It has the
    following structure:

      add_me

    This message does not transport any additional data.

    When receiving this message we add the buddy to our
    buddy list (We do not automatically add all connecting buddies
    to our list, it will also be possible to connect for the
    purpose of group chat or other services, so we need a separate
    step between connecting and adding to the list)
  }
  TMsgAddMe = class(TMsg)
  strict protected
    procedure Serialize; override;
  public
    class function GetCommand: String; override;
    procedure Parse; override;
    procedure Execute; override;
  end;


implementation

{ TMsgAddMe }

class function TMsgAddMe.GetCommand: String;
begin
  Result := 'add_me';
end;

procedure TMsgAddMe.Serialize;
begin
  //
end;

procedure TMsgAddMe.Parse;
begin
  //
end;

procedure TMsgAddMe.Execute;
var
  Buddy: IBuddy;
begin
  Buddy := FConnection.Buddy;
  if not Assigned(Buddy) then
    LogWarningAndIgnore
  else begin
    WriteLn('TMsgAddMe.Execute() add_me from ' + Buddy.ID);
    if Buddy in FClient.TempList then begin
      FClient.TempList.RemoveBuddy(Buddy);
      FClient.Roster.AddBuddy(Buddy);
      Buddy.SendStatus; // one cannot send too many status messages ;-)
    end;
  end;
end;

begin
  RegisterMessageClass(TMsgAddMe);
end.

