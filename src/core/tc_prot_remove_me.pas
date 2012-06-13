{ TorChat - Protocol message 'remove_me' (request being removed from the list)

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
unit tc_prot_remove_me;

{$mode objfpc}{$H+}

interface

uses
  tc_interface,
  tc_protocol;

type
  { TMsgRemoveMe

    This message is sent when the user has removed a buddy from
    the list, it signals the other side to also remove ourselves
    from his list. It has the following structure:

      remove_me

    This message does not transport any additional data.

    When receiving this message we immediately remove the buddy
    from our buddy list. Older versions of TorChat have also
    disconnected the buddy immediately but we don't do this
    anymore because now there exist other services (for example
    group chat) that are independent from the buddy list.
  }
  TMsgRemoveMe = class(TMsg)
  strict protected
    procedure Serialize; override;
  public
    class function GetCommand: String; override;
    procedure Parse; override;
    procedure Execute; override;
  end;


implementation

{ TMsgRemoveMe }

class function TMsgRemoveMe.GetCommand: String;
begin
  Result := 'remove_me';
end;

procedure TMsgRemoveMe.Serialize;
begin
  //
end;

procedure TMsgRemoveMe.Parse;
begin
  //
end;

procedure TMsgRemoveMe.Execute;
var
  Buddy: IBuddy;
begin
  Buddy := FConnection.Buddy;
  if not Assigned(Buddy) then
    LogWarningAndIgnore
  else begin
    if Buddy in FClient.Roster then begin
      Buddy.ResetAllTimes;
      FClient.TempList.AddBuddy(Buddy); // there it will be autoremoved
      FClient.Roster.RemoveBuddy(Buddy);
    end;
  end;
end;

begin
  RegisterMessageClass(TMsgRemoveMe);
end.

