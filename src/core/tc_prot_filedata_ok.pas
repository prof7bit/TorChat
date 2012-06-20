{ TorChat - TMsgFileDataOk

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

unit tc_prot_filedata_ok;

{$mode objfpc}{$H+}

interface

uses
  tc_interface,
  tc_protocol;

type
  { TMsgFileDataOk

  }
  TMsgFileDataOk = class(TMsg)
  strict protected
    procedure Serialize; override;
  public
    class function GetCommand: String; override;
    constructor Create(ABuddy: IBuddy); reintroduce;
    procedure Parse; override;
    procedure Execute; override;
  end;

implementation

{ TMsgFileDataOk }

class function TMsgFileDataOk.GetCommand: String;
begin
  Result := 'filedata_ok';
end;

constructor TMsgFileDataOk.Create(ABuddy: IBuddy);
begin
  inherited Create(ABuddy);
end;

procedure TMsgFileDataOk.Parse;
begin
end;

procedure TMsgFileDataOk.Serialize;
begin
end;

procedure TMsgFileDataOk.Execute;
var
  Buddy: IBuddy;
begin
  Buddy := FConnection.Buddy;
  if Assigned(Buddy) then begin
    //
  end
  else
    LogWarningAndIgnore();
end;

begin
  RegisterMessageClass(TMsgFileDataOk);
end.

