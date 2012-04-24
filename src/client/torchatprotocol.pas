{ TorChat - Protocol messages, protocol specification

  Copyright (C) 2012 Bernd Kreuss <prof7bit@googlemail.com>

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
unit torchatprotocol;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, torchatabstract;

type

  { TMsg }

  TMsg = class(TAMessage)
  strict protected
    FRaw : String;
  public
    constructor Create(AConnection: TAHiddenConnection; AContent: String); override;
  end;

  { TMsgPing }

  TMsgPing = class(TMsg)
    class function GetCommand: String; override;
  end;

  { TMsgPong }

  TMsgPong = class(TMsg)
    class function GetCommand: String; override;
  end;

  TMsgClass = class of TAMessage;

const
  CNT_MSG = 2;
  MsgClasses : array[1..CNT_MSG] of TMsgClass = (
    TMsgPing,
    TMsgPong
  );

function GetMsgClassFromCommand(ACommand: String): TMsgClass;

implementation

function GetMsgClassFromCommand(ACommand: String): TMsgClass;
var
  MsgClass: TMsgClass;
begin
  Result := TMsg; // default class if command is not recognized
  for MsgClass in MsgClasses do
    if MsgClass.GetCommand = ACommand then
      exit(MsgClass);
end;

constructor TMsg.Create(AConnection: TAHiddenConnection; AContent: String);
begin
  FRaw := AContent;
end;

{ TMsgPong }

class function TMsgPong.GetCommand: String;
begin
  Result := 'pong';
end;

{ TMsgPing }

class function TMsgPing.GetCommand: String;
begin
  Result := 'ping';
end;

end.

