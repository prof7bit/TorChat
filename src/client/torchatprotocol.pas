{ TorChat - Protocol messages, protocol specification

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
unit torchatprotocol;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  torchatabstract,
  miscfunc;

type

  { TMsg }

  TMsg = class(TAMessage)
  strict protected
    FBinaryContent : String;
    function GetSendConnection: TAHiddenConnection; virtual;
    procedure Serialize; virtual; abstract;
  public
    constructor Create(AConnection: TAHiddenConnection; AEncodedContent: String); override;
    constructor Create(ABuddy: TABuddy);
    procedure Send; override;
  end;

  TMsgClass = class of TAMessage;

function GetMsgClassFromCommand(ACommand: String): TMsgClass;
function BinaryEncode(ABinary: String): String;
function BinaryDecode(AEncoded: String): String;
function PopFirstWord(var AString: String): String;

implementation
uses
  torchatprotocol_ping,
  torchatprotocol_pong;

const
  CNT_MSG = 2;
  MsgClasses : array[1..CNT_MSG] of TMsgClass = (
    TMsgPing,
    TMsgPong
  );

function GetMsgClassFromCommand(ACommand: String): TMsgClass;
var
  MsgClass: TMsgClass;
begin
  Result := TMsg; // default class if command is not recognized
  for MsgClass in MsgClasses do
    if MsgClass.GetCommand = ACommand then
      exit(MsgClass);
end;

function BinaryEncode(ABinary: String): String;
begin
  Result := ABinary; {$warning implement me}
end;

function BinaryDecode(AEncoded: String): String;
begin
  Result := AEncoded; {$warning implement me}
end;

function PopFirstWord(var AString: String): String;
begin
  Result := Split(AString, ' ');
end;

function TMsg.GetSendConnection: TAHiddenConnection;
begin
  if Assigned(FBuddy) and Assigned(FBuddy.ConnOutgoing) then
    Result := FBuddy.ConnOutgoing
  else
    Result := nil;
end;

{ this is the virtual constructor for incoming messages }
constructor TMsg.Create(AConnection: TAHiddenConnection; AEncodedContent: String);
begin
  FConnection := AConnection;
  FBinaryContent := BinaryDecode(AEncodedContent);
end;

{ this is the constructor for outgoing messages }
constructor TMsg.Create(ABuddy: TABuddy);
begin
  FBuddy := ABuddy;
end;

procedure TMsg.Send;
var
  C : TAHiddenConnection;
begin
  C := GetSendConnection;
  if Assigned(C) then begin
    Serialize;
    C.SendLine(GetCommand + ' ' + BinaryEncode(FBinaryContent));
  end;
  self.Free;
end;


end.

