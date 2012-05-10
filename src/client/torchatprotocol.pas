{ TorChat - Protocol messages base class TMsg and helpers

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

{ This unit contains TMsg which serves as a base class and
  implements the default behavior (decoding, encoding sending)
  of protocol messags, it also contains some helper functions,
  most notably the binary encoding/decoding functions.

  TMsg and all its TMsgXxx descendants which are defined in
  separate units are the core of the TorChat protocol. Each
  Message that is sent or received is represented by an
  instance of its corresponding class. Each of these classes
  is defined in its own unit (torchatprotocol_xxx.pas), all
  of them also contain detailed documentation.

  To unterstand the protocol and the meaning of each protocol
  message you should study the documentation for each message
  class, the Parse() and Serialize() methods will show you how
  the structure of the message looks like and the Execute()
  method will show you how the client is supposed to react to
  an incoming message of this type.

  * See also TReceiver (receiver.pas) for the handling of the
  incoming bytes from the TCP socket.

  * See also TMsgPing (torchatprotocol_ping.pas) which is the
  very first protocol message that is sent (and received) on
  every new TCP connection and constitutes the beginning of
  the handshake.
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
procedure RegisterMessageClass(AClass: TMsgClass);

implementation
var
  MessageClasses: array of TMsgClass;

function GetMsgClassFromCommand(ACommand: String): TMsgClass;
var
  MsgClass: TMsgClass;
begin
  Result := TMsg; // default class if command is not recognized
  for MsgClass in MessageClasses do
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

procedure RegisterMessageClass(AClass: TMsgClass);
var
  L : Integer;
begin
  L := Length(MessageClasses);
  SetLength(MessageClasses, L + 1);
  MessageClasses[L] := AClass;
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
  FClient := FConnection.Client;
  FBinaryContent := BinaryDecode(AEncodedContent);
end;

{ this is the constructor for outgoing messages }
constructor TMsg.Create(ABuddy: TABuddy);
begin
  FBuddy := ABuddy;
  FClient := FBuddy.Client;
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

