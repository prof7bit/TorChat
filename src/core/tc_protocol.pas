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
  implements the default behavior (decoding, encoding and
  sending) of protocol messags, it also contains some helper
  functions, most notably the binary encoding/decoding
  functions.

  TMsg and all its TMsgXxx descendants which are defined in
  separate units are the core of the TorChat protocol. Each
  Message that is sent or received is represented by an
  instance of its corresponding class. Each of these classes
  is defined in its own unit (torchatprotocol_xxx.pas), all
  of them also contain detailed documentation.

  To unterstand the protocol and the meaning of each protocol
  message you should study the documentation for each
  message class, the Parse() and Serialize() methods will
  show you how the structure of the message looks like and
  the Execute() method will show you how the client is
  supposed to react to an incoming message of this type.

  * See also THiddenConnection (tc_conn.pas) for the handling
  of the incoming bytes from the TCP socket.

  * See also TMsgPing (torchatprotocol_ping.pas) which is the
  very first protocol message that is sent (and received) on
  every new TCP connection and constitutes the beginning of
  the handshake.
}
unit tc_protocol;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  tc_interface,
  tc_misc;

type
  { TMsg

    This is the abstract base class for the other protocol
    messages. It implements the basic infrastructure of
    binary decoding, encoding, sending, etc. The concrete
    protocol message classes will introduce additional
    fields and constructors and override GetCommand(),
    Serialize(), Parse() and Execute().
  }
  TMsg = class(TInterfacedObject, IProtocolMessage)
  strict protected
    FConnection: IHiddenConnection;
    FClient: IClient;
    FBuddy: IBuddy;
    FCommand: String;
    FBinaryContent : String;
    function GetSendConnection: IHiddenConnection; virtual;
    function DebugInfo: String;
    procedure LogWarningAndIgnore(AInfo: String='');
    procedure LogReceive;
    procedure LogSend;
    procedure Serialize; virtual; abstract;
    procedure ExecuteWithBuddy; virtual;
    procedure ExecuteWithoutBuddy; virtual;
  public
    class function GetCommand: String; virtual; abstract;
    class function ReceiveOnOutgoing: Boolean; virtual;
    constructor Create(AConnection: IHiddenConnection; ACommand, AEncodedContent: String); virtual;
    constructor Create(ABuddy: IBuddy);
    procedure Parse; virtual; abstract;
    procedure Execute; virtual;
    procedure Send; virtual;
  end;

  { TMsgDefault

    This message is instantiated when we receive an
    unknown command. If the connection has a buddy It
    will reply with 'not_implemented', if it does not
    have a buddy then it will close the connection.
  }
  TMsgDefault = class(TMsg)
    procedure Serialize; override;
    procedure Parse; override;
    procedure Execute; override;
    class function GetCommand: String; override;
  end;

  TMsgClass = class of TMsg;


function GetMsgClassFromCommand(ACommand: String): TMsgClass;
function BinaryEncode(ABinary: String): String;
function BinaryDecode(AEncoded: String): String;
function PopFirstWord(var AString: String): String;
procedure RegisterMessageClass(AClass: TMsgClass);

implementation
uses
  tc_prot_not_implemented;

var
  MessageClasses: array of TMsgClass;

function GetMsgClassFromCommand(ACommand: String): TMsgClass;
var
  MsgClass: TMsgClass;
begin
  Result := TMsgDefault; // default class if command is not recognized
  for MsgClass in MessageClasses do
    if MsgClass.GetCommand = ACommand then
      exit(MsgClass);
end;

function BinaryEncode(ABinary: String): String;
begin
  Result := StringReplace(StringReplace(ABinary,
                '\', '\/', [rfReplaceAll]),
                #10, '\n', [rfReplaceAll]);
end;

function BinaryDecode(AEncoded: String): String;
begin
  Result := StringReplace(StringReplace(AEncoded,
                '\n', #10, [rfReplaceAll]),
                '\/', '\', [rfReplaceAll]);
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

{ TMsgDefault }

procedure TMsgDefault.Serialize;
begin
  // nothing
end;

procedure TMsgDefault.Parse;
begin
  // nothing
end;

procedure TMsgDefault.Execute;
var
  Buddy: IBuddy;
  Msg: IProtocolMessage;
begin
  Buddy := FConnection.Buddy;
  if not Assigned(Buddy) then
    LogWarningAndIgnore
  else begin
    WriteLnF('TMsgDefault.Execute() received unknown %s (%d bytes) from %s',
      [FCommand, Length(FBinaryContent), Buddy.ID]);
    Msg := TMsgNotImplemented.Create(Buddy, FCommand);
    Msg.Send;
  end;
end;

class function TMsgDefault.GetCommand: String;
begin
  Result := '';
end;

function TMsg.GetSendConnection: IHiddenConnection;
begin
  if Assigned(FBuddy) then begin
    if ReceiveOnOutgoing then
      Result := FBuddy.ConnIncoming
    else
      Result := FBuddy.ConnOutgoing;
  end
  else
    Result := nil;
end;

function TMsg.DebugInfo: String;
begin
  if Assigned(FBuddy) then begin
    Result := FBuddy.ID;
  end
  else begin
    if Assigned(FConnection) then begin
      if Assigned(FConnection.Buddy) then
        Result := FConnection.Buddy.ID
      else
        if FConnection.PingBuddyID <> '' then
          Result := 'allegedly ' + FConnection.PingBuddyID
        else
          Result := '(anonymous connection)';
    end;
  end;
end;

procedure TMsg.LogWarningAndIgnore(AInfo: String);
begin
  if (FCommand = '') and (AInfo = '') then
    AInfo := '(empty line)';
  if AInfo = '' then
    AInfo := '(data: ' + IntToStr(Length(FBinaryContent)) + ' bytes)';
  WriteLnF('received "%s" %s from %s, ignoring.',
    [FCommand, AInfo, DebugInfo]);
end;

procedure TMsg.LogReceive;
begin
  if Length(FBinaryContent) > 0 then
    WriteLnF('----> "%s" from %s data: %s',
      [FCommand, DebugInfo, DebugFormatBinary(FBinaryContent)])
  else
    WriteLnF('----> "%s" from %s',
      [FCommand, DebugInfo])
end;

procedure TMsg.LogSend;
begin
  if Length(FBinaryContent) > 0 then
    WriteLnF('<---- "%s" to %s data: %s',
      [GetCommand, DebugInfo, DebugFormatBinary(FBinaryContent)])
  else
    WriteLnF('<---- "%s" to %s',
      [GetCommand, DebugInfo]);
end;

procedure TMsg.ExecuteWithBuddy;
begin

end;

procedure TMsg.ExecuteWithoutBuddy;
begin
  LogWarningAndIgnore;
end;

class function TMsg.ReceiveOnOutgoing: Boolean;
begin
  Result := False;
end;

{ this is the virtual constructor for incoming messages }
constructor TMsg.Create(AConnection: IHiddenConnection; ACommand, AEncodedContent: String);
begin
  FConnection := AConnection;
  FClient := FConnection.Client;
  FBuddy := FConnection.Buddy;
  FCommand := ACommand;
  FBinaryContent := BinaryDecode(AEncodedContent);
  LogReceive;
end;

{ this is the constructor for outgoing messages }
constructor TMsg.Create(ABuddy: IBuddy);
begin
  FBuddy := ABuddy;
  FClient := FBuddy.Client;
end;

procedure TMsg.Execute;
begin
  FBuddy := FConnection.Buddy;
  if Assigned(FBuddy) then
    ExecuteWithBuddy
  else
    ExecuteWithoutBuddy;
end;

procedure TMsg.Send;
var
  C : IHiddenConnection;
begin
  C := GetSendConnection;
  if Assigned(C) then begin
    Serialize;
    LogSend;
    C.SendLine(GetCommand + ' ' + BinaryEncode(FBinaryContent));
  end
  else
    WriteLnF('cannot send "%s" to %s without open connection',
      [GetCommand, FBuddy.ID]);
end;


end.

