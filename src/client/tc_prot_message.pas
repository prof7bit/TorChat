{ TorChat - Protocol message 'message' (transport an im message)

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
unit tc_prot_message;

{$mode objfpc}{$H+}

interface

uses
  tc_interface,
  tc_protocol;

type
  { TMsgMessage

    This message is sent to transport an instant
    message. It has the following structure:

      message <text>

    <text>  the message text UTF-8 ancoded and
            lines separated by 0x0a

    When receiving this message we pass the text to the
    GUI which will show it in a chat window. Note that
    all text must be UTF-8 encoded and multi-line
    messages are using 0x0a as line break.

    Don't be confused about the binary 0x0a, as already
    stated in the protocol overview each message as a
    whole will always be be encoded immedtately before
    sending and decoded immediately after it comes from
    the socket. This happens always for all messages,
    so we never need to take any special precautions
    regarding binary data in the message payload.
  }
  TMsgMessage = class(TMsg)
  strict protected
    FMessageText: String;
    procedure Serialize; override;
  public
    constructor Create(ABuddy: IBuddy; AMessageText: String); reintroduce;
    class function GetCommand: String; override;
    procedure Parse; override;
    procedure Execute; override;
  end;


implementation
uses
  sysutils;

{ TMsgMessage }

class function TMsgMessage.GetCommand: String;
begin
  Result := 'message';
end;

procedure TMsgMessage.Serialize;
begin
  // messages are always transmitted with $0a as line break
  FBinaryContent := StringReplace(FMessageText,
    LineEnding, #$0a, [rfReplaceAll]);
end;

constructor TMsgMessage.Create(ABuddy: IBuddy; AMessageText: String);
begin
  inherited Create(ABuddy);
  FMessageText := AMessageText;
end;

procedure TMsgMessage.Parse;
begin
  // normally we would not need this all. The opposite
  // operation of Serialize() would be enough.

  // These replacements are just to satisfy the paranoid.
  // By specification it should only contain $0a as line
  // ending but there might be broken clients out there,
  // so we replace everything that remotely looks like a
  // line ending with a proper line ending. At the end we
  // always end up with OS-dependent LineEnding so we
  // can then just pass the string to the GUI and it will
  // be ok.
  FMessageText :=
    StringReplace(
    StringReplace(
    StringReplace(
    StringReplace(
    Trim(FBinaryContent),
    #$0d#$0a,   #$0a, [rfReplaceAll]),
    #$0d,       #$0a, [rfReplaceAll]),
    #$0b,       #$0a, [rfReplaceAll]), // 0x0b shift-enter on windows
    #$0a, LineEnding, [rfReplaceAll]);
end;

procedure TMsgMessage.Execute;
var
  Buddy: IBuddy;
begin
  Buddy := FConnection.Buddy;
  if not Assigned(Buddy) then
    LogWarningAndIgnore
  else begin
    WriteLn('TMsgMessage.Execute() ', Length(FMessageText), ' bytes text from ' + Buddy.ID);
    Buddy.Client.OnInstantMessage(Buddy, FMessageText);
  end;
end;

begin
  RegisterMessageClass(TMsgMessage);
end.

