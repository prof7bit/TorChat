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
    procedure ExecuteWithBuddy; override;
  public
    constructor Create(ABuddy: IBuddy; AMessageText: String); reintroduce;
    class function GetCommand: String; override;
    procedure Parse; override;
  end;


implementation
uses
  tc_misc,
  sysutils;

{ TMsgMessage }

class function TMsgMessage.GetCommand: String;
begin
  Result := 'message';
end;

constructor TMsgMessage.Create(ABuddy: IBuddy; AMessageText: String);
begin
  inherited Create(ABuddy);
  FMessageText := AMessageText;
end;

procedure TMsgMessage.Serialize;
begin
  // "Be conservative in what you do..."
  FBinaryContent := LineBreaksAnyToLF(FMessageText);
end;

procedure TMsgMessage.Parse;
begin
  // "...be liberal in what you accept from others."
  FMessageText := LineBreaksAnyToNative(FBinaryContent);
end;

procedure TMsgMessage.ExecuteWithBuddy;
begin
  FClient.OnInstantMessage(FBuddy, FMessageText);
end;

begin
  RegisterMessageClass(TMsgMessage);
end.

