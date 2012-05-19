{ TorChat - TReceiver, reading data from the socket, creating message instances

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

{ This unit implements the receiver thread, each socket
  connection has one of these threads sitting in a blocking
  read on the socket. It is responsible for processing the
  incoming data. It is also responsible for detecting when
  the connection is closing/failing and triggering the
  appropriate actions in this case.

  Each protocol message is transported over the socket as
  one long line of text, binary data is encoded, 0x0a
  (line feed) marks the end of a message and the beginning
  of the next. The first part of the message is a string
  consisting of only the characters [a..z,_], separated by
  a space. This is the command. The rest of the line is the
  payload.

  NOTE: TorChat is a binary protocol! We are strictly
  using always only the byte 0x0a as the message delimiter,
  no automatic conversion of CR or CRLF will be done here.

  Processing incoming messages works like this:
  * read the entire line from the socket until 0x0a is found.
    (see also TReceiver (in receiver.pas) where this mechanism
    is implemented.)
  * separate the first word (delimited by space) from the rest
    of the line. (The rest may also be empty, there exist
    messages without payload.)
  * find the appropriate message class for it (one of the
    TMsgXXX classes depending on what command it is) and feed
    the rest of the line to its constructor. The constructor
    will do the binary decoding of the payload. (The payload
    is always encoded, no matter what it contains (because
    obviously there may not appear a 0x0a in the line), so
    it will also always be decoded. Please note that simple
    ascii strings without '\' or 0x0a will be invariant under
    this encoding, so it might look the same before and after,
    this doesn't mean its not encoded. It is. Always.)
  * call the message object's Parse() method, this will parse
    the decoded payload (the payload after binary decoding is
    one binary blob, depending on the type of the message it
    might be somethig as simple as an UTF8 string or it might
    be a chunk of a file transfer or something else, the Parse()
    method is responsible for dissecting it and populating the
    individual fields of the message object).
  * enqueue the object into TorChat's message queue. The main
    thread will pick it up from there and then call the
    message's Execute() method. The Execute() method will do
    the actual work, it contains the logic to decide what to
    do, trigger responses, manipulate other objects, call the
    GUI, etc.
}
unit tc_conn_rcv;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  tc_interface,
  tc_protocol,
  tc_misc;

type
  { TReceiver - Each conection contains a TAReceiver object which is
    a thread reading from the blocking socket, splitting the received
    data into individual messages and for each of them instantiate the
    appropriate message class to parse and enqueue the message. When
    the connection is closing/failing it will notify its containing
    connection object. See also THiddenConnection. }
  TReceiver = class(TAReceiver)
    constructor Create(AConn: IHiddenConnection);
    destructor Destroy; override;
    procedure Execute; override;
  strict private
    FStdOut: Text;
    FIncompleteMessage: String;
    procedure OnReceivedLine(EncodedLine: String);
  end;

implementation


{ TReceiver }

constructor TReceiver.Create(AConn: IHiddenConnection);
begin
  FStdOut := Output;
  FConnection := AConn;
  FClient := AConn.Client; // the torchat client object
  FIncompleteMessage := '';
  FreeOnTerminate := True;
  inherited Create(False);
end;

destructor TReceiver.Destroy;
begin
  inherited Destroy;
  WriteLn('TReceiver.Destroy() finished');
end;

procedure TReceiver.Execute;
var
  B : array[0..1024] of Char = #0;
  N : Integer;
  R : String;
  M : String;
begin
  Output := FStdOut;
  repeat
    N := FConnection.Stream.Read(B, 1024);
    if N > 0 then begin
      SetLength(R, N);
      Move(B, R[1], N);
      While R <> '' do begin
        try
          M := Split(R, #10);
          OnReceivedLine(FIncompleteMessage + M);
          FIncompleteMessage := '';
        except
          on E:EEndOfString do begin
            FIncompleteMessage := R;
            R := '';
          end;
        end;
      end;
    end;
  until (N <= 0) or Terminated;
  WriteLn('TReceiver.Execute()' + FConnection.DebugInfo + ' detected end of life, beginning termination');
  FConnection.OnTCPFail;
end;

procedure TReceiver.OnReceivedLine(EncodedLine: String);
var
  Command: String;
  Msg: IProtocolMessage;
begin
  try
    Command := PopFirstWord(EncodedLine);
  except
    // reached the end of the line without finding a
    // space, so this is a command without arguments.
    Command := Trim(EncodedLine);
    EncodedLine := '';
  end;

  Msg := GetMsgClassFromCommand(Command).Create(
    Self.FConnection, Command, EncodedLine);
  try
    Msg.Parse;
  except
    on Ex: Exception do begin
      WriteLn(Ex.Message);
    end;
  end;

  if Assigned(Msg) then begin
    Client.Queue.Put(Msg);
  end;
end;

end.

