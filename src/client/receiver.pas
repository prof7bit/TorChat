{ TorChat - TReceiver, reading data from the socket, creating message instances

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
unit receiver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, torchatabstract, torchatprotocol, miscfunc;

type
  { TReceiver - Each conection contains a TAReceiver object which is
    a thread reading from the blocking socket, splitting the received
    data into individual messages and for each of them instantiate the
    appropriate message class to parse and enqueue the message }
  TReceiver = class(TAReceiver)
    constructor Create(AConn: TAHiddenConnection);
    destructor Destroy; override;
    procedure Execute; override;
  strict private
    FIncompleteMessage: String;
    procedure OnReceivedLine(Line: String);
  end;

implementation


{ TReceiver }

constructor TReceiver.Create(AConn: TAHiddenConnection);
begin
  FConnection := AConn;
  FClient := AConn.Client; // the torchat client object
  FIncompleteMessage := '';
  inherited Create(False);
end;

destructor TReceiver.Destroy;
begin
  inherited Destroy;
end;

procedure TReceiver.Execute;
var
  B : array[0..1024] of Char = #0;
  N : Integer;
  R : String;
  M : String;
begin
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
  until (N = 0) or Terminated;
  FConnection.Stream.DoClose; // only shutdown and close the socket handle
  FConnection.OnConnectionClose;
end;

procedure TReceiver.OnReceivedLine(Line: String);
var
  Command: String;
  Msg: TAMessage;
begin
  try
    Command := Split(Line, ' ');
  except
    exit;
  end;

  Msg := GetMsgClassFromCommand(Command).Create(Self.FConnection, Line);
  try
    Msg.Parse;
  except
    on Ex: Exception do begin
      WriteLn(Ex.Message);
      Msg.Free;
      Msg := nil;
    end;
  end;

  if Assigned(Msg) then begin
    Client.Enqueue(Msg);
    Client.OnNotifyGui;
  end;
end;

end.

