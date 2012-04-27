{ TorChat - TTorChatClient, this component is implememting the client

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
unit torchatclient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, torchatabstract, clientconfig, torprocess, networking,
  connection;

type
  { TTorChatClient implements the abstract TAClient. Together with all its
    contained objects this represents a fully functional TorChat client.
    The GUI (or libpurpletorchat or a command line client) will derive a class
    from TTorChatClient overriding the virtual event methods (the methods that
    start with CB (see the definition of TAClient) to hook into the events and
    then create an instance of it.
    The GUI also must call the TorChatClient.ProcessMessages method in regular
    intervals (1 second or so) from within the GUI-thread. Aditionally whenever
    there is an incoming chat message, status change or other event then
    TorChatClient will fire the CBWakeGui event and as a response the GUI should
    schedule an additional call to ProcessMessages a soon as possible. All other
    CB event method calls will then always originate from the thread that is
    calling TorChatClient.ProcessMessages. This method will process one queued
    message per call, it will never block and if there are no messages and
    nothing else to do it will just return.}
  TTorChatClient = class(TAClient)
  strict protected
    FTor: TTor;
    FSock : TSocketWrapper;
    FQueue: TQueue;
    CS: TRTLCriticalSection;
    procedure OnIncomingConnection(AConnection: TAHiddenConnection);
    procedure PopNextMessage;
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;
    procedure Enqueue(AMessage: TAMessage); override;
    procedure ProcessMessages;
  end;

implementation

{ TTorChatClient }

constructor TTorChatClient.Create(AOwner: TComponent);
var
  C : TAHiddenConnection;
begin
  Inherited Create(AOwner);
  InitCriticalSection(CS);
  FQueue := TQueue.Create;
  FTor := TTor.Create(self);
  FSock := TSocketWrapper.Create(Self);
  with FSock do begin
    SocksProxyAddress := ConfGetTorHost;
    SocksProxyPort := ConfGetTorPort;
    OutgoingClass := THiddenConnection;
    IncomingClass := THiddenConnection;
    IncomingCallback := TListenerCallback(@OnIncomingConnection);
    Bind(ConfGetListenPort);
  end;

  (*
  repeat
    try
      WriteLn('trying to connect...');
      C := FSock.Connect('ddcbrqjsdar3dahu.onion', 11009) as THiddenConnection;
      C.Send('this packet'#10'has many'#10'lines in it but the last line ');
      C.Send('ends in the next packet'#10);
      C.Send('and here we have a packet completely without delimiter ');
      C.Send('also ending only in the following line'#10);
      C.Send('and another one');
      C.Send(#10);
      C.Send(#10);
      C.Send('foo'#10);
      C.Send('bar'#10);

      C.Free;
      {$note we are still leaking unfreed *incoming* TConnection objects }

    except
      WriteLn('waiting 3 seconds...');
      Sleep(3000);
    end;
  until False;
  *)
end;

destructor TTorChatClient.Destroy;
var
  Msg: TAMessage;
begin
  inherited Destroy;
  EnterCriticalsection(CS);
  while FQueue.Count > 0 do begin
    Msg := TAMessage(FQueue.Pop);
    Msg.Free;
  end;
  FQueue.Free;
  LeaveCriticalsection(CS);
  DoneCriticalsection(CS);
end;

procedure TTorChatClient.Enqueue(AMessage: TAMessage);
begin
  writeln('enqueue new incoming message');
  EnterCriticalsection(CS);
  FQueue.Push(AMessage);
  LeaveCriticalsection(CS);
end;

procedure TTorChatClient.ProcessMessages;
var
  Msg: TAMessage;
begin
  writeln('GUIIdle called');
  PopNextMessage;
end;

procedure TTorChatClient.OnIncomingConnection(AConnection: TAHiddenConnection);
begin
  writeln('** incoming connection. This code will leak memory, we simply ignore the object but it still exists!');
end;

procedure TTorChatClient.PopNextMessage;
var
  Msg: TAMessage;
begin
  if FQueue.Count > 0 then begin
    EnterCriticalsection(CS);
    Msg := TAMessage(FQueue.Pop);
    LeaveCriticalsection(CS);
    try
      Msg.Execute;
    except
      on E: Exception do begin
        WriteLn(E.Message);
      end;
    end;
    Msg.Free;
  end;
end;

end.

