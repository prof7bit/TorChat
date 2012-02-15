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
  Classes, SysUtils, torchatabstract, clientconfig, torprocess, networking,
  connection;

type

  { TTorChatClient implements the abstract TAClient. Together with all its
    contained objects this represents a fully functional TorChat client.
    The GUI (or libtorchat or a command line client) will derive a class
    from TTorChatClient overriding the virtual event methods to hook into
    the events and then simply create an instance of it. }
  TTorChatClient = class(TAClient)
  strict protected
    FTor: TTor;
    FSock : TSocketWrapper;
    procedure OnIncomingConnection(AConnection: TAHiddenConnection);
  public
    constructor Create(AOwner: TComponent); reintroduce;
  end;


implementation

{ TTorChatClient }

constructor TTorChatClient.Create(AOwner: TComponent);
var
  C : TAHiddenConnection;
begin
  Inherited Create(AOwner);
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

procedure TTorChatClient.OnIncomingConnection(AConnection: TAHiddenConnection);
begin
  writeln('** incoming connection. This code will leak memory, we simply ignore the object but it still exists!');
end;

end.

