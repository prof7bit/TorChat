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
  FSock.SocksProxyAddress := ConfGetTorHost;
  FSock.SocksProxyPort := ConfGetTorPort;
  FSock.ConnectionClass := THiddenConnection;
  FSock.Bind(ConfGetListenPort, TListenerCallback(@OnIncomingConnection));
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

