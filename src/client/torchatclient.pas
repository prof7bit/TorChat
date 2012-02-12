unit torchatclient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, clientconfig, torprocess, networking, torchatabstract;

type

  { TTorChatClient }

  TTorChatClient = class(TAClient)
    constructor Create; reintroduce;
  public
    destructor Destroy; override;
  strict protected
    FTor: TTor;
    FListener: TListener;
    procedure OnIncomingConnection(AConnection: TAHiddenConnection);
  end;


implementation

{ TTorChatClient }

constructor TTorChatClient.Create;
var
  C : TConnection;
begin
  Inherited Create;
  self.FTor := TTor.Create;
  self.FListener := TListener.Create(ConfGetListenPort, TListenerCallback(@OnIncomingConnection));

  repeat
    try
      WriteLn('trying to connect...');
      C := ConnectSocks4a(ConfGetTorHost, ConfGetTorPort, 'ddcbrqjsdar3dahu.onion', 11009);
      C.WriteLn('hello myself via tor... and good bye :-)');
      C.Free;
    except
      WriteLn('waiting 3 seconds...');
      Sleep(3000);
    end;
  until False;
end;

destructor TTorChatClient.Destroy;
begin
  self.FListener.Terminate;
  self.FListener.Free;
  self.FTor.Free;
  inherited Destroy;
end;

procedure TTorChatClient.OnIncomingConnection(AConnection: TAHiddenConnection);
begin
  TReceiver.Create(AConnection);
  //AConnection.WriteLn('good bye!');
  //AConnection.Free;
end;

end.

