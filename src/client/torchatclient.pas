unit torchatclient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, clientconfig, torprocess, networking;

type

  { TTorChatClient }

  TTorChatClient = class(TComponent)
    constructor Create(AOwner: TComponent); reintroduce;
  public
    destructor Destroy; override;
  strict protected
    FTor: TTor;
    FListener: TListener;
    procedure OnIncomingConnection(AConnection: TConnection);
  end;


implementation

{ TTorChatClient }

constructor TTorChatClient.Create(AOwner: TComponent);
var
  C : TConnection;
begin
  Inherited Create(AOwner);
  self.FTor := TTor.Create(self);
  self.FListener := TListener.Create(ConfGetListenPort, @OnIncomingConnection);

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

procedure TTorChatClient.OnIncomingConnection(AConnection: TConnection);
begin
  TReceiver.Create(AConnection);
  //AConnection.WriteLn('good bye!');
  //AConnection.Free;
end;

end.

