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
  strict private
    FTor: TTor;
  end;


implementation

{ TTorChatClient }

constructor TTorChatClient.Create(AOwner: TComponent);
var
  C : TConnection;
begin
  Inherited Create(AOwner);
  self.FTor := TTor.Create(self);
  Sleep(100);
  try
    C := ConnectTCP(GetTorHost, GetTorPort);
    writeln(Format('TConnection object %p', [Pointer(C)]));
  except
    on E: Exception do begin
      WriteLn(E.Message);
    end;
  end;
end;

destructor TTorChatClient.Destroy;
begin
  self.FTor.Free;
  inherited Destroy;
end;

end.

