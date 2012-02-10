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

procedure CB(AConn: TConnection);
begin
  writeln('received conn ' + IntToStr(AConn.Handle));
  AConn.WriteLn('good bye!');
  AConn.Free;
end;

{ TTorChatClient }

constructor TTorChatClient.Create(AOwner: TComponent);
var
  C : TConnection;
  R : TReceiver;
  L : TListener;
  X : String;
begin
  Inherited Create(AOwner);
  //self.FTor := TTor.Create(self);
  //Sleep(500);
  try
    L := TListener.Create(11009, @CB);

    sleep(5000);
    L.Terminate;
    L.Free;
  except
    on E: Exception do begin
      WriteLn(E.Message);
    end;
  end;
end;

destructor TTorChatClient.Destroy;
begin
  //self.FTor.Free;
  inherited Destroy;
end;

end.

