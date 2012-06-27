unit tc_msgqueue;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  tc_interface;

type
  { TMsgQueue }
  TMsgQueue = class(TInterfaceList, IMsgQueue)
  strict private
    FClient: IClient;
  public
    constructor Create(AClient: IClient);
    procedure Put(Msg: IMessage);
    procedure PumpNext;
    procedure Clear;
  end;

  { TMsgCallMethod call a method without arguments from the main thread }
  TMsgCallMethod = class(TInterfacedObject, IMessage)
  strict private
    FObject: IUnknown;
    FMethod : TMethodOfObject;
  public
    constructor Create(AObject: IUnknown; AMethod: TMethodOfObject);
    procedure Execute;
  end;

implementation

{ TMsgQueue }

constructor TMsgQueue.Create(AClient: IClient);
begin
  FClient := AClient;
  inherited Create;
end;

procedure TMsgQueue.Put(Msg: IMessage);
begin
  if not FClient.IsDestroying then begin
    Insert(0, Msg);
    FClient.OnNeedPump;
  end;
end;

procedure TMsgQueue.PumpNext;
var
  Msg: IMessage;
begin
  if not FClient.IsDestroying then begin;
    if Count > 0 then begin
      WriteLn('TMsgQueue.PumpNext() there is a message');
      Msg := IMessage(Last);
      Msg.Execute;
      WriteLn('TMsgQueue.PumpNext() removing message');
      Remove(Msg);
      WriteLn('TMsgQueue.PumpNext() finished');
    end;
  end;
end;

procedure TMsgQueue.Clear;
begin
  inherited Clear;
end;

{ TMsgCallMethod }

constructor TMsgCallMethod.Create(AObject: IUnknown; AMethod: TMethodOfObject);
begin
  FObject := AObject; // keep a reference to it as long as this msg exists
  FMethod := AMethod;
end;

procedure TMsgCallMethod.Execute;
begin
  WriteLn('TMsgCallMethod.Execute');
  FMethod();
end;


end.

