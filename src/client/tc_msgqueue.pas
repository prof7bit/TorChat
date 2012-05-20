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
    FMethod : TMethodOfObject;
  public
    constructor Create(AMethod: TMethodOfObject);
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
  if not FClient.IsDestroying then
    Insert(0, Msg);
end;

procedure TMsgQueue.PumpNext;
var
  Msg: IMessage;
begin
  if not FClient.IsDestroying then begin;
    if Count > 0 then begin
      Msg := IMessage(Last);
      Msg.Execute;
      Remove(Msg);
    end;
  end;
end;

procedure TMsgQueue.Clear;
begin
  inherited Clear;
end;

{ TMsgCallMethod }

constructor TMsgCallMethod.Create(AMethod: TMethodOfObject);
begin
  FMethod := AMethod;
end;

procedure TMsgCallMethod.Execute;
begin
  FMethod();
end;


end.

