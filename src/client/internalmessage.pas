unit internalmessage;

{$mode objfpc}{$H+}

interface

uses
  interfaces;

type
  { TMsgCallMethod call a method without arguments from the main thread }
  TMsgCallMethod = class(TInterfacedObject, IMessage)
  strict private
    FMethod : TMethodOfObject;
  public
    constructor Create(AMethod: TMethodOfObject);
    procedure Execute;
  end;

implementation

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

