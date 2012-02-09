unit torprocess;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process;

type

  { TTor }

  TTor = class(TProcess)
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{ TTor }

constructor TTor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.Executable := 'tor';
  Self.Execute;
end;

end.

