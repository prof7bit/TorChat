unit tc_generic;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  { TGenericInterfaceEnumerator can be used as an enumerator for
    classes that derive from TInterfaceList/IInterfaceList }
  generic TGenericInterfaceEnumerator<T> = class
  strict private
    FPosition: Integer;
    FList: IInterfaceList;
  public
    constructor Create(AList: IInterfaceList);
    function GetCurrent: T;
    function MoveNext: Boolean;
    property Current: T read GetCurrent;
  end;


implementation

{ TGenericInterfaceEnumerator }

constructor TGenericInterfaceEnumerator.Create(AList: IInterfaceList);
begin
  FList := AList;
  FPosition := -1;
end;

function TGenericInterfaceEnumerator.GetCurrent: T;
begin
  Result := T(FList[FPosition]);
end;

function TGenericInterfaceEnumerator.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FList.Count;
end;

end.

