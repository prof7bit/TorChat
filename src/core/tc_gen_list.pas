unit tc_gen_list;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes,
  SysUtils;

type
  { GListEnumerator }

  generic GListEnumerator<T> = class(TObject)
  private type
    TGetFunc = function(Index: Integer): T of object;
  protected
    FCount: Integer;
    FGetFunc: TGetFunc;
    FPosition: Integer;
    function GetCurrent: T;
  public
    constructor Create(AGetFunc: TGetFunc; ACout: Integer);
    function MoveNext: Boolean;
    property Current: T read GetCurrent;
  end;

  { GList }

  generic GList<T> = class(TInterfacedObject)
  private
  public type
    TEnumerator = specialize GListEnumerator<T>;
  strict protected
    FEmpty: T;
    FCount: Integer;
    FList: Array of T;
    procedure CheckExpand;
    procedure CheckShrink;
  public
    constructor Create;
    procedure Add(var Item: T);
    procedure Remove(var Item: T);
    function IndexOf(var Item: T): Integer;
    function GetItem(Index: Integer): T;
    property Count: Integer read FCount;
    function GetEnumerator: TEnumerator;
    function Contains(var Item: T): Boolean;
  end;

implementation

{ GListEnumerator }

function GListEnumerator.GetCurrent: T;
begin
  Result := FGetFunc(FPosition);
end;

constructor GListEnumerator.Create(AGetFunc: TGetFunc; ACout: Integer);
begin
  FCount := ACout;
  FGetFunc := AGetFunc;
  FPosition := -1;
end;

function GListEnumerator.MoveNext: Boolean;
begin
  inc(FPosition);
  Result := FPosition < FCount;
end;

{ GList }


procedure GList.CheckExpand;
begin
  if FCount < Length(FList) then
    exit;
  if FCount < 16 then
    SetLength(FList, FCount + 8)
  else
    SetLength(FList, FCount + FCount shr 1);
end;

procedure GList.CheckShrink;
begin
  if (FCount shl 2) < Length(FList) then
    SetLength(FList, Length(FList) shr 1);
end;

constructor GList.Create;
begin
  FillByte(FEmpty, SizeOf(T), 0);
  FCount := 0;
end;

procedure GList.Add(var Item: T);
begin
  CheckExpand;
  FList[FCount] := Item;
  Inc(FCount);
end;

procedure GList.Remove(var Item: T);
var
  I: Integer;
begin
  I := IndexOf(Item);
  if I >= 0 then begin
    Dec(FCount);
    FList[I] := FEmpty; // deref it
    if I < FCount then begin
      while I < FCount do begin
        FList[I] := FList[I+1];
        Inc(I);
      end;
      FList[FCount] := FEmpty; // deref last element
    end;
    CheckShrink;
  end;
end;

function GList.IndexOf(var Item: T): Integer;
begin
  Result := FCount - 1;
  while Result >= 0 do begin
    if FList[Result] = Item then
      exit;
    Dec(Result);
  end;
end;

function GList.GetItem(Index: Integer): T;
begin
  Result := FList[Index];
end;

function GList.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(@GetItem, FCount);
end;

function GList.Contains(var Item: T): Boolean;
begin
  Result := IndexOf(Item) >= 0;
end;

end.

