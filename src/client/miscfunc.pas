unit miscfunc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  EEndOfString = class(Exception)
  end;

function Split(var Line: String; Sep: Char): String;

implementation

{ split the string Line at the first occurrence of Sep, return the left part
  in the function result and the right part in var Line. If no separator is
  found then an EEndOfString exception is generated }
function Split(var Line: String; Sep: Char): String;
var
  P : Integer;
begin
  P := Pos(Sep, Line);
  if P > 0 then begin
    Result := LeftStr(Line, P-1);
    Line := RightStr(Line, Length(Line) - P);
  end
  else
    raise EEndOfString.Create('');
end;

end.

