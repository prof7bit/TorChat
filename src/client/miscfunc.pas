{ TorChat - Some small helper functions

  Copyright (C) 2012 Bernd Kreuss <prof7bit@gmail.com>

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 3 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}
unit miscfunc;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils;

type
  EEndOfString = class(Exception)
  end;

function SecondsSince(Start: TDateTime): QWord;

{ split the string Line at the first occurrence of Sep, return the left part
  in the function result and the right part in var Line. If no separator is
  found then an EEndOfString exception is generated }
function Split(var Line: String; Sep: Char): String;

{ This function exists only to make the compiler happy when we have unused
  function arguments, we can then just Ignore() them and the compiler will not
  emit a warning anymore. Unneccessary warnings only spam the console when
  compiling and bury legitimate problems. This function is declared inline,
  it does absolutely nothing and will therefore be optimized away completely. }
function Ignore(P: Pointer): Pointer; Inline;

implementation

function SecondsSince(Start: TDateTime): QWord;
begin
  Result := Round((Now - Start) * 24 * 60 * 60);
end;

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

function Ignore(P: Pointer): Pointer; inline;
begin
  Result := P;
end;

end.

