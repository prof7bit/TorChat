{ TorChat - Some small helper functions

  Copyright (C) 2012 Bernd Kreuss <prof7bit@googlemail.com>

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

