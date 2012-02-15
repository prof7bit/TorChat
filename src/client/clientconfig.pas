{ TorChat - Get/Set client configuration settings

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
unit clientconfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil;

  function ConfGetDataDir: String;
  function ConfGetTorExe: String;
  function ConfGetListenPort: DWord;
  function ConfGetTorHost: String;
  function ConfGetTorPort: DWord;

implementation

function ConfGetDataDir: String;
begin
  Result := ProgramDirectory;
end;

function ConfGetTorExe: String;
begin
  {$ifdef win32}
  Result := AppendPathDelim(ProgramDirectory) + 'tor\tor.exe';
  {$else}
  Result := '/usr/sbin/tor';
  {$endif}
end;

function ConfGetListenPort: DWord;
begin
  Result := 11009;
end;

function ConfGetTorHost: String;
begin
  Result := 'localhost'
end;

function ConfGetTorPort: DWord;
begin
  Result := 11109;
end;

end.

