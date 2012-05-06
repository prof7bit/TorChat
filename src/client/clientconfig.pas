{ TorChat - Get/Set client configuration settings

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
unit clientconfig;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef windows}shlobj,{$endif}
  Classes, SysUtils;

const
  SECONDS_WAIT_FOR_HOSTNAME_FILE = 20;

  function ConfGetDataDir: String;
  function ConfGetTorExe: String;
  function ConfGetListenPort: DWord;
  function ConfGetTorHost: String;
  function ConfGetTorPort: DWord;
  function ConfGetHiddenServiceName: String;

implementation

function ConfGetDataDir: String;
{$ifdef windows}
var
  AppDataPath: Array[0..MaxPathLen] of Char;
{$endif}
begin
  {$ifdef windows}
    SHGetSpecialFolderPath(0, AppDataPath, CSIDL_APPDATA, false);
    Result := ConcatPaths([AppDataPath, 'torchat2']);
    //{$fatal Windows is not yet supported}
  {$else}
    {$warning home directory hardcoded, dirty hack}
    Result := ExpandFileName('~/.torchat2');
  {$endif}
  if not DirectoryExists(Result) then begin
    writeln('creating config dir ' + Result);
    if not CreateDir(Result) then begin
      writeln('(0) could not create config dir ' + Result);
    end;
  end;
end;

function ConfGetTorExe: String;
begin
  {$ifdef windows}
    Result := GetCurrentDir + '\tor\tor.exe';
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

function ConfGetHiddenServiceName: String;
var
  FileName: String;
  HostnameFile: TFileStream = nil;
const
  OnionLength = 16;
begin
  FileName := ConcatPaths([ConfGetDataDir, 'tor/hidden_service/hostname']);
  SetLength(Result, OnionLength);
  try
    HostnameFile := TFileStream.Create(FileName, fmOpenRead);
    if HostnameFile.Read(Result[1], OnionLength) < OnionLength then
      Result := '';
  except
    Result := '';
  end;
  if Assigned(HostnameFile) then FreeAndNil(HostnameFile);
end;

end.

