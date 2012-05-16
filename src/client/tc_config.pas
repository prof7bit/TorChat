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
unit tc_config;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef windows}shlobj,{$endif} // for finding %APPDATA% etc.
  Classes,
  SysUtils,
  tc_interface;

const
  SECONDS_WAIT_FOR_HOSTNAME_FILE = 20;


type
  { TClientConfig }

  TClientConfig = class(TInterfacedObject, IClientConfig)
  strict protected
    FProfileName: String;
    FPathTorExe: String;
  public
    constructor Create(AProfileName: String);
    function DataDir: String;
    function PathTorExe: String;
    function ListenPort: DWord;
    function TorHostName: String;
    function TorPort: DWord;
    function HiddenServiceName: String;
  end;

function DefaultPathTorExe: String;

implementation

{ TClientConfig }

constructor TClientConfig.Create(AProfileName: String);
begin
  FProfileName := AProfileName;
  FPathTorExe := DefaultPathTorExe;
end;

function TClientConfig.DataDir: String;
{$ifdef windows}
var
  AppDataPath: Array[0..MaxPathLen] of Char;
{$endif}
begin
  {$note need different home directories for different "accounts"}
  {$ifdef windows}
    SHGetSpecialFolderPath(0, AppDataPath, CSIDL_APPDATA, false);
    Result := ConcatPaths([AppDataPath, 'torchat2']);
    //{$fatal Windows is not yet supported}
  {$else}
    Result := ExpandFileName('~/.torchat2');
  {$endif}
  if not DirectoryExists(Result) then begin
    writeln('creating config dir ' + Result);
    if not CreateDir(Result) then begin
      writeln('(0) could not create config dir ' + Result);
    end;
  end;
end;

function TClientConfig.PathTorExe: String;
begin
  Result := FPathTorExe;
end;

function TClientConfig.ListenPort: DWord;
begin
  Result := 11009;
end;

function TClientConfig.TorHostName: String;
begin
  Result := 'localhost'
end;

function TClientConfig.TorPort: DWord;
begin
  Result := 11109;
end;

function TClientConfig.HiddenServiceName: String;
var
  FileName: String;
  HostnameFile: TFileStream = nil;
const
  OnionLength = 16;
begin
  FileName := ConcatPaths([DataDir, 'tor/hidden_service/hostname']);
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

function DefaultPathTorExe: String;
{$ifdef windows}
var
  ProgramsPath: Array[0..MaxPathLen] of Char;
{$endif}
begin
  {$ifdef windows}
    SHGetSpecialFolderPath(0, ProgramsPath, CSIDL_PROGRAM_FILES, false);
    Result := ConcatPaths([ProgramsPath, 'Tor', 'tor.exe']);
  {$else}
    Result := '/usr/sbin/tor';
  {$endif}
end;

end.

