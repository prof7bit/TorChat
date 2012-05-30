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
  SECONDS_SEND_KEEPLIVE = 120;   // don't change, protocol specification
  SECONDS_WAIT_KEEPALIVE = 240;  // don't change, protocol specification
  SECONDS_WAIT_FOR_PONG = 240;   // incoming conn timeout, should be 240

  SECONDS_WAIT_FOR_HOSTNAME_FILE = 20;
  SECONDS_INITIAL_RECONNECT = 10;
  RECONNECT_SLOWDOWN = 1.2;


type
  { TClientConfig }

  TClientConfig = class(TInterfacedObject, IClientConfig)
  strict private
    FProfileName: String;
    FPathTorExe: String;
  public
    constructor Create(AProfileName: String);
    destructor Destroy; override;
    function DataDir: String;
    function PathTorExe: String;
    function ListenPort: DWord;
    function TorHostName: String;
    function TorPort: DWord;
  end;

function DefaultPathTorExe: String;

implementation

{ TClientConfig }

constructor TClientConfig.Create(AProfileName: String);
begin
  FProfileName := AProfileName;
  FPathTorExe := DefaultPathTorExe;
end;

destructor TClientConfig.Destroy;
begin
  WriteLn('TClientConfig.Destroy() ' + FProfileName);
  inherited Destroy;
end;

function TClientConfig.DataDir: String;
var
  Success: Boolean;
  {$ifdef windows}
  AppDataPath: Array[0..MaxPathLen] of Char;
  {$endif}
begin
  {$ifdef windows}
    SHGetSpecialFolderPath(0, AppDataPath, CSIDL_APPDATA, false);
    Result := ConcatPaths([AppDataPath, 'torchat2']);
    //{$fatal Windows is not yet supported}
  {$else}
    Result := ExpandFileName('~/.torchat2');
  {$endif}

  if FProfileName <> '' then
    Result := Result + '_' + FProfileName;

  if not DirectoryExists(Result) then begin
    Success := False;
    if CreateDir(Result) then begin
      if CreateDir(ConcatPaths([Result, 'tor'])) then begin
        writeln('I created empty config directory ' + Result);
        Success := True;
      end;
    end;
    if not Success then
      writeln('E could not create config directory ' + Result);
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

