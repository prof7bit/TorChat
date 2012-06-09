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
  tc_interface,
  tc_const;

type
  { TClientConfig }

  TClientConfig = class(TInterfacedObject, IClientConfig)
  strict private
    FProfileName: String;
    FPathTorExe: String;
    FAvatarData: String;
    FAvatarAlphaData: String;
  public
    constructor Create(AProfileName: String);
    destructor Destroy; override;
    procedure Load;
    procedure Save;
    procedure SetAvatarData(RGB, Alpha: String);
    function DataDir: String;
    function PathTorExe: String;
    function ListenPort: DWord;
    function TorHostName: String;
    function TorPort: DWord;
    function AvatarData: String;
    function AvatarAlphaData: String;
  end;

function DefaultPathTorExe: String;

implementation
uses
  tc_misc,
  base64,
  fpjson,
  jsonparser;

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

procedure TClientConfig.Load;
var
  FS: TFileStream = nil;
  JParser: TJSONParser = nil;
  JObj: TJSONObject = nil;

  function TryReadString(Name: String; Base64: Boolean=False): String;
  begin
    try
      if Base64 then
        Result := DecodeStringBase64(JObj.Strings[Name])
      else
        Result := JObj.Strings[Name];
      WriteLn('config read ' + Name);
    except
      Result := '';
      WriteLn('E config read error: ' + Name);
    end;
  end;

begin
  try
    FS := TFileStream.Create(ConcatPaths([DataDir, 'config.json']), fmOpenRead);
    JParser :=TJSONParser.Create(FS);
    JObj := JParser.Parse as TJSONObject;

    FAvatarData := TryReadString('Avatar', True);
    if (Length(FAvatarData) > 0) and (Length(FAvatarData) <> 12288) then begin
      FAvatarData := '';
      WriteLn('E avatar data from config has wrong size');
    end;
    FAvatarAlphaData := TryReadString('AvatarAlpha', True);
    if (Length(FAvatarAlphaData) > 0) and (Length(FAvatarAlphaData) <> 4096) then begin
      FAvatarAlphaData := '';
      WriteLn('E avatar alpha channel data from config has wrong size');
    end;

  except
    on E: Exception do begin
      WriteLn('I TClientConfig.Load() could not load: ' + E.Message);
      WriteLn('I Will use default config');
    end;
  end;
  if assigned(FS) then FreeAndNil(FS);
  if assigned(JObj) then FreeAndNil(JObj);
  if assigned(JParser) then FreeAndNil(JParser);
end;

procedure TClientConfig.Save;
var
  Path: String;
  JObj : TJSONObject;
  JData: String;
  FileName: String;
  TempName: StrinG;
  FS: TFileStream = nil;
  Success: Boolean;
begin
  Success := False;
  Path := DataDir;
  TempName := ConcatPaths([Path,'_config.json']);
  FileName := ConcatPaths([Path,'config.json']);
  JObj := TJSONObject.Create;

  JObj.Add('Avatar', EncodeStringBase64(FAvatarData));
  JObj.Add('AvatarAlpha', EncodeStringBase64(FAvatarAlphaData));

  JData := JObj.FormatJSON();
  JObj.Free;
  try
    FS := TFileStream.Create(TempName, fmCreate + fmOpenWrite);
    FS.Write(JData[1], Length(JData));
    Success := True;
  except
    on E: Exception do begin
      writeln('E TClientConfig.Save() could not save: ' + E.Message);
    end;
  end;
  if Assigned(FS) then FreeAndNil(FS);

  if Success then begin
    SafeDelete(FileName);
    RenameFile(TempName, FileName);
  end
  else
    SafeDelete(TempName);
end;

procedure TClientConfig.SetAvatarData(RGB, Alpha: String);
begin
  FAvatarData := RGB;
  FAvatarAlphaData := Alpha;
  Save;
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

function TClientConfig.AvatarData: String;
begin
  Result := FAvatarData;
end;

function TClientConfig.AvatarAlphaData: String;
begin
  Result := FAvatarAlphaData;
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

