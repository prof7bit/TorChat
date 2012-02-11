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

