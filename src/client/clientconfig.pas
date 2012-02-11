unit clientconfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil;

  function ConfGetDataDir: String;
  function ConfGetListenPort: DWord;
  function ConfGetTorHost: String;
  function ConfGetTorPort: DWord;

implementation

function ConfGetDataDir: String;
begin
  Result := ProgramDirectory;
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

