unit clientconfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil;

  function GetDataDir: String;
  function GetTorHost: String;
  function GetTorPort: DWord;

implementation

function GetDataDir: String;
begin
  Result := ProgramDirectory;
end;

function GetTorHost: String;
begin
  Result := 'localhost'
end;

function GetTorPort: DWord;
begin
  Result := 11109;
end;

end.

