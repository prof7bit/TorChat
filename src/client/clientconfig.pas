unit clientconfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil;

  function GetDataDir : String;

implementation

function GetDataDir: String;
begin
  Result := ProgramDirectory;
end;

end.

