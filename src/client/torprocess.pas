unit torprocess;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process, clientconfig, FileUtil;

type

  { TTor }

  TTor = class(TProcess)
    constructor Create; reintroduce;
  public
    destructor Destroy; override;
  end;

implementation

{ TTor }

constructor TTor.Create;
begin
  inherited Create(nil);
  self.Options := [poStderrToOutPut];
  Self.CurrentDirectory := FileUtil.AppendPathDelim(ConfGetDataDir) + 'tor';
  Self.Executable := ConfGetTorExe;
  Self.Parameters.Add('-f');
  Self.Parameters.Add('torrc.txt');
  Self.Execute;
end;

destructor TTor.Destroy;
begin
  Self.Terminate(0);
  inherited Destroy;
end;

end.

