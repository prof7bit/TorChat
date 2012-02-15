unit torprocess;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process, clientconfig, FileUtil;

type

  { TTor }

  TTor = class(TProcess)
    constructor Create(AOwner: TComponent); override;
  public
    destructor Destroy; override;
  end;

implementation

{ TTor }

constructor TTor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Options := [poStderrToOutPut];
  CurrentDirectory := FileUtil.AppendPathDelim(ConfGetDataDir) + 'tor';
  Executable := ConfGetTorExe;
  Parameters.Add('-f');
  Parameters.Add('torrc.txt');
  Execute;
end;

destructor TTor.Destroy;
begin
  Terminate(0);
  inherited Destroy;
end;

end.

