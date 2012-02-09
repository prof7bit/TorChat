unit torprocess;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process, clientconfig, FileUtil;

type

  { TTor }

  TTor = class(TProcess)
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
  end;

implementation

{ TTor }

constructor TTor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  self.Options := [poStderrToOutPut];
  Self.CurrentDirectory := FileUtil.AppendPathDelim(GetDataDir) + 'tor';
  Self.Executable := '/usr/sbin/tor';
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

