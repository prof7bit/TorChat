program torchat;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, main, connection;

{$R *.res}

begin
  Application.Title := 'project1';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TFMain, FMain);
  Application.Run;
end.

