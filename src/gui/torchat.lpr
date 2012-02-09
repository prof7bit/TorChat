program torchat;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, main, language, torprocess, clientconfig
  { you can add units after this };

{$R *.res}

begin
  Application.Title := 'project1';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TFMain, FMain);
  Application.Run;
end.

