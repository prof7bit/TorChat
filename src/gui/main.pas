unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, language,
  torprocess;

type

  { TFMain }

  TFMain = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
    Tor : TTor;
  public
    { public declarations }
  end;

var
  FMain: TFMain;

implementation

{$R *.lfm}

{ TFMain }

procedure TFMain.FormCreate(Sender: TObject);
begin
  Self.Tor := TTor.Create(Self);
end;

procedure TFMain.FormDestroy(Sender: TObject);
begin
  Self.Tor.Free;
end;

end.

