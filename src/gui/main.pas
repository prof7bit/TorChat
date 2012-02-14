unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, language,
  torchatclient;

type
  { derive own class of the client to override the virtual event methods }
  TMyClient = class(TTorChatClient)

  end;

  { TFMain }
  TFMain = class(TForm)
    procedure FormCreate(Sender: TObject);
  strict private
    FClient : TMyClient;
  end;

var
  FMain: TFMain;

implementation

{$R *.lfm}

{ TFMain }

procedure TFMain.FormCreate(Sender: TObject);
begin
  Self.FClient := TMyClient.Create(Self);
end;


end.

