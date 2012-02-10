unit torchatclient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, torprocess;

type

  { TTorChatClient }

  TTorChatClient = class(TComponent)
    constructor Create(AOwner: TComponent); reintroduce;
  public
    destructor Destroy; override;
  strict private
    FTor: TTor;
  end;


implementation

{ TTorChatClient }

constructor TTorChatClient.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner);
  self.FTor := TTor.Create(self);
end;

destructor TTorChatClient.Destroy;
begin
  self.FTor.Free;
  inherited Destroy;
end;

end.

