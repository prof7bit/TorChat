unit torchatprotocol_pong;

{$mode objfpc}{$H+}

interface

uses
  torchatabstract,
  torchatprotocol;

type
  { TMsgPong }

  TMsgPong = class(TMsg)
    class function GetCommand: String; override;
  end;


implementation

{ TMsgPong }

class function TMsgPong.GetCommand: String;
begin
  Result := 'pong';
end;


end.

