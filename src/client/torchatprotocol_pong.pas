unit torchatprotocol_pong;

{$mode objfpc}{$H+}

interface

uses
  torchatabstract,
  torchatprotocol;

type
  { TMsgPong }

  TMsgPong = class(TMsg)
  strict protected
    FCookie: String;
    procedure Serialize; override;
  public
    constructor Create(ABuddy: TABuddy; ACookie: String); reintroduce;
    procedure Parse; override;
    procedure Execute; override;
    class function GetCommand: String; override;
  end;


implementation

{ TMsgPong }

constructor TMsgPong.Create(ABuddy: TABuddy; ACookie: String);
begin
  inherited Create(ABuddy);
  FCookie := ACookie;
end;

procedure TMsgPong.Serialize;
begin
  FBinaryContent := FCookie;
end;

procedure TMsgPong.Parse;
begin
  FCookie := FBinaryContent;
end;

procedure TMsgPong.Execute;
var
  ABuddy: TABuddy;
begin
  WriteLn('TMsgPong.Execute');
  WriteLn(FCookie);
  ABuddy := Client.BuddyList.FindBuddyByCookie(FCookie);
  if Assigned(ABuddy) then begin
    ABuddy.SetIncoming(FConnection);
    ABuddy.OnIncomingConnection;
  end;
end;

class function TMsgPong.GetCommand: String;
begin
  Result := 'pong';
end;

begin
  RegisterMessageClass(TMsgPong);
end.

