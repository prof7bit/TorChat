unit torchatprotocol_ping;

{$mode objfpc}{$H+}

interface

uses
  torchatabstract,
  torchatprotocol;

type
  { TMsgPing }

  TMsgPing = class(TMsg)
  strict protected
    FID: String;
    FCookie: String;
    procedure Serialize; override;
  public
    class function GetCommand: String; override;
    constructor Create(ABuddy: TABuddy; ACookie: String); reintroduce;
    procedure Parse; override;
    procedure Execute; override;
  end;

implementation

{ TMsgPing }

class function TMsgPing.GetCommand: String;
begin
  Result := 'ping';
end;

constructor TMsgPing.Create(ABuddy: TABuddy; ACookie: String);
begin
  inherited Create(ABuddy);
  FCookie := ACookie;
end;

procedure TMsgPing.Parse;
begin
  FID := PopFirstWord(FBinaryContent);
  FCookie := FBinaryContent;
end;

procedure TMsgPing.Serialize;
begin
  FBinaryContent := FBuddy.ID + ' ' + FCookie;
end;

procedure TMsgPing.Execute;
var
  ABuddy: TABuddy;
begin
  WriteLn('TMsgPing.Execute()');
  Writeln('ID=' + FID + ' cookie=' + FCookie);

  ABuddy := Client.BuddyList.FindBuddy(FID);
  if Assigned(ABuddy) then
    ABuddy.MustSendPong(FCookie)
  else begin
    Writeln('I got Ping from unknown Buddy: ' + FID);
  end;
end;

begin
  RegisterMessageClass(TMsgPing);
end.

