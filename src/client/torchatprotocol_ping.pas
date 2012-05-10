unit torchatprotocol_ping;

{$mode objfpc}{$H+}

interface

uses
  torchatabstract,
  torchatprotocol;

type
  { TMsgPing }

  TMsgPing = class(TMsg)
    FCookie: String;
    class function GetCommand: String; override;
    constructor Create(ABuddy: TABuddy; ACookie: String); reintroduce;
    procedure Parse; override;
    procedure Serialize; override;
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
  FCookie := FBinaryContent;
end;

procedure TMsgPing.Serialize;
begin
  FBinaryContent := FCookie;
end;

procedure TMsgPing.Execute;
begin
  WriteLn('TMsgPing.Execute()');

  WriteLn('will now simply close the connection to see if this would work without leaking memory');
  FConnection.Stream.DoClose;
end;

end.

