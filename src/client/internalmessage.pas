unit internalmessage;

{$mode objfpc}{$H+}

interface

uses
  torchatabstract,
  miscfunc;

type

  { TInternalMessage }

  TInternalMessage = class(TAMessage)
    constructor Create(AConnection: TAHiddenConnection; AEncodedContent: String); override;
    constructor Create; reintroduce;
    procedure Parse; override;
    procedure Execute; override;
    procedure Send; override;
    class function GetCommand: String; override;
  end;

  { TMsgCallMethod }

  TMsgCallMethod = class(TInternalMessage)
    Method : procedure of object;
    procedure Execute; override;
  end;

implementation

{ TMsgCallMethod }

procedure TMsgCallMethod.Execute;
begin
  Method();
end;

{ TInternalMessage }

constructor TInternalMessage.Create(AConnection: TAHiddenConnection; AEncodedContent: String);
begin
  Ignore(AConnection);
  Ignore(@AEncodedContent);
  // nothing
end;

constructor TInternalMessage.Create;
begin
  // nothing
end;

procedure TInternalMessage.Parse;
begin
  // nothing
end;

procedure TInternalMessage.Execute;
begin
  // nothing
end;

procedure TInternalMessage.Send;
begin
  // nothing
end;

class function TInternalMessage.GetCommand: String;
begin
  // nothing
  Result := '';
end;

end.

