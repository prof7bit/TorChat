unit torchatprotocol;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, torchatabstract, miscfunc;

type

  { TMsg }

  TMsg = class(TAMessage)
  strict protected
    FRaw : String;
  public
    constructor Create(AConnection: TAHiddenConnection; AContent: String); override;
  end;

  { TMsgPing }

  TMsgPing = class(TMsg)
    class function GetCommand: String; override;
  end;

  { TMsgPong }

  TMsgPong = class(TMsg)
    class function GetCommand: String; override;
  end;

  TMsgClass = class of TAMessage;

const
  CNT_MSG = 2;
  MsgClasses : array[1..CNT_MSG] of TMsgClass = (
    TMsgPing,
    TMsgPong
  );

function GetMsgClassFromCommand(ACommand: String): TMsgClass;

implementation

function GetMsgClassFromCommand(ACommand: String): TMsgClass;
var
  MsgClass: TMsgClass;
begin
  Result := TMsg; // default class if command is not recognized
  for MsgClass in MsgClasses do
    if MsgClass.GetCommand = ACommand then
      exit(MsgClass);
end;

constructor TMsg.Create(AConnection: TAHiddenConnection; AContent: String);
begin
  FRaw := AContent;
end;

{ TMsgPong }

class function TMsgPong.GetCommand: String;
begin
  Result := 'pong';
end;

{ TMsgPing }

class function TMsgPing.GetCommand: String;
begin
  Result := 'ping';
end;

end.

