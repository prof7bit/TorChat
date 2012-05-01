unit buddy;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, torchatabstract;

type

  { TBuddy }

  TBuddy = class(TABuddy)
    constructor Create(AClient: TAClient; AID: String; ANick: String); reintroduce;
    procedure CheckState; override;
    function AsJsonObject: TJSONObject; override;
    procedure SetNick(ANick: String); override;
    procedure SetIncoming(AConn: TAHiddenConnection); override;
    procedure SetOutgoing(AConn: TAHiddenConnection); override;
    procedure OnOutgoingConnection; override;
    procedure OnOutgoingConnectionFail; override;
  end;

implementation

{ TBuddy }

constructor TBuddy.Create(AClient: TAClient; AID: String; ANick: String);
begin
  Inherited Create(AClient);
  FClient := AClient;
  FID := AID;
  FNick := ANick;
end;

procedure TBuddy.CheckState;
begin

end;

function TBuddy.AsJsonObject: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('ID', TJSONString.Create(ID));
  Result.Add('nick', TJSONString.Create(Nick));
end;

procedure TBuddy.SetNick(ANick: String);
begin
  FNick := ANick;
  FClient.BuddyList.Save;
end;

procedure TBuddy.SetIncoming(AConn: TAHiddenConnection);
begin
  FConnIncoming := AConn;
end;

procedure TBuddy.SetOutgoing(AConn: TAHiddenConnection);
begin
  FConnOutgoing := AConn;
end;

procedure TBuddy.OnOutgoingConnection;
begin

end;

procedure TBuddy.OnOutgoingConnectionFail;
begin

end;

end.

