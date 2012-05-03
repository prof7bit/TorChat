unit buddy;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, torchatabstract;

type

  { TBuddy }

  TBuddy = class(TABuddy)
    procedure CheckState; override;
    function AsJsonObject: TJSONObject; override;
    procedure InitFromJsonObect(AObject: TJSONObject); override;
    procedure InitID(AID: String); override;
    procedure SetIncoming(AConn: TAHiddenConnection); override;
    procedure SetOutgoing(AConn: TAHiddenConnection); override;
    procedure OnOutgoingConnection; override;
    procedure OnOutgoingConnectionFail; override;
  end;

implementation

{ TBuddy }

procedure TBuddy.CheckState;
begin

end;

function TBuddy.AsJsonObject: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('id', TJSONString.Create(FID));
  Result.Add('friendlyname', TJSONString.Create(FFriendlyName));
end;

procedure TBuddy.InitFromJsonObect(AObject: TJSONObject);
begin
  // these fields are mandatory, failing will raise exception
  FID := AObject.Strings['id'];
  FFriendlyName := AObject.Strings['friendlyname'];

  // the following fields are optional (backwards compatibility)
  // they will be tried in excatly this order from oldest fields
  // first to newest last and failing at any point will be ignored
  try
    // nothing yet
  except
    // ignore from here on (this all followig fields) because it
    // was created by an older TorChat. Fields have resonable defaults.
  end;
end;

procedure TBuddy.InitID(AID: String);
begin
  FID := AID;
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

