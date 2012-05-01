unit buddy;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, torchatabstract;

type

  { TBuddy }

  TBuddy = class(TABuddy)
    constructor Create(AClient: TAClient; AID: String); reintroduce;
    procedure CheckState; override;
    procedure SetIncoming(AConn: TAHiddenConnection); override;
    procedure SetOutgoing(AConn: TAHiddenConnection); override;
    procedure OnOutgoingConnection; override;
    procedure OnOutgoingConnectionFail; override;
  end;

implementation

{ TBuddy }

constructor TBuddy.Create(AClient: TAClient; AID: String);
begin
  Inherited Create(AClient);
  FClient := AClient;
  FID := AID;
end;

procedure TBuddy.CheckState;
begin

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

