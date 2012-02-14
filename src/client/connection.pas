unit connection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, networking, torchatabstract, receiver;

type

  { THiddenConnection }

  THiddenConnection = class(TAHiddenConnection)
    constructor Create(AHandle: THandle); override;
    procedure Send(AData: String); override;
    procedure SendLine(ALine: String); override;
    procedure OnConnectionClose; override;
    destructor Destroy; override;
  end;

implementation

{ THiddenConnection }


constructor THiddenConnection.Create(AHandle: THandle);
begin
  inherited Create(AHandle);
  FReceiver := TReceiver.Create(Self);
  WriteLn('created connection');
end;

procedure THiddenConnection.Send(AData: String);
begin
  Self.Write(AData[1], Length(AData));
end;

procedure THiddenConnection.SendLine(ALine: String);
begin
  Self.Send(ALine + #10);
end;

procedure THiddenConnection.OnConnectionClose;
begin
  WriteLn('*** OnConnectionClose');
end;

destructor THiddenConnection.Destroy;
begin
  Self.DoClose; // this will also let the receiver leave the blocking recv()
  FReceiver.Terminate;
  FReceiver.Free;
  inherited Destroy;
end;

initialization
  // tell the networking unit which TConnection descendant
  // we want it to use when creating new connection obects
  networking.ConnectionClass := THiddenConnection;
  networking.SOCKS_USER_ID := 'torchat';
end.

