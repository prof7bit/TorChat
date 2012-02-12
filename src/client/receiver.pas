unit receiver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, torchatabstract;

type
  { TReceiver - Each conection contains a TAReceiver object which is
    a thread reading from the blocking socket, splitting the received
    data into individual messages and for each of them instantiate the
    appropriate message class to parse and process the message }
  TReceiver = class(TAReceiver)
    constructor Create(AConn: TAHiddenConnection);
    destructor Destroy; override;
    procedure Execute; override;
  end;

implementation

{ TReceiver }

constructor TReceiver.Create(AConn: TAHiddenConnection);
begin
  FConnection := AConn;
  inherited Create(False);
end;

destructor TReceiver.Destroy;
begin
  inherited Destroy;
end;

procedure TReceiver.Execute;
var
  B : array[0..1024] of Char = #0;
  N : Integer;
begin
  repeat
    N := FConnection.Read(B, 1024);
    B[N] := #0;
    write(PChar(@B[0]));
  until (N = 0) or Terminated;
  FConnection.DoClose; // only shutdown and close the socket handle
  FConnection.OnConnectionClose;
end;

end.

