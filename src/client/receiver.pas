unit receiver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, torchatabstract, torchatprotocol, miscfunc;

type
  { TReceiver - Each conection contains a TAReceiver object which is
    a thread reading from the blocking socket, splitting the received
    data into individual messages and for each of them instantiate the
    appropriate message class to parse and process the message }
  TReceiver = class(TAReceiver)
    constructor Create(AConn: TAHiddenConnection);
    destructor Destroy; override;
    procedure Execute; override;
  strict private
    FIncompleteMessage: String;
    procedure OnReceivedLine(Line: String);
  end;

implementation


{ TReceiver }

constructor TReceiver.Create(AConn: TAHiddenConnection);
begin
  FConnection := AConn;
  FIncompleteMessage := '';
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
  R : String;
  M : String;
begin
  repeat
    N := FConnection.Read(B, 1024);
    if N > 0 then begin
      SetLength(R, N);
      Move(B, R[1], N);
      While R <> '' do begin
        try
          M := Split(R, #10);
          OnReceivedLine(FIncompleteMessage + M);
          FIncompleteMessage := '';
        except
          on E:EEndOfString do begin
            FIncompleteMessage := R;
            R := '';
          end;
        end;
      end;
    end;
  until (N = 0) or Terminated;
  FConnection.DoClose; // only shutdown and close the socket handle
  FConnection.OnConnectionClose;
end;

procedure TReceiver.OnReceivedLine(Line: String);
var
  Command: String;
  Msg: TAMessage;
begin
  try
    Command := Split(Line, ' ');
  except
    exit;
  end;

  Msg := GetMsgClassFromCommand(Command).Create(Self.FConnection, Line);
  try
    Msg.Parse;
    Msg.Execute;
  except
    on Ex: Exception do begin
      WriteLn(Ex.Message);
    end;
  end;
end;

end.

