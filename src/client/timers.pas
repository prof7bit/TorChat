unit timers;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils;

type
  PTimerFunc = function(): Boolean of object;

  TTimerEntry = record
    ID: Integer;
    Interval: Integer;
    Time: QWord;
    Func: PTimerFunc;
    Remove: Boolean;
  end;

  { TTimer }

  TTimer = class(TThread)
    CS: TRTLCriticalSection;
    NextID: Integer;
    TimeTable: array of TTimerEntry;
    constructor Create();
    destructor Destroy; override;
    procedure Execute; override;
    procedure CheckInterval;
    procedure AddTimer(Interval: Integer; Func: PTimerFunc);
  strict private
    function CallFunc(Index: Integer): Boolean;
  end;

implementation

function NowMilli: QWord; inline;
begin
  Result := round(Now*24*60*60*1000);
end;

constructor TTimer.Create;
begin
  InitCriticalsection(CS);
  NextID := 1;
  Inherited Create(False);
end;

destructor TTimer.Destroy;
var
  Entry: TTimerEntry;
begin
  DoneCriticalsection(CS);
  inherited Destroy;
end;

procedure TTimer.Execute;
begin
  while not Terminated do begin
    CheckInterval;
    Sleep(100);
  end;
end;

procedure TTimer.CheckInterval;
var
  I, P, L: Integer;
begin
  P := 0;
  while P < Length(TimeTable) do begin
    if TimeTable[P].Time < NowMilli then begin
      if CallFunc(P) then begin
        Inc(TimeTable[P].Time, TimeTable[P].Interval);
        if TimeTable[P].Time < NowMilli then
          TimeTable[P].Time := NowMilli + TimeTable[P].Interval;
      end
      else
        TimeTable[P].Remove := True;
    end;
    Inc(P);
  end;

  EnterCriticalsection(CS);
  L := Length(TimeTable);
  P := 0;
  while P < L do begin
    if TimeTable[P].Remove then begin
      if P < L-1 then begin
        TimeTable[P] := TimeTable[L-1];
      end;
      Dec(P);
      Dec(L);
    end;
    Inc(P);
  end;
  SetLength(TimeTable, L);
  LeaveCriticalsection(CS);
end;

procedure TTimer.AddTimer(Interval: Integer; Func: PTimerFunc);
var
  P: Integer;
begin
  EnterCriticalsection(CS);
  P := Length(TimeTable);
  SetLength(TimeTable, P+1);
  TimeTable[P].ID := NextID;
  TimeTable[P].Interval := Interval;
  TimeTable[P].Time := round(Now*24*60*60*1000) + Interval;
  TimeTable[P].Func := Func;
  TimeTable[P].Remove := False;
  Inc(NextID);
  LeaveCriticalsection(CS);
end;

function TTimer.CallFunc(Index: Integer): Boolean;
begin
  try
    Result := TimeTable[Index].Func();
  except
    Result := False;
  end;
end;


end.

