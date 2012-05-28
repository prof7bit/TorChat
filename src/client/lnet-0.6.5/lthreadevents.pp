unit lThreadEvents;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  lNet, lEvents;
  
type

  { TWorkThread }

  TLWorkThread = class(TThread)
   private
    FEventer: TLEventer;
    FWorking: Boolean;
    FQuit: Boolean;
   public
    constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt=
                     DefaultStackSize);
    destructor Destroy; override;
    procedure Execute; override;
    property Working: Boolean read FWorking;
    property Quit: Boolean read FQuit write FQuit;
    property Eventer: TLEventer read FEventer;
  end;

  { TLThreadedEventer }

  TLThreadedEventer = class(TLEventer)
   protected
    FWorkThread: array of TLWorkThread;
    FThreadCount: Integer;
    FThreadsCreated: Boolean;
    FTimeout: Integer;
    procedure CreateWorkThreads(aEventerClass: TLEventerClass);
    
    function GetWorkThread(const i: Integer): TLWorkThread;
    function GetCount: Integer; override;
    function GetTimeout: Integer; override;
    procedure SetTimeout(const aValue: Integer); override;
    procedure SetThreadCount(const aValue: Integer);
   public
    constructor Create(const aThreadCount: Integer);
    constructor Create; override;
    destructor Destroy; override;
    { AddHandle is called from within lNet unit as FEventer.AddHandle
      base on TLConnection's eventer, which means this eventer }
    function AddHandle(aHandle: TLHandle): Boolean; override;
    function CallAction: Boolean; override;
   public
    property WorkThreads[i: Integer]: TLWorkThread read GetWorkThread;
    property ThreadCount: Integer read FThreadCount write SetThreadCount;
  end;
  TLThreadedEventerClass = class of TLThreadedEventer;

implementation

{ TLWorkThread }

constructor TLWorkThread.Create(CreateSuspended: Boolean;
  const StackSize: SizeUInt);
begin
  FWorking := True; // needed for special case
  
  inherited Create(CreateSuspended, StackSize);
end;

destructor TLWorkThread.Destroy;
begin
  FEventer.Free;

  inherited Destroy;
end;

procedure TLWorkThread.Execute;
begin
  FWorking := True;
  
  while not FQuit do
    FEventer.CallAction;

  FWorking := False;
  FQuit := False; // auto-flip
end;

{ TLThreadedEventer }

procedure TLThreadedEventer.CreateWorkThreads(aEventerClass: TLEventerClass);
var
  i: Integer;
begin
  SetLength(FWorkThread, FThreadCount);

  for i := 0 to FThreadCount - 1 do begin
    FWorkThread[i] := TLWorkThread.Create(True);
    FWorkThread[i].FEventer := aEventerClass.Create;
    FWorkThread[i].FEventer.Timeout := FTimeout;
    FWorkThread[i].Resume;
  end;
  
  FThreadsCreated := True;
end;

function TLThreadedEventer.GetWorkThread(const i: Integer): TLWorkThread;
begin
  Result := FWorkThread[i];
end;

function TLThreadedEventer.GetCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FThreadCount - 1 do
    Result := Result + FWorkThread[i].Eventer.Count;
end;

function TLThreadedEventer.GetTimeout: Integer;
begin
  Result := FTimeout;
end;

procedure TLThreadedEventer.SetTimeout(const aValue: Integer);
var
  i: Integer;
begin
  if aValue < 0 then
    raise Exception.Create('TThreadedEventer must have Timeout >= 0');

  FTimeout := aValue;
  if FThreadsCreated then
    for i := 0 to FThreadCount - 1 do
      FWorkThread[i].Eventer.Timeout := aValue;
end;

procedure TLThreadedEventer.SetThreadCount(const aValue: Integer);
begin
  if aValue > 0 then
    FThreadCount := aValue
  else
    FThreadCount := 1;
end;

constructor TLThreadedEventer.Create(const aThreadCount: Integer);
begin
  inherited Create;

  FTimeout := 50; // default, good enough
  SetThreadCount(aThreadCount);
end;

constructor TLThreadedEventer.Create;
begin
  Create(1);
end;

destructor TLThreadedEventer.Destroy;
var
  i: Integer;
begin
  if FThreadsCreated then begin
    for i := 0 to FThreadCount - 1 do // tell them all to quit at once, so we wait max DEF_TIMEOUT ms
      if FWorkThread[i].Working then
        FWorkThread[i].Quit := True;

    for i := 0 to FThreadCount - 1 do begin
      FWorkThread[i].WaitFor;
      FWorkThread[i].Free;
    end;
  end;

  inherited Destroy;
end;

function TLThreadedEventer.AddHandle(aHandle: TLHandle): Boolean;
var
  i, j, c: Integer;
begin
  if not FThreadsCreated then
    CreateWorkThreads(BestEventerClass);

  if aHandle is TLSocket then
    TLSocket(aHandle).SetState(ssBlocking, True);
    
  { Find the thread with lowest count }
  c := FWorkThread[0].Eventer.Count;
  j := 0;
  for i := 0 to FThreadCount - 1 do
    if FWorkThread[i].Eventer.Count < c then begin
      c := FWorkThread[i].Eventer.Count;
      j := i;
    end;
  { And add the new handle to it }
  Result := FWorkThread[j].Eventer.AddHandle(aHandle);
end;

function TLThreadedEventer.CallAction: Boolean;
begin
  Result := inherited;

  Sleep(FTimeout);
end;

end.

