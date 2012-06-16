{ lNet Events abstration

  CopyRight (C) 2006-2008 Ales Katona

  This library is Free software; you can rediStribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is diStributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; withOut even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a Copy of the GNU Library General Public License
  along with This library; if not, Write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
  
  This license has been modified. See File LICENSE.ADDON for more inFormation.
  Should you find these sources without a LICENSE File, please contact
  me at ales@chello.sk
}

unit lEvents;

{$mode objfpc}{$H+}
{$inline on}
{$define nochoice}  // let's presume we don't have "optimized" eventer

interface

uses
  {$ifdef Linux}
    {$undef nochoice} // undefine for all "Optimized" targets
    Linux, Contnrs, Errors,
  {$endif}
  {$ifdef BSD}
    {$undef nochoice}
    BSD, Errors,
  {$endif}
  {$i sys/osunits.inc}

type
  TLHandle = class;
  TLEventer = class;

  TLHandleEvent = procedure (aHandle: TLHandle) of object;
  TLHandleErrorEvent = procedure (aHandle: TLHandle; const msg: string) of object;
  TLEventerErrorEvent = procedure (const msg: string; Sender: TLEventer) of object;
  
  { TLHandle }

  TLHandle = class(TObject)
   protected
    FHandle: THandle;
    FEventer: TLEventer;     // "queue holder"
    FOnRead: TLHandleEvent;
    FOnWrite: TLHandleEvent;
    FOnError: TLHandleErrorEvent;
    FIgnoreWrite: Boolean;   // so we can do edge-triggered
    FIgnoreRead: Boolean;    // so we can do edge-triggered
    FIgnoreError: Boolean;   // so we can do edge-triggered
    FIsAcceptor: Boolean;    // if socket was server-accepted
    FDispose: Boolean;       // will free in the after-cycle
    FFreeing: Boolean;       // used to see if it's in the "to be freed" list
    FPrev: TLHandle;
    FNext: TLHandle;
    FFreeNext: TLHandle;
    FInternalData: Pointer;
    
    procedure SetIgnoreError(const aValue: Boolean);
    procedure SetIgnoreWrite(const aValue: Boolean);
    procedure SetIgnoreRead(const aValue: Boolean);
   public
    UserData: Pointer;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Free; virtual;          // this is a trick
    property Prev: TLHandle read FPrev write FPrev;
    property Next: TLHandle read FNext write FNext;
    property FreeNext: TLHandle read FFreeNext write FFreeNext;
    property IgnoreWrite: Boolean read FIgnoreWrite write SetIgnoreWrite;
    property IgnoreRead: Boolean read FIgnoreRead write SetIgnoreRead;
    property IgnoreError: Boolean read FIgnoreError write SetIgnoreError;
    property OnRead: TLHandleEvent read FOnRead write FOnRead;
    property OnWrite: TLHandleEvent read FOnWrite write FOnWrite;
    property OnError: TLHandleErrorEvent read FOnError write FOnError;
    property Dispose: Boolean read FDispose write FDispose;
    property Handle: THandle read FHandle write FHandle;
    property Eventer: TLEventer read FEventer;
  end;

  { TLTimer }
{
  TLTimer = class(TObject)
  protected
    FOnTimer: TNotifyEvent;
    FInterval: TDateTime;
    FTimeout: TDateTime;
    FPeriodic: Boolean;
    FEnabled: Boolean;
    FNext: TLTimer;

    function  GetInterval: Integer;
    procedure SetEnabled(NewEnabled: Boolean);
    procedure SetInterval(NewInterval: Integer);
  public
    procedure CallAction;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Interval: Integer read GetInterval write SetInterval;
    property Periodic: Boolean read FPeriodic write FPeriodic;
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
  end;
}
  { TLTimeoutManager }
{
  TLSetTimeout = procedure(NewTimeout: DWord) of object;

  TLTimeoutManager = class
  protected
    FFirst: TLTimer;
    FLast: TLTimer;
    FTimeout: DWord;
    FSetTimeout: TLSetTimeout;
  public
    destructor Destroy; override;

    procedure AddTimer(ATimer: TLTimer);
    procedure RemoveTimer(ATimer: TLTimer);

    procedure CallAction;
  end;
}
  { TLEventer }

  TLEventer = class
   protected
    FRoot: TLHandle;
    FCount: Integer;
    FOnError: TLEventerErrorEvent;
    FReferences: Integer;
    FFreeRoot: TLHandle; // the root of "free" list if any
    FFreeIter: TLHandle; // the last of "free" list if any
    FInLoop: Boolean;
    function GetCount: Integer; virtual;
    function GetTimeout: Integer; virtual;
    procedure SetTimeout(const Value: Integer); virtual;
    function Bail(const msg: string; const Ernum: Integer): Boolean;
    procedure AddForFree(aHandle: TLHandle);
    procedure FreeHandles;
    procedure HandleIgnoreError(aHandle: TLHandle); virtual;
    procedure HandleIgnoreWrite(aHandle: TLHandle); virtual;
    procedure HandleIgnoreRead(aHandle: TLHandle); virtual;
    function GetInternalData(aHandle: TLHandle): Pointer;
    procedure SetInternalData(aHandle: TLHandle; const aData: Pointer);
    procedure SetHandleEventer(aHandle: TLHandle);
    procedure InternalUnplugHandle(aHandle: TLHandle); virtual;
   public
    constructor Create; virtual;
    destructor Destroy; override;
    function AddHandle(aHandle: TLHandle): Boolean; virtual;
    function CallAction: Boolean; virtual;
    procedure RemoveHandle(aHandle: TLHandle); virtual;
    procedure UnplugHandle(aHandle: TLHandle);
    procedure UnregisterHandle(aHandle: TLHandle); virtual;
    procedure LoadFromEventer(aEventer: TLEventer); virtual;
    procedure Clear;
    procedure AddRef;
    procedure DeleteRef;
    property Timeout: Integer read GetTimeout write SetTimeout;
    property OnError: TLEventerErrorEvent read FOnError write FOnError;
    property Count: Integer read GetCount;
  end;
  TLEventerClass = class of TLEventer;
  
  { TLSelectEventer }

  TLSelectEventer = class(TLEventer)
   protected
    FTimeout: TTimeVal;
    FReadFDSet: TFDSet;
    FWriteFDSet: TFDSet;
    FErrorFDSet: TFDSet;
    function GetTimeout: Integer; override;
    procedure SetTimeout(const Value: Integer); override;
    procedure ClearSets;
   public
    constructor Create; override;
    function CallAction: Boolean; override;
  end;
  
{$i sys/lkqueueeventerh.inc}
{$i sys/lepolleventerh.inc}

  function BestEventerClass: TLEventerClass;
  
implementation

uses
  syncobjs,
  lCommon;
  
var
  CS: TCriticalSection;
  
{ TLHandle }

procedure TLHandle.SetIgnoreError(const aValue: Boolean);
begin
  if FIgnoreError <> aValue then begin
    FIgnoreError := aValue;
    if Assigned(FEventer) then
      FEventer.HandleIgnoreError(Self);
  end;
end;

procedure TLHandle.SetIgnoreWrite(const aValue: Boolean);
begin
  if FIgnoreWrite <> aValue then begin
    FIgnoreWrite := aValue;
    if Assigned(FEventer) then
      FEventer.HandleIgnoreWrite(Self);
  end;
end;

procedure TLHandle.SetIgnoreRead(const aValue: Boolean);
begin
  if FIgnoreRead <> aValue then begin
    FIgnoreRead := aValue;
    if Assigned(FEventer) then
      FEventer.HandleIgnoreRead(Self);
  end;
end;

constructor TLHandle.Create;
begin
  FOnRead := nil;
  FOnWrite := nil;
  FOnError := nil;
  UserData := nil;
  FEventer := nil;
  FPrev := nil;
  FNext := nil;
  FFreeNext := nil;
  FFreeing := False;
  FDispose := False;
  FIgnoreWrite := False;
  FIgnoreRead := False;
  FIgnoreError := False;
end;

destructor TLHandle.Destroy;
begin
  if Assigned(FEventer) then
    FEventer.InternalUnplugHandle(Self);
end;

procedure TLHandle.Free;
begin
  CS.Enter;

  if Assigned(FEventer) and FEventer.FInLoop then
    FEventer.AddForFree(Self)
  else
    inherited Free;

  CS.Leave;
end;

{ TLTimer }
{
function TLTimer.GetInterval: Integer;
begin
  Result := Round(FInterval * MSecsPerDay);
end;

procedure TLTimer.SetEnabled(NewEnabled: integer);
begin
  FTimeout := Now + Interval;
  FEnabled := true;
end;

procedure TLTimer.SetInterval(const aValue: Integer);
begin
  FInterval := AValue / MSecsPerDay;
end;

procedure TLTimer.CallAction;
begin
  if FEnabled and Assigned(FOnTimer) and (Now - FStarted >= FInterval) then 
  begin
    FOnTimer(Self);
    if not FOneShot then
      FStarted := Now
    else
      FEnabled := false;
  end;
end;
}
{ TLEventer }

constructor TLEventer.Create;
begin
  FRoot := nil;
  FFreeRoot := nil;
  FFreeIter := nil;
  FInLoop := False;
  FCount := 0;
  FReferences := 1;
end;

destructor TLEventer.Destroy;
begin
  Clear;
end;

function TLEventer.GetCount: Integer;
begin
  Result := FCount;
end;

function TLEventer.GetTimeout: Integer;
begin
  Result := 0;
end;

procedure TLEventer.SetTimeout(const Value: Integer);
begin
end;

function TLEventer.Bail(const msg: string; const Ernum: Integer): Boolean;
begin
  Result := False; // always false, substitute for caller's result
  if Assigned(FOnError) then
    FOnError(msg + LStrError(Ernum), Self);
end;

procedure TLEventer.AddForFree(aHandle: TLHandle);
begin
  if not aHandle.FFreeing then begin
    aHandle.FFreeing := True;
    if not Assigned(FFreeIter) then begin
      FFreeIter := aHandle;
      FFreeRoot := aHandle;
    end else begin
      FFreeIter.FreeNext := aHandle;
      FFreeIter := aHandle;
    end;
  end;
end;

procedure TLEventer.FreeHandles;
var
  Temp, Temp2: TLHandle;
begin
  Temp := FFreeRoot;
  while Assigned(Temp) do begin
    Temp2 := Temp.FreeNext;
    Temp.Free;
    Temp := Temp2;
  end;
  FFreeRoot := nil;
  FFreeIter := nil;
end;

procedure TLEventer.HandleIgnoreError(aHandle: TLHandle);
begin

end;

procedure TLEventer.HandleIgnoreWrite(aHandle: TLHandle);
begin

end;

procedure TLEventer.HandleIgnoreRead(aHandle: TLHandle);
begin

end;

function TLEventer.GetInternalData(aHandle: TLHandle): Pointer;
begin
  Result := aHandle.FInternalData;
end;

procedure TLEventer.SetInternalData(aHandle: TLHandle; const aData: Pointer);
begin
  aHandle.FInternalData := aData;
end;

procedure TLEventer.SetHandleEventer(aHandle: TLHandle);
begin
  aHandle.FEventer := Self;
end;

procedure TLEventer.InternalUnplugHandle(aHandle: TLHandle);
begin
  if aHandle.FEventer = Self then begin
    if aHandle.FEventer.FInLoop then begin
      aHandle.FEventer.AddForFree(aHandle);
      Exit;
    end;

    aHandle.FEventer := nil; // avoid recursive AV
    if Assigned(aHandle.FPrev) then begin
      aHandle.FPrev.FNext := aHandle.FNext;
      if Assigned(aHandle.FNext) then
        aHandle.FNext.FPrev := aHandle.FPrev;
    end else if Assigned(aHandle.FNext) then begin
      aHandle.FNext.FPrev := aHandle.FPrev;
      if aHandle = FRoot then
        FRoot := aHandle.FNext;
    end else FRoot := nil;
    if FCount > 0 then
      Dec(FCount);
  end;
end;

function TLEventer.AddHandle(aHandle: TLHandle): Boolean;
begin
  Result := False;
  if not Assigned(aHandle.FEventer) then begin
    if not Assigned(FRoot) then begin
      FRoot := aHandle;
    end else begin
      if Assigned(FRoot.FNext) then begin
        FRoot.FNext.FPrev := aHandle;
        aHandle.FNext := FRoot.FNext;
      end;
      FRoot.FNext := aHandle;
      aHandle.FPrev := FRoot;
    end;
    aHandle.FEventer := Self;
    Inc(FCount);
    Result := True;
  end;
end;

function TLEventer.CallAction: Boolean;
begin
  Result := True;
  // override in ancestor
end;

procedure TLEventer.RemoveHandle(aHandle: TLHandle);
begin
  aHandle.Free;
end;

procedure TLEventer.UnplugHandle(aHandle: TLHandle);
begin
  CS.Enter;

  InternalUnplugHandle(aHandle);

  CS.Leave;
end;

procedure TLEventer.UnregisterHandle(aHandle: TLHandle);
begin
  // do nothing, specific to win32 LCLEventer crap (windows is shit)
end;

procedure TLEventer.LoadFromEventer(aEventer: TLEventer);
begin
  Clear;
  FRoot := aEventer.FRoot;
  FOnError := aEventer.FOnError;
end;

procedure TLEventer.Clear;
var
  Temp1, Temp2: TLHandle;
begin
  Temp1 := FRoot;
  Temp2 := FRoot;
  while Assigned(Temp2) do begin
    Temp1 := Temp2;
    Temp2 := Temp1.FNext;
    Temp1.Free;
  end;
  FRoot := nil;
end;

procedure TLEventer.AddRef;
begin
  Inc(FReferences);
end;

procedure TLEventer.DeleteRef;
begin
  if FReferences > 0 then
    Dec(FReferences);
  if FReferences = 0 then
    Free;
end;

{ TLSelectEventer }

constructor TLSelectEventer.Create;
begin
  inherited Create;
  FTimeout.tv_sec := 0;
  FTimeout.tv_usec := 0;
end;

function TLSelectEventer.GetTimeout: Integer;
begin
  if FTimeout.tv_sec < 0 then
    Result := -1
  else
    Result := (FTimeout.tv_sec * 1000) + FTimeout.tv_usec;
end;

procedure TLSelectEventer.SetTimeout(const Value: Integer);
begin
  if Value >= 0 then begin
    FTimeout.tv_sec := Value div 1000;
    FTimeout.tv_usec := Value mod 1000;
  end else begin
    FTimeout.tv_sec := -1;
    FTimeout.tv_usec := 0;
  end;
end;

procedure TLSelectEventer.ClearSets;
begin
  fpFD_ZERO(FReadFDSet);
  fpFD_ZERO(FWriteFDSet);
  fpFD_ZERO(FErrorFDSet);
end;

function TLSelectEventer.CallAction: Boolean;
var
  Temp, Temp2: TLHandle;
  n: Integer;
  MaxHandle: THandle;
  TempTime: TTimeVal;
begin
  if FInLoop then
    Exit;

  if not Assigned(FRoot) then begin
    Sleep(FTimeout.tv_sec * 1000 + FTimeout.tv_usec div 1000);
    Exit;
  end;

  FInLoop := True;
  Temp := FRoot;
  MaxHandle := 0;
  ClearSets;
  while Assigned(Temp) do begin
    if  (not Temp.FDispose       )  // handle still valid
    and (   (not Temp.IgnoreWrite)  // check write or
         or (not Temp.IgnoreRead )  // check read or
         or (not Temp.IgnoreError)) // check for errors
    then begin
      if not Temp.IgnoreWrite then
        fpFD_SET(Temp.FHandle, FWriteFDSet);
      if not Temp.IgnoreRead then
        fpFD_SET(Temp.FHandle, FReadFDSet);
      if not Temp.IgnoreError then
        fpFD_SET(Temp.FHandle, FErrorFDSet);
      if Temp.FHandle > MaxHandle then
        MaxHandle := Temp.FHandle;
    end;
    Temp2 := Temp;
    Temp := Temp.FNext;
    if Temp2.FDispose then
      Temp2.Free;
  end;

  TempTime := FTimeout;
  if FTimeout.tv_sec >= 0 then
    n := fpSelect(MaxHandle + 1, @FReadFDSet, @FWriteFDSet, @FErrorFDSet, @TempTime)
  else
    n := fpSelect(MaxHandle + 1, @FReadFDSet, @FWriteFDSet, @FErrorFDSet, nil);
  
  if n < 0 then
    Bail('Error on select', LSocketError);
  Result := n > 0;
  
  if Result then begin
    Temp := FRoot;
    while Assigned(Temp) do begin
      if not Temp.FDispose and (fpFD_ISSET(Temp.FHandle, FWriteFDSet) <> 0) then
        if Assigned(Temp.FOnWrite) and not Temp.IgnoreWrite then
          Temp.FOnWrite(Temp);
      if not Temp.FDispose and (fpFD_ISSET(Temp.FHandle, FReadFDSet) <> 0) then
        if Assigned(Temp.FOnRead) and not Temp.IgnoreRead then
          Temp.FOnRead(Temp);
      if not Temp.FDispose and (fpFD_ISSET(Temp.FHandle, FErrorFDSet) <> 0) then
        if Assigned(Temp.FOnError) and not Temp.IgnoreError then
          Temp.FOnError(Temp, 'Handle error' + LStrError(LSocketError));
      Temp2 := Temp;
      Temp := Temp.FNext;
      if Temp2.FDispose then
        AddForFree(Temp2);
    end;
  end;
  FInLoop := False;
  if Assigned(FFreeRoot) then
    FreeHandles;
end;

{$i sys/lkqueueeventer.inc}
{$i sys/lepolleventer.inc}

{$ifdef nochoice}

function BestEventerClass: TLEventerClass;
begin
  Result := TLSelectEventer;
end;

{$endif}

initialization
  CS := TCriticalSection.Create;

finalization
  CS.Free;

end.
