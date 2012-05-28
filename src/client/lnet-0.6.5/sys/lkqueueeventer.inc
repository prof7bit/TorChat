{% lkqueueeventer.inc included by levents.pas }

{$ifdef BSD}

{ TLKQueueEventer }

constructor TLKQueueEventer.Create;
begin
  inherited Create;
  Inflate;
  FFreeSlot := 0;
  FTimeout.tv_sec := 0;
  FTimeout.tv_nsec := 0;
  FQueue := KQueue;
  if FQueue < 0 then
    raise Exception.Create('Unable to create kqueue: ' + StrError(fpGetErrno));
end;

destructor TLKQueueEventer.Destroy;
begin
  fpClose(FQueue);
  inherited Destroy;
end;

function TLKQueueEventer.GetTimeout: Integer;
begin
  Result := FTimeout.tv_sec + FTimeout.tv_nsec * 1000 * 1000;
end;

procedure TLKQueueEventer.SetTimeout(const Value: Integer);
begin
  if Value >= 0 then begin
    FTimeout.tv_sec := Value div 1000;
    FTimeout.tv_nsec := (Value mod 1000) * 1000;
  end else begin
    FTimeout.tv_sec := -1;
    FTimeout.tv_nsec := 0;
  end;
end;

procedure TLKQueueEventer.HandleIgnoreRead(aHandle: TLHandle);
const
  INBOOL: array[Boolean] of Integer = (EV_ENABLE, EV_DISABLE);
begin
  EV_SET(@FChanges[FFreeSlot], aHandle.FHandle, EVFILT_READ,
         INBOOL[aHandle.IgnoreRead], 0, 0, Pointer(aHandle));

  Inc(FFreeSlot);
  if FFreeSlot > Length(FChanges) then
    Inflate;
end;

procedure TLKQueueEventer.Inflate;
const
  BASE_SIZE = 100;
var
  OldLength: Integer;
begin
  OldLength := Length(FChanges);
  if OldLength > 1 then begin
    SetLength(FChanges, Sqr(OldLength));
    SetLength(FEvents, Sqr(OldLength));
  end else begin
    SetLength(FChanges, BASE_SIZE);
    SetLength(FEvents, BASE_SIZE);
  end;
end;

function TLKQueueEventer.AddHandle(aHandle: TLHandle): Boolean;
begin
  Result := inherited AddHandle(aHandle);

  if FFreeSlot > Length(FChanges) then
    Inflate;
  EV_SET(@FChanges[FFreeSlot], aHandle.FHandle, EVFILT_WRITE,
         EV_ADD or EV_CLEAR, 0, 0, Pointer(aHandle));
  Inc(FFreeSlot);

  if FFreeSlot > Length(FChanges) then
    Inflate;
  if not aHandle.FIgnoreRead then begin
    EV_SET(@FChanges[FFreeSlot], aHandle.FHandle, EVFILT_READ,
           EV_ADD, 0, 0, Pointer(aHandle));
    Inc(FFreeSlot);
  end;
end;

function TLKQueueEventer.CallAction: Boolean;
var
  i, n: Integer;
  Temp: TLHandle;
begin
  Result := False;
  if FInLoop then
    Exit;

  if FTimeout.tv_sec >= 0 then
    n := KEvent(FQueue, @FChanges[0], FFreeSlot,
              @FEvents[0], Length(FEvents), @FTimeout)
  else
    n := KEvent(FQueue, @FChanges[0], FFreeSlot,
              @FEvents[0], Length(FEvents), nil);

  FFreeSlot := 0;
  if n < 0 then
    Bail('Error on kqueue', LSocketError);
  Result := n > 0;
  if Result then begin
    FInLoop := True;
    for i := 0 to n-1 do begin
      Temp := TLHandle(FEvents[i].uData);
      
      if  (not Temp.FDispose)
      and (FEvents[i].Filter = EVFILT_WRITE) then
        if Assigned(Temp.FOnWrite) and not Temp.IgnoreWrite then
          Temp.FOnWrite(Temp);

      if  (not Temp.FDispose)
      and (FEvents[i].Filter = EVFILT_READ) then
        if Assigned(Temp.FOnRead) and not Temp.IgnoreRead then
          Temp.FOnRead(Temp);
      
      if  (not Temp.FDispose)
      and ((FEvents[i].Flags and EV_ERROR) > 0) then
        if Assigned(Temp.FOnError) and not Temp.IgnoreError then
          Temp.FOnError(Temp, 'Handle error' + LStrError(LSocketError));

      if Temp.FDispose then
        AddForFree(Temp);
    end;
    FInLoop := False;
    if Assigned(FFreeRoot) then
      FreeHandles;
  end;
end;

function BestEventerClass: TLEventerClass;
begin
  {$IFNDEF FORCE_SELECT}
  Result := TLKQueueEventer;
  {$ELSE}
  Result := TLSelectEventer;
  {$ENDIF}
end;

{$endif} // BSD

