{% lepolleventer.inc included by levents.pas }

{$ifdef Linux}

{ TLEpollEventer }

const
  BASE_SIZE = 100;
  // bug in fpc 2.0.4-
  EPOLL_CTL_ADD = 1;
  EPOLL_CTL_DEL = 2;
  EPOLL_CTL_MOD = 3;
  
  EPOLLIN  = $01; { The associated file is available for read(2) operations. }
  EPOLLPRI = $02; { There is urgent data available for read(2) operations. }
  EPOLLOUT = $04; { The associated file is available for write(2) operations. }
  EPOLLERR = $08; { Error condition happened on the associated file descriptor. }
  EPOLLHUP = $10; { Hang up happened on the associated file descriptor. }
  EPOLLONESHOT = 1 shl 30;
  EPOLLET  = 1 shl 31; { Sets  the  Edge  Triggered  behaviour  for  the  associated file descriptor. }


constructor TLEpollEventer.Create;
var
  lEvent: TEpollEvent;
begin
  inherited Create;
  FFreeList := TFPObjectList.Create;
  Inflate;
  FTimeout := 0;
  FEpollFD := epoll_create(BASE_SIZE);
  FEpollReadFD := epoll_create(BASE_SIZE);
  FEpollMasterFD := epoll_create(2);
  if (FEPollFD < 0) or (FEpollReadFD < 0) or (FEpollMasterFD < 0) then
    raise Exception.Create('Unable to create epoll: ' + StrError(fpgeterrno));
  lEvent.events := EPOLLIN or EPOLLOUT or EPOLLPRI or EPOLLERR or EPOLLHUP or EPOLLET;
  lEvent.data.fd := FEpollFD;
  if epoll_ctl(FEpollMasterFD, EPOLL_CTL_ADD, FEpollFD, @lEvent) < 0 then
    raise Exception.Create('Unable to add FDs to master epoll FD: ' + StrError(fpGetErrno));
  lEvent.data.fd := FEpollReadFD;
  if epoll_ctl(FEpollMasterFD, EPOLL_CTL_ADD, FEpollReadFD, @lEvent) < 0 then
    raise Exception.Create('Unable to add FDs to master epoll FD: ' + StrError(fpGetErrno));
end;

destructor TLEpollEventer.Destroy;
begin
  fpClose(FEpollFD);
  FFreeList.Free;
  inherited Destroy;
end;

function TLEpollEventer.GetTimeout: Integer;
begin
  Result := FTimeout;
end;

procedure TLEpollEventer.SetTimeout(const Value: Integer);
begin
  if Value >= 0 then
    FTimeout := Value
  else
    FTimeout := -1;
end;

procedure TLEpollEventer.HandleIgnoreRead(aHandle: TLHandle);
var
  lEvent: TEpollEvent;
begin
  lEvent.data.ptr := aHandle;
  lEvent.events := EPOLLIN or EPOLLPRI or EPOLLHUP;
  if not aHandle.IgnoreRead then begin
    if epoll_ctl(FEpollReadFD, EPOLL_CTL_ADD, aHandle.Handle, @lEvent) < 0 then
      Bail('Error modifying handle for reads', LSocketError);
  end else begin
    if epoll_ctl(FEpollReadFD, EPOLL_CTL_DEL, aHandle.Handle, @lEvent) < 0 then
      Bail('Error modifying handle for reads', LSocketError);
  end;
end;

procedure TLEpollEventer.Inflate;
var
  OldLength: Integer;
begin
  OldLength := Length(FEvents);
  if OldLength > 1 then
    SetLength(FEvents, Sqr(OldLength))
  else
    SetLength(FEvents, BASE_SIZE);
  SetLength(FEventsRead, Length(FEvents));
end;

function TLEpollEventer.AddHandle(aHandle: TLHandle): Boolean;

var
  lEvent: TEpollEvent;
begin
  Result := inherited AddHandle(aHandle);
  if Result then begin
    Result := False;
    lEvent.events := EPOLLET or EPOLLOUT or EPOLLERR;
    lEvent.data.ptr := aHandle;
    if epoll_ctl(FEpollFD, EPOLL_CTL_ADD, aHandle.FHandle, @lEvent) < 0 then
      Bail('Error adding handle to epoll', LSocketError);
    lEvent.events := EPOLLIN or EPOLLPRI or EPOLLHUP;
    if not aHandle.IgnoreRead then begin
      if epoll_ctl(FEpollReadFD, EPOLL_CTL_ADD, aHandle.FHandle, @lEvent) < 0 then
        Bail('Error adding handle to epoll', LSocketError);
    end;
    if FCount > High(FEvents) then
      Inflate;
  end;
end;

function Max(const a, b: Integer): Integer; inline;
begin
  if a > b then
    Result := a
  else
    Result := b;
end;

function TLEpollEventer.CallAction: Boolean;
var
  i, MasterChanges, Changes, ReadChanges: Integer;
  Temp, TempRead: TLHandle;
  MasterEvents: array[0..1] of TEpollEvent;
begin
  Result := False;
  if FInLoop then
    Exit;
    
  Changes := 0;
  ReadChanges := 0;

  MasterChanges := epoll_wait(FEpollMasterFD, @MasterEvents[0], 2, FTimeout);

  if MasterChanges > 0 then begin
    for i := 0 to MasterChanges - 1 do
      if MasterEvents[i].Data.fd = FEpollFD then
        Changes := epoll_wait(FEpollFD, @FEvents[0], FCount, 0)
      else
        ReadChanges := epoll_wait(FEpollReadFD, @FEventsRead[0], FCount, 0);
    if (Changes < 0) or (ReadChanges < 0) then
      Bail('Error on epoll', LSocketError)
    else
      Result := Changes + ReadChanges > 0;
      
    if Result then begin
      FInLoop := True;
      for i := 0 to Max(Changes, ReadChanges) - 1 do begin
        Temp := nil;
        if i < Changes then begin
          Temp := TLHandle(FEvents[i].data.ptr);

          if  (not Temp.FDispose)
          and (FEvents[i].events and EPOLLOUT = EPOLLOUT) then
            if Assigned(Temp.FOnWrite) and not Temp.IgnoreWrite then
              Temp.FOnWrite(Temp);

          if Temp.FDispose then
            AddForFree(Temp);
        end; // writes

        if i < ReadChanges then begin
          TempRead := TLHandle(FEventsRead[i].data.ptr);

          if  (not TempRead.FDispose)
          and ((FEventsRead[i].events and EPOLLIN = EPOLLIN)
          or  (FEventsRead[i].events and EPOLLHUP = EPOLLHUP)
          or  (FEventsRead[i].events and EPOLLPRI = EPOLLPRI)) then
            if Assigned(TempRead.FOnRead) and not TempRead.IgnoreRead then
              TempRead.FOnRead(TempRead);

          if TempRead.FDispose then
            AddForFree(TempRead);
        end; // reads
        
        if i < Changes then begin
          if not Assigned(Temp) then
            Temp := TLHandle(FEvents[i].data.ptr);

          if  (not Temp.FDispose)
          and (FEvents[i].events and EPOLLERR = EPOLLERR) then
            if Assigned(Temp.FOnError) and not Temp.IgnoreError then
              Temp.FOnError(Temp, 'Handle error' + LStrError(LSocketError));

          if Temp.FDispose then
            AddForFree(Temp);
        end; // errors
      end;
      FInLoop := False;
      if Assigned(FFreeRoot) then
        FreeHandles;
    end;
  end else if MasterChanges < 0 then
    Bail('Error on epoll', LSocketError);
end;

function BestEventerClass: TLEventerClass;
var
  tmp: THandle;
begin
  {$IFNDEF FORCE_SELECT}
  try
    tmp := epoll_create(1);
    if tmp >= 0 then begin
      FpClose(tmp);
      Result := TLEpollEventer;
    end else
      Result := TLSelectEventer;
  except
    Result := TLSelectEventer;
  end;
  {$ELSE}
  Result := TLSelectEventer;
  {$ENDIF}
end;

{$endif} // Linux
