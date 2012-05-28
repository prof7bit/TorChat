constructor TLFront.Create(const DefaultItem: __front_type__);
begin
  FEmptyItem:=DefaultItem;
  Clear;
end;

function TLFront.GetEmpty: Boolean;
begin
  Result:=FCount = 0;
end;

function TLFront.First: __front_type__;
begin
  Result:=FEmptyItem;
  if FCount > 0 then
    Result:=FItems[FBottom];
end;

function TLFront.Remove: __front_type__;
begin
  Result:=FEmptyItem;
  if FCount > 0 then begin
    Result:=FItems[FBottom];
    Dec(FCount);
    Inc(FBottom);
    if FBottom >= MAX_FRONT_ITEMS then
      FBottom:=0;
  end;
end;

function TLFront.Insert(const Value: __front_type__): Boolean;
begin
  Result:=False;
  if FCount < MAX_FRONT_ITEMS then begin
    if FTop >= MAX_FRONT_ITEMS then
      FTop:=0;
    FItems[FTop]:=Value;
    Inc(FCount);
    Inc(FTop);
    Result:=True;
  end;
end;

procedure TLFront.Clear;
begin
  FCount:=0;
  FBottom:=0;
  FTop:=0;
end;

