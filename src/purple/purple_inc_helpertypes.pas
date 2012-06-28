{$ifdef _type}
  { TGlistHelper }
  TGlistHelper = record helper for TGList
    class function Create(Item: Pointer): PGList; static;
    procedure Append(Item: Pointer);
    function Prepend(Item: Pointer): PGList;
    function Reverse: PGList;
    function DeleteLink(Item: Pointer): PGList;
    function DeleteFirst: PGList;
  end;

  { TGSlistHelper }

  TGSlistHelper = record helper for TGSList
    class function Create(Item: Pointer): PGSList; static;
    procedure Append(Item: Pointer);
    function Prepend(Item: Pointer): PGSList;
    function Reverse: PGSList;
    function DeleteLink(Item: Pointer): PGSList;
    function DeleteFirst: PGSList;
  end;
{$endif}

{$ifdef _impl}
{ TGListHelper }

class function TGlistHelper.Create(Item: Pointer): PGList;
begin
  Result := g_list_append(nil, Item);
end;

procedure TGListHelper.Append(Item: Pointer);
begin
  if @Self = nil then
    raise Exception.Create('Cannot append() to nil. Use Create() instead');
  g_list_append(@Self, Item);
end;

function TGlistHelper.Prepend(Item: Pointer): PGList;
begin
  Result := g_list_prepend(@Self, Item);
end;

function TGlistHelper.Reverse: PGList;
begin
  Result := g_list_reverse(@Self);
end;

function TGlistHelper.DeleteLink(Item: Pointer): PGList;
begin
  Result := g_list_delete_link(@Self, Item);
end;

function TGlistHelper.DeleteFirst: PGList;
begin
  Result := g_list_delete_link(@Self, @Self);
end;

{ TGSlistHelper }

class function TGSlistHelper.Create(Item: Pointer): PGSList;
begin
  Result := g_slist_append(nil, Item);
end;

procedure TGSlistHelper.Append(Item: Pointer);
begin
  if @Self = nil then
    raise Exception.Create('Cannot Append() to nil. Use Create() instead');
  g_slist_append(@Self, Item);
end;

function TGSlistHelper.Prepend(Item: Pointer): PGSList;
begin
  Result := g_slist_prepend(@Self, Item);
end;

function TGSlistHelper.Reverse: PGSList;
begin
  Result := g_slist_reverse(@Self);
end;

function TGSlistHelper.DeleteLink(Item: Pointer): PGSList;
begin
  Result := g_slist_delete_link(@Self, Item);
end;

function TGSlistHelper.DeleteFirst: PGSList;
begin
  Result := g_slist_delete_link(@Self, @Self);
end;

{$endif}

