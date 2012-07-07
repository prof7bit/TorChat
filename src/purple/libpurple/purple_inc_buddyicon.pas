{$ifdef _type_forward}
  PPurpleBuddyIcon = ^TPurpleBuddyIcon;
{$endif}


{$ifdef _type_forward}

  { TPurpleBuddyIcon }

  TPurpleBuddyIcon = object
    function GetFullPath: String;
    procedure SetData(Data: Pointer; Len: PtrUInt; CheckSum: String);
  end;
{$endif}

{$ifdef _func_public}
function purple_buddy_icons_get_cache_dir: PChar; cdecl; external LIBPURPLE;
{$endif}

{$ifdef _func}
function purple_buddy_icon_get_full_path(icon: PPurpleBuddyIcon): PChar; cdecl; external LIBPURPLE;
procedure purple_buddy_icon_set_data(icon: PPurpleBuddyIcon; data: PChar;
  len: csize_t; checksum: PChar); cdecl; external LIBPURPLE;
procedure purple_buddy_icons_set_for_user(account: PPurpleAccount;
  username: PChar; icon_data: Pointer; icon_len: csize_t; checksum: PChar); cdecl; external LIBPURPLE;
function purple_buddy_icons_find(account: PPurpleAccount; username: PChar)
  : PPurpleBuddyIcon; cdecl; external LIBPURPLE;
{$endif}


{$ifdef _impl}
function TPurpleBuddyIcon.GetFullPath: String;
begin
  Result := purple_buddy_icon_get_full_path(@Self);
end;

procedure TPurpleBuddyIcon.SetData(Data: Pointer; Len: PtrUInt; CheckSum: String);
begin
  purple_buddy_icon_set_data(@Self, Data, Len, Pointer(CheckSum));
end;

{$endif}

