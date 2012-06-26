{$ifdef _func}
procedure purple_buddy_icons_set_for_user(account: PPurpleAccount;
  username: PChar; icon_data: Pointer; icon_len: csize_t; checksum: PChar); cdecl; external LIBPURPLE;
{$endif}

