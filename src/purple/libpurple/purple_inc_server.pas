{$ifdef _func}
procedure serv_got_alias(gc: PPurpleConnection; who, alias_: PChar); external LIBPURPLE;
procedure serv_got_im(gc: PPurpleConnection; who, msg: PChar;
  flags: TPurpleMessageFlags; mtime: time_t); cdecl; external LIBPURPLE;
{$endif}

// I have added these functions as methods to the PPurpleConnection object
