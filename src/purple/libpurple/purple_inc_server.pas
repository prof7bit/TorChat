{$ifdef _func_public}
procedure serv_got_alias(gc: PPurpleConnection; who, aalias: PChar); external LIBPURPLE;
procedure serv_got_im(gc: PPurpleConnection; who, msg: PChar;
  flags: TPurpleMessageFlags; mtime: time_t); cdecl; external LIBPURPLE;
{$endif}
