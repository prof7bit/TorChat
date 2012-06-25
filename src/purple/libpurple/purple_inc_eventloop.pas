{$ifdef _func_public}
function  purple_timeout_add(Interval: cint; cb: TGSourceFunc; UserData: Pointer): cint; cdecl; external LIBPURPLE;
function  purple_timeout_remove(handle: cint): GBoolean; cdecl; external LIBPURPLE;
{$endif}

