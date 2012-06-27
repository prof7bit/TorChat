{$ifdef _type}
  { TPurpleDebug is just abused as a namespace for the debug
    function, there actually never exists any instace of it
    and therefore also no corresponding pointer type}
  TPurpleDebug = object
    class procedure Misc(Category, Text: String);
    class procedure Info(Category, Text: String);
    class procedure Warning(Category, Text: String);
    class procedure Error(Category, Text: String);
  end;
{$endif}

{$ifdef _func}
procedure purple_debug_misc(category: PChar; format: PChar; args: array of const); cdecl; external LIBPURPLE;
procedure purple_debug_info(category: PChar; format: PChar; args: array of const); cdecl; external LIBPURPLE;
procedure purple_debug_warning(category: PChar; format: PChar; args: array of const); cdecl; external LIBPURPLE;
procedure purple_debug_error(category: PChar; format: PChar; args: array of const); cdecl; external LIBPURPLE;
{$endif}

{$ifdef _impl}
class procedure TPurpleDebug.Misc(Category, Text: String);
begin
  purple_debug_misc(Pointer(Category), Pointer(Text), []);
end;

class procedure TPurpleDebug.Info(Category, Text: String);
begin
  purple_debug_info(Pointer(Category), Pointer(Text), []);
end;

class procedure TPurpleDebug.Warning(Category, Text: String);
begin
  purple_debug_warning(Pointer(Category), Pointer(Text), []);
end;

class procedure TPurpleDebug.Error(Category, Text: String);
begin
  purple_debug_error(Pointer(Category), Pointer(Text), []);
end;
{$endif}
