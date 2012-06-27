{$ifdef _type}
  { TPurpleDebug is just abused as a namespace for the debug
    function, there actually never exists any instace of it
    and therefore also no corresponding pointer type}
  TPurpleDebug = object
    class procedure Misc(Category, Text: String); inline;
    class procedure Info(Category, Text: String); inline;
    class procedure Warning(Category, Text: String); inline;
    class procedure Error(Category, Text: String); inline;
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
  purple_debug_misc(C(Category), C(Text), []);
end;

class procedure TPurpleDebug.Info(Category, Text: String);
begin
  purple_debug_info(C(Category), C(Text), []);
end;

class procedure TPurpleDebug.Warning(Category, Text: String);
begin
  purple_debug_warning(C(Category), C(Text), []);
end;

class procedure TPurpleDebug.Error(Category, Text: String);
begin
  purple_debug_error(C(Category), C(Text), []);
end;
{$endif}
