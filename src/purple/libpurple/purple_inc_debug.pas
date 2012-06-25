{$ifdef interface_type}
  { TPurpleDebug is just abused as a namespace for the debug function}
  TPurpleDebug = object
    class procedure Misc(Category, Text: String);
    class procedure Info(Category, Text: String);
    class procedure Warning(Category, Text: String);
    class procedure Error(Category, Text: String);
  end;
{$endif}

{$ifdef import_func}
procedure purple_debug_misc(category: PChar; format: PChar; args: array of const); cdecl; external LIBPURPLE;
procedure purple_debug_info(category: PChar; format: PChar; args: array of const); cdecl; external LIBPURPLE;
procedure purple_debug_warning(category: PChar; format: PChar; args: array of const); cdecl; external LIBPURPLE;
procedure purple_debug_error(category: PChar; format: PChar; args: array of const); cdecl; external LIBPURPLE;
{$endif}

{$ifdef implementation}
class procedure TPurpleDebug.Misc(Category, Text: String);
begin
  purple_debug_misc(_PChar(Category), _PChar(Text), []);
end;

class procedure TPurpleDebug.Info(Category, Text: String);
begin
  purple_debug_info(_PChar(Category), _PChar(Text), []);
end;

class procedure TPurpleDebug.Warning(Category, Text: String);
begin
  purple_debug_warning(_PChar(Category), _PChar(Text), []);
end;

class procedure TPurpleDebug.Error(Category, Text: String);
begin
  purple_debug_error(_PChar(Category), _PChar(Text), []);
end;
{$endif}
