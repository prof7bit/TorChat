{$ifdef _type}
  PPurpleAccountOption = ^TPurpleAccountOption;
  TPurpleAccountOption = Object
    class function CreateString(Text, Name, DefaultValue: String): PPurpleAccountOption;
  end;
{$endif}

{$ifdef _func}
function  purple_account_option_string_new(text, pref_name, default_value: PChar): PPurpleAccountOption; cdecl; external LIBPURPLE;
{$endif}

{$ifdef _impl}
class function TPurpleAccountOption.CreateString(Text, Name, DefaultValue: String): PPurpleAccountOption;
begin
  Result := purple_account_option_string_new(Pointer(Text), Pointer(Name), Pointer(DefaultValue));
end;
{$endif}
