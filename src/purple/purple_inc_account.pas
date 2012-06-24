{$ifdef purple_interface}
type
  { TPurpleAccount }
  TPurpleAccount = class
    function GetString(Name, DefaultValue: String): String;
    function GetUsername: String;
    function GetConnection: TPurpleConnection;
    function GetPresence: TPurplePresence;
  end;

{$endif}
{$ifdef purple_implementation}

function purple_account_get_string(account: TPurpleAccount; aname, default_value: PChar): PChar; cdecl; external LIBPURPLE;
function purple_account_get_connection(account: TPurpleAccount): TPurpleConnection; cdecl; external LIBPURPLE;
function purple_account_get_presence(account: TPurpleAccount): TPurplePresence; cdecl; external LIBPURPLE;
function purple_account_get_username(account: TPurpleAccount): PChar; cdecl; external LIBPURPLE;

{ TPurpleAccount }

function TPurpleAccount.GetString(Name, DefaultValue: String): String;
begin
  Result := purple_account_get_string(Self, PChar(Name), PChar(DefaultValue));
end;

function TPurpleAccount.GetUsername: String;
begin
  Result := purple_account_get_username(Self);
end;

function TPurpleAccount.GetConnection: TPurpleConnection;
begin
  Result := purple_account_get_connection(Self);
end;

function TPurpleAccount.GetPresence: TPurplePresence;
begin
  Result := purple_account_get_presence(Self);
end;

{$endif}
