{$ifdef interface_type}
  { TPurpleAccount }

  // PPurpleAccount is already declared forward in purple.pas
  TPurpleAccount = object
    function GetString(Name, DefaultValue: String): String;
    function GetUsername: String;
    function GetConnection: PPurpleConnection;
    function GetPresence: PPurplePresence;
  end;

{$endif}
{$ifdef implementation}

function purple_account_get_string(account: PPurpleAccount; aname, default_value: PChar): PChar; cdecl; external LIBPURPLE;
function purple_account_get_connection(account: PPurpleAccount): PPurpleConnection; cdecl; external LIBPURPLE;
function purple_account_get_presence(account: PPurpleAccount): PPurplePresence; cdecl; external LIBPURPLE;
function purple_account_get_username(account: PPurpleAccount): PChar; cdecl; external LIBPURPLE;

{ TPurpleAccount }

function TPurpleAccount.GetString(Name, DefaultValue: String): String;
begin
  Result := purple_account_get_string(@Self, _PChar(Name), _PChar(DefaultValue));
end;

function TPurpleAccount.GetUsername: String;
begin
  Result := purple_account_get_username(@Self);
end;

function TPurpleAccount.GetConnection: PPurpleConnection;
begin
  Result := purple_account_get_connection(@Self);
end;

function TPurpleAccount.GetPresence: PPurplePresence;
begin
  Result := purple_account_get_presence(@Self);
end;

{$endif}
