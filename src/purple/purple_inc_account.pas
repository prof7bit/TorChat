{$ifdef purple_interface}
type
  { TPurpleAccount }
  TPurpleAccount = class
    function GetUsername: String;
    function GetConnection: TPurpleConnection;
    function GetPresence: PPurplePresence;
  end;

{$endif}
{$ifdef purple_implementation}

function purple_account_get_connection(account: TPurpleAccount): TPurpleConnection; cdecl; external LIBPURPLE;
function purple_account_get_presence(account: TPurpleAccount): PPurplePresence; cdecl; external LIBPURPLE;
function purple_account_get_username(account: TPurpleAccount): PChar; cdecl; external LIBPURPLE;

{ TPurpleAccount }

function TPurpleAccount.GetUsername: String;
begin
  Result := purple_account_get_username(Self);
end;

function TPurpleAccount.GetConnection: TPurpleConnection;
begin
  Result := purple_account_get_connection(Self);
end;

function TPurpleAccount.GetPresence: PPurplePresence;
begin
  Result := purple_account_get_presence(Self);
end;

{$endif}
