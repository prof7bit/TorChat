{$ifdef _type_forward}
  PPurpleAccount = ^TPurpleAccount;
{$endif}

{$ifdef _type}
  TPurpleAccount = object
    function GetString(Name, DefaultValue: String): String;
    function GetUsername: String;
    function GetConnection: PPurpleConnection;
    function GetPresence: PPurplePresence;
    procedure SetIconForBuddy(Who: String; IconData: Pointer; IconLen: PtrUInt; CheckSum: String);
    function FindBuddies(AName: String): PGSList;
    procedure GotUserStatus(AName, AStatusID: String);
  end;

  PPurpleAccountUnregistrationCallback = procedure(); cdecl; // fixme: signature?
  PPurpleSetPublicAliasSuccessCallback = procedure(); cdecl; // fixme: signature?
  PPurpleSetPublicAliasFailureCallback = procedure(); cdecl; // fixme: signature?
  PPurpleGetPublicAliasSuccessCallback = procedure(); cdecl; // fixme: signature?
  PPurpleGetPublicAliasFailureCallback = procedure(); cdecl; // fixme: signature?
{$endif}


{$ifdef _func}
function purple_account_get_string(account: PPurpleAccount; aname, default_value: PChar): PChar; cdecl; external LIBPURPLE;
function purple_account_get_connection(account: PPurpleAccount): PPurpleConnection; cdecl; external LIBPURPLE;
function purple_account_get_presence(account: PPurpleAccount): PPurplePresence; cdecl; external LIBPURPLE;
function purple_account_get_username(account: PPurpleAccount): PChar; cdecl; external LIBPURPLE;
{This function is actually declared in buddyicon.h }
procedure purple_buddy_icons_set_for_user(account: PPurpleAccount;
  username: PChar; icon_data: Pointer; icon_len: csize_t; checksum: PChar); cdecl; external LIBPURPLE;
{$endif}


{$ifdef _impl}
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

procedure TPurpleAccount.SetIconForBuddy(Who: String; IconData: Pointer; IconLen: PtrUInt; CheckSum: String);
begin
  purple_buddy_icons_set_for_user(@Self, _PChar(Who), IconData, IconLen, _PChar(CheckSum));
end;

function TPurpleAccount.FindBuddies(AName: String): PGSList;
begin
  Result := purple_find_buddies(@Self, _PChar(AName));
end;

procedure TPurpleAccount.GotUserStatus(AName, AStatusID: String);
begin
  purple_prpl_got_user_status(@self, _PChar(AName), _PChar(AStatusID));
end;
{$endif}

