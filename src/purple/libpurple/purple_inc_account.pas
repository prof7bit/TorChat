{$ifdef _type_forward}
  PPurpleAccount = ^TPurpleAccount;
{$endif}

{$ifdef _type}

  { TPurpleAccount }

  TPurpleAccount = object
    function GetString(Name, DefaultValue: String): String;
    function GetUsername: String;
    function GetConnection: PPurpleConnection;
    function GetPresence: PPurplePresence;
    procedure SetIconForBuddy(Who: String; IconData: Pointer; IconLen: PtrUInt; CheckSum: String);
    function FindBuddy(AName: String): PPurpleBuddy;
    function FindBuddies(AName: String): PGSList;
    function FindIcon(AName: String): PPurpleBuddyIcon;
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
{$endif}


{$ifdef _impl}
function TPurpleAccount.GetString(Name, DefaultValue: String): String;
begin
  Result := purple_account_get_string(@Self, Pointer(Name), Pointer(DefaultValue));
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
  purple_buddy_icons_set_for_user(@Self, Pointer(Who), IconData, IconLen, Pointer(CheckSum));
end;

function TPurpleAccount.FindBuddy(AName: String): PPurpleBuddy;
begin
  Result := purple_find_buddy(@Self, Pointer(AName));
end;

function TPurpleAccount.FindBuddies(AName: String): PGSList;
begin
  Result := purple_find_buddies(@Self, Pointer(AName));
end;

function TPurpleAccount.FindIcon(AName: String): PPurpleBuddyIcon;
begin
  purple_buddy_icons_find(@Self, Pointer(AName));
end;

procedure TPurpleAccount.GotUserStatus(AName, AStatusID: String);
begin
  purple_prpl_got_user_status(@self, Pointer(AName), Pointer(AStatusID), [nil]);
end;
{$endif}

