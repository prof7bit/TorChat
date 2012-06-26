{$ifdef _type_forward}
  PPurpleBuddy = ^TPurpleBuddy;
{$endif}

{$ifdef _type}
  PPurpleContact            = Pointer;
  PPurpleBlistNode          = Pointer;
  PPurpleChat               = Pointer;

  PPurpleGroup = ^TPurpleGroup;

  { TPurpleBuddy }

  TPurpleBuddy = object
    class function Create(Acc: PPurpleAccount; AName, AAlias: String): PPurpleBuddy;
    class function Find(Acc: PPurpleAccount; AName: String): PPurpleBuddy;
    class function FindMany(Acc: PPurpleAccount; AName: String): PGSList;
    function GetAccount: PPurpleAccount;
    function GetAliasOnly: String;
    function GetName: String;
    function GetPresence: TPurplePresence;
    procedure SetAlias(AAlias: String);
    procedure Remove;
    procedure BlistAdd(Contact: PPurpleContact; Group: PPurpleGroup; Node: PPurpleBlistNode);
  end;

  { TPurpleGroup }

  TPurpleGroup = object
    class function Create(AName: String): PPurpleGroup;
    class function Find(AName: String): PPurpleGroup;
    procedure Add(Node: PPurpleBlistNode);
  end;
{$endif}

{$ifdef _func}
procedure purple_blist_alias_buddy(buddy: PPurpleBuddy; aalias: PChar); cdecl; external LIBPURPLE;
procedure purple_blist_remove_buddy(buddy: PPurpleBuddy); cdecl; external LIBPURPLE;
function  purple_buddy_get_account(buddy: PPurpleBuddy): PPurpleAccount; cdecl; external LIBPURPLE;
function  purple_buddy_get_alias_only(buddy: PPurpleBuddy): PChar; cdecl; external LIBPURPLE;
function  purple_buddy_get_name(buddy: PPurpleBuddy): PChar; cdecl; external LIBPURPLE;
function  purple_buddy_get_presence(buddy: PPurpleBuddy): TPurplePresence; cdecl; external LIBPURPLE;
function  purple_buddy_new(account: PPurpleAccount; aname, aalias: PChar): PPurpleBuddy; cdecl; external LIBPURPLE;
procedure purple_blist_add_buddy(buddy: PPurpleBuddy; contact: PPurpleContact;
  group: PPurpleGroup; node: PPurpleBlistNode); cdecl; external LIBPURPLE;
function  purple_group_new(name_: PChar): PPurpleGroup; cdecl; external LIBPURPLE;
function  purple_find_group(name_: PChar): PPurpleGroup; cdecl; external LIBPURPLE;
procedure purple_blist_add_group(group: PPurpleGroup; node: PPurpleBlistNode); cdecl; external LIBPURPLE;

function  purple_find_buddy(account: PPurpleAccount; aname: PChar): PPurpleBuddy; cdecl; external LIBPURPLE;
function  purple_find_buddies(account: PPurpleAccount; aname: PChar): PGSList; cdecl; external LIBPURPLE;
{$endif}


{$ifdef _impl}
class function TPurpleBuddy.Create(Acc: PPurpleAccount; AName, AAlias: String): PPurpleBuddy;
begin
  Result := purple_buddy_new(Acc, C(AName), C(AAlias));
end;

class function TPurpleBuddy.Find(Acc: PPurpleAccount; AName: String): PPurpleBuddy;
begin
  Result := purple_find_buddy(Acc, C(AName));
end;

class function TPurpleBuddy.FindMany(Acc: PPurpleAccount; AName: String): PGSList;
begin
  Result := purple_find_buddies(Acc, C(AName));
end;

function TPurpleBuddy.GetAccount: PPurpleAccount;
begin
  Result := purple_buddy_get_account(@Self);
end;

function TPurpleBuddy.GetAliasOnly: String;
begin
  Result := purple_buddy_get_alias_only(@Self);
end;

function TPurpleBuddy.GetName: String;
begin
  Result := purple_buddy_get_name(@Self);
end;

function TPurpleBuddy.GetPresence: TPurplePresence;
begin
  Result := purple_buddy_get_presence(@Self);
end;

procedure TPurpleBuddy.SetAlias(AAlias: String);
begin
  purple_blist_alias_buddy(@Self, C(AAlias));
end;

procedure TPurpleBuddy.Remove;
begin
  purple_blist_remove_buddy(@Self);
end;

procedure TPurpleBuddy.BlistAdd(Contact: PPurpleContact; Group: PPurpleGroup; Node: PPurpleBlistNode);
begin
  purple_blist_add_buddy(@Self, Contact, Group, Node);
end;


class function TPurpleGroup.Create(AName: String): PPurpleGroup;
begin
  Result := purple_group_new(C(AName));
end;

class function TPurpleGroup.Find(AName: String): PPurpleGroup;
begin
  Result := purple_find_group(C(AName));
end;

procedure TPurpleGroup.Add(Node: PPurpleBlistNode);
begin
  purple_blist_add_group(@Self, Node);
end;

{$endif}
