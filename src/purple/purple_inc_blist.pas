{$ifdef purple_interface}
type
  PPurpleContact            = Pointer;
  PPurpleBlistNode          = Pointer;

  TPurpleGroup = class;

  { TPurpleBuddy }

  TPurpleBuddy = class(TWrapper)
    class function Create(Acc: PPurpleAccount; AName, AAlias: String): TPurpleBuddy;
    class function Find(Acc: PPurpleAccount; AName: String): TPurpleBuddy;
    function GetAccount: PPurpleAccount;
    function GetAliasOnly: String;
    function GetName: String;
    function GetPresence: TPurplePresence;
    procedure SetAlias(AAlias: String);
    procedure Remove;
    procedure BlistAdd(Contact: PPurpleContact; Group: TPurpleGroup; Node: PPurpleBlistNode);
  end;

  { TPurpleGroup }

  TPurpleGroup = class(TWrapper)
    class function Create(AName: String): TPurpleGroup;
    class function Find(AName: String): TPurpleGroup;
    procedure Add(Node: PPurpleBlistNode);
  end;

{$endif}
{$ifdef purple_implementation}

function  purple_find_buddy(account: PPurpleAccount; aname: PChar): TPurpleBuddy; cdecl; external LIBPURPLE;

procedure purple_blist_alias_buddy(buddy: TPurpleBuddy; aalias: PChar); cdecl; external LIBPURPLE;
procedure purple_blist_remove_buddy(buddy: TPurpleBuddy); cdecl; external LIBPURPLE;
function  purple_buddy_get_account(buddy: TPurpleBuddy): PPurpleAccount; cdecl; external LIBPURPLE;
function  purple_buddy_get_alias_only(buddy: TPurpleBuddy): PChar; cdecl; external LIBPURPLE;
function  purple_buddy_get_name(buddy: TPurpleBuddy): PChar; cdecl; external LIBPURPLE;
function  purple_buddy_get_presence(buddy: TPurpleBuddy): TPurplePresence; cdecl; external LIBPURPLE;
function  purple_buddy_new(account: PPurpleAccount; aname, aalias: PChar): TPurpleBuddy; cdecl; external LIBPURPLE;
procedure purple_blist_add_buddy(buddy: TPurpleBuddy; contact: PPurpleContact;
  group: TPurpleGroup; node: PPurpleBlistNode); cdecl; external LIBPURPLE;

function  purple_group_new(name_: PChar): TPurpleGroup; cdecl; external LIBPURPLE;
function  purple_find_group(name_: PChar): TPurpleGroup; cdecl; external LIBPURPLE;
procedure purple_blist_add_group(group: TPurpleGroup; node: PPurpleBlistNode); cdecl; external LIBPURPLE;



class function TPurpleBuddy.Create(Acc: PPurpleAccount; AName, AAlias: String): TPurpleBuddy;
begin
  Result := purple_buddy_new(Acc, _PChar(AName), _PChar(AAlias));
end;

class function TPurpleBuddy.Find(Acc: PPurpleAccount; AName: String): TPurpleBuddy;
begin
  Result := purple_find_buddy(Acc, _PChar(AName));
end;

function TPurpleBuddy.GetAccount: PPurpleAccount;
begin
  Result := purple_buddy_get_account(Self);
end;

function TPurpleBuddy.GetAliasOnly: String;
begin
  Result := purple_buddy_get_alias_only(Self);
end;

function TPurpleBuddy.GetName: String;
begin
  Result := purple_buddy_get_name(Self);
end;

function TPurpleBuddy.GetPresence: TPurplePresence;
begin
  Result := purple_buddy_get_presence(Self);
end;

procedure TPurpleBuddy.SetAlias(AAlias: String);
begin
  purple_blist_alias_buddy(Self, _PChar(AAlias));
end;

procedure TPurpleBuddy.Remove;
begin
  purple_blist_remove_buddy(Self);
end;

procedure TPurpleBuddy.BlistAdd(Contact: PPurpleContact; Group: TPurpleGroup; Node: PPurpleBlistNode);
begin
  purple_blist_add_buddy(Self, Contact, Group, Node);
end;


class function TPurpleGroup.Create(AName: String): TPurpleGroup;
begin
  Result := purple_group_new(_PChar(AName));
end;

class function TPurpleGroup.Find(AName: String): TPurpleGroup;
begin
  Result := purple_find_group(_PChar(AName));
end;

procedure TPurpleGroup.Add(Node: PPurpleBlistNode);
begin
  purple_blist_add_group(Self, Node);
end;

{$endif}
