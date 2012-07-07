{$ifdef _type_forward}
  PPurpleBlistNode = ^TPurpleBlistNode;
  PPurpleBuddy = ^TPurpleBuddy;
  PPurpleGroup = ^TPurpleGroup;
{$endif}

{$ifdef _type}
  PPurpleContact            = Pointer;
  PPurpleChat               = Pointer;

  { TPurpleBlistNode }
  TPurpleBlistNode = object
    procedure SetBool(Key: String; Value: Boolean);
    function GetBool(Key: String): Boolean;
    procedure SetString(Key, Value: String);
    function GetString(Key: String): String;
    procedure UpdateIcon;
  end;

  { TPurpleBuddy }
  TPurpleBuddy = object(TPurpleBlistNode)
    class function Create(Acc: PPurpleAccount; AName, AAlias: String): PPurpleBuddy;
    class function Find(Acc: PPurpleAccount; AName: String): PPurpleBuddy;
    class function FindMany(Acc: PPurpleAccount; AName: String): PGSList;
    function GetAccount: PPurpleAccount;
    function GetAliasOnly: String;
    function GetIcon: PPurpleBuddyIcon;
    function GetName: String;
    function GetPresence: TPurplePresence;
    procedure SetAlias(AAlias: String);
    procedure Remove;
    procedure BlistAdd(Contact: PPurpleContact; Group: PPurpleGroup; Node: PPurpleBlistNode);
  end;

  { TPurpleGroup }

  TPurpleGroup = object(TPurpleBlistNode)
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
function  purple_buddy_get_icon(buddy: PPurpleBuddy): PPurpleBuddyIcon; cdecl; external LIBPURPLE;
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

function  purple_blist_node_get_bool(node: PPurpleBlistNode; key: PChar): gboolean; cdecl; external LIBPURPLE;
procedure purple_blist_node_set_bool(node: PPurpleBlistNode; key: PChar; value: gboolean); cdecl; external LIBPURPLE;
function  purple_blist_node_get_string(node: PPurpleBlistNode; key: PChar): PChar; cdecl; external LIBPURPLE;
procedure purple_blist_node_set_string(node: PPurpleBlistNode; key, value: PChar); cdecl; external LIBPURPLE;

procedure purple_blist_update_node_icon(node: PPurpleBlistNode); cdecl; external LIBPURPLE;
{$endif}


{$ifdef _impl}
{ TPurpleBlistNode }

procedure TPurpleBlistNode.SetBool(Key: String; Value: Boolean);
begin
  purple_blist_node_set_bool(@Self, Pointer(Key), Value);
end;

function TPurpleBlistNode.GetBool(Key: String): Boolean;
begin
  Result := purple_blist_node_get_bool(@Self, Pointer(Key));
end;

procedure TPurpleBlistNode.SetString(Key, Value: String);
begin
  purple_blist_node_set_string(@Self, Pointer(Key), Pointer(Value));
end;

function TPurpleBlistNode.GetString(Key: String): String;
begin
  Result := purple_blist_node_get_string(@Self, Pointer(Key));
end;

procedure TPurpleBlistNode.UpdateIcon;
begin
  purple_blist_update_node_icon(@self);
end;

{ TPurpleBuddy }

class function TPurpleBuddy.Create(Acc: PPurpleAccount; AName, AAlias: String): PPurpleBuddy;
begin
  Result := purple_buddy_new(Acc, Pointer(AName), Pointer(AAlias));
end;

class function TPurpleBuddy.Find(Acc: PPurpleAccount; AName: String): PPurpleBuddy;
begin
  Result := purple_find_buddy(Acc, Pointer(AName));
end;

class function TPurpleBuddy.FindMany(Acc: PPurpleAccount; AName: String): PGSList;
begin
  Result := purple_find_buddies(Acc, Pointer(AName));
end;

function TPurpleBuddy.GetAccount: PPurpleAccount;
begin
  Result := purple_buddy_get_account(@Self);
end;

function TPurpleBuddy.GetAliasOnly: String;
begin
  Result := purple_buddy_get_alias_only(@Self);
end;

function TPurpleBuddy.GetIcon: PPurpleBuddyIcon;
begin
  Result := purple_buddy_get_icon(@Self);
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
  purple_blist_alias_buddy(@Self, Pointer(AAlias));
end;

procedure TPurpleBuddy.Remove;
begin
  purple_blist_remove_buddy(@Self);
end;

procedure TPurpleBuddy.BlistAdd(Contact: PPurpleContact; Group: PPurpleGroup; Node: PPurpleBlistNode);
begin
  purple_blist_add_buddy(@Self, Contact, Group, Node);
end;

{ TPurpleGroup }

class function TPurpleGroup.Create(AName: String): PPurpleGroup;
begin
  Result := purple_group_new(Pointer(AName));
end;

class function TPurpleGroup.Find(AName: String): PPurpleGroup;
begin
  Result := purple_find_group(Pointer(AName));
end;

procedure TPurpleGroup.Add(Node: PPurpleBlistNode);
begin
  purple_blist_add_group(@Self, Node);
end;



{$endif}
