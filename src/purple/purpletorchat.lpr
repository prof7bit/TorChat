{ TorChat - libpurpletorchat, a libpurple (Pidgin) plugin for TorChat

  Copyright (C) 2012 Bernd Kreuss <prof7bit@googlemail.com>

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 3 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}
library purpletorchat;

{ Naming Conventions:

  Type names:
  -----------
  Normally in Pascal we have Uppercase/CamelCase identifiers and
  type names start with 'T', pointer types with 'P', interfaces
  with 'I', which will then look like TSomeThing or PSomeThing.

  C programmers on the other hand normally don't define separate
  names to denote pointer types, they just use the asterisk
  everywhere in their code and write SomeType and SomeType*

  I don't do the Pascal equivalent of * here (which would be
  ^SomeType) because it would make it totally inconsistent.
  Type names are the most important names of all, they should
  be clear and consistent. I have translated the purple types:

    PurpleSomething  is now TPurpleSomething
    PurpleSomething* is now PPurpleSomething

  and I am trying to avoid using the name PurpleSomething (without
  T or P) entirely. (PurpleSomething would be a good name for a
  variable in Pascal but this would confuse the hell out of every
  libpurple dev who is used to know this as a type name. So I'm
  not using it at all.)

  Variable names and function names
  ---------------------------------
  Since this is a wrapper and an interface between a Pascal'ish
  API and a C'ish API and contains callbacks from both sides and
  has many variables that hold libpurple types and their
  equivalents from the Pascal units with similar names appearing
  side by side in the same functions it can become very confusing.
  I am trying to name the libpurple callback functions and
  variables that hold libpurple or glib types with lowercase and
  under_score and all other variables are using the normal Pascal
  naming conventions. I know this is ugly but it will help the
  people who are familiar with libpurple avoid some confusion.
}

{$mode objfpc}{$H+}

uses
  {$ifdef UseHeapTrc} // do it with -dUseHeapTrc, not with -gh
    heaptrc,
  {$endif}
  {$ifdef unix}
    cthreads,
  {$endif}
  Classes,
  sysutils,
  contnrs,
  glib2,
  purple,
  purplehelper,
  tc_interface,
  tc_client,
  tc_config,
  tc_misc;

const
  PRPL_ID_OFFLINE = 'offline';
  PRPL_ID_AVAILABLE = 'available';
  PRPL_ID_AWAY = 'away';
  PRPL_ID_XA = 'xa';
  PRPL_ID_INVISIBLE = 'invisible';

type
  { TTorchatPurpleClient wraps the TorChat client}
  TTorChatPurpleClient = class(TTorChatClient)
  public
    purple_account: PPurpleAccount;
    purple_timer: Integer;
    constructor Create(AOwner: TComponent; AProfileName: String;
      account: PPurpleAccount); reintroduce;
    procedure OnNotifyGui; override;
    procedure OnBuddyStatusChange(ABuddy: IBuddy); override;
    procedure OnBuddyAdded(ABuddy: IBuddy); override;
    procedure OnBuddyRemoved(ABuddy: IBuddy); override;
  end;

  { TTorChatClients holds a list of clients since we can have
    multiple "sccounts" in pidgin at the same time }
  TTorChatClients = class(TFPHashObjectList)
    function Get(Account: PPurpleAccount): TTorChatPurpleClient;
  end;

var
  TorChatClients: TTorChatClients;

function Client(acc: PPurpleAccount): TTorChatPurpleClient;
begin
  Result := TorChatClients.Find(acc^.username) as TTorChatPurpleClient;
end;

function cb_purple_timer(data: Pointer): GBoolean; cdecl;
begin
  Ignore(data);
  TTorChatPurpleClient(data).Pump;
  Result := True;
end;

function cb_purple_timer_oneshot(data: Pointer): GBoolean; cdecl;
begin
  Ignore(data);
  TTorChatPurpleClient(data).Pump;
  Result := False; // purple timer will not fire again
end;

function torchat_load(var plugin: TPurplePlugin): GBoolean; cdecl;
begin
  Ignore(@plugin);
  TorChatClients := TTorChatClients.Create(False);
  WriteLn('plugin loaded');
  Result := True;
end;

function torchat_unload(var plugin: TPurplePlugin): GBoolean; cdecl;
begin
  Ignore(@plugin);
  TorChatClients.Free;
  WriteLn('plugin unloaded');
  Result := True;
end;

function torchat_status_types(acc: PPurpleAccount): PGList; cdecl;
begin
  Ignore(acc);
  // pidgin (or libpurple or both) has a bug, it will offer "invisible" even
  // if we don't register it (and it will not offer "extended away" even if
  // we register it), so we are registerig all of them and will deal with them
  // in the set_status callback.
  Result := nil;
  Result := g_list_append(Result, purple_status_type_new_full(PURPLE_STATUS_AVAILABLE, PRPL_ID_AVAILABLE, nil, True, True, False));
  Result := g_list_append(Result, purple_status_type_new_full(PURPLE_STATUS_AWAY, PRPL_ID_AWAY, nil, True, True, False));
  Result := g_list_append(Result, purple_status_type_new_full(PURPLE_STATUS_UNAVAILABLE, PRPL_ID_XA, nil, True, True, False));
  Result := g_list_append(Result, purple_status_type_new_full(PURPLE_STATUS_INVISIBLE, PRPL_ID_INVISIBLE, nil, True, True, False));
  Result := g_list_append(Result, purple_status_type_new_full(PURPLE_STATUS_OFFLINE, PRPL_ID_OFFLINE, nil, True, True, False));
end;

procedure torchat_set_status(acc: PPurpleAccount; status: PPurpleStatus); cdecl;
var
  status_prim   : TPurpleStatusPrimitive;
  TorchatStatus : TTorchatStatus;
begin
  status_prim := purple_status_type_get_primitive(purple_status_get_type(status));
  case status_prim of
    PURPLE_STATUS_AVAILABLE: TorchatStatus := TORCHAT_AVAILABLE;
    PURPLE_STATUS_UNAVAILABLE: TorchatStatus := TORCHAT_XA;
    PURPLE_STATUS_AWAY: TorchatStatus := TORCHAT_AWAY;
    PURPLE_STATUS_EXTENDED_AWAY: TorchatStatus := TORCHAT_XA;
    PURPLE_STATUS_INVISIBLE: TorchatStatus := TORCHAT_OFFLINE;
  end;
  TorChatClients.Get(acc).SetStatus(TorchatStatus);
end;

procedure torchat_alias_buddy(gc: PPurpleConnection; who, aalias: PChar); cdecl;
var
  TorChat: TTorChatPurpleClient;
  Buddy: IBuddy;
begin
  TorChat := Client(gc^.account);
  if Assigned(TorChat) then begin
    Buddy := TorChat.Roster.ByID(who);
    Buddy.SetFriendlyName(aalias);
    serv_got_alias(gc, who, aalias);
  end;
end;

function torchat_get_text_table(acc: PPurpleAccount): PGHashTable; cdecl;
begin
  Ignore(acc);
  Result := g_hash_table_new(@g_str_hash, @g_str_equal);
  g_hash_table_insert(Result, PChar('login_label'), PChar('profile name'));
end;

function torchat_list_icon(acc: PPurpleAccount; buddy: PPurpleBuddy): PChar; cdecl;
begin
  Ignore(acc);
  Ignore(buddy);
  Result := 'torchat';
  // now it will look for torchat.png in several resolutions
  // in the folders /usr/share/pixmaps/pidgin/protocols/*/
  // the installer for the plugin must install these files.
end;

procedure torchat_login(acc: PPurpleAccount); cdecl;
var
  TorChat: TTorChatPurpleClient;
  Buddy: IBuddy;
  group_name: PChar;
  purple_id: PChar;
  purple_alias: PChar;
  purple_status: PPurpleStatus;
  purple_buddy: PPurpleBuddy;
  purple_list: PGSList;
  purple_group: PPurpleGroup;

begin
  TorChat := TTorChatPurpleClient.Create(nil, acc^.username, acc);
  TorChat.purple_timer := purple_timeout_add(1000, @cb_purple_timer, TorChat);
  TorChatClients.Add(acc^.username, TorChat);
  purple_connection_set_state(acc^.gc, PURPLE_CONNECTED);

  group_name := GetMemAndCopy('TorChat ' + TorChat.ProfileName);
  purple_group := purple_find_group(group_name);

  // remove buddies from purple's list that not in TorChat's list
  purple_list := purple_find_buddies(acc, nil);
  while Assigned(purple_list) do begin
    purple_id := purple_buddy_get_name(purple_list^.data);
    if not Assigned(TorChat.Roster.ByID(purple_id)) then begin
      purple_blist_remove_buddy(purple_list^.data);
    end;
    purple_list := g_slist_delete_link(purple_list, purple_list);
  end;

  // add buddies to purple's buddy list that are not in purple's list
  TorChat.Roster.Lock;
  for Buddy in TorChat.Roster do begin
    purple_id := GetMemAndCopy(Buddy.ID);
    purple_alias := GetMemAndCopy(Buddy.FriendlyName);
    purple_buddy := purple_find_buddy(acc, purple_id);
    if not Assigned(purple_buddy) then begin
      if not Assigned(purple_group) then begin
        purple_group := purple_group_new(group_name);
        purple_blist_add_group(purple_group, nil);
      end;
      purple_buddy := purple_buddy_new(acc, purple_id, purple_alias);
      purple_blist_add_buddy(purple_buddy, nil, purple_group, nil);
    end
    else begin
      serv_got_alias(acc^.gc, purple_id, purple_alias);
      purple_blist_alias_buddy(purple_buddy, purple_alias);
    end;
    FreeMem(purple_id);
    FreeMem(purple_alias);
  end;
  TorChat.Roster.Unlock;

  // it won't call set_status after login, so we have to do it ourselves
  purple_status := purple_presence_get_active_status(acc^.presence);
  torchat_set_status(acc, purple_status);

  FreeMem(group_name);
end;

procedure torchat_close(gc: PPurpleConnection); cdecl;
var
  TorChat: TTorChatPurpleClient;
begin
  TorChat := Client(gc^.account);
  if Assigned(TorChat) then begin
    purple_timeout_remove(TorChat.purple_timer);
    TorChatClients.Remove(TorChat);
    TorChat.Free;
  end;
end;

{ TClients }

function TTorChatClients.Get(Account: PPurpleAccount): TTorChatPurpleClient;
begin
  Result := Find(Account^.username) as TTorChatPurpleClient;
end;


{ TTorchatPurpleClient }

constructor TTorChatPurpleClient.Create(AOwner: TComponent; AProfileName: String;
  account: PPurpleAccount);
begin
  purple_account := account;
  inherited Create(AOwner, AProfileName);
end;

procedure TTorChatPurpleClient.OnNotifyGui;
begin
  purple_timeout_add(0, @cb_purple_timer_oneshot, Self);
end;

procedure TTorChatPurpleClient.OnBuddyStatusChange(ABuddy: IBuddy);
var
  buddy_name: PChar;
  status_id: PChar;
begin
  buddy_name := GetMemAndCopy(ABuddy.ID);
  case ABuddy.Status of
    TORCHAT_AVAILABLE: status_id := GetMemAndCopy(PRPL_ID_AVAILABLE);
    TORCHAT_AWAY: status_id := GetMemAndCopy(PRPL_ID_AWAY);
    TORCHAT_XA: status_id := GetMemAndCopy(PRPL_ID_XA);
    TORCHAT_OFFLINE: status_id := GetMemAndCopy(PRPL_ID_OFFLINE);
  end;
  purple_prpl_got_user_status(purple_account, buddy_name, status_id);
  FreeMem(status_id);
  FreeMem(buddy_name);
end;

procedure TTorChatPurpleClient.OnBuddyAdded(ABuddy: IBuddy);
var
  group_name: PChar;
  buddy_name: PChar;
  buddy_alias: PChar;
  purple_group: PPurpleGroup;
  purple_buddy: PPurpleBuddy;
begin
  buddy_name := GetMemAndCopy(ABuddy.ID);
  buddy_alias := GetMemAndCopy(ABuddy.FriendlyName);
  group_name := GetMemAndCopy('TorChat ' + ABuddy.Client.ProfileName);
  if not assigned(purple_find_buddy(purple_account, buddy_name)) then begin
    purple_group := purple_find_group(group_name);
    if not Assigned(purple_group) then begin
      purple_group := purple_group_new(group_name);
      purple_blist_add_group(purple_group, nil);
    end;
    purple_buddy := purple_buddy_new(purple_account, buddy_name, buddy_alias);
    purple_blist_add_buddy(purple_buddy, nil, purple_group, nil);
  end;
  FreeMem(buddy_alias);
  FreeMem(buddy_name);
  FreeMem(group_name);
end;

procedure TTorChatPurpleClient.OnBuddyRemoved(ABuddy: IBuddy);
var
  buddy_name: PChar;
  purple_buddy: PPurpleBuddy;
begin
  buddy_name := GetMemAndCopy(ABuddy.ID);
  purple_buddy := purple_find_buddy(purple_account, buddy_name);
  if Assigned(purple_buddy) then begin
    purple_blist_remove_buddy(purple_buddy);
  end;
  FreeMem(buddy_name);
end;

procedure Init;
var
  acc_opt: PPurpleAccountOption;
  TorPath: PChar;
begin
  with plugin_info do begin
    magic := PURPLE_PLUGIN_MAGIC;
    major_version := PURPLE_MAJOR_VERSION;
    minor_version := PURPLE_MINOR_VERSION;
    plugintype := PURPLE_PLUGIN_PROTOCOL;
    priority := PURPLE_PRIORITY_DEFAULT;
    id := 'prpl-prof7bit-torchat';
    name := 'TorChat';
    version := '2.0';
    summary := 'TorChat Protocol';
    description := 'TorChat protocol plugin for libpurple / Pidgin';
    author := 'Bernd Kreuss <prof7bit@gmail.com>';
    homepage := 'https://github.com/prof7bit/TorChat';
    load := @torchat_load;
    unload := @torchat_unload;
    extra_info := @plugin_protocol_info;
  end;

  with plugin_protocol_info do begin
    options := OPT_PROTO_NO_PASSWORD or OPT_PROTO_REGISTER_NOSCREENNAME;
    list_icon := @torchat_list_icon;
    status_types := @torchat_status_types;
    get_account_text_table := @torchat_get_text_table;
    login := @torchat_login;
    close := @torchat_close;
    set_status := @torchat_set_status;
    alias_buddy := @torchat_alias_buddy;
    struct_size := SizeOf(TPurplePluginProtocolInfo);
  end;

  // add additional fields to the settings dialog
  TorPath := PChar(DefaultPathTorExe);
  acc_opt := purple_account_option_string_new('Tor binary', 'tor', TorPath);
  plugin_protocol_info.protocol_options := g_list_append(nil, acc_opt);

  {$ifdef UseHeapTrc}
    WriteLn('W plugin has been compiled with -dUseHeapTrc. Not recommended.');
  {$endif}
end;

exports
  purple_init_plugin;

initialization
  Init;
end.

{ Things happen in the following order:

    * purple loads this library, unit initialization sections will execute:
      + WriteLn redirection will be installed (by purplehelper.pas)
      + Init() procedure is exected (PluginInfo and PluginProtocolInfo
        will be initialized, see above)

    * libpurple calls purple_init_plugin() (implementd in purple.pas):
      + PluginInfo records are passed to purple, registration complete.

    * torchat_load() callback is called by purple

    the above happens only once during application start.
    Then for every account (TorChat profile) that is configured in
    Pidgin and activated it will call the login function:

    * torchat_login() once for every account when going online
    * torchat_close() once for every account when going offline

    when unloading the plugin (on pidgin shutdown) it will

    * switch all accounts to offline (call the above mentioned
      torchat_close() for every account that is currrently online)

    * torchat_unload() which will be the last function it will
      ever call, after this it will unload the library.

}
