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
  type names start with 'T', pointer types with 'P' which will then
  look like TSomeThing or PSomeThing.

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
  torchatabstract,
  torchatclient,
  clientconfig,
  miscfunc;

const
  PRPL_OFFLINE = 'offline';
  PRPL_AVAILABLE = 'available';
  PRPL_AWAY = 'away';
  PRPL_XA = 'xa';
  PRPL_INVISIBLE = 'invisible';

type
  { TTorchatPurpleClient wraps the TorChat client}
  TTorChatPurpleClient = class(TTorChatClient)
  public
    purple_account: PPurpleAccount;
    purple_timer: Integer;
    procedure OnNotifyGui; override;
    procedure OnBuddyStatusChange(ABuddy: TABuddy); override;
    procedure OnBuddyAdded(ABuddy: TABuddy); override;
    procedure OnBuddyRemoved(ABuddy: TABuddy); override;
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
  TTorChatPurpleClient(data).ProcessMessages;
  Result := True;
end;

function cb_purple_timer_oneshot(data: Pointer): GBoolean; cdecl;
begin
  Ignore(data);
  TTorChatPurpleClient(data).ProcessMessages;
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
  Result := g_list_append(Result, purple_status_type_new_full(PURPLE_STATUS_AVAILABLE, PRPL_AVAILABLE, nil, True, True, False));
  Result := g_list_append(Result, purple_status_type_new_full(PURPLE_STATUS_AWAY, PRPL_AWAY, nil, True, True, False));
  Result := g_list_append(Result, purple_status_type_new_full(PURPLE_STATUS_UNAVAILABLE, PRPL_XA, nil, True, True, False));
  Result := g_list_append(Result, purple_status_type_new_full(PURPLE_STATUS_INVISIBLE, PRPL_INVISIBLE, nil, True, True, False));
  Result := g_list_append(Result, purple_status_type_new_full(PURPLE_STATUS_OFFLINE, PRPL_OFFLINE, nil, True, True, False));
end;

procedure torchat_set_status(acc: PPurpleAccount; status: PPurpleStatus); cdecl;
var
  status_prim   : TPurpleStatusPrimitive;
  TorchatStatus : TTorchatStatus;
begin
  status_prim := purple_status_type_get_primitive(purple_status_get_type(status));
  case status_prim of
    PURPLE_STATUS_AVAILABLE: TorchatStatus := TORCHAT_AVAILABLE;
    PURPLE_STATUS_UNAVAILABLE: TorchatStatus := TORCHAT_EXTENDED_AWAY;
    PURPLE_STATUS_AWAY: TorchatStatus := TORCHAT_AWAY;
    PURPLE_STATUS_EXTENDED_AWAY: TorchatStatus := TORCHAT_EXTENDED_AWAY;
    PURPLE_STATUS_INVISIBLE: TorchatStatus := TORCHAT_OFFLINE;
  end;
  TorChatClients.Get(acc).SetStatus(TorchatStatus);
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
  NewClient: TTorChatPurpleClient;
  TorchatBuddy: TABuddy;
  TorchatList: TABuddyList;
  purple_id: PChar;
  purple_alias: PChar;
  purple_status: PPurpleStatus;
  purple_buddy: PPurpleBuddy;
  purple_list: PGSList;

begin
  NewClient := TTorChatPurpleClient.Create(nil);
  NewClient.purple_account := acc;
  NewClient.purple_timer := purple_timeout_add(1000, @cb_purple_timer, NewClient);
  TorChatClients.Add(acc^.username, NewClient);
  purple_connection_set_state(acc^.gc, PURPLE_CONNECTED);

  // remove buddies from purple's list that not in TorChat's TorchatList
  TorchatList := NewClient.BuddyList;
  purple_list := purple_find_buddies(acc, nil);
  while Assigned(purple_list) do begin
    purple_id := purple_buddy_get_name(purple_list^.data);
    if not Assigned(TorchatList.FindBuddy(purple_id)) then begin
      purple_blist_remove_buddy(purple_list^.data);
    end;
    purple_list := g_slist_delete_link(purple_list, purple_list);
  end;

  // add buddies to purple's buddy list that are not in purple's TorchatList
  TorchatList.Lock;
  for TorchatBuddy in TorchatList.Buddies do begin
    if purple_find_buddy(acc, PChar(TorchatBuddy.ID)) = nil then begin
      purple_id := GetMemAndCopy(TorchatBuddy.ID);
      purple_alias := GetMemAndCopy(TorchatBuddy.FriendlyName);
      purple_buddy := purple_buddy_new(acc, purple_id, purple_alias);
      purple_blist_add_buddy(purple_buddy, nil, nil, nil);
      FreeMem(purple_id);
      FreeMem(purple_alias);
    end;
  end;
  TorchatList.Unlock;

  // it won't call set_status after login, so we have to do it ourselves
  purple_status := purple_presence_get_active_status(acc^.presence);
  torchat_set_status(acc, purple_status);
end;

procedure torchat_close(gc: PPurpleConnection); cdecl;
var
  ClientToClose: TTorChatPurpleClient;
begin
  ClientToClose := Client(gc^.account);
  if Assigned(ClientToClose) then begin
    purple_timeout_remove(ClientToClose.purple_timer);
    TorChatClients.Remove(ClientToClose);
    ClientToClose.Free;
  end;
end;

{ TClients }

function TTorChatClients.Get(Account: PPurpleAccount): TTorChatPurpleClient;
begin
  Result := Find(Account^.username) as TTorChatPurpleClient;
end;


{ TTorchatPurpleClient }

procedure TTorChatPurpleClient.OnNotifyGui;
begin
  purple_timeout_add(0, @cb_purple_timer_oneshot, self);
end;

procedure TTorChatPurpleClient.OnBuddyStatusChange(ABuddy: TABuddy);
var
  buddy_id: PChar;
  status_id: PChar;
begin
  buddy_id := PChar(ABuddy.ID);
  case ABuddy.Status of
    TORCHAT_AVAILABLE: status_id := PRPL_AVAILABLE;
    TORCHAT_AWAY: status_id := PRPL_AWAY;
    TORCHAT_EXTENDED_AWAY: status_id := PRPL_XA;
    TORCHAT_OFFLINE: status_id := PRPL_OFFLINE;
  end;
  purple_prpl_got_user_status(purple_account, buddy_id, status_id);
end;

procedure TTorChatPurpleClient.OnBuddyAdded(ABuddy: TABuddy);
begin

end;

procedure TTorChatPurpleClient.OnBuddyRemoved(ABuddy: TABuddy);
begin

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
    struct_size := SizeOf(TPurplePluginProtocolInfo);
  end;

  // add additional fields to the settings dialog
  TorPath := PChar(ConfGetTorExe + '/');
  acc_opt := purple_account_option_string_new('Tor binary', 'tor', TorPath);
  plugin_protocol_info.protocol_options := g_list_append(nil, acc_opt);

  {$ifdef UseHeapTrc}
    WriteLn('W plugin has been compiled with -dUseHeapTrc. Not recommended.');
    {$ifdef windows}
      // we have no stdout when running on windows
      heaptrc.SetHeapTraceOutput(ConcatPaths([ConfGetDataDir, 'heaptrc.log']));
    {$endif}
  {$endif}

  {$ifdef NoOutputRedirect}
    WriteLn('W plugin has been compiled with -dNoOutputRedirect. This will crash on windows.');
  {$endif}
end;

exports
  purple_init_plugin;

initialization
  Init;
end.

{ Things happen in the following order:

    * purple loads this library, unit initialization sections will execute:
      + WriteLn() redirection will be installed (by purple.pas)
      + PluginInfo and PluginProtocolInfo will be populated (see above)

    * purple calls purple_init_plugin() (in purple.pas):
      + Info records are passed to purple, registration complete.

    * torchat_load() callback is called by purple

    the above happens only once during application start

    * torchat_login() callback is called for each account when going online
    * torchat_close() callback is called for each account when going offline

}
