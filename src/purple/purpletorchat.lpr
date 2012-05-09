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

{$mode objfpc}{$H+}

uses
  {$ifdef UseHeapTrc} // do it with -dUseHeapTrc, not with -gh
    heaptrc,
  {$endif}
  {$ifdef unix}
    cthreads,
  {$endif}
  Classes, sysutils, contnrs, glib2,
  purple, torchatabstract, torchatclient, clientconfig, miscfunc;

type
  { TTorchatPurpleClient }
  TTorChatPurpleClient = class(TTorChatClient)
  public
    PurpleAccount: PPurpleAccount;
    PurpleTimer: Integer;
    procedure OnNotifyGui; override;
  end;

  { TTorChatClients }

  TTorChatClients = class(TFPHashObjectList)
    function Get(Account: PPurpleAccount): TTorChatPurpleClient;
  end;

var
  TorChatClients: TTorChatClients;

function Client(Account: PPurpleAccount): TTorChatPurpleClient;
begin
  Result := TorChatClients.Find(Account^.username) as TTorChatPurpleClient;
end;

function OnPurpleTimer(Data: Pointer): GBoolean; cdecl;
begin
  Ignore(Data);
  TTorChatPurpleClient(Data).ProcessMessages;
  Result := True;
end;

function OnPurpleTimerOneShot(Data: Pointer): GBoolean; cdecl;
begin
  Ignore(Data);
  TTorChatPurpleClient(Data).ProcessMessages;
  Result := False; // purple timer will not fire again
end;

function OnLoad(var Plugin: TPurplePlugin): GBoolean; cdecl;
begin
  Ignore(@Plugin);
  TorChatClients := TTorChatClients.Create(False);
  WriteLn('plugin loaded');
  Result := True;
end;

function OnUnload(var Plugin: TPurplePlugin): GBoolean; cdecl;
begin
  Ignore(@Plugin);
  TorChatClients.Free;
  WriteLn('plugin unloaded');
  Result := True;
end;

function OnStatusTypes(Account: PPurpleAccount): PGList; cdecl;
begin
  Ignore(Account);
  // pidgin (or libpurple or both) has a bug, it will offer "invisible" even
  // if we don't register it (and it will not offer "extended away" even if
  // we register it), so we are registerig all of them and will deal with them
  // in the set_status callback.
  Result := nil;
  Result := g_list_append(Result, purple_status_type_new_full(PURPLE_STATUS_AVAILABLE, nil, nil, True, True, False));
  Result := g_list_append(Result, purple_status_type_new_full(PURPLE_STATUS_UNAVAILABLE, nil, nil, True, True, False));
  Result := g_list_append(Result, purple_status_type_new_full(PURPLE_STATUS_AWAY, nil, nil, True, True, False));
  Result := g_list_append(Result, purple_status_type_new_full(PURPLE_STATUS_EXTENDED_AWAY, nil, nil, True, True, False));
  Result := g_list_append(Result, purple_status_type_new_full(PURPLE_STATUS_INVISIBLE, nil, nil, True, True, False));
  Result := g_list_append(Result, purple_status_type_new_full(PURPLE_STATUS_OFFLINE, nil, nil, True, True, False));
end;

procedure OnSetStatus(Account: PPurpleAccount; Status: PPurpleStatus); cdecl;
var
  PurpleStatusP : TPurpleStatusPrimitive;
  TorchatStatus : TTorchatStatus;
begin
  PurpleStatusP := purple_status_type_get_primitive(purple_status_get_type(Status));
  case PurpleStatusP of
    PURPLE_STATUS_AVAILABLE: TorchatStatus := TORCHAT_AVAILABLE;
    PURPLE_STATUS_UNAVAILABLE: TorchatStatus := TORCHAT_EXTENDED_AWAY;
    PURPLE_STATUS_AWAY: TorchatStatus := TORCHAT_AWAY;
    PURPLE_STATUS_EXTENDED_AWAY: TorchatStatus := TORCHAT_EXTENDED_AWAY;
    PURPLE_STATUS_INVISIBLE: TorchatStatus := TORCHAT_OFFLINE;
  end;
  TorChatClients.Get(Account).SetStatus(TorchatStatus);
end;

function OnGetTextTable(Account: PPurpleAccount): PGHashTable; cdecl;
begin
  Ignore(Account);
  WriteLn('OnGetTextTable');
  Result := g_hash_table_new(@g_str_hash, @g_str_equal);
  g_hash_table_insert(Result, PChar('login_label'), PChar('account'));
end;

function OnListIcon(Account: PPurpleAccount; Buddy: PPurpleBuddy): PChar; cdecl;
begin
  Ignore(Account);
  Ignore(Buddy);
  Result := 'torchat';
  // now it will look for torchat.png in several resolutions
  // in the folders /usr/share/pixmaps/pidgin/protocols/*/
  // the installer for the plugin must install these files.
end;

procedure OnLogin(Account: PPurpleAccount); cdecl;
var
  Client: TTorChatPurpleClient;
  Status: PPurpleStatus;
  TorChatBuddy: TABuddy;
  PurpleBuddy: PPurpleBuddy;
  TorchatList: TABuddyList;
  PurpleList: PGSList;
  ID : String;
  TorExe: String;

begin
  WriteLn('OnLogin');

  TorExe := purple_account_get_string(Account, 'tor', nil);
  WriteLn('Path to Tor configured for this account: ' + TorExe);

  Client := TTorChatPurpleClient.Create(nil);
  Client.PurpleAccount := Account;
  Client.PurpleTimer := purple_timeout_add(1000, @OnPurpleTimer, Client);
  TorChatClients.Add(Account^.username, Client);
  purple_connection_set_state(Account^.gc, PURPLE_CONNECTED);
  WriteLn('created torchat client object for ' + Account^.username);

  // remove buddies from purple's list that not in TorChat's list
  TorchatList := TorChatClients.Get(Account).BuddyList;
  PurpleList := purple_find_buddies(Account, nil);
  while Assigned(PurpleList) do begin
    ID := purple_buddy_get_name(PurpleList^.data);
    WriteLn('found ' + ID + ' on purple buddy list');
    if not Assigned(TorchatList.FindBuddy(ID)) then begin
      purple_blist_remove_buddy(PurpleList^.data);
    end;
    PurpleList := g_slist_delete_link(PurpleList, PurpleList);
  end;

  // add buddies to purple's buddy list that are not in purple's list
  TorchatList.Lock;
  for TorChatBuddy in TorchatList.Buddies do begin
    if purple_find_buddy(Account, PChar(TorChatBuddy.ID)) = nil then begin
      PurpleBuddy := purple_buddy_new(Account, PChar(TorChatBuddy.ID), PChar(TorChatBuddy.FriendlyName));
      purple_blist_add_buddy(PurpleBuddy, nil, nil, nil);
    end;
  end;
  TorchatList.Unlock;

  // it won't call set_status after login, so we have to do it ourselves
  Status := purple_presence_get_active_status(Account^.presence);
  OnSetStatus(Account, Status);
end;

procedure OnClose(gc: PPurpleConnection); cdecl;
var
  ClientToClose: TTorChatPurpleClient;
begin
  WriteLn('OnClose');
  ClientToClose := Client(gc^.account);
  if Assigned(ClientToClose) then begin
    purple_timeout_remove(ClientToClose.PurpleTimer);
    TorChatClients.Remove(ClientToClose);
    ClientToClose.Free;
    WriteLn('destroyed torchat client object for ' + String(gc^.account^.username));
  end;
end;

procedure OnInit(var Plugin: TPurplePlugin);
var
  Option: PPurpleAccountOption;
  Tor: PChar;
begin
  Ignore(@Plugin);
  Tor := PChar(ConfGetTorExe + '/');
  Option := purple_account_option_string_new('Tor binary', 'tor', Tor);
  PluginProtocolInfo.protocol_options := g_list_append(nil, Option);
end;

{ TClients }

function TTorChatClients.Get(Account: PPurpleAccount): TTorChatPurpleClient;
begin
  Result := Find(Account^.username) as TTorChatPurpleClient;
end;


{ TTorchatPurpleClient }

procedure TTorChatPurpleClient.OnNotifyGui;
begin
  purple_timeout_add(0, @OnPurpleTimerOneShot, self);
end;

exports
  purple_init_plugin;

begin
  with PluginInfo do begin
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
    load := @OnLoad;
    unload := @OnUnload;
    extra_info := @PluginProtocolInfo;
  end;

  with PluginProtocolInfo do begin
    options := OPT_PROTO_NO_PASSWORD or OPT_PROTO_REGISTER_NOSCREENNAME;
    list_icon := @OnListIcon;
    status_types := @OnStatusTypes;
    get_account_text_table := @OnGetTextTable;
    login := @OnLogin;
    close := @OnClose;
    set_status := @OnSetStatus;
    struct_size := SizeOf(TPurplePluginProtocolInfo);
  end;

  PluginInitProc := @OnInit;

  {$ifdef UseHeapTrc}
    WriteLn('W plugin has been compiled with -dUseHeapTrc. Not recommended.');
    {$ifdef windows}
      // we have no stdout when running on windows
      heaptrc.SetHeapTraceOutput(ConcatPaths([ConfGetDataDir, 'heaptrc.log']));
    {$endif}
  {$endif}
end.

{ Things happen in the following order:

    * purple loads this library, unit initialization sections will execute:
      + WriteLn() redirection will be installed (by purple.pas)
      + PluginInfo and PluginProtocolInfo will be populated (see above)

    * purple calls purple_init_plugin() (in purple.pas):
      + PluginInitProc() callback is executed (if it is assigned)
      + Info records are passed to purple, registration complete.

    * load() callback is called by purple

    the above happens only once during application start

    * login() callback is called for each account when going online
    * close() callback is called for each account when going offline

}
