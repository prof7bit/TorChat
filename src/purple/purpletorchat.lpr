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
    {$ifdef windows}  // need config path for heaptrc output
      clientconfig,
    {$endif}
  {$endif}
  {$ifdef unix}
    cthreads,
  {$endif}
  Classes, sysutils, contnrs, glib2,
  purple, torchatabstract, torchatclient, miscfunc;

type
  { TTorchatPurpleClient }
  TTorChatPurpleClient = class(TTorChatClient)
  public
    PurpleAccount: PPurpleAccount;
    PurpleTimer: Integer;
    procedure OnNotifyGui; override;
  end;

  { TClients }

  TClients = class(TFPHashObjectList)
    function Get(Account: PPurpleAccount): TTorChatPurpleClient;
  end;

var
  Clients: TClients;

function Client(Account: PPurpleAccount): TTorChatPurpleClient;
begin
  Result := Clients.Find(Account^.username) as TTorChatPurpleClient;
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
  Clients := TClients.Create(False);
  _info('plugin loaded');
  Result := True;
end;

function OnUnload(var Plugin: TPurplePlugin): GBoolean; cdecl;
begin
  Ignore(@Plugin);
  Clients.Free;
  _info('plugin unloaded');
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
  Clients.Get(Account).SetStatus(TorchatStatus);
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
  NewClient: TTorChatPurpleClient;
  Status: PPurpleStatus;
  Buddy: TABuddy;
  PurpleBuddy: PPurpleBuddy;
  List: TABuddyList;
begin
  _info('OnLogin');
  NewClient := TTorChatPurpleClient.Create(nil);
  NewClient.PurpleAccount := Account;
  NewClient.PurpleTimer := purple_timeout_add(1000, @OnPurpleTimer, NewClient);
  Clients.Add(Account^.username, NewClient);
  purple_connection_set_state(Account^.gc, PURPLE_CONNECTED);
  _info('created torchat client object for ' + Account^.username);

  // it won't call set_status after login, so we have to do it ourselves
  Status := purple_presence_get_active_status(Account^.presence);
  OnSetStatus(Account, Status);

  // add all our buddies to purple's buddy list
  List := Clients.Get(Account).BuddyList;
  List.Lock;
  for Buddy in List.Buddies do begin
    if purple_find_buddy(Account, PChar(Buddy.ID)) = nil then begin
      PurpleBuddy := purple_buddy_new(Account, PChar(Buddy.ID), PChar(Buddy.FriendlyName));
      purple_blist_add_buddy(PurpleBuddy, nil, nil, nil);
    end;
  end;
  List.Unlock;
end;

procedure OnClose(gc: PPurpleConnection); cdecl;
var
  ClientToClose: TTorChatPurpleClient;
begin
  _info('OnClose');
  ClientToClose := Client(gc^.account);
  if Assigned(ClientToClose) then begin
    purple_timeout_remove(ClientToClose.PurpleTimer);
    Clients.Remove(ClientToClose);
    ClientToClose.Free;
    _info('destroyed torchat client object for ' + gc^.account^.username);
  end;
end;

{ TClients }

function TClients.Get(Account: PPurpleAccount): TTorChatPurpleClient;
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
    login := @OnLogin;
    close := @OnClose;
    set_status := @OnSetStatus;
    struct_size := SizeOf(PluginProtocolInfo);
  end;

  {$ifdef UseHeapTrc}
    {$warning compiling with -dUseHeapTrc. Not recommended for release.}
    {$ifdef windows}
      // we have no stdout when running on windows
      heaptrc.SetHeapTraceOutput(ConcatPaths([ConfGetDataDir, 'heaptrc.log']));
    {$endif}
  {$endif}
end.


