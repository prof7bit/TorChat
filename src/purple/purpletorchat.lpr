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
  Normally in Pascal we use Uppercase/CamelCase identifiers and
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
  libpurple callback functions and variables that hold libpurple
  or glib types are named with lowercase and under_score and all
  other variables are using the normal Pascal naming conventions.

  The TorChat engine will call overridden virtual methods when
  TorChat events happen, these are the methods that start with

    TTorChatPurpleClient.OnXxxx()

  and the callback fuctions that are called by libpurple when
  Pidgin/libpurple wants something to happen are the procedures
  and functions that start with

    torchat_xxx()

  and the xxx corresponds to the field names in the info records
  where these callbacks have been registered.
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
  tc_buddy,
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
    procedure OnNeedPump; override;
    procedure OnGotOwnID; override;
    procedure OnBuddyStatusChange(ABuddy: IBuddy); override;
    procedure OnBuddyAdded(ABuddy: IBuddy); override;
    procedure OnBuddyRemoved(ABuddy: IBuddy); override;
    procedure OnInstantMessage(ABuddy: IBuddy; AText: String); override;
  end;

  { TTorChatClients holds a list of clients since we can have
    multiple "sccounts" in pidgin at the same time }
  TTorChatClients = class(TFPHashObjectList)
    function Find(Account: PPurpleAccount): TTorChatPurpleClient;
  end;

var
  PurplePlugin: PPurplePlugin;
  TorChatClients: TTorChatClients;


(********************************************************************
 *                                                                  *
 *  The timer functions are called  by libpurple timers, they are   *
 *  fired in regular intervals and all OnXxxx method calls from     *
 *  TorChat into libpurple will ultimately originate from one of    *
 *  these calls to Pump(). This is needed because we may not just   *
 *  call purple_xxx functions from our own threads, everything      *
 *  needs to happen on the main thread.                             *
 *                                                                  *
 ********************************************************************)

function cb_purple_timer(data: Pointer): GBoolean; cdecl;
begin
  TTorChatPurpleClient(data).Pump;
  Result := True;
end;

function cb_purple_timer_oneshot(data: Pointer): GBoolean; cdecl;
begin
  TTorChatPurpleClient(data).Pump;
  Result := False; // purple timer will not fire again
end;

//{$define DebugTimer}
{$ifdef DebugTimer}
const
  DEBUG_TIMER_DELAY = 120;
  DEBUG_TIMER_INTERVAL = 30;
  DEBUG_FUNC_ADDR: TGSourceFunc = nil;
{ this timer function is only used for debugging, it
  can be used to automatically trigger certain events
  at runtime more easily than doing it manually. }
function __debug(Data: Pointer): gboolean; cdecl;
var
  I: Integer;
  TorChat: TTorChatClient;
begin
  WriteLn('W *** debug timer triggered, unexpected events begin...');
  for I := 0 to TorChatClients.Count-1 do begin
    TorChat := TTorChatClient(TorChatClients.Items[I]);
    TorChat.Roster.DoDisconnectAll;
    Sleep(400);
  end;
  WriteLn('W *** debug timer end');
  if DEBUG_TIMER_INTERVAL > 0 then
    purple_timeout_add(DEBUG_TIMER_INTERVAL*1000, DEBUG_FUNC_ADDR, nil);
  Result := False;
end;
{$endif DebugTimer}

(******************************************************************
 *                                                                *
 *  All the following functions are callbacks that are called by  *
 *  libpurple when the user interacts with the application.       *
 *  They have been registered in the two info records during      *
 *  initialization (see the notes at the end of this file).       *
 *                                                                *
 ******************************************************************)

function torchat_load(plugin: PPurplePlugin): GBoolean; cdecl;
begin
  PurplePlugin := plugin;
  TorChatClients := TTorChatClients.Create(False);
  {$ifdef DebugTimer}
  DEBUG_FUNC_ADDR := @__debug;
  purple_timeout_add(DEBUG_TIMER_DELAY*1000, DEBUG_FUNC_ADDR, nil);
  WriteLn('E Debug timer installed, crazy things will happen unexpectedly...');
  {$endif DebugTimer}
  WriteLn('plugin loaded');
  Result := True;
end;

function torchat_unload(plugin: PPurplePlugin): GBoolean; cdecl;
begin
  Ignore(@plugin);
  TorChatClients.Free;
  WriteLn('plugin unloaded');
  Result := True;
end;

function torchat_status_types(acc: PPurpleAccount): PGList; cdecl;
begin
  Ignore(acc);
  // pidgin has some strange policy regardig usable status types:
  // as soon as there are more than one protocols active it will
  // fall back to a standard list of status types, no matter whether
  // all the protocols support them or any of them requested them,
  // so we are forced to register them all and then map them to
  // TorChat statuses in our torchat_set_status() callback.
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
  TorChatClients.Find(acc).SetStatus(TorchatStatus);
end;

procedure torchat_add_buddy(gc: PPurpleConnection; purple_buddy: PPurpleBuddy; group: PPurpleGroup); cdecl;
var
  TorChat : TTorChatPurpleClient;
  Buddy: IBuddy;
  purple_id: PChar;
  purple_alias: PChar;
begin
  Ignore(group);
  TorChat := TorChatClients.Find(gc^.account);
  if Assigned(TorChat) then begin
    purple_id := purple_buddy_get_name(purple_buddy);
    purple_alias := purple_buddy_get_alias_only(purple_buddy);

    {$note move all the following into a TTorChatClient method, there is way too much knowledge about implementation details here}

    // first try the templist
    Buddy := TorChat.TempList.ByID(purple_id);
    if Assigned(Buddy) then begin
      Buddy.SetFriendlyName(purple_alias);
      TorChat.Roster.AddBuddyNoCallback(Buddy);
      TorChat.TempList.RemoveBuddy(Buddy);
      Buddy.ResetConnectInterval;
      Buddy.ResetTimeout;
      Buddy.SendAddMe;
    end

    // otherwise try to create a new one
    else begin
      Buddy := TBuddy.Create(TorChat);
      if Buddy.InitID(purple_id) then begin
        Buddy.SetFriendlyName(purple_alias);
        TorChat.Roster.AddBuddyNoCallback(Buddy);
      end
      else begin
        purple_notify_message(PurplePlugin, PURPLE_NOTIFY_MSG_ERROR,
          'Cannot add buddy',
          'A buddy with this ID cannot be added',
          'Either this ID contains invalid characters or it is incomplete or the ID is already on the list.',
          nil,
          nil);
        purple_blist_remove_buddy(purple_buddy);
      end;
    end;
  end;
end;

function torchat_send_im(gc: PPurpleConnection; who, message: PChar; flags: TPurpleMessageFlags): Integer; cdecl;
var
  TorChat: TTorChatPurpleClient;
  Buddy: IBuddy;
  Msg: String;
begin
  Msg := StringReplace(message, '<br>', LineEnding, [rfReplaceAll]);
  TorChat := TorChatClients.Find(gc^.account);
  if Assigned(TorChat) then begin
    Buddy := TorChat.Roster.ByID(who);
    if Assigned(Buddy) then begin
      Result := Integer(Buddy.SendIM(Msg));
    end;
  end;
end;

procedure torchat_remove_buddy(gc: PPurpleConnection; purple_buddy: PPurpleBuddy; group: PPurpleGroup); cdecl;
var
  TorChat: IClient;
  Buddy: IBuddy;
  purple_id: PChar;
begin
  Ignore(group);
  purple_id := purple_buddy_get_name(purple_buddy);
  TorChat := TorChatClients.Find(gc^.account);
  Buddy := TorChat.Roster.ByID(purple_id);
  if Assigned(Buddy) then
    Buddy.RemoveYourself;
end;

procedure torchat_alias_buddy(gc: PPurpleConnection; who, aalias: PChar); cdecl;
var
  TorChat: TTorChatPurpleClient;
  Buddy: IBuddy;
begin
  TorChat := TorChatClients.Find(gc^.account);
  if Assigned(TorChat) then begin
    Buddy := TorChat.Roster.ByID(who);
    Buddy.SetFriendlyName(aalias);
    serv_got_alias(gc, who, aalias);
  end;
end;

procedure torchat_tooltip_text(purple_buddy: PPurpleBuddy; user_info: PPurpleNotifyUserInfo; full: gboolean); cdecl;
var
  buddy_id : PChar;
  TorChat: TTorChatPurpleClient;
  Buddy: IBuddy;
begin
  if not full then exit;
  TorChat := TorChatClients.Find(purple_buddy_get_account(purple_buddy));
  if Assigned(TorChat) then begin
    buddy_id := purple_buddy_get_name(purple_buddy);
    Buddy := TorChat.Roster.ByID(buddy_id);
    if Assigned(Buddy) then begin
      if Buddy.Software <> '' then begin
        purple_notify_user_info_add_pair(
          user_info,
          'Client',
          PChar(Buddy.Software + '-' + Buddy.SoftwareVersion)
        );
      end;;
    end;
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
  purple_status: PPurpleStatus;
begin
  TorChat := TTorChatPurpleClient.Create(nil, acc^.username, acc);
  TorChat.purple_timer := purple_timeout_add(1000, @cb_purple_timer, TorChat);
  TorChatClients.Add(acc^.username, TorChat);

  // it won't call set_status after login, so we have to do it ourselves
  purple_status := purple_presence_get_active_status(acc^.presence);
  torchat_set_status(acc, purple_status);
end;

procedure torchat_close(gc: PPurpleConnection); cdecl;
var
  TorChat: TTorChatPurpleClient;
begin
  TorChat := TorChatClients.Find(gc^.account);
  if Assigned(TorChat) then begin
    purple_timeout_remove(TorChat.purple_timer);
    TorChatClients.Remove(TorChat);
    TorChat.Free;
  end;
end;

(********************************************************************
 *                 end of libpurple callbacks                       *
 ********************************************************************)

{ TClients }

function TTorChatClients.Find(Account: PPurpleAccount): TTorChatPurpleClient;
begin
  Result := inherited Find(Account^.username) as TTorChatPurpleClient;
end;

{ TTorchatPurpleClient }

constructor TTorChatPurpleClient.Create(AOwner: TComponent; AProfileName: String;
  account: PPurpleAccount);
begin
  purple_account := account;
  inherited Create(AOwner, AProfileName);
end;


(********************************************************************
 *                                                                  *
 *  All the following methods are called when TorChat feels the     *
 *  need to notify libpurple/Pidgin about events that happened.     *
 *                                                                  *
 *  They all ultimately originate from one of the calls to Pump()   *
 *  in the timer functions because everything has to happen on      *
 *  libpurple's main thread. The only exception is OnNeedPump()     *
 *  which can come from any thread and is only used to request      *
 *  another Pump() to be scheduled as soon as posible.              *
 *                                                                  *
 ********************************************************************)

procedure TTorChatPurpleClient.OnNeedPump;
begin
  purple_timeout_add(0, @cb_purple_timer_oneshot, Self);
end;

procedure TTorChatPurpleClient.OnGotOwnID;
var
  Buddy: IBuddy;
  purple_buddy: PPurpleBuddy;
  purple_id: PChar;
  purple_alias: PChar;
  group_name: PChar;
  purple_group: PPurpleGroup;
  purple_list: PGSList;
begin
  purple_connection_set_state(purple_account^.gc, PURPLE_CONNECTED);

  group_name := GetMemAndCopy(Roster.GroupName);
  purple_group := purple_find_group(group_name);

  // remove buddies from purple's list that not in TorChat's list
  purple_list := purple_find_buddies(purple_account, nil);
  while Assigned(purple_list) do begin
    purple_id := purple_buddy_get_name(purple_list^.data);
    if not Assigned(Roster.ByID(purple_id)) then begin
      purple_blist_remove_buddy(purple_list^.data);
    end;
    purple_list := g_slist_delete_link(purple_list, purple_list);
  end;

  // add buddies to purple's buddy list that are not in purple's list
  Roster.Lock;
  for Buddy in Roster do begin
    purple_id := GetMemAndCopy(Buddy.ID);
    purple_alias := GetMemAndCopy(Buddy.FriendlyName);
    purple_buddy := purple_find_buddy(purple_account, purple_id);
    if not Assigned(purple_buddy) then begin
      if not Assigned(purple_group) then begin
        purple_group := purple_group_new(group_name);
        purple_blist_add_group(purple_group, nil);
      end;
      purple_buddy := purple_buddy_new(purple_account, purple_id, purple_alias);
      purple_blist_add_buddy(purple_buddy, nil, purple_group, nil);
    end
    else begin
      serv_got_alias(purple_account^.gc, purple_id, purple_alias);
      purple_blist_alias_buddy(purple_buddy, purple_alias);
    end;
    FreeMem(purple_id);
    FreeMem(purple_alias);
  end;
  Roster.Unlock;
  FreeMem(group_name);
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
  if not HSNameOk then exit; // bcaue we don't have a group name yet
  buddy_name := GetMemAndCopy(ABuddy.ID);
  buddy_alias := GetMemAndCopy(ABuddy.FriendlyName);
  group_name := GetMemAndCopy(Roster.GroupName);
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

procedure TTorChatPurpleClient.OnInstantMessage(ABuddy: IBuddy; AText: String);
begin
  serv_got_im(
    purple_account^.gc,
    PChar(ABuddy.ID),
    Pchar(AText),
    PURPLE_MESSAGE_RECV,
    NowUTCUnix
  );
end;

procedure Init;
var
  acc_opt: PPurpleAccountOption;
  TorPath: PChar;
begin
  SOFTWARE_NAME := 'libpurple/TorChat'; // for the 'client' message

  with plugin_info do begin
    magic := PURPLE_PLUGIN_MAGIC;
    major_version := PURPLE_MAJOR_VERSION;
    minor_version := PURPLE_MINOR_VERSION;
    plugintype := PURPLE_PLUGIN_PROTOCOL;
    priority := PURPLE_PRIORITY_DEFAULT;
    id := 'prpl-prof7bit-torchat';
    name := 'TorChat';
    version := PChar(SOFTWARE_VERSION);
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
    add_buddy := @torchat_add_buddy;
    remove_buddy := @torchat_remove_buddy;
    alias_buddy := @torchat_alias_buddy;
    tooltip_text := @torchat_tooltip_text;
    send_im := @torchat_send_im;
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
