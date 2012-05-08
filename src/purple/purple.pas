{ purple.pas - libpurple API (not complete) for protocol plugins.

  Copyright (C) 2012 Bernd Kreuss <prof7bit@gmail.com>

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

unit purple;
{$mode objfpc}{$H+}

interface
uses
  Classes, glib2;

const
  {$ifdef windows}
    LIBPURPLE = 'libpurple.dll';
  {$else}
    LIBPURPLE = 'purple';
  {$endif}

type
  {$ifdef cpu64}
    time_t = UInt64;
  {$else}
    time_t = UInt32;
  {$endif}

(************************************************************************
 * Most comments from the C headers have been removed to keep this unit *
 * small, for complete documentation refer to the original API docs.    *
 *                                                                      *
 * There would be many more type definitions and zillions of additional *
 * functions that could also be imported from libpurple but here is     *
 * only the absolute minimum needed for a protocol plugin, so it could  *
 * immediately be tested for correctness. More stuff can be added later *
 * once it is needed.                                                   *
 ************************************************************************)

(****************************************
 *                                      *
 *   Const and Type declarations        *
 *                                      *
 *   from various libpurple/*.h         *
 *   (for function imports scroll       *
 *   down to the next section).         *
 *                                      *
 ****************************************)
{$calling cdecl}
const
  PURPLE_PLUGIN_MAGIC = 5;
  PURPLE_MAJOR_VERSION = 2;
  PURPLE_MINOR_VERSION = 10;
  PURPLE_PRIORITY_DEFAULT = 0;

type
  TPurplePluginType = (
    PURPLE_PLUGIN_UNKNOWN  := -1,  // Unknown type.
    PURPLE_PLUGIN_STANDARD := 0,   // Standard plugin.
    PURPLE_PLUGIN_LOADER,          // Loader plugin.
    PURPLE_PLUGIN_PROTOCOL         // Protocol plugin.
  );

  TPurplePluginPriority = Integer;
  PPurplePluginUiInfo = Pointer;

  PPurplePluginInfo = ^TPurplePluginInfo;

  PPurplePlugin = ^TPurplePlugin;
  TPurplePlugin = packed record
    native_plugin: GBoolean;       // Native C plugin.
    loaded: GBoolean;              // The loaded state.
    handle: Pointer;               // The module handle.
    path: PChar;                   // The path to the plugin.
    info: PPurplePluginInfo;       // The plugin information.
    error: PChar;
    ipc_data: Pointer;             // IPC data.
    extra: Pointer;                // Plugin-specific data.
    unloadable: GBoolean;          // Unloadable
    dependent_plugins: PGList;     // Plugins depending on this

    _purple_reserved1: Pointer;
    _purple_reserved2: Pointer;
    _purple_reserved3: Pointer;
    _purple_reserved4: Pointer;
  end;

  TPurplePluginInfo = packed record
    magic: Integer;
    major_version: Integer;
    minor_version: Integer;
    plugintype: TPurplePluginType;
    ui_requirement: PChar;
    flags: LongInt;
    dependencies: PGList;
    priority: TPurplePluginPriority;
    id: PChar;
    name: PChar;
    version: PChar;
    summary: PChar;
    description: PChar;
    author: PChar;
    homepage: PChar;
    load: function(var Plugin: TPurplePlugin): GBoolean;
    unload: function(var Plugin: TPurplePlugin): GBoolean;
    destroy: procedure(var Plugin: TPurplePlugin);
    ui_info: Pointer;
    extra_info: Pointer;
    prefs_info: PPurplePluginUiInfo;
    actions: function(var Plugin: TPurplePlugin; Context: Pointer): PGList;

    _purple_reserved1: Pointer;
    _purple_reserved2: Pointer;
    _purple_reserved3: Pointer;
    _purple_reserved4: Pointer;
  end;


  TPurpleProtocolOptions = DWord; // bitfield of OPT_PROTO_ constants
const
  OPT_PROTO_UNIQUE_CHATNAME = $00000004;
  OPT_PROTO_CHAT_TOPIC = $00000008;
  OPT_PROTO_NO_PASSWORD = $00000010;
  OPT_PROTO_MAIL_CHECK = $00000020;
  OPT_PROTO_IM_IMAGE = $00000040;
  OPT_PROTO_PASSWORD_OPTIONAL = $00000080;
  OPT_PROTO_USE_POINTSIZE = $00000100;
  OPT_PROTO_REGISTER_NOSCREENNAME = $00000200;
  OPT_PROTO_SLASH_COMMANDS_NATIVE = $00000400;
  OPT_PROTO_INVITE_MESSAGE = $00000800;

type
  TPurpleIconScaleRules = (
    PURPLE_ICON_SCALE_DISPLAY = $01,    // We scale the icon when we display it
    PURPLE_ICON_SCALE_SEND = $02        // We scale the icon before we send it to the server
  );

  TPurpleBuddyIconSpec = packed record
    format: PChar;                          // Comma delimited list of formats
    min_width: Integer;                     // Minimum width of this icon
    min_height: Integer;                    // Minimum height of this icon
    max_width: Integer;                     // Maximum width of this icon
    max_height: Integer;                    // Maximum height of this icon
    max_filesize: PtrUInt;                  // Maximum size in bytes
    scale_rules: TPurpleIconScaleRules;     // How to stretch this icon
  end;

  PPurpleProxyInfo = Pointer;
  PPurplePrivacyType = Pointer;
  PPurplePresence = Pointer;
  PPurpleLog = Pointer;
  PPurpleAccountRegistrationCb = procedure(); {$note fixme: define this}

  PPurpleConnection = ^TPurpleConnection;

  PPurpleAccount = ^TPurpleAccount;
  TPurpleAccount = packed record
    username: PChar;             // The username.
    aalias: PChar;               // How you appear to yourself.
    password: PChar;             // The account password.
    user_info: PChar;            // User information.
    buddy_icon_path: PChar;      // The buddy icon's non-cached path.
    remember_pass: GBoolean;     // Remember the password.
    protocol_id: PChar;          // The ID of the protocol.
    gc: PPurpleConnection;       // The connection handle.
    disconnecting: GBoolean;     // The account is currently disconnecting
    settings: PGHashTable;       // Protocol-specific settings.
    ui_settings: PGHashTable;    // UI-specific settings.
    proxy_info: PPurpleProxyInfo; { Proxy information.  This will be set
                                    to NULL when the account inherits
                                    proxy settings from global prefs.}
    permit: PGSList;                // Permit list.
    deny: PGSList;                  // Deny list.
    perm_deny: PPurplePrivacyType;  // The permit/deny setting.
    status_types: PGList;           // Status types.
    presence: PPurplePresence;      // Presence.                              */
    sytem_log: PPurpleLog;          // The system log                         */
    ui_data: Pointer;               // The UI can put data here.              */
    registration_cb: PPurpleAccountRegistrationCb;
    registration_cb_user_data: Pointer;
    priv: Pointer;                  // Pointer to opaque private data.
  end;

  TPurpleConnectionFlags = Integer;

  TPurpleConnectionState = (
  	PURPLE_DISCONNECTED = 0, // Disconnected.
  	PURPLE_CONNECTED,        // Connected.
  	PURPLE_CONNECTING        // Connecting.
  );

  TPurpleConnection = packed record
    prpl: PPurplePlugin;            // The protocol plugin.
    flags: TPurpleConnectionFlags;  // Connection flags.
    state: TPurpleConnectionState;  // The connection state.
    account: PPurpleAccount;        // The account being connected to.
    password: PChar;                // The password used.
    inpa: Integer;                  // The input watcher.
    buddy_chats: PGSList;           {  A list of active chats
                                       (#PurpleConversation structs of type
                                       #PURPLE_CONV_TYPE_CHAT).}
    proto_data: Pointer;            // Protocol-specific data.
    display_name: PChar;            // How you appear to other people.
    keepalive: Integer;             // Keep-alive.
    wants_to_die: GBoolean;
    disconnect_timeout: Integer;    // Timer used for nasty stack tricks
    last_received: time_t;          {  When we last received a packet. Set by the
                                       prpl to avoid sending unneeded keepalives}
  end;

  PPurpleAccountOption = Pointer;
  PPurpleBuddy = Pointer;
  PPurpleContact = Pointer;
  PPurpleGroup = Pointer;
  PPurpleUserInfo = Pointer;
  PPurpleStatus = Pointer;
  PPurpleStatusType = Pointer;
  PPurpleStoredImage = Pointer;
  PPurpleBlistNode = Pointer;
  PPurpleChat = Pointer;
  PPurpleRoomlist = Pointer;
  PPurpleRoomlistRoom = Pointer;
  PPurpleXfer = Pointer;
  PPurpleWhiteboardPrplOps = Pointer;
  TPurpleMessageFlags = Integer;
  TPurpleTypingState = Integer;
  TPurpleMediaSessionType = Integer;
  TPurpleMediaCaps = Integer;
  TPurpleMood = Integer;
  PPurpleSetPublicAliasSuccessCallback = procedure(); {$note fixme: define this}
  PPurpleSetPublicAliasFailureCallback = procedure();
  PPurpleGetPublicAliasSuccessCallback = procedure();
  PPurpleGetPublicAliasFailureCallback = procedure();

  TPurpleStatusPrimitive = (
    PURPLE_STATUS_UNSET = 0,
    PURPLE_STATUS_OFFLINE,
    PURPLE_STATUS_AVAILABLE,
    PURPLE_STATUS_UNAVAILABLE,
    PURPLE_STATUS_INVISIBLE,
    PURPLE_STATUS_AWAY,
    PURPLE_STATUS_EXTENDED_AWAY,
    PURPLE_STATUS_MOBILE,
    PURPLE_STATUS_TUNE,
    PURPLE_STATUS_MOOD,
    PURPLE_STATUS_NUM_PRIMITIVES
  );

  TPurplePluginProtocolInfo = packed record
    options: TPurpleProtocolOptions;
    user_splits: PGList;      // A GList of PurpleAccountUserSplit
    protocol_options: PGList; // A GList of PurpleAccountOption
    icon_spec: TPurpleBuddyIconSpec; // The icon spec.
    list_icon: function(account: PPurpleAccount; buddy: PPurpleBuddy): PChar;
    list_emblem: function(buddy: PPurpleBuddy): PChar;
    status_text: function(buddy: PPurpleBuddy): PChar;
    tooltip_text: procedure(buddy: PPurpleBuddy; user_info: PPurpleUserInfo; full: GBoolean);
    status_types: function(account: PPurpleAccount): PGList;
    blist_node_menu: function(node: PPurpleBlistNode): PGList;
    chat_info: function(gc: PPurpleConnection): PGList;
    chat_info_defaults: function(gc: PPurpleConnection; chat_name: PChar): PGHashTable;
    login: procedure(acc: PPurpleAccount);
    close: procedure(gc: PPurpleConnection);
    send_im: function(gc: PPurpleConnection; who: PChar; message: PChar; flags: TPurpleMessageFlags): Integer;
    set_info: procedure(gc: PPurpleConnection; info: PChar);
    send_typing: function(gc: PPurpleConnection; name: PChar; state: TPurpleTypingState): Integer;
    get_info: procedure(gc: PPurpleConnection; who: PChar);
    set_status: procedure(account: PPurpleAccount; status: PPurpleStatus);
    set_idle: procedure(gc: PPurpleConnection; idletime: Integer);
    change_passwd: procedure(gc: PPurpleConnection; old_pass, newpass: PChar);
    add_buddy: procedure(gc: PPurpleConnection; buddy: PPurpleBuddy; group: PPurpleGroup);
    add_buddies: procedure(gc: PPurpleConnection; buddies: PGList; groups: PGList);
    remove_buddy: procedure(gc: PPurpleConnection; buddy: PPurpleBuddy; group: PPurpleGroup);
    remove_buddies: procedure(gc: PPurpleConnection; buddies: PGList; groups: PGList);
    add_permit: procedure(gc: PPurpleConnection; name: PChar);
    add_deny: procedure(gc: PPurpleConnection; name: PChar);
    rem_permit: procedure(gc: PPurpleConnection; name: PChar);
    rem_deny: procedure(gc: PPurpleConnection; name: PChar);
    set_permit_deny: procedure(gc: PPurpleConnection);
    join_chat: procedure(gc: PPurpleConnection; components: PGHashTable);
    reject_chat: procedure(gc: PPurpleConnection; components: PGHashTable);
    get_chat_name: function(components: PGHashTable): PChar;
    chat_invite: procedure(gc: PPurpleConnection; id: Integer; message: PChar; who: PChar);
    chat_leave: procedure(gc: PPurpleConnection);
    chat_whisper: procedure(gc: PPurpleConnection; id: Integer; who: PChar; message: PChar);
    chat_send: function(gc: PPurpleConnection; id: Integer; message: PChar; flags: TPurpleMessageFlags): Integer;
    keepalive: procedure(gc: PPurpleConnection);
    register_user: procedure(acc: PPurpleAccount);
    get_cb_info: Pointer; // nothing because its deprecated
    get_cb_away: Pointer; // nothing because its deprecated
    alias_buddy: procedure(gc: PPurpleConnection; who: PChar; alias: PChar);
    group_buddy: procedure(gc: PPurpleConnection; who, old_group, new_group: PChar);
    rename_group: procedure(gc: PPurpleConnection; old_name: PChar; group: PPurpleGroup; moved_buddies: PGList);
    buddy_free: procedure(buddy: PPurpleBuddy);
    convo_closed: procedure(gc: PPurpleConnection; who: PChar);
    normalize: function(acc: PPurpleAccount; who: PChar): PChar;
    set_buddy_icon: procedure(gc: PPurpleConnection; img: PPurpleStoredImage);
    remove_group: procedure(gc: PPurpleConnection; group: PPurpleGroup);
    get_cb_real_name: function(gc: PPurpleConnection; id: Integer; who: PChar): PChar;
    set_chat_topic: procedure(gc: PPurpleConnection; id: Integer; topic: PChar);
    find_blist_chat: function(acc: PPurpleAccount; name: PChar): PPurpleChat;
    roomlist_get_list: function(gc: PPurpleConnection): PPurpleRoomlist;
    roomlist_cancel: procedure(list: PPurpleRoomlist);
    roomlist_expand_category: procedure(list: PPurpleRoomlist; category: PPurpleRoomlistRoom);
    can_receive_file: function(gc: PPurpleConnection; who: PChar): GBoolean;
    send_file: procedure(gc: PPurpleConnection; who: PChar; filename: PChar);
    new_xfer: function(gc: PPurpleConnection; who: PChar): PPurpleXfer;
    offline_message: function(buddy: PPurpleBuddy): GBoolean;
    whiteboard_prpl_ops : PPurpleWhiteboardPrplOps;
    send_raw: function(gc: PPurpleConnection; buf: Pointer; len: Integer): Integer;
    roomlist_room_serialize: function(room: PPurpleRoomlistRoom): PChar;
    unregister_user: procedure(acc: PPurpleAccount; cb: Pointer; user_data: Pointer); {$note fixme: define the callback function}
    send_attention: function(gc: PPurpleConnection; username: PChar; typ: UInt32): GBoolean;
    get_attention_types: function(acc: PPurpleAccount): PGList;
    struct_size: UInt32;
    get_account_text_table: function(acc: PPurpleAccount): PGHashTable;
    initiate_media: function(acc: PPurpleAccount; who: PChar; typ: TPurpleMediaSessionType): GBoolean;
    get_media_caps: function(acc: PPurpleAccount; who: PChar): TPurpleMediaCaps;
    get_moods: function(acc: PPurpleAccount): TPurpleMood;
    set_public_alias: procedure(gc: PPurpleConnection; aalias: PChar;
      success_cb: PPurpleSetPublicAliasSuccessCallback;
      failure_cp: PPurpleSetPublicAliasFailureCallback);
    get_public_alias: procedure(acc: PPurpleAccount;
      success_cb: PPurpleGetPublicAliasSuccessCallback;
      failure_cb: PPurpleGetPublicAliasFailureCallback);
    add_buddy_with_invite: procedure(gc: PPurpleConnection; buddy: PPurpleBuddy; group: PPurpleGroup; message: PChar);
    add_buddies_with_invite: procedure(gc: PPurpleConnection; buddies: PGList; groups: PGList; message: PChar);
  end;

  TPurpleNotifyMsgType = (
    PURPLE_NOTIFY_MSG_ERROR   = 0, // Error notification.
    PURPLE_NOTIFY_MSG_WARNING,     // Warning notification.
    PURPLE_NOTIFY_MSG_INFO         // Information notification.
  );

  PPurpleNotifyCloseCallback = procedure(UserData: Pointer);



(********************************************
 *                                          *
 *   function imports from libpurple        *
 *                                          *
 ********************************************)
{$calling cdecl}

function purple_plugin_register(var Plugin: TPurplePlugin): GBoolean; external LIBPURPLE;
function purple_timeout_add(Interval: Integer; cb: TGSourceFunc; UserData: Pointer): Integer; external LIBPURPLE;
function purple_timeout_remove(handle: Integer): GBoolean; external LIBPURPLE;
procedure purple_debug_info(category: PChar; format: PChar; args: array of const); external LIBPURPLE;
procedure purple_debug_warning(category: PChar; format: PChar; args: array of const); external LIBPURPLE;
procedure purple_debug_error(category: PChar; format: PChar; args: array of const); external LIBPURPLE;
function purple_notify_message(var Plugin: TPurplePlugin;
 typ: TPurpleNotifyMsgType; title: PChar; primary: PChar; secondary: PChar;
 cb: PPurpleNotifyCloseCallback; UserData: Pointer): GBoolean; external LIBPURPLE;
function purple_status_type_new_full(primitive: TPurpleStatusPrimitive;
  id: PChar; name: Pchar; saveable: GBoolean; user_settable: GBoolean;
  independent: GBoolean): PPurpleStatusType; external LIBPURPLE;
procedure purple_connection_set_state(gc: PPurpleConnection; state: TPurpleConnectionState); external LIBPURPLE;
function purple_status_type_get_primitive(status_type: PPurpleStatusType): TPurpleStatusPrimitive; external LIBPURPLE;
function purple_status_get_type(status: PPurpleStatus): PPurpleStatusType; external LIBPURPLE;
function purple_presence_get_active_status(presence: PPurplePresence): PPurpleStatus; external LIBPURPLE;
function purple_buddy_get_name(buddy: PPurpleBuddy): PChar; external LIBPURPLE;
function purple_find_buddies(account: PPurpleAccount; aname: PChar): PGSList; external LIBPURPLE;
function purple_find_buddy(account: PPurpleAccount; aname: PChar): PPurpleBuddy; external LIBPURPLE;
function purple_account_option_string_new(text, pref_name, default_value: PChar): PPurpleAccountOption; cdecl; external LIBPURPLE;
function purple_account_get_string(account: PPurpleAccount; aname, default_value: PChar): PChar; cdecl; external;
function purple_buddy_new(account: PPurpleAccount; aname, aalias: PChar): PPurpleBuddy; external LIBPURPLE;
procedure purple_blist_add_buddy(buddy: PPurpleBuddy; contact: PPurpleContact;
  group: PPurpleGroup; node: PPurpleBlistNode); external LIBPURPLE;
procedure purple_blist_remove_buddy(buddy: PPurpleBuddy); external LIBPURPLE;



(****************************************
 *                                      *
 *   This is the only exported symbol   *
 *                                      *
 *   It is called when libpurple is     *
 *   probing all libs in the plugin     *
 *   folder. In the C examples this     *
 *   is hidden behind the macro         *
 *   PURPLE_INIT_PLUGIN but here we     *
 *   don't have macros to hide such     *
 *   things and the plugin library      *
 *   using this unit must explicitly    *
 *   export it.                         *
 *                                      *
 ****************************************)
function purple_init_plugin(var Plugin: TPurplePlugin): GBoolean;



(****************************************
 *                                      *
 *   Our own functions and helpers      *
 *                                      *
 *   from here on we also switch back   *
 *   to the default calling convention  *
 *                                      *
 ****************************************)
{$calling default}
type
  TDebugLevel = (
    DEBUG_INFO,
    DEBUG_WARNING,
    DEBUG_ERROR
  );

  { TWritelnRedirect will catch everything that is WriteLn() to stdout and
    redirects it to the libpurple debug logger. We will create an instance
    of this and replace it with the standard output stream }
  TWritelnRedirect = class(TStream)
    function Write(const Buffer; Count : Longint) : Longint; override;
  end;

procedure _info(Msg: String);
procedure _warning(Msg: String);
procedure _error(Msg: String);

{ sometimes we need to allocate a char* for which
  libpurple will take ownership. This will allocate
  memory directly from the system and not from the
  FPC heap manager! }
function AllocPurpleString(Str: String): PChar;

var
  PluginInfo: TPurplePluginInfo;
  PluginProtocolInfo: TPurplePluginProtocolInfo;
  PluginInitProc: procedure(var Plugin: TPurplePlugin) = nil;

implementation
uses
  sysutils, StreamIO;

var
  OldStdOut: Text;
  WritelnRedirect: TWritelnRedirect;

procedure _purple_debug(Level: TDebugLevel; Msg: String);
begin
  case Level of
    DEBUG_INFO: purple_debug_info(PluginInfo.id, PChar(Msg + LineEnding), []);
    DEBUG_WARNING: purple_debug_warning(PluginInfo.id, PChar(Msg + LineEnding), []);
    DEBUG_ERROR: purple_debug_error(PluginInfo.id, PChar(Msg + LineEnding), []);
  end;
  {$ifdef DebugToConsole}
  try
    WriteLn(OldStdOut, '[', PluginInfo.id, ' ', Level, '] ', Msg);
  except
    // There is no stdout on windows if pidgin is not run with --debug
  end;
  {$endif}
end;

procedure _info(Msg: String);
begin
  _purple_debug(DEBUG_INFO, Msg);
end;

procedure _warning(Msg: String);
begin
  _purple_debug(DEBUG_WARNING, Msg);
end;

procedure _error(Msg: String);
begin
  _purple_debug(DEBUG_ERROR, Msg);
end;

function AllocPurpleString(Str: String): PChar;
var
  L : Integer;
begin
  L := Length(Str);
  if L = 0 then
    Result := nil
  else begin
    // bypass the FPC heap manager and malloc() directly from the system.
    // Of course we do this only for strings that we pass to libpurple and
    // where libpurple promises to free it again.
    Result := g_malloc(L+1);
    Move(Str[1], Result[0], L);
    Result[L] := #0;
  end;
end;

{ TWritelnRedirect }
function TWritelnRedirect.Write(const Buffer; Count: Longint): Longint;
var
  Msg, Lvl, Txt: String;
begin
  Result := Count;
  SetLength(Msg, Count);
  Move(Buffer, Msg[1], Count);
  Msg := Trim(Msg);
  Lvl := LeftStr(Msg, 4);
  Txt := RightStr(Msg, Length(Msg)-4);
  case Lvl of
    '(0) ': _error(Txt);
    '(1) ': _warning(Txt);
    '(2) ': _info(Txt);
  else
    _info(Msg);
  end;
end;

{ The TorChat units are logging their debug info with WriteLn(). It is
  the responsibility of the frontend to catch them and log them or display
  them appropriately. Here we install the redirection that will do this. }
procedure InstallWritelnRedirect;
begin
  OldStdOut := Output;
  WritelnRedirect := TWritelnRedirect.Create();
  AssignStream(Output, WritelnRedirect);
  Rewrite(Output);
end;

procedure UninstallWritelnRedirect;
begin
  Output := OldStdOut;
  WritelnRedirect.Free;
end;

{ This re-implements the stuff that is behind the PURPLE_INIT_PLUGIN macro.
  In C the macro would define the function and export it, here we only
  define it and the library must export it (because we can't export it
  from within a unit directly). }
function purple_init_plugin(var Plugin: TPurplePlugin): GBoolean; cdecl;
begin
  {$ifdef DebugToConsole}
  {$warning compiling with -dDebugToConsole. Not recommended for release.}
  _warning('Plugin has been compiled with -dDebugToConsole. Not recommended.');
  {$endif}
  Plugin.info := @PluginInfo;
  if Assigned(PluginInitProc) then PluginInitProc(Plugin);
  Result := purple_plugin_register(Plugin);
end;

initialization
  InstallWritelnRedirect;
  FillByte(PluginInfo, Sizeof(PluginInfo), 0);
  FillByte(PluginProtocolInfo, SizeOf(PluginProtocolInfo), 0);
finalization
  UninstallWritelnRedirect;
end.

