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

{ libpurple plugin API
  This unit defines types and functions found in the
  libpurple headers, needed for protocol plugins.

  Most comments from the C headers have been removed
  to keep this unit small, for complete documentation
  refer to the original API docs.

  There would be many more type definitions and zillions
  of additional functions that could also be imported
  from libpurple but here is only the absolute minimum
  needed for a protocol plugin, so it could immediately
  be tested for correctness. More stuff can be added later
  once it is needed.
}
unit purple;
{$mode objfpc}{$H+}

interface
uses
  Classes,
  glib2;

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
    time_t = DWord;
  {$endif}


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
    load: function(plugin: PPurplePlugin): GBoolean;
    unload: function(plugin: PPurplePlugin): GBoolean;
    destroy: procedure(plugin: PPurplePlugin);
    ui_info: Pointer;
    extra_info: Pointer;
    prefs_info: PPurplePluginUiInfo;
    actions: function(plugin: PPurplePlugin; Context: Pointer): PGList;

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
  PPurpleAccountRegistrationCb = procedure();

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
  PPurpleNotifyUserInfo = Pointer;
  PPurpleStatus = Pointer;
  PPurpleStatusType = Pointer;
  PPurpleStoredImage = Pointer;
  PPurpleBlistNode = Pointer;
  PPurpleBuddyIcon = Pointer;
  PPurpleChat = Pointer;
  PPurpleRoomlist = Pointer;
  PPurpleRoomlistRoom = Pointer;
  PPurpleXfer = Pointer;
  PPurpleWhiteboardPrplOps = Pointer;
  TPurpleTypingState = Integer;
  TPurpleMediaSessionType = Integer;
  TPurpleMediaCaps = Integer;
  TPurpleMood = Integer;
  PPurpleAccountUnregistrationCallback = procedure();
  PPurpleSetPublicAliasSuccessCallback = procedure();
  PPurpleSetPublicAliasFailureCallback = procedure();
  PPurpleGetPublicAliasSuccessCallback = procedure();
  PPurpleGetPublicAliasFailureCallback = procedure();
  PPurpleConversation = Pointer;
  PPurpleConvIm = Pointer;

  TPurpleConversationType = (
    PURPLE_CONV_TYPE_UNKNOWN = 0,
    PURPLE_CONV_TYPE_IM,
    PURPLE_CONV_TYPE_CHAT,
    PURPLE_CONV_TYPE_MISC,
    PURPLE_CONV_TYPE_ANY
  );

  TPurpleMessageFlags = Integer; // PURPLE_MESSAGE_XXX flags
const
  PURPLE_MESSAGE_SEND = $0001;
  PURPLE_MESSAGE_RECV = $0002;
  PURPLE_MESSAGE_SYSTEM = $0004;
  PURPLE_MESSAGE_AUTO_RESP = $0008;
  PURPLE_MESSAGE_ACTIVE_ONLY = $0010;
  PURPLE_MESSAGE_NICK = $0020;
  PURPLE_MESSAGE_NO_LOG = $0040;
  PURPLE_MESSAGE_WHISPER = $0080;
  PURPLE_MESSAGE_ERROR = $0200;
  PURPLE_MESSAGE_DELAYED = $0400;
  PURPLE_MESSAGE_RAW = $0800;
  PURPLE_MESSAGE_IMAGES = $1000;
  PURPLE_MESSAGE_NOTIFY = $2000;
  PURPLE_MESSAGE_NO_LINKIFY = $4000;
  PURPLE_MESSAGE_INVISIBLE = $8000;

type
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
    options : TPurpleProtocolOptions;  (**< Protocol options.           *)
    user_splits : PGList;  (**< A GList of PurpleAccountUserSplit  *)
    protocol_options : PGList;  (**< A GList of PurpleAccountOption     *)
    icon_spec : TPurpleBuddyIconSpec;  (**< The icon spec.  *)
    list_icon : function(account: PPurpleAccount; buddy: PPurpleBuddy): PChar;
    list_emblem : function(buddy: PPurpleBuddy): PChar;
    status_text : function(buddy: PPurpleBuddy): PChar;
    tooltip_text : procedure(buddy: PPurpleBuddy; user_info: PPurpleNotifyUserInfo; full: gboolean);
    status_types : function(account: PPurpleAccount): PGList;
    blist_node_menu : function(node: PPurpleBlistNode): PGList;
    chat_info : function(gc: PPurpleConnection): PGList;
    chat_info_defaults : function(gc: PPurpleConnection; chat_name: PChar): PGHashTable;
    login : procedure(account: PPurpleAccount);
    close : procedure(gc: PPurpleConnection);
    send_im : function(gc: PPurpleConnection; who, message: PChar; flags: TPurpleMessageFlags): Integer;
    set_info : procedure(gc: PPurpleConnection; info: PChar);
    send_typing : function(gc: PPurpleConnection; name_: PChar; state: TPurpleTypingState): Integer;
    get_info : procedure(gc: PPurpleConnection; who: PChar);
    set_status : procedure(account: PPurpleAccount; status: PPurpleStatus);
    set_idle : procedure(gc: PPurpleConnection; idletime: Integer);
    change_passwd : procedure(gc: PPurpleConnection; old_pass, new_pass: PChar);
    add_buddy : procedure(gc: PPurpleConnection; buddy: PPurpleBuddy; group: PPurpleGroup);
    add_buddies : procedure(gc: PPurpleConnection; buddies, groups: PGList);
    remove_buddy : procedure(gc: PPurpleConnection; buddy: PPurpleBuddy; group: PPurpleGroup);
    remove_buddies : procedure(gc: PPurpleConnection; buddies, groups: PGList);
    add_permit : procedure(gc: PPurpleConnection; name_: PChar);
    add_deny : procedure(gc: PPurpleConnection; name_: PChar);
    rem_permit : procedure(gc: PPurpleConnection; name_: PChar);
    rem_deny : procedure(gc: PPurpleConnection; name_: PChar);
    set_permit_deny : procedure(gc: PPurpleConnection);
    join_chat : procedure(gc: PPurpleConnection; components: PGHashTable);
    reject_chat : procedure(gc: PPurpleConnection; components: PGHashTable);
    get_chat_name : function(components: PGHashTable): PChar;
    chat_invite : procedure(gc: PPurpleConnection; id: Integer; message, who: PChar);
    chat_leave : procedure(gc: PPurpleConnection; id: Integer);
    chat_whisper : procedure(gc: PPurpleConnection; id: Integer; who, message: PChar);
    chat_send : function(gc: PPurpleConnection; id: Integer; message: PChar; flags: TPurpleMessageFlags): Integer;
    keepalive : procedure(gc: PPurpleConnection);
    register_user : procedure(account: PPurpleAccount);
    get_cb_info : procedure(gc: PPurpleConnection; par1: Integer; who: PChar);
    get_cb_away : procedure(gc: PPurpleConnection; par1: Integer; who: PChar);
    alias_buddy : procedure(gc: PPurpleConnection; who, alias_: PChar);
    group_buddy : procedure(gc: PPurpleConnection; who, old_group, new_group: PChar);
    rename_group : procedure(gc: PPurpleConnection; old_name: PChar; group: PPurpleGroup; moved_buddies: PGList);
    buddy_free : procedure(buddy: PPurpleBuddy);
    convo_closed : procedure(gc: PPurpleConnection; who: PChar);
    normalize : function(account: PPurpleAccount; who: PChar): PChar;
    set_buddy_icon : procedure(gc: PPurpleConnection; img: PPurpleStoredImage);
    remove_group : procedure(gc: PPurpleConnection; group: PPurpleGroup);
    get_cb_real_name : function(gc: PPurpleConnection; id: Integer; who: PChar): PChar;
    set_chat_topic : procedure(gc: PPurpleConnection; id: Integer; topic: PChar);
    find_blist_chat : function(account: PPurpleAccount; name_: PChar): PPurpleChat;
    roomlist_get_list : function(gc: PPurpleConnection): PPurpleRoomlist;
    roomlist_cancel : procedure(list: PPurpleRoomlist);
    roomlist_expand_category : procedure(list: PPurpleRoomlist; category: PPurpleRoomlistRoom);
    can_receive_file : function(gc: PPurpleConnection; who: PChar): gboolean;
    send_file : procedure(gc: PPurpleConnection; who, filename: PChar);
    new_xfer : function(gc: PPurpleConnection; who: PChar): PPurpleXfer;
    offline_message : function(buddy: PPurpleBuddy): gboolean;
    whiteboard_prpl_ops : PPurpleWhiteboardPrplOps;
    send_raw : function(gc: PPurpleConnection; buf: PChar; len: Integer): Integer;
    roomlist_room_serialize : function(room: PPurpleRoomlistRoom): PChar;
    unregister_user : procedure(account: PPurpleAccount; cb: PPurpleAccountUnregistrationCallback; user_data: Pointer);
    send_attention : function(gc: PPurpleConnection; username: PChar; type_: guint): gboolean;
    get_attention_types : function(acct: PPurpleAccount): PGList;
    struct_size : Integer;
    get_account_text_table : function(account: PPurpleAccount): PGHashTable;
    initiate_media : function(account: PPurpleAccount; who: PChar; type_: TPurpleMediaSessionType): gboolean;
    get_media_caps : function(account: PPurpleAccount; who: PChar): TPurpleMediaCaps;
    get_moods : function(account: PPurpleAccount): TPurpleMood;
    set_public_alias : procedure(gc: PPurpleConnection; alias_: PChar; success_cb: PPurpleSetPublicAliasSuccessCallback; failure_cb: PPurpleSetPublicAliasFailureCallback);
    get_public_alias : procedure(gc: PPurpleConnection; success_cb: PPurpleGetPublicAliasSuccessCallback; failure_cb: PPurpleGetPublicAliasFailureCallback);
    add_buddy_with_invite : procedure(pc: PPurpleConnection; buddy: PPurpleBuddy; group: PPurpleGroup; message: PChar);
    add_buddies_with_invite : procedure(pc: PPurpleConnection; buddies, groups: PGList; message: PChar);
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

function  purple_account_option_string_new(text, pref_name, default_value: PChar): PPurpleAccountOption; external LIBPURPLE;
function  purple_account_get_string(account: PPurpleAccount; aname, default_value: PChar): PChar; external LIBPURPLE;
procedure purple_blist_add_buddy(buddy: PPurpleBuddy; contact: PPurpleContact;
  group: PPurpleGroup; node: PPurpleBlistNode); external LIBPURPLE;
procedure purple_blist_add_group(group: PPurpleGroup; node: PPurpleBlistNode); external LIBPURPLE;
procedure purple_blist_alias_buddy(buddy: PPurpleBuddy; aalias: PChar); external LIBPURPLE;
function  purple_find_group(name_: PChar): PPurpleGroup; external LIBPURPLE;
function  purple_group_new(name_: PChar): PPurpleGroup; external LIBPURPLE;
procedure purple_blist_remove_buddy(buddy: PPurpleBuddy); external LIBPURPLE;
function  purple_buddy_get_account(buddy: PPurpleBuddy): PPurpleAccount; external LIBPURPLE;
function  purple_buddy_get_alias_only(buddy: PPurpleBuddy): PChar; external LIBPURPLE;
function  purple_buddy_get_name(buddy: PPurpleBuddy): PChar; external LIBPURPLE;
function  purple_buddy_get_presence(buddy: PPurpleBuddy): PPurplePresence; external LIBPURPLE;
function  purple_buddy_icon_new(account: PPurpleAccount; username: PChar;
  icon_data: Pointer; icon_len: PtrUInt; checksum: PChar): PPurpleBuddyIcon; external LIBPURPLE;
function  purple_buddy_new(account: PPurpleAccount; aname, aalias: PChar): PPurpleBuddy; external LIBPURPLE;
procedure purple_connection_set_state(gc: PPurpleConnection; state: TPurpleConnectionState); external LIBPURPLE;
procedure purple_debug_misc(category: PChar; format: PChar; args: array of const); external LIBPURPLE;
procedure purple_debug_info(category: PChar; format: PChar; args: array of const); external LIBPURPLE;
procedure purple_debug_warning(category: PChar; format: PChar; args: array of const); external LIBPURPLE;
procedure purple_debug_error(category: PChar; format: PChar; args: array of const); external LIBPURPLE;
function  purple_find_buddies(account: PPurpleAccount; aname: PChar): PGSList; external LIBPURPLE;
function  purple_find_buddy(account: PPurpleAccount; aname: PChar): PPurpleBuddy; external LIBPURPLE;
function  purple_notify_message(Plugin: PPurplePlugin;
 typ: TPurpleNotifyMsgType; title: PChar; primary: PChar; secondary: PChar;
 cb: PPurpleNotifyCloseCallback; UserData: Pointer): GBoolean; external LIBPURPLE;
procedure purple_notify_user_info_add_pair(user_info: PPurpleNotifyUserInfo;
  alabel, avalue: PChar); external LIBPURPLE;
function  purple_plugin_register(var Plugin: TPurplePlugin): GBoolean; external LIBPURPLE;
function  purple_presence_get_active_status(presence: PPurplePresence): PPurpleStatus; external LIBPURPLE;
procedure purple_presence_switch_status(presence: PPurplePresence; status_id: PChar); external LIBPURPLE;
procedure purple_prpl_got_user_status(account: PPurpleAccount;
  aname, status_id: PChar); external LIBPURPLE;
function  purple_status_get_type(status: PPurpleStatus): PPurpleStatusType; external LIBPURPLE;
function  purple_status_type_get_primitive(status_type: PPurpleStatusType): TPurpleStatusPrimitive; external LIBPURPLE;
function  purple_status_type_new_full(primitive: TPurpleStatusPrimitive;
  id: PChar; name: Pchar; saveable: GBoolean; user_settable: GBoolean;
  independent: GBoolean): PPurpleStatusType; external LIBPURPLE;
function  purple_timeout_add(Interval: Integer; cb: TGSourceFunc; UserData: Pointer): Integer; external LIBPURPLE;
function  purple_timeout_remove(handle: Integer): GBoolean; external LIBPURPLE;
procedure serv_got_alias(gc: PPurpleConnection; who, aalias: PChar); external LIBPURPLE;
procedure serv_got_im(gc: PPurpleConnection; who, msg: PChar;
  flags: TPurpleMessageFlags; mtime: time_t); external LIBPURPLE;


{ purple_init_plugin is the only exported symbol.
  It is called when libpurple is probing all libs in the plugin
  folder. In the C examples this is hidden behind the macro
  PURPLE_INIT_PLUGIN but here we don't have macros to hide
  such things and the plugin library that is using this unit
  must explicitly export it. }
function purple_init_plugin(var Plugin: TPurplePlugin): GBoolean;

var
  plugin_info: TPurplePluginInfo;
  plugin_protocol_info: TPurplePluginProtocolInfo;

implementation

{ This re-implements the stuff that is behind the PURPLE_INIT_PLUGIN macro.
  In C the macro would define the function and export it, here we only
  define it and the library must export it (because we can't export it
  from within a unit directly). }
function purple_init_plugin(var Plugin: TPurplePlugin): GBoolean;
begin
  Plugin.info := @plugin_info;
  Result := purple_plugin_register(Plugin);
end;

initialization
  FillByte(plugin_info, Sizeof(plugin_info), 0);
  FillByte(plugin_protocol_info, SizeOf(plugin_protocol_info), 0);
end.


