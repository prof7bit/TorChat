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
{$packrecords c}

interface
uses
  Classes,
  ctypes,
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

  { TWrapper }

  TWrapper = class
    constructor Create;
    procedure Free;
  end;

// some forward declarations that will be resolved
// in the following include files

  PPurpleAccount = ^TPurpleAccount;

{$define purple_interface}
{$include purple_inc_plugin.pas}
{$include purple_inc_connection.pas}
{$include purple_inc_account.pas}
{$include purple_inc_ft.pas}
{$include purple_inc_presence.pas}
{$include purple_inc_blist.pas}
{$undef purple_interface}


(****************************************
 *                                      *
 *   Const and Type declarations        *
 *                                      *
 *   from various libpurple/*.h         *
 *   (for function imports scroll       *
 *   down to the next section).         *
 *                                      *
 ****************************************)

const
  PURPLE_PLUGIN_MAGIC = 5;
  PURPLE_MAJOR_VERSION = 2;
  PURPLE_MINOR_VERSION = 7; // instead of 10 because of Debian stale :-(
  PURPLE_PRIORITY_DEFAULT = 0;

type


  TPurpleProtocolOptions = DWord; // bitfield of OPT_PROTO_ constants
const
  OPT_PROTO_UNIQUE_CHATNAME       = $00000004;
  OPT_PROTO_CHAT_TOPIC            = $00000008;
  OPT_PROTO_NO_PASSWORD           = $00000010;
  OPT_PROTO_MAIL_CHECK            = $00000020;
  OPT_PROTO_IM_IMAGE              = $00000040;
  OPT_PROTO_PASSWORD_OPTIONAL     = $00000080;
  OPT_PROTO_USE_POINTSIZE         = $00000100;
  OPT_PROTO_REGISTER_NOSCREENNAME = $00000200;
  OPT_PROTO_SLASH_COMMANDS_NATIVE = $00000400;
  OPT_PROTO_INVITE_MESSAGE        = $00000800;

type
  TPurpleIconScaleRules = (
    PURPLE_ICON_SCALE_DISPLAY = $01,    // We scale the icon when we display it
    PURPLE_ICON_SCALE_SEND    = $02     // We scale the icon before we send it to the server
  );

  TPurpleBuddyIconSpec = record
    format        : PChar;                  // Comma delimited list of formats
    min_width     : cint;                   // Minimum width of this icon
    min_height    : cint;                   // Minimum height of this icon
    max_width     : cint;                   // Maximum width of this icon
    max_height    : cint;                   // Maximum height of this icon
    max_filesize  : csize_t;                // Maximum size in bytes
    scale_rules   : TPurpleIconScaleRules;  // How to stretch this icon
  end;





  PPurpleAccountOption      = Pointer;
  PPurpleNotifyUserInfo     = Pointer;
  PPurpleStatusType         = Pointer;
  PPurpleStoredImage        = Pointer;
  PPurpleChat               = Pointer;
  PPurpleRoomlist           = Pointer;
  PPurpleRoomlistRoom       = Pointer;
  PPurpleWhiteboardPrplOps  = Pointer;
  TPurpleTypingState        = cint;
  TPurpleMediaSessionType   = cint;
  TPurpleMediaCaps          = cint;
  TPurpleMood               = cint;
  PPurpleAccountUnregistrationCallback = procedure(); cdecl; // fixme: signature?
  PPurpleSetPublicAliasSuccessCallback = procedure(); cdecl; // fixme: signature?
  PPurpleSetPublicAliasFailureCallback = procedure(); cdecl; // fixme: signature?
  PPurpleGetPublicAliasSuccessCallback = procedure(); cdecl; // fixme: signature?
  PPurpleGetPublicAliasFailureCallback = procedure(); cdecl; // fixme: signature?
  PPurpleConversation       = Pointer;
  PPurpleRequestFields      = Pointer;
  PPurpleRequestFieldGroup  = Pointer;
  PPurpleRequestField       = Pointer;

  TPurpleConversationType = (
    PURPLE_CONV_TYPE_UNKNOWN = 0,
    PURPLE_CONV_TYPE_IM,
    PURPLE_CONV_TYPE_CHAT,
    PURPLE_CONV_TYPE_MISC,
    PURPLE_CONV_TYPE_ANY
  );


  TPurpleMessageFlags = cint; // PURPLE_MESSAGE_XXX flags
const
  PURPLE_MESSAGE_SEND         = $0001;
  PURPLE_MESSAGE_RECV         = $0002;
  PURPLE_MESSAGE_SYSTEM       = $0004;
  PURPLE_MESSAGE_AUTO_RESP    = $0008;
  PURPLE_MESSAGE_ACTIVE_ONLY  = $0010;
  PURPLE_MESSAGE_NICK         = $0020;
  PURPLE_MESSAGE_NO_LOG       = $0040;
  PURPLE_MESSAGE_WHISPER      = $0080;
  PURPLE_MESSAGE_ERROR        = $0200;
  PURPLE_MESSAGE_DELAYED      = $0400;
  PURPLE_MESSAGE_RAW          = $0800;
  PURPLE_MESSAGE_IMAGES       = $1000;
  PURPLE_MESSAGE_NOTIFY       = $2000;
  PURPLE_MESSAGE_NO_LINKIFY   = $4000;
  PURPLE_MESSAGE_INVISIBLE    = $8000;

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

  TPurplePluginProtocolInfo = record
    options                 : TPurpleProtocolOptions;  (**< Protocol options.           *)
    user_splits             : PGList;  (**< A GList of PurpleAccountUserSplit  *)
    protocol_options        : PGList;  (**< A GList of PurpleAccountOption     *)
    icon_spec               : TPurpleBuddyIconSpec;  (**< The icon spec.  *)
    list_icon               : function(account: PPurpleAccount; buddy: TPurpleBuddy): PChar; cdecl;
    list_emblem             : function(buddy: TPurpleBuddy): PChar; cdecl;
    status_text             : function(buddy: TPurpleBuddy): PChar; cdecl;
    tooltip_text            : procedure(buddy: TPurpleBuddy; user_info: PPurpleNotifyUserInfo; full: gboolean); cdecl;
    status_types            : function(account: PPurpleAccount): PGList; cdecl;
    blist_node_menu         : function(node: PPurpleBlistNode): PGList; cdecl;
    chat_info               : function(gc: TPurpleConnection): PGList; cdecl;
    chat_info_defaults      : function(gc: TPurpleConnection; chat_name: PChar): PGHashTable; cdecl;
    login                   : procedure(account: PPurpleAccount); cdecl;
    close                   : procedure(gc: TPurpleConnection); cdecl;
    send_im                 : function(gc: TPurpleConnection; who, message: PChar; flags: TPurpleMessageFlags): cint; cdecl;
    set_info                : procedure(gc: TPurpleConnection; info: PChar); cdecl;
    send_typing             : function(gc: TPurpleConnection; name_: PChar; state: TPurpleTypingState): cint; cdecl;
    get_info                : procedure(gc: TPurpleConnection; who: PChar); cdecl;
    set_status              : procedure(account: PPurpleAccount; status: PPurpleStatus); cdecl;
    set_idle                : procedure(gc: TPurpleConnection; idletime: cint); cdecl;
    change_passwd           : procedure(gc: TPurpleConnection; old_pass, new_pass: PChar); cdecl;
    add_buddy               : procedure(gc: TPurpleConnection; buddy: TPurpleBuddy; group: TPurpleGroup); cdecl;
    add_buddies             : procedure(gc: TPurpleConnection; buddies, groups: PGList); cdecl;
    remove_buddy            : procedure(gc: TPurpleConnection; buddy: TPurpleBuddy; group: TPurpleGroup); cdecl;
    remove_buddies          : procedure(gc: TPurpleConnection; buddies, groups: PGList); cdecl;
    add_permit              : procedure(gc: TPurpleConnection; name_: PChar); cdecl;
    add_deny                : procedure(gc: TPurpleConnection; name_: PChar); cdecl;
    rem_permit              : procedure(gc: TPurpleConnection; name_: PChar); cdecl;
    rem_deny                : procedure(gc: TPurpleConnection; name_: PChar); cdecl;
    set_permit_deny         : procedure(gc: TPurpleConnection); cdecl;
    join_chat               : procedure(gc: TPurpleConnection; components: PGHashTable); cdecl;
    reject_chat             : procedure(gc: TPurpleConnection; components: PGHashTable); cdecl;
    get_chat_name           : function(components: PGHashTable): PChar; cdecl;
    chat_invite             : procedure(gc: TPurpleConnection; id: cint; message, who: PChar); cdecl;
    chat_leave              : procedure(gc: TPurpleConnection; id: cint); cdecl;
    chat_whisper            : procedure(gc: TPurpleConnection; id: cint; who, message: PChar); cdecl;
    chat_send               : function(gc: TPurpleConnection; id: cint; message: PChar; flags: TPurpleMessageFlags): cint; cdecl;
    keepalive               : procedure(gc: TPurpleConnection); cdecl;
    register_user           : procedure(account: PPurpleAccount); cdecl;
    get_cb_info             : procedure(gc: TPurpleConnection; par1: cint; who: PChar); cdecl;
    get_cb_away             : procedure(gc: TPurpleConnection; par1: cint; who: PChar); cdecl;
    alias_buddy             : procedure(gc: TPurpleConnection; who, alias_: PChar); cdecl;
    group_buddy             : procedure(gc: TPurpleConnection; who, old_group, new_group: PChar); cdecl;
    rename_group            : procedure(gc: TPurpleConnection; old_name: PChar; group: TPurpleGroup; moved_buddies: PGList); cdecl;
    buddy_free              : procedure(buddy: TPurpleBuddy); cdecl;
    convo_closed            : procedure(gc: TPurpleConnection; who: PChar); cdecl;
    normalize               : function(account: PPurpleAccount; who: PChar): PChar; cdecl;
    set_buddy_icon          : procedure(gc: TPurpleConnection; img: PPurpleStoredImage); cdecl;
    remove_group            : procedure(gc: TPurpleConnection; group: TPurpleGroup); cdecl;
    get_cb_real_name        : function(gc: TPurpleConnection; id: cint; who: PChar): PChar; cdecl;
    set_chat_topic          : procedure(gc: TPurpleConnection; id: cint; topic: PChar); cdecl;
    find_blist_chat         : function(account: PPurpleAccount; name_: PChar): PPurpleChat; cdecl;
    roomlist_get_list       : function(gc: TPurpleConnection): PPurpleRoomlist; cdecl;
    roomlist_cancel         : procedure(list: PPurpleRoomlist); cdecl;
    roomlist_expand_category: procedure(list: PPurpleRoomlist; category: PPurpleRoomlistRoom); cdecl;
    can_receive_file        : function(gc: TPurpleConnection; who: PChar): gboolean; cdecl;
    send_file               : procedure(gc: TPurpleConnection; who, filename: PChar); cdecl;
    new_xfer                : function(gc: TPurpleConnection; who: PChar): TPurpleXfer; cdecl;
    offline_message         : function(buddy: TPurpleBuddy): gboolean; cdecl;
    whiteboard_prpl_ops     : PPurpleWhiteboardPrplOps;
    send_raw                : function(gc: TPurpleConnection; buf: PChar; len: cint): cint; cdecl;
    roomlist_room_serialize : function(room: PPurpleRoomlistRoom): PChar; cdecl;
    unregister_user         : procedure(account: PPurpleAccount; cb: PPurpleAccountUnregistrationCallback; user_data: Pointer); cdecl;
    send_attention          : function(gc: TPurpleConnection; username: PChar; type_: guint): gboolean; cdecl;
    get_attention_types     : function(acct: PPurpleAccount): PGList; cdecl;
    struct_size             : cint;
    get_account_text_table  : function(account: PPurpleAccount): PGHashTable; cdecl;
    initiate_media          : function(account: PPurpleAccount; who: PChar; type_: TPurpleMediaSessionType): gboolean; cdecl;
    get_media_caps          : function(account: PPurpleAccount; who: PChar): TPurpleMediaCaps; cdecl;
    get_moods               : function(account: PPurpleAccount): TPurpleMood; cdecl;
    set_public_alias        : procedure(gc: TPurpleConnection; alias_: PChar; success_cb: PPurpleSetPublicAliasSuccessCallback; failure_cb: PPurpleSetPublicAliasFailureCallback); cdecl;
    get_public_alias        : procedure(gc: TPurpleConnection; success_cb: PPurpleGetPublicAliasSuccessCallback; failure_cb: PPurpleGetPublicAliasFailureCallback); cdecl;
    add_buddy_with_invite   : procedure(pc: TPurpleConnection; buddy: TPurpleBuddy; group: TPurpleGroup; message: PChar); cdecl;
    add_buddies_with_invite : procedure(pc: TPurpleConnection; buddies, groups: PGList; message: PChar); cdecl;
  end;

  TPurpleNotifyMsgType = (
    PURPLE_NOTIFY_MSG_ERROR   = 0, // Error notification.
    PURPLE_NOTIFY_MSG_WARNING,     // Warning notification.
    PURPLE_NOTIFY_MSG_INFO         // Information notification.
  );

  PPurpleNotifyCloseCb = procedure(user_data: Pointer); cdecl;
  PPurpleRequestDlgBtnCb = procedure(user_data: Pointer; fields: PPurpleRequestFields); cdecl;


(********************************************
 *                                          *
 *   function imports from libpurple        *
 *                                          *
 ********************************************)

function  purple_account_option_string_new(text, pref_name, default_value: PChar): PPurpleAccountOption; cdecl; external LIBPURPLE;
procedure purple_buddy_icons_set_for_user(account: PPurpleAccount;
  username: PChar; icon_data: Pointer; icon_len: csize_t; checksum: PChar); cdecl; external LIBPURPLE;
procedure purple_debug_misc(category: PChar; format: PChar; args: array of const); cdecl; external LIBPURPLE;
procedure purple_debug_info(category: PChar; format: PChar; args: array of const); cdecl; external LIBPURPLE;
procedure purple_debug_warning(category: PChar; format: PChar; args: array of const); cdecl; external LIBPURPLE;
procedure purple_debug_error(category: PChar; format: PChar; args: array of const); cdecl; external LIBPURPLE;
function  purple_find_buddies(account: PPurpleAccount; aname: PChar): PGSList; cdecl; external LIBPURPLE;
function  purple_imgstore_get_data(img: PPurpleStoredImage): Pointer; cdecl; external LIBPURPLE;
function  purple_imgstore_get_size(img: PPurpleStoredImage): csize_t; cdecl; external LIBPURPLE;
function  purple_notify_message(Plugin: PPurplePlugin;
 typ: TPurpleNotifyMsgType; title: PChar; primary: PChar; secondary: PChar;
 cb: PPurpleNotifyCloseCb; UserData: Pointer): GBoolean; cdecl; external LIBPURPLE;
procedure purple_notify_user_info_add_pair(user_info: PPurpleNotifyUserInfo;
  label_, value: PChar); cdecl; external LIBPURPLE;
function  purple_plugin_action_new(label_: PChar; callback: PPurplePluginActionCb): PPurplePluginAction; cdecl; external LIBPURPLE;
function  purple_plugin_register(var Plugin: PPurplePlugin): GBoolean; cdecl; external LIBPURPLE;
procedure purple_prpl_got_user_status(account: PPurpleAccount;
  aname, status_id: PChar); cdecl; external LIBPURPLE;
function  purple_request_fields_new: PPurpleRequestFields; cdecl; external LIBPURPLE;
procedure purple_request_fields_add_group(fields: PPurpleRequestFields;
  group: PPurpleRequestFieldGroup); cdecl; external LIBPURPLE;
function  purple_request_fields(handle: Pointer;
  title, primary, secondary: PChar; fields: PPurpleRequestFields; ok_text: PChar;
  ok_cb:  PPurpleRequestDlgBtnCb; cancel_text: PChar; cancel_cb: PPurpleRequestDlgBtnCb;
  account: PPurpleAccount; who: PChar; conv: PPurpleConversation;
  user_data: Pointer): Pointer; cdecl; external LIBPURPLE; {<< this function
  differs from the C headers, they originally defined the callbacks as
  GCallback (which would take only one argument) but the callbacks will
  actually be given two aruments. Therefore I have made the following
  change: both callbacks now must be of type PPurpleRequestDlgBtnCb which
  receives two arguments: user_data and fields. }
function  purple_request_fields_get_string(fields: PPurpleRequestFields; id: PChar): PChar; cdecl; external LIBPURPLE;
procedure purple_request_field_group_add_field(group: PPurpleRequestFieldGroup;
  field: PPurpleRequestField); cdecl; external LIBPURPLE;
function  purple_request_field_group_new(title: PChar): PPurpleRequestFieldGroup; cdecl; external LIBPURPLE;
function  purple_request_field_string_new(id, text, default_value: PChar;
  multiline: gboolean): PPurpleRequestField; cdecl; external LIBPURPLE;
function  purple_status_get_type(status: PPurpleStatus): PPurpleStatusType; cdecl; external LIBPURPLE;
function  purple_status_type_get_primitive(status_type: PPurpleStatusType): TPurpleStatusPrimitive; cdecl; external LIBPURPLE;
function  purple_status_type_new_full(primitive: TPurpleStatusPrimitive;
  id: PChar; name: Pchar; saveable: GBoolean; user_settable: GBoolean;
  independent: GBoolean): PPurpleStatusType; cdecl; external LIBPURPLE;
function  purple_timeout_add(Interval: cint; cb: TGSourceFunc; UserData: Pointer): cint; cdecl; external LIBPURPLE;
function  purple_timeout_remove(handle: cint): GBoolean; cdecl; external LIBPURPLE;
procedure serv_got_alias(gc: TPurpleConnection; who, aalias: PChar); external LIBPURPLE;
procedure serv_got_im(gc: TPurpleConnection; who, msg: PChar;
  flags: TPurpleMessageFlags; mtime: time_t); cdecl; external LIBPURPLE;


{ purple_init_plugin is the only exported symbol.
  It is called when libpurple is probing all libs in the plugin
  folder. In the C examples this is hidden behind the macro
  PURPLE_INIT_PLUGIN but here we don't have macros to hide
  such things and the plugin library that is using this unit
  must explicitly export it. }
function purple_init_plugin(Plugin: PPurplePlugin): GBoolean; cdecl;

var
  plugin_info: TPurplePluginInfo;
  plugin_protocol_info: TPurplePluginProtocolInfo;

implementation
uses
  sysutils;

function _PChar(S: String): PChar; inline;
begin
  if Length(S) = 0 then
    Result := nil
  else
    Result := PChar(S);
end;

{ TWrapper }

constructor TWrapper.Create;
begin
  raise Exception.Create('you cannot create this wrapper object yourself: ' + Self.ToString);
end;

procedure TWrapper.Free;
begin
  raise Exception.Create('you cannot free this wrapper object yourself: ' + Self.ToString);
end;


{$define purple_implementation}
{$include purple_inc_plugin.pas}
{$include purple_inc_connection.pas}
{$include purple_inc_account.pas}
{$include purple_inc_ft.pas}
{$include purple_inc_presence.pas}
{$include purple_inc_blist.pas}
{$undef purple_implementation}


{ This re-implements the stuff that is behind the PURPLE_INIT_PLUGIN macro.
  In C the macro would define the function and export it, here we only
  define it and the library must export it (because we can't export it
  from within a unit directly). }
function purple_init_plugin(Plugin: PPurplePlugin): GBoolean; cdecl;
begin
  Plugin^.info := @plugin_info;
  Result := purple_plugin_register(Plugin);
end;

initialization
  FillByte(plugin_info, Sizeof(plugin_info), 0);
  FillByte(plugin_protocol_info, SizeOf(plugin_protocol_info), 0);
end.


