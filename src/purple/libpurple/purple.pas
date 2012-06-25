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

{$define interface_const}
{$include purple_inc_all.pas}
{$undef interface_const}

type
  {$ifdef cpu64}
    time_t = UInt64;
  {$else}
    time_t = DWord;
  {$endif}

  PPurpleAccount = ^TPurpleAccount;
  PPurplePresence = ^TPurplePresence;

{$define interface_type}
{$include purple_inc_all.pas}
{$undef interface_type}


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
function  purple_plugin_register(Plugin: PPurplePlugin): GBoolean; cdecl; external LIBPURPLE;
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
procedure serv_got_alias(gc: PPurpleConnection; who, aalias: PChar); external LIBPURPLE;
procedure serv_got_im(gc: PPurpleConnection; who, msg: PChar;
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


{$define implementation}
{$include purple_inc_all.pas}
{$undef implementation}


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


