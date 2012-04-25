{ purple.pas

  A minimalistic header translation for making libpurple plugins.

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02111-1301  USA
}

unit purple;
{$mode objfpc}{$H+}

interface

type
  { This class can be used to pass all kinds of data to our timer callbacks.
    The structure is internal to our plugin and may contain whatever we want. }
  TUserData = class
    buddy_from: String;
    buddy_to: String;
    Status: String;
    Message: String;
    Param1: Integer;
    Param2: Integer;
    // not yet decided what other fields we will need
  end;


(******************************
 *                            *
 *       some GLIB stuff      *
 *                            *
 ******************************)
type
  PGList = ^TGList;
  TGList = packed record
    data: Pointer;
    next: PGlist;
    prev: PGList;
  end;

  GBoolean = Boolean32;

  PGSourceFunc = function(UserData: TUserData): GBoolean; cdecl;


(******************************
 *                            *
 *  from libpurple/plugin.h   *
 *                            *
 ******************************)
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
  PPurplePluginUiInfo= Pointer; {$warning define me!}

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

    { If a plugin defines a 'load' function, and it returns FALSE,
      then the plugin will not be loaded.}
    load: function(var Plugin: TPurplePlugin): GBoolean; cdecl;
    unload: function(var Plugin: TPurplePlugin): GBoolean; cdecl;
    destroy: procedure(var Plugin: TPurplePlugin); cdecl;

    ui_info: Pointer;
    extra_info: Pointer;

    { Used by any plugin to display preferences.
      If #ui_info has been specified, this will be ignored.}
    prefs_info: PPurplePluginUiInfo;

    { This callback has a different use depending on whether this
      plugin type is PURPLE_PLUGIN_STANDARD or PURPLE_PLUGIN_PROTOCOL.

      If PURPLE_PLUGIN_STANDARD then the list of actions will show up
      in the Tools menu, under a submenu with the name of the plugin.
      context will be NULL.

      If PURPLE_PLUGIN_PROTOCOL then the list of actions will show up
      in the Accounts menu, under a submenu with the name of the
      account.  context will be set to the PurpleConnection for that
      account.  This callback will only be called for online accounts.}
    actions: function(var Plugin: TPurplePlugin; Context: Pointer): PGList; cdecl;

    _purple_reserved1: Pointer;
    _purple_reserved2: Pointer;
    _purple_reserved3: Pointer;
    _purple_reserved4: Pointer;
  end;



(******************************
 *                            *
 *  from libpurple/notify.h   *
 *                            *
 ******************************)
type
  TPurpleNotifyMsgType = (
	  PURPLE_NOTIFY_MSG_ERROR   = 0, // Error notification.
	  PURPLE_NOTIFY_MSG_WARNING,     // Warning notification.
	  PURPLE_NOTIFY_MSG_INFO         // Information notification.
  );

  PPurpleNotifyCloseCallback = procedure(UserData: TUserData); cdecl;


var
  PluginInfo: TPurplePluginInfo;


(****************************************
 *                                      *
 *  functions imported from libpurple   *
 *                                      *
 ****************************************)

  purple_plugin_register: function(var Plugin: TPurplePlugin): GBoolean; cdecl;
  purple_timeout_add: function(Interval: Integer; cb: PGSourceFunc; UserData: TUserData): Integer; cdecl;
  purple_timeout_remove: function(handle: Integer): GBoolean; cdecl;
  purple_debug_info: procedure(category: PChar; format: PChar; args: array of const); cdecl;
  purple_debug_warning: procedure(category: PChar; format: PChar; args: array of const); cdecl;
  purple_debug_error: procedure(category: PChar; format: PChar; args: array of const); cdecl;
  purple_notify_message: function(var Plugin: TPurplePlugin;
    typ: TPurpleNotifyMsgType; title: PChar; primary: PChar; secondary: PChar;
    cb: PPurpleNotifyCloseCallback; UserData: TUserData): GBoolean; cdecl;


type
  TDebugLevel = (
    DEBUG_INFO,
    DEBUG_WARNING,
    DEBUG_ERROR
  );

procedure _info(Msg: String);
procedure _info(Msg: String; Args: array of const);
procedure _warning(Msg: String);
procedure _warning(Msg: String; Args: array of const);
procedure _error(Msg: String);
procedure _error(Msg: String; Args: array of const);

function purple_init_plugin(var Plugin: TPurplePlugin): GBoolean; cdecl;

implementation
uses
  dynlibs, sysutils;

var
  HPurple: TLibHandle = NilHandle;
  ImportsLoaded: Boolean = False;

procedure _purple_debug(Level: TDebugLevel; Msg: String);
begin
  {$ifdef DebugToConsole}
  try
    writeln(PluginInfo.id, ': ', Level, ' ', Msg);
  except
  end;
  {$endif}
  if ImportsLoaded then begin
    case Level of
      DEBUG_INFO: purple_debug_info(PluginInfo.id, PChar(Msg + LineEnding), []);
      DEBUG_WARNING: purple_debug_warning(PluginInfo.id, PChar(Msg + LineEnding), []);
      DEBUG_ERROR: purple_debug_error(PluginInfo.id, PChar(Msg + LineEnding), []);
    end;
  end
  else begin
    {$ifndef DebugToConsole}
    try
      writeln(PluginInfo.id, ': ', Level, ' ', Msg);
    except
    end;
    {$endif}
  end;
end;

procedure _purple_debug(Level: TDebugLevel; Msg: String; Args: array of const);
begin
  try
    _purple_debug(Level, Format(Msg, Args));
  except
    _purple_debug(DEBUG_ERROR,
      'could not format arguments for the following debug message: "'
      + StringReplace(Msg, '%', '$', [rfReplaceAll]) + '"')
  end;
end;

procedure _info(Msg: String);
begin
  _purple_debug(DEBUG_INFO, Msg);
end;

procedure _info(Msg: String; Args: array of const);
begin
  _purple_debug(DEBUG_INFO, Msg, Args);
end;

procedure _warning(Msg: String);
begin
  _purple_debug(DEBUG_WARNING, Msg);
end;

procedure _warning(Msg: String; Args: array of const);
begin
  _purple_debug(DEBUG_WARNING, Msg, Args);
end;

procedure _error(Msg: String);
begin
  _purple_debug(DEBUG_ERROR, Msg);
end;

procedure _error(Msg: String; Args: array of const);
begin
  _purple_debug(DEBUG_ERROR, Msg, Args);
end;

procedure UnloadImports;
begin
  if HPurple <> NilHandle then begin
    FreeLibrary(HPurple);
    HPurple := NilHandle;
    ImportsLoaded := False;
  end;
end;

procedure LoadImports;
var
  PossibleNames : array[1..2] of string = ('libpurple', 'libpurple.so.0');
  LibName: String;
  Error: Boolean = False;

  procedure Connect(const ProcVar; ProcName: String);
  var
    P : Pointer;
  begin
    P := GetProcAddress(HPurple, ProcName);
    if p <> nil then
      PPointer(@ProcVar)^ := P
    else begin
      _error('could not find symbol "' + ProcName + '"');
      Error := True;
    end;
  end;

begin
  if ImportsLoaded then
    exit;
  for LibName in PossibleNames do begin
    HPurple := LoadLibrary(LibName);
    if HPurple <> NilHandle then begin
      Connect(purple_plugin_register, 'purple_plugin_register');
      Connect(purple_timeout_add, 'purple_timeout_add');
      Connect(purple_timeout_remove, 'purple_timeout_remove');
      Connect(purple_debug_info, 'purple_debug_info');
      Connect(purple_debug_warning, 'purple_debug_warning');
      Connect(purple_debug_error, 'purple_debug_error');
      Connect(purple_notify_message, 'purple_notify_message');
      if Error then
        UnloadImports
      else
        ImportsLoaded := True;
      break;
    end;
  end;
  if HPurple = NilHandle then
    _error('could not load libpurple');
end;

{ This re-implements the stuff that is behind the PURPLE_INIT_PLUGIN macro.
  In C the macro would define the function and export it, here we only
  define it and the library must export it (because we can't export it
  from within a unit directly). }
function purple_init_plugin(var Plugin: TPurplePlugin): GBoolean; cdecl;
begin
  {$ifdef DebugToConsole}
  _warning('Plugin has been compiled with -dDebugToConsole. Not recommended.');
  {$endif}
  LoadImports;
  Result := False;
  if ImportsLoaded then begin;
    Plugin.info := @PluginInfo;
    Result := purple_plugin_register(Plugin);
  end
  else begin
    _error('plugin will not load');
    Result := False;
  end;
end;

initialization
  FillByte(PluginInfo, Sizeof(PluginInfo), 0);
finalization
  UnloadImports;
end.

