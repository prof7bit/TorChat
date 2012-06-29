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
{$if FPC_FULLVERSION < 20600}
  {$fatal *** You need Free Pascal Compiler version 2.6.0 or higher *** }
{$endif}
{$mode objfpc}{$H+}{$T+}
{$packrecords c}
{$modeswitch advancedrecords}
{$modeswitch autoderef}


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

(************************************************
 * The includes are parsed in multiple separate *
 * passes to make them appear nicely ordered in *
 * the code explorer and most importantly to be *
 * able to resolve the cyclic dependencies with *
 * forward type declarations in a separate pass *
 ************************************************)
const
  {$define _const}
  {$include purple_inc_all.pas}
  {$undef _const}

type
  {$define _type_forward}
  {$include purple_inc_all.pas}
  {$undef _type_forward}

  {$define _type}
  {$include purple_inc_all.pas}
  {$undef _type}

{$define _func_public}
{$include purple_inc_all.pas}
{$undef _func_public}

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

{$define _func}
{$include purple_inc_all.pas}
{$undef _func}

{$define _impl}
{$include purple_inc_all.pas}
{$undef _impl}

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


