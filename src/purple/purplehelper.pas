{ purplehelper.pas - useful helper for libpurple plugins.

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

{ libpurple helpers
  This unit contains some helpers for libpurple
  plugins, its usage is optional. It helps redirecting
  the stdout (writeln) to the purple debug window.
}
unit purplehelper;

{$mode objfpc}{$H+}

interface

uses
  purple,
  glib2,
  Classes,
  sysutils,
  StreamIO;

{ This will allocate memory from the FPC heap,
  copy the sring and return the pointer. The memory
  must be freed with FreeMem(), leaks can be traced
  with heaptrc. }
function GetMemAndCopy(Str: String): PChar; overload;

{ allocate a string that is supposed to be given
  to and owned by libpurple. This will allocate
  memory directly from the system and not from the
  FPC heap manager, heaptrc will not show leaks! }
function PurpleGetMemAndCopy(Str: String): PChar;

{ allocate memory that is supposed to be given to
  and owned by libpurple. This will allocate
  memory directly from the system and not from the
  FPC heap manager, heaptrc will not show leaks! }
function PurpleGetMem(Size: PtrUInt): PChar; overload;

{ free the memory that was allocated by PurpleGetMem()
  or by any other purple function and must be freed. }
procedure PurpleFreeMem(P: Pointer);


implementation

type
  TDebugLevel = (
    DEBUG_MISC,
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

var
  OldStdOut: Text;
  WritelnRedirect: TWritelnRedirect;
  PurpleThread: TThreadID;

function GetMemAndCopy(Str: String): PChar;
var
  L : Integer;
begin
  L := Length(Str);
  if L = 0 then
    Result := nil
  else begin
    Result := GetMem(L+1);
    Move(Str[1], Result[0], L);
    Result[L] := #0;
  end;
end;

function PurpleGetMemAndCopy(Str: String): PChar;
var
  L : Integer;
begin
  L := Length(Str);
  if L = 0 then
    Result := nil
  else begin
    // bypass the FPC heap manager and malloc() directly from the system.
    Result := g_malloc(L+1);
    Move(Str[1], Result[0], L);
    Result[L] := #0;
  end;
end;

function PurpleGetMem(Size: PtrUInt): PChar;
begin
  Result := g_malloc(Size);
end;

procedure PurpleFreeMem(P: Pointer);
begin
  g_free(P);
end;

procedure _purple_debug(Level: TDebugLevel; Msg: String);
begin
  case Level of
    DEBUG_MISC: purple_debug_misc(plugin_info.id, PChar(Msg + LineEnding), []);
    DEBUG_INFO: purple_debug_info(plugin_info.id, PChar(Msg + LineEnding), []);
    DEBUG_WARNING: purple_debug_warning(plugin_info.id, PChar(Msg + LineEnding), []);
    DEBUG_ERROR: purple_debug_error(plugin_info.id, PChar(Msg + LineEnding), []);
  end;
  {$ifdef DebugToConsole}
  try
    WriteLn(OldStdOut, '[', Level, '] ', Msg);
  except
    { There is no stdout on windows if pidgin is not run with --debug
      and if you run it with --debug then there is not much need for
      DebugToConsole anymore, it would be even more confusing instead. }
  end;
  {$endif}
end;

function CBDebugToPurple(Data: Pointer): gboolean; cdecl;
var
  Msg, Lvl, Txt: String;
begin
  Msg := AnsiString(PChar(Data));
  Msg := Trim(Msg);
  Lvl := LeftStr(Msg, 2);
  Txt := RightStr(Msg, Length(Msg) - 2);
  case Lvl of
    'E ': _purple_debug(DEBUG_ERROR, Txt);
    'W ': _purple_debug(DEBUG_WARNING, Txt);
    'I ': _purple_debug(DEBUG_INFO, Txt);
  else
    _purple_debug(DEBUG_MISC, Msg);
  end;
  PurpleFreeMem(Data);
  Result := False
end;

{ TWritelnRedirect }

function TWritelnRedirect.Write(const Buffer; Count: Longint): Longint;
var
  Msg : String;
begin
  Result := Count;
  SetLength(Msg, Count);
  Move(Buffer, Msg[1], Count);
  Msg := Trim(Msg);
  if ThreadID <> PurpleThread then begin
    Msg := Format('%s (msg from thread %x)', [Msg, ThreadID]);
    purple_timeout_add(0, @CBDebugToPurple, PurpleGetMemAndCopy(Msg));
  end
  else begin
    CBDebugToPurple(PurpleGetMemAndCopy(Msg));
  end;
end;


{ The TorChat units are logging their debug info with WriteLn(). It is
  the responsibility of the frontend to catch them and log them or display
  them appropriately. Here we install the redirection that will do this. }
procedure InstallWritelnRedirect;
begin
  PurpleThread := ThreadID;
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

initialization
  {$ifndef NoOutputRedirect}
    InstallWritelnRedirect;
  {$endif}
finalization
  {$ifndef NoOutputRedirect}
    UninstallWritelnRedirect;
  {$endif}
end.

