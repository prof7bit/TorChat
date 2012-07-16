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
  Classes;

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

{ Current UTC as unix time stamp }
function NowUTCUnix: UInt64;

{ Current UTC as TDateTime }
function NowUTC: TDateTime;

{ The notification hint window will stay completely empty
  if there are *any* occurrences of < or > in any of the
  fields, even if it is a valid tag, not even <br> works,
  so converting to proper HTML is not an option, the only
  simple way around it is to escape all < and > }
function EscapeAngleBrackets(AText: String): String;

procedure SetLogDir(APath: String);

implementation
uses
  {$ifdef windows}
  windows,
  shlobj,
  {$else}
  Unix,
  BaseUnix,
  {$endif}
  StreamIO,
  sysutils,
  syncobjs;

type
  TDebugLevel = (
    DEBUG_MISC,
    DEBUG_INFO,
    DEBUG_WARNING,
    DEBUG_ERROR
  );

  { TOutputRedirect will catch everything that is WriteLn()
    to stdout and redirects it to the libpurple debug logger.
    We will create an instance of this and replace it with
    the standard output stream }
  TOutputRedirect = class(TStream)
    function Write(const Buffer; Count : Longint) : Longint; override;
  end;

var
  OldStdOut: Text;
  DebugFile: Text;
  OutputRedirect: TOutputRedirect;
  OutputLock: TCriticalSection;
  PurpleThread: TThreadID;
  LogDir: String;

{$ifdef win32}
function AttachConsole(dwProcessId: Longint): LongBool; stdcall; external 'kernel32.dll';

procedure AttachWindowsConsole32;
begin
  AttachConsole(-1);
  try
    WriteLn(StdOut, '');
    Flush(StdOut);
  except
  end;
end;
{$endif}

function GetHomeDir: String;
{$ifdef windows}
var
  AppDataPath: Array[0..MaxPathLen] of Char;
begin
  SHGetSpecialFolderPath(0, AppDataPath, CSIDL_APPDATA, false);
  Result := AppDataPath;
end;
{$else}
begin
  Result := ExpandFileName('~');
end;
{$endif}

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
    Result := PurpleGetMem(L+1);
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

function NowUTCUnix: UInt64;
begin
  Result := Trunc((NowUTC - UnixEpoch) * SecsPerDay);
end;

function NowUTC: TDateTime;
{$ifdef windows}
var
  st: TSystemTime;
begin
  GetSystemTime (st);
  Result := SystemTimeToDateTime (st);
end;
{$else}
var
  timeval: TTimeVal;
  timezone: PTimeZone;
  a: Double;
begin
  TimeZone := nil;
  fpGetTimeOfDay (@TimeVal, TimeZone);
  // Convert to milliseconds
  a := (TimeVal.tv_sec * 1000.0) + (TimeVal.tv_usec / 1000.0);
  Result := (a / MSecsPerDay) + UnixDateDelta;
end;
{$endif}

function EscapeAngleBrackets(AText: String): String;
begin
  Result := StringReplace(AText, '<', '&lt;', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
end;

procedure SetLogDir(APath: String);
begin
  LogDir := APath;
  if LogDir <> '' then begin;
    Filemode := fmShareDenyNone;
    Assign(DebugFile, ConcatPaths([LogDir, 'plugin.log']));
    Rewrite(DebugFile);
  end;
end;

procedure _purple_debug(Level: TDebugLevel; Msg: String);
begin
  case Level of
    DEBUG_MISC: TPurpleDebug.Misc(plugin_info.id, PChar(Msg + LineEnding));
    DEBUG_INFO: TPurpleDebug.Info(plugin_info.id, PChar(Msg + LineEnding));
    DEBUG_WARNING: TPurpleDebug.Warning(plugin_info.id, PChar(Msg + LineEnding));
    DEBUG_ERROR: TPurpleDebug.Error(plugin_info.id, PChar(Msg + LineEnding));
  end;
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
    'M ': _purple_debug(DEBUG_MISC, Txt);
  else
    _purple_debug(DEBUG_MISC, Msg);
  end;
  PurpleFreeMem(Data);
  Result := False
end;

function FormatDebugForConsole(Msg: String): String;
var
  Lvl, Txt, M: String;
begin
  Lvl := UpperCase(LeftStr(Msg, 2));
  Txt := RightStr(Msg, Length(Msg) - 2);
  case Lvl of
    'E ': M := '[E] ' + Txt;
    'W ': M := '[W] ' + Txt;
    'I ': M := '[I] ' + Txt;
    'M ': M := '[M] ' + Txt;
  else
    M := '[M] ' + Msg;
  end;
  {$note dont localize the date}
  Result := FormatDateTime('mmm dd hh:nn:ss.zzz ', Now) + M;
end;

procedure DebugToConsole(Msg: String);
begin
  WriteLn(OldStdOut, FormatDebugForConsole(Msg));
  Flush(OldStdOut);
end;

procedure DebugToFile(Msg: String);
begin
  WriteLn(DebugFile, FormatDebugForConsole(Msg));
  Flush(DebugFile);
end;

{ TWritelnRedirect }

function TOutputRedirect.Write(const Buffer; Count: Longint): Longint;
var
  Msg : String;
begin
  OutputLock.Acquire;
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
  {$ifdef DebugToConsole}
  try
    DebugToConsole(Msg);
  except
  end;
  {$endif}
  if LogDir <> '' then begin;
    try
      DebugToFile(Msg);
    except
    end;
  end;
  OutputLock.Release;
end;


{ The TorChat units are logging their debug info with WriteLn().
  It is the responsibility of the frontend to catch them and log
  them or display them appropriately. Here we install the
  redirection that will do this. }
procedure InstallOutputRedirect;
begin
  PurpleThread := ThreadID;
  OutputLock := TCriticalSection.Create;
  OldStdOut := Output;
  OutputRedirect := TOutputRedirect.Create();
  AssignStream(Output, OutputRedirect);
  Rewrite(Output);

  {$ifdef DebugToConsole}
    {$ifdef win32}
      AttachWindowsConsole32;
    {$endif}
    WriteLn('W plugin has been compiled with -dDebugToConsole. Not recommended.');
  {$endif}
end;

procedure UninstallOutputRedirect;
begin
  Flush(Output);
  OutputLock.Acquire;
  Output := OldStdOut;
  OutputRedirect.Free;
  OutputLock.Release;
  OutputLock.Free;
  if LogDir <> '' then
    CloseFile(DebugFile);
end;

initialization
  InstallOutputRedirect;
finalization
  UninstallOutputRedirect;
end.

