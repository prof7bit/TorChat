library purpletorchat;

{$mode objfpc}{$H+}

uses
  {$ifdef unix}cthreads,{$endif}
  Classes, sysutils, StreamIO,
  purple, torchatclient, clientconfig, miscfunc;

type
  { TTorchatPurpleClient }
  TTorChatPurpleClient = class(TTorChatClient)
    procedure OnNotifyGui; override;
  end;

  { TWritelnRedirect will catch everything that is WriteLn() to stdout and
    redirects it to the libpurple debug logger. We will create an instance
    of this and replace it with the standard output stream }
  TWritelnRedirect = class(TStream)
    function Write(const Buffer; Count : Longint) : Longint; override;
  end;

var
  Client: TTorChatPurpleClient;
  HPurpleTimer: Integer;

  // these two variables are needed for the stdout redirection.
  OldStdOut: Text;
  WritelnRedirect: TWritelnRedirect;

function OnPurpleTimer(Data: Pointer): GBoolean; cdecl;
begin
  Ignore(Data);
  Client.ProcessMessages;
  Result := True;
end;

function OnPurpleTimerOneShot(Data: Pointer): GBoolean; cdecl;
begin
  Ignore(Data);
  Client.ProcessMessages;
  Result := False; // purple timer will not fire again
end;

function OnLoad(var Plugin: TPurplePlugin): GBoolean; cdecl;
begin
  Ignore(@Plugin);
  Client := TTorChatPurpleClient.Create(nil);
  HPurpleTimer := purple_timeout_add(1000, @OnPurpleTimer, nil);
  _info(ConfGetHiddenServiceName);
  _info('loaded');
  Result := True;
end;

function OnUnload(var Plugin: TPurplePlugin): GBoolean; cdecl;
begin
  Ignore(@Plugin);
  purple_timeout_remove(HPurpleTimer);
  Client.Free;
  Client := nil;
  _info('unloaded');
  Result := True;
end;

{ TTorchatPurpleClient }

procedure TTorChatPurpleClient.OnNotifyGui;
begin
  purple_timeout_add(0, @OnPurpleTimerOneShot, nil);
end;

{ TDebugStream }

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
procedure RedirectWriteln;
begin
  OldStdOut := Output;
  WritelnRedirect := TWritelnRedirect.Create();
  AssignStream(Output, WritelnRedirect);
  Rewrite(Output);
end;

exports
  purple_init_plugin;

begin
  RedirectWriteln;
  with PluginInfo do begin
    magic := PURPLE_PLUGIN_MAGIC;
    major_version := PURPLE_MAJOR_VERSION;
    minor_version := PURPLE_MINOR_VERSION;
    plugintype := PURPLE_PLUGIN_STANDARD;
    priority := PURPLE_PRIORITY_DEFAULT;
    id := 'prpl-prof7bit-torchat';
    name := 'TorChat';
    version := '2.0';
    summary := 'TorChat Protocol';
    description := 'TorChat protocol plugin for libpurple / Pidgin';
    author := 'Bernd Kreuss';
    homepage := 'https://github.com/prof7bit/TorChat';
    load := @OnLoad;
    unload := @OnUnload;
  end;
end.


