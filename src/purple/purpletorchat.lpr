library purpletorchat;

{$mode objfpc}{$H+}

uses
  {$ifdef unix}cthreads,{$endif}
  purple, torchatclient, clientconfig;

type

  { TTorchatPurpleClient }

  TTorChatPurpleClient = class(TTorChatClient)
    procedure CBWakeGui; override;
  end;

var
  Client: TTorChatPurpleClient;
  HPurpleTimer: Integer;

function OnPurpleTimer(Data: Pointer): GBoolean; cdecl;
begin
  Client.GuiIdle;
  Result := True;
end;

function OnPurpleTimerOneShot(Data: Pointer): GBoolean; cdecl;
begin
  Client.GuiIdle;
  Result := False; // purple timer will not fire again
end;

function OnLoad(var Plugin: TPurplePlugin): GBoolean; cdecl;
begin
  Client := TTorChatPurpleClient.Create(nil);
  HPurpleTimer := purple_timeout_add(1000, @OnPurpleTimer, nil);
  _info(ConfGetHiddenServiceName);
  _info('loaded');
  Result := True;
end;

function OnUnload(var Plugin: TPurplePlugin): GBoolean; cdecl;
begin
  purple_timeout_remove(HPurpleTimer);
  Client.Free;
  Client := nil;
  _info('unloaded');
  Result := True;
end;

exports
  purple_init_plugin;

{ TTorchatPurpleClient }

procedure TTorChatPurpleClient.CBWakeGui;
begin
  purple_timeout_add(0, @OnPurpleTimerOneShot, nil);
end;

begin
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


