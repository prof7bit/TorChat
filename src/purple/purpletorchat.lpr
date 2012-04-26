library purpletorchat;

{$mode objfpc}{$H+}

uses
  purple, torchatclient;

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

  PCallBack = function(UserData: TUserData): GBoolean; cdecl;

procedure PupleCall(Func: PCallBack; UserData: TUserData); inline;
begin
  purple_timeout_add(0, PGSourceFunc(@Func), UserData);
end;

function OnLoad(var Plugin: TPurplePlugin): GBoolean; cdecl;
begin
  _info('loaded');
  Result := True;
end;

function OnUnload(var Plugin: TPurplePlugin): GBoolean; cdecl;
begin
  _info('unloaded');
  Result := True;
end;

exports
  purple_init_plugin;

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


