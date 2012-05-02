library purpletorchat;

{$mode objfpc}{$H+}

uses
  {$ifdef unix}cthreads,{$endif}
  Classes, sysutils,
  purple, torchatclient, clientconfig, miscfunc;

type
  { TTorchatPurpleClient }
  TTorChatPurpleClient = class(TTorChatClient)
    procedure OnNotifyGui; override;
  end;

var
  Client: TTorChatPurpleClient;
  HPurpleTimer: Integer;

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

function OnListIcon(account: PPurpleAccount; buddy: PPurpleBuddy): PChar; cdecl;
begin
  _info('OnListIcon');
  Result := nil;
end;

procedure OnLogin(acc: PPurpleAccount); cdecl;
begin
  _info('OnLogin');
end;

procedure OnClose(conn: PPurpleConnection); cdecl;
begin
  _info('OnClose');
end;


{ TTorchatPurpleClient }

procedure TTorChatPurpleClient.OnNotifyGui;
begin
  purple_timeout_add(0, @OnPurpleTimerOneShot, nil);
end;

exports
  purple_init_plugin;

begin
  with PluginInfo do begin
    magic := PURPLE_PLUGIN_MAGIC;
    major_version := PURPLE_MAJOR_VERSION;
    minor_version := PURPLE_MINOR_VERSION;
    plugintype := PURPLE_PLUGIN_PROTOCOL;
    priority := PURPLE_PRIORITY_DEFAULT;
    id := 'prpl-prof7bit-torchat';
    name := 'TorChat';
    version := '2.0';
    summary := 'TorChat Protocol';
    description := 'TorChat protocol plugin for libpurple / Pidgin';
    author := 'Bernd Kreuss <prof7bit@gmail.com>';
    homepage := 'https://github.com/prof7bit/TorChat';
    load := @OnLoad;
    unload := @OnUnload;
    extra_info := @PluginProtocolInfo;
  end;

  with PluginProtocolInfo do begin
    options := OPT_PROTO_NO_PASSWORD;
    list_icon := @OnListIcon;
    login := @OnLogin;
    close := @OnClose;
  end;
end.


