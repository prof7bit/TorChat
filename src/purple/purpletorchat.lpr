library purpletorchat;

{$mode objfpc}{$H+}

uses
  {$ifdef unix}cthreads,{$endif}
  Classes, sysutils, contnrs,
  purple, torchatclient, miscfunc;

type
  { TTorchatPurpleClient }
  TTorChatPurpleClient = class(TTorChatClient)
  public
    PurpleAccount: PPurpleAccount;
    PurpleTimer: Integer;
    procedure OnNotifyGui; override;
  end;

var
  Clients: TFPHashObjectList;

function OnPurpleTimer(Data: Pointer): GBoolean; cdecl;
begin
  Ignore(Data);
  TTorChatPurpleClient(Data).ProcessMessages;
  Result := True;
end;

function OnPurpleTimerOneShot(Data: Pointer): GBoolean; cdecl;
begin
  Ignore(Data);
  TTorChatPurpleClient(Data).ProcessMessages;
  Result := False; // purple timer will not fire again
end;

function OnLoad(var Plugin: TPurplePlugin): GBoolean; cdecl;
begin
  Ignore(@Plugin);
  Clients := TFPHashObjectList.Create(False);
  _info('plugin loaded');
  Result := True;
end;

function OnUnload(var Plugin: TPurplePlugin): GBoolean; cdecl;
begin
  Ignore(@Plugin);
  Clients.Free;
  _info('plugin unloaded');
  Result := True;
end;

function OnStatusTypes(account: PPurpleAccount): PGList; cdecl;
begin
  _info('OnStatusTypes()');
  Ignore(account);
  // we neeed to define offline and online statuses
  // or else it will not call our login callback
  Result := nil;
  Result := GListAppend(Result, purple_status_type_new_full(PURPLE_STATUS_AVAILABLE, nil, nil, True, True, False));
  Result := GListAppend(Result, purple_status_type_new_full(PURPLE_STATUS_AWAY, nil, nil, True, True, False));
  Result := GListAppend(Result, purple_status_type_new_full(PURPLE_STATUS_EXTENDED_AWAY, nil, nil, True, True, False));
  Result := GListAppend(Result, purple_status_type_new_full(PURPLE_STATUS_OFFLINE, nil, nil, True, True, False));
end;

procedure OnSetStatus(account: PPurpleAccount; status: PPurpleStatus); cdecl;
begin
  _info('OnSetStatus');
end;

function OnListIcon(account: PPurpleAccount; buddy: PPurpleBuddy): PChar; cdecl;
begin
  Ignore(account);
  Ignore(buddy);
  Result := 'torchat';
  // now it will look for torchat.png in several resolutions
  // in the folders /usr/share/pixmaps/pidgin/protocols/*/
  // the installer for the plugin must install these files.
end;

procedure OnLogin(acc: PPurpleAccount); cdecl;
var
  NewClient: TTorChatPurpleClient;
begin
  _info('OnLogin');
  NewClient := TTorChatPurpleClient.Create(nil);
  NewClient.PurpleAccount := acc;
  NewClient.PurpleTimer := purple_timeout_add(1000, @OnPurpleTimer, NewClient);
  Clients.Add(acc^.username, NewClient);
  _info('created torchat client object for ' + acc^.username);
end;

procedure OnClose(gc: PPurpleConnection); cdecl;
var
  ClientToClose: TTorChatPurpleClient;
begin
  _info('OnClose');
  ClientToClose := Clients.Find(gc^.account^.username) as TTorChatPurpleClient;
  if Assigned(ClientToClose) then begin
    purple_timeout_remove(ClientToClose.PurpleTimer);
    Clients.Remove(ClientToClose);
    ClientToClose.Free;
    _info('destroyed torchat client object for ' + gc^.account^.username);
  end;
end;


{ TTorchatPurpleClient }

procedure TTorChatPurpleClient.OnNotifyGui;
begin
  purple_timeout_add(0, @OnPurpleTimerOneShot, self);
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
    options := OPT_PROTO_NO_PASSWORD or OPT_PROTO_REGISTER_NOSCREENNAME;
    list_icon := @OnListIcon;
    status_types := @OnStatusTypes;
    login := @OnLogin;
    close := @OnClose;
    set_status := @OnSetStatus;
    struct_size := SizeOf(PluginProtocolInfo);
  end;
end.


