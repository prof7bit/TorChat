library purpletorchat;

{$mode objfpc}{$H+}

uses
  {$ifdef UseHeapTrc} // do it with -dUseHeapTrc, not with -gh
    heaptrc,
    {$ifdef windows}  // need config path for heaptrc output
      clientconfig,
    {$endif}
  {$endif}
  {$ifdef unix}
    cthreads,
  {$endif}
  Classes, sysutils, contnrs, glib2,
  purple, torchatabstract, torchatclient, miscfunc;

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
  Ignore(account);
  // pidgin (or libpurple or both) has a bug, it will offer "invisible" even
  // if we don't register it (and it will not offer "extended away" even if
  // we register it), so we are registerig all of them and will deal with them
  // in the set_status callback.
  Result := nil;
  Result := g_list_append(Result, purple_status_type_new_full(PURPLE_STATUS_AVAILABLE, nil, nil, True, True, False));
  Result := g_list_append(Result, purple_status_type_new_full(PURPLE_STATUS_UNAVAILABLE, nil, nil, True, True, False));
  Result := g_list_append(Result, purple_status_type_new_full(PURPLE_STATUS_AWAY, nil, nil, True, True, False));
  Result := g_list_append(Result, purple_status_type_new_full(PURPLE_STATUS_EXTENDED_AWAY, nil, nil, True, True, False));
  Result := g_list_append(Result, purple_status_type_new_full(PURPLE_STATUS_INVISIBLE, nil, nil, True, True, False));
  Result := g_list_append(Result, purple_status_type_new_full(PURPLE_STATUS_OFFLINE, nil, nil, True, True, False));
end;

procedure OnSetStatus(account: PPurpleAccount; status: PPurpleStatus); cdecl;
var
  SP : TPurpleStatusPrimitive;
  ST : TTorchatStatus;
  C : TTorChatPurpleClient;
begin
  SP := purple_status_type_get_primitive(purple_status_get_type(status));
  case SP of
    PURPLE_STATUS_AVAILABLE: ST := TORCHAT_AVAILABLE;
    PURPLE_STATUS_UNAVAILABLE: ST := TORCHAT_EXTENDED_AWAY;
    PURPLE_STATUS_AWAY: ST := TORCHAT_AWAY;
    PURPLE_STATUS_EXTENDED_AWAY: ST := TORCHAT_EXTENDED_AWAY;
    PURPLE_STATUS_INVISIBLE: ST := TORCHAT_OFFLINE;
  end;
  C := Clients.Find(account^.username) as TTorChatPurpleClient;
  C.SetStatus(ST);
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
  SP: PPurpleStatus;
begin
  _info('OnLogin');
  NewClient := TTorChatPurpleClient.Create(nil);
  NewClient.PurpleAccount := acc;
  NewClient.PurpleTimer := purple_timeout_add(1000, @OnPurpleTimer, NewClient);
  Clients.Add(acc^.username, NewClient);
  purple_connection_set_state(acc^.gc, PURPLE_CONNECTED);
  _info('created torchat client object for ' + acc^.username);

  // it won't call set_status after login, so we have to do it ourselves
  SP := purple_presence_get_active_status(acc^.presence);
  OnSetStatus(acc, SP);
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

  {$ifdef UseHeapTrc}
    {$warning compiling with -dUseHeapTrc. Not recommended for release.}
    {$ifdef windows}
      // we have no stdout when running on windows
      heaptrc.SetHeapTraceOutput(ConcatPaths([ConfGetDataDir, 'heaptrc.log']));
    {$endif}
  {$endif}
end.


