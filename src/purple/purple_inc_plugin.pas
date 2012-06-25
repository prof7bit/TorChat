{$ifdef purple_interface}
//type
  TPurplePluginType = (
    PURPLE_PLUGIN_UNKNOWN  := -1,  // Unknown type.
    PURPLE_PLUGIN_STANDARD := 0,   // Standard plugin.
    PURPLE_PLUGIN_LOADER,          // Loader plugin.
    PURPLE_PLUGIN_PROTOCOL         // Protocol plugin.
  );

  TPurplePluginPriority = cint;
  PPurplePluginUiInfo = Pointer;

  PPurplePluginInfo = ^TPurplePluginInfo;

  PPurplePlugin = ^TPurplePlugin;
  TPurplePlugin = record
    native_plugin     : GBoolean;           // Native C plugin.
    loaded            : GBoolean;           // The loaded state.
    handle            : Pointer;            // The module handle.
    path              : PChar;              // The path to the plugin.
    info              : PPurplePluginInfo;  // The plugin information.
    error             : PChar;
    ipc_data          : Pointer;            // IPC data.
    extra             : Pointer;            // Plugin-specific data.
    unloadable        : GBoolean;           // Unloadable
    dependent_plugins : PGList;             // Plugins depending on this

    _purple_reserved1 : Pointer;
    _purple_reserved2 : Pointer;
    _purple_reserved3 : Pointer;
    _purple_reserved4 : Pointer;
  end;

  PPurplePluginAction = ^TPurplePluginAction;
  PPurplePluginActionCb = procedure(act: PPurplePluginAction); cdecl;
  TPurplePluginAction = record
    label_    : PChar;
    callback  : PPurplePluginActionCb;
    plugin    : PPurplePlugin;
    context   : gpointer;
    user_data : gpointer;
  end;

  TPurplePluginInfo = record
    magic           : cint;
    major_version   : cint;
    minor_version   : cint;
    plugintype      : TPurplePluginType;
    ui_requirement  : PChar;
    flags           : culong;
    dependencies    : PGList;
    priority        : TPurplePluginPriority;
    id              : PChar;
    name            : PChar;
    version         : PChar;
    summary         : PChar;
    description     : PChar;
    author          : PChar;
    homepage        : PChar;
    load            : function(plugin: PPurplePlugin): GBoolean; cdecl;
    unload          : function(plugin: PPurplePlugin): GBoolean; cdecl;
    destroy         : procedure(plugin: PPurplePlugin); cdecl;
    ui_info         : Pointer;
    extra_info      : Pointer;
    prefs_info      : PPurplePluginUiInfo;
    actions         : function(plugin: PPurplePlugin; context: Pointer): PGList; cdecl;

    _purple_reserved1: procedure(); cdecl;
    _purple_reserved2: procedure(); cdecl;
    _purple_reserved3: procedure(); cdecl;
    _purple_reserved4: procedure(); cdecl;
  end;

{$endif}
{$ifdef purple_implementation}


{$endif}

