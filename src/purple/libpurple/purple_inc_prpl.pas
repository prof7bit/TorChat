{$ifdef _const}
  OPT_PROTO_UNIQUE_CHATNAME       = $00000004;
  OPT_PROTO_CHAT_TOPIC            = $00000008;
  OPT_PROTO_NO_PASSWORD           = $00000010;
  OPT_PROTO_MAIL_CHECK            = $00000020;
  OPT_PROTO_IM_IMAGE              = $00000040;
  OPT_PROTO_PASSWORD_OPTIONAL     = $00000080;
  OPT_PROTO_USE_POINTSIZE         = $00000100;
  OPT_PROTO_REGISTER_NOSCREENNAME = $00000200;
  OPT_PROTO_SLASH_COMMANDS_NATIVE = $00000400;
  OPT_PROTO_INVITE_MESSAGE        = $00000800;
{$endif}

{$ifdef _type}
  TPurpleProtocolOptions = DWord; // bitfield of OPT_PROTO_ constants

  TPurpleIconScaleRules = (
    PURPLE_ICON_SCALE_DISPLAY = $01,    // We scale the icon when we display it
    PURPLE_ICON_SCALE_SEND    = $02     // We scale the icon before we send it to the server
  );

  TPurpleBuddyIconSpec = record
    format        : PChar;                  // Comma delimited list of formats
    min_width     : cint;                   // Minimum width of this icon
    min_height    : cint;                   // Minimum height of this icon
    max_width     : cint;                   // Maximum width of this icon
    max_height    : cint;                   // Maximum height of this icon
    max_filesize  : csize_t;                // Maximum size in bytes
    scale_rules   : TPurpleIconScaleRules;  // How to stretch this icon
  end;

  TPurplePluginProtocolInfo = record
    options                 : TPurpleProtocolOptions;  (**< Protocol options.           *)
    user_splits             : PGList;  (**< A GList of PurpleAccountUserSplit  *)
    protocol_options        : PGList;  (**< A GList of PurpleAccountOption     *)
    icon_spec               : TPurpleBuddyIconSpec;  (**< The icon spec.  *)
    list_icon               : function(account: PPurpleAccount; buddy: PPurpleBuddy): PChar; cdecl;
    list_emblem             : function(buddy: PPurpleBuddy): PChar; cdecl;
    status_text             : function(buddy: PPurpleBuddy): PChar; cdecl;
    tooltip_text            : procedure(buddy: PPurpleBuddy; user_info: PPurpleNotifyUserInfo; full: gboolean); cdecl;
    status_types            : function(account: PPurpleAccount): PGList; cdecl;
    blist_node_menu         : function(node: PPurpleBlistNode): PGList; cdecl;
    chat_info               : function(gc: PPurpleConnection): PGList; cdecl;
    chat_info_defaults      : function(gc: PPurpleConnection; chat_name: PChar): PGHashTable; cdecl;
    login                   : procedure(account: PPurpleAccount); cdecl;
    close                   : procedure(gc: PPurpleConnection); cdecl;
    send_im                 : function(gc: PPurpleConnection; who, message: PChar; flags: TPurpleMessageFlags): cint; cdecl;
    set_info                : procedure(gc: PPurpleConnection; info: PChar); cdecl;
    send_typing             : function(gc: PPurpleConnection; name_: PChar; state: TPurpleTypingState): cint; cdecl;
    get_info                : procedure(gc: PPurpleConnection; who: PChar); cdecl;
    set_status              : procedure(account: PPurpleAccount; status: PPurpleStatus); cdecl;
    set_idle                : procedure(gc: PPurpleConnection; idletime: cint); cdecl;
    change_passwd           : procedure(gc: PPurpleConnection; old_pass, new_pass: PChar); cdecl;
    add_buddy               : procedure(gc: PPurpleConnection; buddy: PPurpleBuddy; group: PPurpleGroup); cdecl;
    add_buddies             : procedure(gc: PPurpleConnection; buddies, groups: PGList); cdecl;
    remove_buddy            : procedure(gc: PPurpleConnection; buddy: PPurpleBuddy; group: PPurpleGroup); cdecl;
    remove_buddies          : procedure(gc: PPurpleConnection; buddies, groups: PGList); cdecl;
    add_permit              : procedure(gc: PPurpleConnection; name_: PChar); cdecl;
    add_deny                : procedure(gc: PPurpleConnection; name_: PChar); cdecl;
    rem_permit              : procedure(gc: PPurpleConnection; name_: PChar); cdecl;
    rem_deny                : procedure(gc: PPurpleConnection; name_: PChar); cdecl;
    set_permit_deny         : procedure(gc: PPurpleConnection); cdecl;
    join_chat               : procedure(gc: PPurpleConnection; components: PGHashTable); cdecl;
    reject_chat             : procedure(gc: PPurpleConnection; components: PGHashTable); cdecl;
    get_chat_name           : function(components: PGHashTable): PChar; cdecl;
    chat_invite             : procedure(gc: PPurpleConnection; id: cint; message, who: PChar); cdecl;
    chat_leave              : procedure(gc: PPurpleConnection; id: cint); cdecl;
    chat_whisper            : procedure(gc: PPurpleConnection; id: cint; who, message: PChar); cdecl;
    chat_send               : function(gc: PPurpleConnection; id: cint; message: PChar; flags: TPurpleMessageFlags): cint; cdecl;
    keepalive               : procedure(gc: PPurpleConnection); cdecl;
    register_user           : procedure(account: PPurpleAccount); cdecl;
    get_cb_info             : procedure(gc: PPurpleConnection; par1: cint; who: PChar); cdecl;
    get_cb_away             : procedure(gc: PPurpleConnection; par1: cint; who: PChar); cdecl;
    alias_buddy             : procedure(gc: PPurpleConnection; who, alias_: PChar); cdecl;
    group_buddy             : procedure(gc: PPurpleConnection; who, old_group, new_group: PChar); cdecl;
    rename_group            : procedure(gc: PPurpleConnection; old_name: PChar; group: PPurpleGroup; moved_buddies: PGList); cdecl;
    buddy_free              : procedure(buddy: PPurpleBuddy); cdecl;
    convo_closed            : procedure(gc: PPurpleConnection; who: PChar); cdecl;
    normalize               : function(account: PPurpleAccount; who: PChar): PChar; cdecl;
    set_buddy_icon          : procedure(gc: PPurpleConnection; img: PPurpleStoredImage); cdecl;
    remove_group            : procedure(gc: PPurpleConnection; group: PPurpleGroup); cdecl;
    get_cb_real_name        : function(gc: PPurpleConnection; id: cint; who: PChar): PChar; cdecl;
    set_chat_topic          : procedure(gc: PPurpleConnection; id: cint; topic: PChar); cdecl;
    find_blist_chat         : function(account: PPurpleAccount; name_: PChar): PPurpleChat; cdecl;
    roomlist_get_list       : function(gc: PPurpleConnection): PPurpleRoomlist; cdecl;
    roomlist_cancel         : procedure(list: PPurpleRoomlist); cdecl;
    roomlist_expand_category: procedure(list: PPurpleRoomlist; category: PPurpleRoomlistRoom); cdecl;
    can_receive_file        : function(gc: PPurpleConnection; who: PChar): gboolean; cdecl;
    send_file               : procedure(gc: PPurpleConnection; who, filename: PChar); cdecl;
    new_xfer                : function(gc: PPurpleConnection; who: PChar): PPurpleXfer; cdecl;
    offline_message         : function(buddy: PPurpleBuddy): gboolean; cdecl;
    whiteboard_prpl_ops     : PPurpleWhiteboardPrplOps;
    send_raw                : function(gc: PPurpleConnection; buf: PChar; len: cint): cint; cdecl;
    roomlist_room_serialize : function(room: PPurpleRoomlistRoom): PChar; cdecl;
    unregister_user         : procedure(account: PPurpleAccount; cb: PPurpleAccountUnregistrationCallback; user_data: Pointer); cdecl;
    send_attention          : function(gc: PPurpleConnection; username: PChar; type_: guint): gboolean; cdecl;
    get_attention_types     : function(acct: PPurpleAccount): PGList; cdecl;
    struct_size             : cint;
    get_account_text_table  : function(account: PPurpleAccount): PGHashTable; cdecl;
    initiate_media          : function(account: PPurpleAccount; who: PChar; type_: TPurpleMediaSessionType): gboolean; cdecl;
    get_media_caps          : function(account: PPurpleAccount; who: PChar): TPurpleMediaCaps; cdecl;
    get_moods               : function(account: PPurpleAccount): TPurpleMood; cdecl;
    set_public_alias        : procedure(gc: PPurpleConnection; alias_: PChar; success_cb: PPurpleSetPublicAliasSuccessCallback; failure_cb: PPurpleSetPublicAliasFailureCallback); cdecl;
    get_public_alias        : procedure(gc: PPurpleConnection; success_cb: PPurpleGetPublicAliasSuccessCallback; failure_cb: PPurpleGetPublicAliasFailureCallback); cdecl;
    add_buddy_with_invite   : procedure(pc: PPurpleConnection; buddy: PPurpleBuddy; group: PPurpleGroup; message: PChar); cdecl;
    add_buddies_with_invite : procedure(pc: PPurpleConnection; buddies, groups: PGList; message: PChar); cdecl;
  end;
{$endif}



{$ifdef _func}
procedure purple_prpl_got_user_status(account: PPurpleAccount;
  name, status_id: PChar; attrs: array of const); cdecl; external LIBPURPLE;
{$endif}

// purple_prpl_got_user_status() is used as a method of TPurpleAccount

