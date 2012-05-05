{ purple.pas

  A minimalistic header translation for making libpurple plugins.

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02111-1301  USA
}

unit purple;
{$mode objfpc}{$H+}

interface
uses
  Classes, glib2;

type
  {$ifdef cpu64}
    time_t = UInt64;
  {$else}
    time_t = UInt32;
  {$endif}

(****************************************
 *                                      *
 *   Const and Type declarations        *
 *                                      *
 *   from various libpurple/*.h         *
 *   (for function imports scroll       *
 *   down to the next section).         *
 *                                      *
 *   Most of the original comments      *
 *   from the C headers and also most   *
 *   of the function declarations in    *
 *   their original C syntax have       *
 *   been copied into the comments,     *
 *   so this unit looks much bigger     *
 *   than it really is.                 *
 *                                      *
 ****************************************)
{$calling cdecl}
const
  PURPLE_PLUGIN_MAGIC = 5;
  PURPLE_MAJOR_VERSION = 2;
  PURPLE_MINOR_VERSION = 10;
  PURPLE_PRIORITY_DEFAULT = 0;

type
  TPurplePluginType = (
    PURPLE_PLUGIN_UNKNOWN  := -1,  // Unknown type.
    PURPLE_PLUGIN_STANDARD := 0,   // Standard plugin.
    PURPLE_PLUGIN_LOADER,          // Loader plugin.
    PURPLE_PLUGIN_PROTOCOL         // Protocol plugin.
  );

  TPurplePluginPriority = Integer;
  PPurplePluginUiInfo = Pointer;

  PPurplePluginInfo = ^TPurplePluginInfo;

  PPurplePlugin = ^TPurplePlugin;
  TPurplePlugin = packed record
    native_plugin: GBoolean;       // Native C plugin.
    loaded: GBoolean;              // The loaded state.
    handle: Pointer;               // The module handle.
    path: PChar;                   // The path to the plugin.
    info: PPurplePluginInfo;       // The plugin information.
    error: PChar;

    ipc_data: Pointer;             // IPC data.
    extra: Pointer;                // Plugin-specific data.
    unloadable: GBoolean;          // Unloadable
    dependent_plugins: PGList;     // Plugins depending on this

    _purple_reserved1: Pointer;
    _purple_reserved2: Pointer;
    _purple_reserved3: Pointer;
    _purple_reserved4: Pointer;
  end;

  TPurplePluginInfo = packed record
    magic: Integer;
    major_version: Integer;
    minor_version: Integer;
    plugintype: TPurplePluginType;
    ui_requirement: PChar;
    flags: LongInt;
    dependencies: PGList;
    priority: TPurplePluginPriority;

    id: PChar;
    name: PChar;
    version: PChar;
    summary: PChar;
    description: PChar;
    author: PChar;
    homepage: PChar;

    { If a plugin defines a 'load' function, and it returns FALSE,
      then the plugin will not be loaded.}
    load: function(var Plugin: TPurplePlugin): GBoolean;
    unload: function(var Plugin: TPurplePlugin): GBoolean;
    destroy: procedure(var Plugin: TPurplePlugin);

    ui_info: Pointer;
    extra_info: Pointer;

    { Used by any plugin to display preferences.
      If #ui_info has been specified, this will be ignored.}
    prefs_info: PPurplePluginUiInfo;

    { This callback has a different use depending on whether this
      plugin type is PURPLE_PLUGIN_STANDARD or PURPLE_PLUGIN_PROTOCOL.

      If PURPLE_PLUGIN_STANDARD then the list of actions will show up
      in the Tools menu, under a submenu with the name of the plugin.
      context will be NULL.

      If PURPLE_PLUGIN_PROTOCOL then the list of actions will show up
      in the Accounts menu, under a submenu with the name of the
      account.  context will be set to the PurpleConnection for that
      account.  This callback will only be called for online accounts.}
    actions: function(var Plugin: TPurplePlugin; Context: Pointer): PGList;

    _purple_reserved1: Pointer;
    _purple_reserved2: Pointer;
    _purple_reserved3: Pointer;
    _purple_reserved4: Pointer;
  end;


  (**
   * Protocol options
   *
   * These should all be stuff that some plugins can do and others can't.
   * Allowed values are the constants that start with OPT_PROTO_
   *)
  TPurpleProtocolOptions = DWord;

const
  (**
   * User names are unique to a chat and are not shared between rooms.
   *
   * XMPP lets you choose what name you want in chats, so it shouldn't
   * be pulling the aliases from the buddy list for the chat list;
   * it gets annoying.
   *)
  OPT_PROTO_UNIQUE_CHATNAME = $00000004;

  (**
   * Chat rooms have topics.
   *
   * IRC and XMPP support this.
   *)
  OPT_PROTO_CHAT_TOPIC = $00000008;

  (**
   * Don't require passwords for sign-in.
   *
   * Zephyr doesn't require passwords, so there's no
   * need for a password prompt.
   *)
  OPT_PROTO_NO_PASSWORD = $00000010;

  (**
   * Notify on new mail.
   *
   * MSN and Yahoo notify you when you have new mail.
   *)
  OPT_PROTO_MAIL_CHECK = $00000020;

  (**
   * Images in IMs.
   *
   * Oscar lets you send images in direct IMs.
   *)
  OPT_PROTO_IM_IMAGE = $00000040;

  (**
   * Allow passwords to be optional.
   *
   * Passwords in IRC are optional, and are needed for certain
   * functionality.
   *)
  OPT_PROTO_PASSWORD_OPTIONAL = $00000080;

  (**
   * Allows font size to be specified in sane point size
   *
   * Probably just XMPP and Y!M
   *)
  OPT_PROTO_USE_POINTSIZE = $00000100;

  (**
   * Set the Register button active even when the username has not
   * been specified.
   *
   * Gadu-Gadu doesn't need a username to register new account (because
   * usernames are assigned by the server).
   *)
  OPT_PROTO_REGISTER_NOSCREENNAME = $00000200;

  (**
   * Indicates that slash commands are native to this protocol.
   * Used as a hint that unknown commands should not be sent as messages.
   * @since 2.1.0
   *)
  OPT_PROTO_SLASH_COMMANDS_NATIVE = $00000400;

  (**
   * Indicates that this protocol supports sending a user-supplied message
   * along with an invitation.
   * @since 2.8.0
   *)
  OPT_PROTO_INVITE_MESSAGE = $00000800;

type
  TPurpleIconScaleRules = (
    PURPLE_ICON_SCALE_DISPLAY = $01,    // We scale the icon when we display it
    PURPLE_ICON_SCALE_SEND = $02        // We scale the icon before we send it to the server
  );

  (**
   * A description of a Buddy Icon specification.  This tells Purple what kind of image file
   * it should give this prpl, and what kind of image file it should expect back.
   * Dimensions less than 1 should be ignored and the image not scaled.
   *)
  TPurpleBuddyIconSpec = packed record
    (** This is a comma-delimited list of image formats or @c NULL if icons
     *  are not supported.  Neither the core nor the prpl will actually
     *  check to see if the data it's given matches this; it's entirely up
     *  to the UI to do what it wants
     *)
    format: PChar;
    min_width: Integer;                     // Minimum width of this icon
    min_height: Integer;                    // Minimum height of this icon
    max_width: Integer;                     // Maximum width of this icon
    max_height: Integer;                    // Maximum height of this icon
    max_filesize: PtrUInt;                  // Maximum size in bytes
    scale_rules: TPurpleIconScaleRules;     // How to stretch this icon
  end;

  PPurpleProxyInfo = Pointer;
  PPurplePrivacyType = Pointer;
  PPurplePresence = Pointer;
  PPurpleLog = Pointer;
  PPurpleAccountRegistrationCb = procedure(); {$note fixme: define this}

  PPurpleConnection = ^TPurpleConnection;

  (** Structure representing an account.
   *)
  PPurpleAccount = ^TPurpleAccount;
  TPurpleAccount = packed record
    username: PChar;             // The username.
    aalias: PChar;               // How you appear to yourself.
    password: PChar;             // The account password.
    user_info: PChar;            // User information.
    buddy_icon_path: PChar;      // The buddy icon's non-cached path.
    remember_pass: GBoolean;     // Remember the password.
    protocol_id: PChar;          // The ID of the protocol.
    gc: PPurpleConnection;       // The connection handle.
    disconnecting: GBoolean;     // The account is currently disconnecting
    settings: PGHashTable;       // Protocol-specific settings.
    ui_settings: PGHashTable;    // UI-specific settings.
    proxy_info: PPurpleProxyInfo;// Proxy information.  This will be set
                                 //   to NULL when the account inherits
                                 //   proxy settings from global prefs.

    (*
     * TODO: Supplementing the next two linked lists with hash tables
     * should help performance a lot when these lists are long.  This
     * matters quite a bit for protocols like MSN, where all your
     * buddies are added to your permit list.  Currently we have to
     * iterate through the entire list if we want to check if someone
     * is permitted or denied.  We should do this for 3.0.0.
     * Or maybe use a GTree.
     *)
    permit: PGSList;                // Permit list.
    deny: PGSList;                  // Deny list.
    perm_deny: PPurplePrivacyType;  // The permit/deny setting.
    status_types: PGList;           // Status types.
    presence: PPurplePresence;      // Presence.                              */
    sytem_log: PPurpleLog;          // The system log                         */
    ui_data: Pointer;               // The UI can put data here.              */
    registration_cb: PPurpleAccountRegistrationCb;
    registration_cb_user_data: Pointer;
    priv: Pointer;                  // Pointer to opaque private data.
  end;

  TPurpleConnectionFlags = Integer;

  TPurpleConnectionState = (
  	PURPLE_DISCONNECTED = 0, // Disconnected.
  	PURPLE_CONNECTED,        // Connected.
  	PURPLE_CONNECTING        // Connecting.
  );

  (* Represents an active connection on an account. *)
  TPurpleConnection = packed record
    prpl: PPurplePlugin;            // The protocol plugin.
    flags: TPurpleConnectionFlags;  // Connection flags.
    state: TPurpleConnectionState;  // The connection state.
    account: PPurpleAccount;        // The account being connected to.
    password: PChar;                // The password used.
    inpa: Integer;                  // The input watcher.
    buddy_chats: PGSList;           // A list of active chats
                                    //  (#PurpleConversation structs of type
                                    //  #PURPLE_CONV_TYPE_CHAT).
    proto_data: Pointer;            // Protocol-specific data.
    display_name: PChar;            // How you appear to other people.
    keepalive: Integer;             // Keep-alive.

    (** Wants to Die state.  This is set when the user chooses to log out, or
     * when the protocol is disconnected and should not be automatically
     * reconnected (incorrect password, etc.).  prpls should rely on
     * purple_connection_error_reason() to set this for them rather than
     * setting it themselves.
     * @see purple_connection_error_is_fatal
     *)
    wants_to_die: GBoolean;
    disconnect_timeout: Integer;    // Timer used for nasty stack tricks
    last_received: time_t;          // When we last received a packet. Set by the
                                    // prpl to avoid sending unneeded keepalives
  end;

  PPurpleBuddy = Pointer;
  PPurpleGroup = Pointer;
  PPurpleUserInfo = Pointer;
  PPurpleStatus = Pointer;
  PPurpleStatusType = Pointer;
  PPurpleStoredImage = Pointer;
  PPurpleBlistNode = Pointer;
  PPurpleChat = Pointer;
  PPurpleRoomlist = Pointer;
  PPurpleRoomlistRoom = Pointer;
  PPurpleXfer = Pointer;
  PPurpleWhiteboardPrplOps = Pointer;
  TPurpleMessageFlags = Integer;
  TPurpleTypingState = Integer;
  TPurpleMediaSessionType = Integer;
  TPurpleMediaCaps = Integer;
  TPurpleMood = Integer;
  PPurpleSetPublicAliasSuccessCallback = procedure(); {$note fixme: define this}
  PPurpleSetPublicAliasFailureCallback = procedure();
  PPurpleGetPublicAliasSuccessCallback = procedure();
  PPurpleGetPublicAliasFailureCallback = procedure();

  TPurpleStatusPrimitive = (
    PURPLE_STATUS_UNSET = 0,
    PURPLE_STATUS_OFFLINE,
    PURPLE_STATUS_AVAILABLE,
    PURPLE_STATUS_UNAVAILABLE,
    PURPLE_STATUS_INVISIBLE,
    PURPLE_STATUS_AWAY,
    PURPLE_STATUS_EXTENDED_AWAY,
    PURPLE_STATUS_MOBILE,
    PURPLE_STATUS_TUNE,
    PURPLE_STATUS_MOOD,
    PURPLE_STATUS_NUM_PRIMITIVES
  );

  (**
   * A protocol plugin information structure.
   *
   * Every protocol plugin initializes this structure. It is the gateway
   * between purple and the protocol plugin.  Many of these callbacks can be
   * NULL.  If a callback must be implemented, it has a comment indicating so.
   *)
  TPurplePluginProtocolInfo = packed record
    options: TPurpleProtocolOptions;
    user_splits: PGList;      // A GList of PurpleAccountUserSplit
    protocol_options: PGList; // A GList of PurpleAccountOption
    icon_spec: TPurpleBuddyIconSpec; // The icon spec.

    (**
     * Returns the base icon name for the given buddy and account.
     * If buddy is NULL and the account is non-NULL, it will return the
     * name to use for the account's icon. If both are NULL, it will
     * return the name to use for the protocol's icon.
     *
     * This must be implemented.
     *)
    list_icon: function(account: PPurpleAccount; buddy: PPurpleBuddy): PChar;
    //const char *(*list_icon)(PurpleAccount *account, PurpleBuddy *buddy);

    (**
     * Fills the four char**'s with string identifiers for "emblems"
     * that the UI will interpret and display as relevant
     *)
    list_emblem: function(buddy: PPurpleBuddy): PChar;
    //const char *(*list_emblem)(PurpleBuddy *buddy);

    (**
     * Gets a short string representing this buddy's status.  This will
     * be shown on the buddy list.
     *)
    status_text: function(buddy: PPurpleBuddy): PChar;
    //char *(*status_text)(PurpleBuddy *buddy);

    (**
     * Allows the prpl to add text to a buddy's tooltip.
     *)
    tooltip_text: procedure(buddy: PPurpleBuddy; user_info: PPurpleUserInfo; full: GBoolean);
    //void (*tooltip_text)(PurpleBuddy *buddy, PurpleNotifyUserInfo *user_info, gboolean full);

    (**
     * Returns a list of #PurpleStatusType which exist for this account;
     * this must be implemented, and must add at least the offline and
     * online states.
     *)
    status_types: function(account: PPurpleAccount): PGList;
    //GList *(*status_types)(PurpleAccount *account);

    (**
     * Returns a list of #PurpleMenuAction structs, which represent extra
     * actions to be shown in (for example) the right-click menu for @a
     * node.
     *)
    blist_node_menu: function(node: PPurpleBlistNode): PGList;
    //GList *(*blist_node_menu)(PurpleBlistNode *node);

    (**
     * Returns a list of #proto_chat_entry structs, which represent
     * information required by the PRPL to join a chat. libpurple will
     * call join_chat along with the information filled by the user.
     *
     * @return A list of #proto_chat_entry structs
     *)
    chat_info: function(gc: PPurpleConnection): PGList;
    //GList *(*chat_info)(PurpleConnection *);

    (**
     * Returns a hashtable which maps #proto_chat_entry struct identifiers
     * to default options as strings based on chat_name. The resulting
     * hashtable should be created with g_hash_table_new_full(g_str_hash,
     * g_str_equal, NULL, g_free);. Use #get_chat_name if you instead need
     * to extract a chat name from a hashtable.
     *
     * @param chat_name The chat name to be turned into components
     * @return Hashtable containing the information extracted from chat_name
     *)
    chat_info_defaults: function(gc: PPurpleConnection; chat_name: PChar): PGHashTable;
    //GHashTable *(*chat_info_defaults)(PurpleConnection *, const char *chat_name);


    (* All the server-related functions *)

    (** This must be implemented. *)
    login: procedure(acc: PPurpleAccount);
    //void (*login)(PurpleAccount *);

    (** This must be implemented. *)
    close: procedure(gc: PPurpleConnection);
    //void (*close)(PurpleConnection *);

    (**
     * This PRPL function should return a positive value on success.
     * If the message is too big to be sent, return -E2BIG.  If
     * the account is not connected, return -ENOTCONN.  If the
     * PRPL is unable to send the message for another reason, return
     * some other negative value.  You can use one of the valid
     * errno values, or just big something.  If the message should
     * not be echoed to the conversation window, return 0.
     *)
    send_im: function(gc: PPurpleConnection; who: PChar; message: PChar; flags: TPurpleMessageFlags): Integer;
    //int  (*send_im)(PurpleConnection *, const char *who,
    //        const char *message,
    //        PurpleMessageFlags flags);

    set_info: procedure(gc: PPurpleConnection; info: PChar);
    //void (*set_info)(PurpleConnection *, const char *info);

    (**
     * @return If this protocol requires the PURPLE_TYPING message to
     *         be sent repeatedly to signify that the user is still
     *         typing, then the PRPL should return the number of
     *         seconds to wait before sending a subsequent notification.
     *         Otherwise the PRPL should return 0.
     *)
    send_typing: function(gc: PPurpleConnection; name: PChar; state: TPurpleTypingState): Integer;
    //unsigned int (*send_typing)(PurpleConnection *, const char *name, PurpleTypingState state);

    (**
     * Should arrange for purple_notify_userinfo() to be called with
     * @a who's user info.
     *)
    get_info: procedure(gc: PPurpleConnection; who: PChar);
    //void (*get_info)(PurpleConnection *, const char *who);
    set_status: procedure(account: PPurpleAccount; status: PPurpleStatus);
    //void (*set_status)(PurpleAccount *account, PurpleStatus *status);
    set_idle: procedure(gc: PPurpleConnection; idletime: Integer);
    //void (*set_idle)(PurpleConnection *, int idletime);
    change_passwd: procedure(gc: PPurpleConnection; old_pass, newpass: PChar);
    //void (*change_passwd)(PurpleConnection *, const char *old_pass,
    //            const char *new_pass);

    (**
     * Add a buddy to a group on the server.
     *
     * This PRPL function may be called in situations in which the buddy is
     * already in the specified group. If the protocol supports
     * authorization and the user is not already authorized to see the
     * status of \a buddy, \a add_buddy should request authorization.
     *
     * @deprecated Since 2.8.0, add_buddy_with_invite is preferred.
     * @see add_buddy_with_invite
     *)
    add_buddy: procedure(gc: PPurpleConnection; buddy: PPurpleBuddy; group: PPurpleGroup);
    //void (*add_buddy)(PurpleConnection *, PurpleBuddy *buddy, PurpleGroup *group);
    add_buddies: procedure(gc: PPurpleConnection; buddies: PGList; groups: PGList);
    //void (*add_buddies)(PurpleConnection *, GList *buddies, GList *groups);
    remove_buddy: procedure(gc: PPurpleConnection; buddy: PPurpleBuddy; group: PPurpleGroup);
    //void (*remove_buddy)(PurpleConnection *, PurpleBuddy *buddy, PurpleGroup *group);
    remove_buddies: procedure(gc: PPurpleConnection; buddies: PGList; groups: PGList);
    //void (*remove_buddies)(PurpleConnection *, GList *buddies, GList *groups);
    add_permit: procedure(gc: PPurpleConnection; name: PChar);
    //void (*add_permit)(PurpleConnection *, const char *name);
    add_deny: procedure(gc: PPurpleConnection; name: PChar);
    //void (*add_deny)(PurpleConnection *, const char *name);
    rem_permit: procedure(gc: PPurpleConnection; name: PChar);
    //void (*rem_permit)(PurpleConnection *, const char *name);
    rem_deny: procedure(gc: PPurpleConnection; name: PChar);
    //void (*rem_deny)(PurpleConnection *, const char *name);
    set_permit_deny: procedure(gc: PPurpleConnection);
    //void (*set_permit_deny)(PurpleConnection *);

    (**
     * Called when the user requests joining a chat. Should arrange for
     * #serv_got_joined_chat to be called.
     *
     * @param components A hashtable containing information required to
     *                   join the chat as described by the entries returned
     *                   by #chat_info. It may also be called when accepting
     *                   an invitation, in which case this matches the
     *                   data parameter passed to #serv_got_chat_invite.
     *)
    join_chat: procedure(gc: PPurpleConnection; components: PGHashTable);
    //void (*join_chat)(PurpleConnection *, GHashTable *components);

    (**
     * Called when the user refuses a chat invitation.
     *
     * @param components A hashtable containing information required to
     *                   join the chat as passed to #serv_got_chat_invite.
     *)
    reject_chat: procedure(gc: PPurpleConnection; components: PGHashTable);
    //void (*reject_chat)(PurpleConnection *, GHashTable *components);

    (**
     * Returns a chat name based on the information in components. Use
     * #chat_info_defaults if you instead need to generate a hashtable
     * from a chat name.
     *
     * @param components A hashtable containing information about the chat.
     *)
    get_chat_name: function(components: PGHashTable): PChar;
    //char *(*get_chat_name)(GHashTable *components);

    (**
     * Invite a user to join a chat.
     *
     * @param id      The id of the chat to invite the user to.
     * @param message A message displayed to the user when the invitation
     *                is received.
     * @param who     The name of the user to send the invation to.
     *)
    chat_invite: procedure(gc: PPurpleConnection; id: Integer; message: PChar; who: PChar);
    //void (*chat_invite)(PurpleConnection *, int id,
    //          const char *message, const char *who);

    (**
     * Called when the user requests leaving a chat.
     *
     * @param id The id of the chat to leave
     *)
    chat_leave: procedure(gc: PPurpleConnection);
    //void (*chat_leave)(PurpleConnection *, int id);

    (**
     * Send a whisper to a user in a chat.
     *
     * @param id      The id of the chat.
     * @param who     The name of the user to send the whisper to.
     * @param message The message of the whisper.
     *)
    chat_whisper: procedure(gc: PPurpleConnection; id: Integer; who: PChar; message: PChar);
    //void (*chat_whisper)(PurpleConnection *, int id,
    //           const char *who, const char *message);

    (**
     * Send a message to a chat.
     * This PRPL function should return a positive value on success.
     * If the message is too big to be sent, return -E2BIG.  If
     * the account is not connected, return -ENOTCONN.  If the
     * PRPL is unable to send the message for another reason, return
     * some other negative value.  You can use one of the valid
     * errno values, or just big something.  If the message should
     * not be echoed to the conversation window, return 0.
     *
     * @param id      The id of the chat to send the message to.
     * @param message The message to send to the chat.
     * @param flags   A bitwise OR of #PurpleMessageFlags representing
     *                message flags.
     * @return    A positive number or 0 in case of succes,
     *                a negative error number in case of failure.
     *)
    chat_send: function(gc: PPurpleConnection; id: Integer; message: PChar; flags: TPurpleMessageFlags): Integer;
    //int  (*chat_send)(PurpleConnection *, int id, const char *message, PurpleMessageFlags flags);

    (** If implemented, this will be called regularly for this prpl's
     *  active connections.  You'd want to do this if you need to repeatedly
     *  send some kind of keepalive packet to the server to avoid being
     *  disconnected.  ("Regularly" is defined by
     *  <code>KEEPALIVE_INTERVAL</code> in <tt>libpurple/connection.c</tt>.)
     *)
    keepalive: procedure(gc: PPurpleConnection);
    //void (*keepalive)(PurpleConnection *);

    (** new user registration *)
    register_user: procedure(acc: PPurpleAccount);
    //void (*register_user)(PurpleAccount *);

    (**
     * @deprecated Use #PurplePluginProtocolInfo.get_info instead.
     *)
    get_cb_info: Pointer; // nothing because its deprecated
    //void (*get_cb_info)(PurpleConnection *, int, const char *who);

    (**
     * @deprecated Use #PurplePluginProtocolInfo.get_cb_real_name and
     *             #PurplePluginProtocolInfo.status_text instead.
     *)
    get_cb_away: Pointer; // nothing because its deprecated
    //void (*get_cb_away)(PurpleConnection *, int, const char *who);

    (** save/store buddy's alias on server list/roster *)
    alias_buddy: procedure(gc: PPurpleConnection; who: PChar; alias: PChar);
    //void (*alias_buddy)(PurpleConnection *, const char *who,
    //          const char *alias);

    (** change a buddy's group on a server list/roster *)
    group_buddy: procedure(gc: PPurpleConnection; who, old_group, new_group: PChar);
    //void (*group_buddy)(PurpleConnection *, const char *who,
    //          const char *old_group, const char *new_group);

    (** rename a group on a server list/roster *)
    rename_group: procedure(gc: PPurpleConnection; old_name: PChar; group: PPurpleGroup; moved_buddies: PGList);
    //void (*rename_group)(PurpleConnection *, const char *old_name,
    //           PurpleGroup *group, GList *moved_buddies);

    buddy_free: procedure(buddy: PPurpleBuddy);
    //void (*buddy_free)(PurpleBuddy *);

    convo_closed: procedure(gc: PPurpleConnection; who: PChar);
    //void (*convo_closed)(PurpleConnection *, const char *who);

    (**
     *  Convert the username @a who to its canonical form.  (For example,
     *  AIM treats "fOo BaR" and "foobar" as the same user; this function
     *  should return the same normalized string for both of those.)
     *)
    normalize: function(acc: PPurpleAccount; who: PChar): PChar;
    //const char *(*normalize)(const PurpleAccount *, const char *who);

    (**
     * Set the buddy icon for the given connection to @a img.  The prpl
     * does NOT own a reference to @a img; if it needs one, it must
     * #purple_imgstore_ref(@a img) itself.
     *)
    set_buddy_icon: procedure(gc: PPurpleConnection; img: PPurpleStoredImage);
    //void (*set_buddy_icon)(PurpleConnection *, PurpleStoredImage *img);

    remove_group: procedure(gc: PPurpleConnection; group: PPurpleGroup);
    //void (*remove_group)(PurpleConnection *gc, PurpleGroup *group);

    (** Gets the real name of a participant in a chat.  For example, on
     *  XMPP this turns a chat room nick <tt>foo</tt> into
     *  <tt>room\@server/foo</tt>
     *  @param gc  the connection on which the room is.
     *  @param id  the ID of the chat room.
     *  @param who the nickname of the chat participant.
     *  @return    the real name of the participant.  This string must be
     *             freed by the caller.
     *)
    get_cb_real_name: function(gc: PPurpleConnection; id: Integer; who: PChar): PChar;
    //char *(*get_cb_real_name)(PurpleConnection *gc, int id, const char *who);

    set_chat_topic: procedure(gc: PPurpleConnection; id: Integer; topic: PChar);
    //void (*set_chat_topic)(PurpleConnection *gc, int id, const char *topic);

    find_blist_chat: function(acc: PPurpleAccount; name: PChar): PPurpleChat;
    //PurpleChat *(*find_blist_chat)(PurpleAccount *account, const char *name);

    (* room listing prpl callbacks *)
    roomlist_get_list: function(gc: PPurpleConnection): PPurpleRoomlist;
    //PurpleRoomlist *(*roomlist_get_list)(PurpleConnection *gc);

    roomlist_cancel: procedure(list: PPurpleRoomlist);
    //void (*roomlist_cancel)(PurpleRoomlist *list);

    roomlist_expand_category: procedure(list: PPurpleRoomlist; category: PPurpleRoomlistRoom);
    //void (*roomlist_expand_category)(PurpleRoomlist *list, PurpleRoomlistRoom *category);

    (* file transfer callbacks *)
    can_receive_file: function(gc: PPurpleConnection; who: PChar): GBoolean;
    //gboolean (*can_receive_file)(PurpleConnection *, const char *who);

    send_file: procedure(gc: PPurpleConnection; who: PChar; filename: PChar);
    //void (*send_file)(PurpleConnection *, const char *who, const char *filename);

    new_xfer: function(gc: PPurpleConnection; who: PChar): PPurpleXfer;
    //PurpleXfer *(*new_xfer)(PurpleConnection *, const char *who);

    (** Checks whether offline messages to @a buddy are supported.
     *  @return @c TRUE if @a buddy can be sent messages while they are
     *          offline, or @c FALSE if not.
     *)
    offline_message: function(buddy: PPurpleBuddy): GBoolean;
    //gboolean (*offline_message)(const PurpleBuddy *buddy);

    whiteboard_prpl_ops : PPurpleWhiteboardPrplOps;

    (** For use in plugins that may understand the underlying protocol *)
    send_raw: function(gc: PPurpleConnection; buf: Pointer; len: Integer): Integer;
    //int (*send_raw)(PurpleConnection *gc, const char *buf, int len);

    (* room list serialize *)
    roomlist_room_serialize: function(room: PPurpleRoomlistRoom): PChar;
    //char *(*roomlist_room_serialize)(PurpleRoomlistRoom *room);

    (** Remove the user from the server.  The account can either be
     * connected or disconnected. After the removal is finished, the
     * connection will stay open and has to be closed!
     *)
    (* This is here rather than next to register_user for API compatibility
     * reasons.
     *)
    unregister_user: procedure(acc: PPurpleAccount; cb: Pointer; user_data: Pointer); {$note fixme: define the callback function}
    //void (*unregister_user)(PurpleAccount *, PurpleAccountUnregistrationCb cb, void *user_data);

    (* Attention API for sending & receiving zaps/nudges/buzzes etc. *)
    send_attention: function(gc: PPurpleConnection; username: PChar; typ: UInt32): GBoolean;
    //gboolean (*send_attention)(PurpleConnection *gc, const char *username, guint type);

    get_attention_types: function(acc: PPurpleAccount): PGList;
    //GList *(*get_attention_types)(PurpleAccount *acct);

    (**
     * The size of the PurplePluginProtocolInfo. This should always be sizeof(PurplePluginProtocolInfo).
     * This allows adding more functions to this struct without requiring a major version bump.
     *)
    struct_size: UInt32;

    (* NOTE:
     * If more functions are added, they should accessed using the following syntax:
     *
     *    if (PURPLE_PROTOCOL_PLUGIN_HAS_FUNC(prpl, new_function))
     *      prpl->new_function(...);
     *
     * instead of
     *
     *    if (prpl->new_function != NULL)
     *      prpl->new_function(...);
     *
     * The PURPLE_PROTOCOL_PLUGIN_HAS_FUNC macro can be used for the older member
     * functions (e.g. login, send_im etc.) too.
     *)

    (** This allows protocols to specify additional strings to be used for
     * various purposes.  The idea is to stuff a bunch of strings in this hash
     * table instead of expanding the struct for every addition.  This hash
     * table is allocated every call and MUST be unrefed by the caller.
     *
     * @param account The account to specify.  This can be NULL.
     * @return The protocol's string hash table. The hash table should be
     *         destroyed by the caller when it's no longer needed.
     *)
    get_account_text_table: function(acc: PPurpleAccount): PGHashTable;
    //GHashTable *(*get_account_text_table)(PurpleAccount *account);

    (**
     * Initiate a media session with the given contact.
     *
     * @param account The account to initiate the media session on.
     * @param who The remote user to initiate the session with.
     * @param type The type of media session to initiate.
     * @return TRUE if the call succeeded else FALSE. (Doesn't imply the media session or stream will be successfully created)
     *)
    initiate_media: function(acc: PPurpleAccount; who: PChar; typ: TPurpleMediaSessionType): GBoolean;
    //gboolean (*initiate_media)(PurpleAccount *account, const char *who,
    //        PurpleMediaSessionType type);

    (**
     * Checks to see if the given contact supports the given type of media session.
     *
     * @param account The account the contact is on.
     * @param who The remote user to check for media capability with.
     * @return The media caps the contact supports.
     *)
    get_media_caps: function(acc: PPurpleAccount; who: PChar): TPurpleMediaCaps;
    //PurpleMediaCaps (*get_media_caps)(PurpleAccount *account,
    //          const char *who);

    (**
     * Returns an array of "PurpleMood"s, with the last one having
     * "mood" set to @c NULL.
     * @since 2.7.0
     *)
    get_moods: function(acc: PPurpleAccount): TPurpleMood;
    //PurpleMood *(*get_moods)(PurpleAccount *account);

    (**
     * Set the user's "friendly name" (or alias or nickname or
     * whatever term you want to call it) on the server.  The
     * protocol plugin should call success_cb or failure_cb
     * *asynchronously* (if it knows immediately that the set will fail,
     * call one of the callbacks from an idle/0-second timeout) depending
     * on if the nickname is set successfully.
     *
     * @param gc    The connection for which to set an alias
     * @param alias The new server-side alias/nickname for this account,
     *              or NULL to unset the alias/nickname (or return it to
     *              a protocol-specific "default").
     * @param success_cb Callback to be called if the public alias is set
     * @param failure_cb Callback to be called if setting the public alias
     *                   fails
     * @see purple_account_set_public_alias
     * @since 2.7.0
     *)
    set_public_alias: procedure(gc: PPurpleConnection; aalias: PChar;
      success_cb: PPurpleSetPublicAliasSuccessCallback;
      failure_cp: PPurpleSetPublicAliasFailureCallback);
    //void (*set_public_alias)(PurpleConnection *gc, const char *alias,
    //                         PurpleSetPublicAliasSuccessCallback success_cb,
    //                         PurpleSetPublicAliasFailureCallback failure_cb);


    (**
     * Retrieve the user's "friendly name" as set on the server.
     * The protocol plugin should call success_cb or failure_cb
     * *asynchronously* (even if it knows immediately that the get will fail,
     * call one of the callbacks from an idle/0-second timeout) depending
     * on if the nickname is retrieved.
     *
     * @param gc    The connection for which to retireve the alias
     * @param success_cb Callback to be called with the retrieved alias
     * @param failure_cb Callback to be called if the prpl is unable to
     *                   retrieve the alias
     * @see purple_account_get_public_alias
     * @since 2.7.0
     *)
    get_public_alias: procedure(acc: PPurpleAccount;
      success_cb: PPurpleGetPublicAliasSuccessCallback;
      failure_cb: PPurpleGetPublicAliasFailureCallback);
    //void (*get_public_alias)(PurpleConnection *gc,
    //                         PurpleGetPublicAliasSuccessCallback success_cb,
    //                         PurpleGetPublicAliasFailureCallback failure_cb);

    (**
     * Add a buddy to a group on the server.
     *
     * This PRPL function may be called in situations in which the buddy is
     * already in the specified group. If the protocol supports
     * authorization and the user is not already authorized to see the
     * status of \a buddy, \a add_buddy should request authorization.
     *
     * If authorization is required, then use the supplied invite message.
     *
     * @since 2.8.0
     *)
    add_buddy_with_invite: procedure(gc: PPurpleConnection; buddy: PPurpleBuddy; group: PPurpleGroup; message: PChar);
    //void (*add_buddy_with_invite)(PurpleConnection *pc, PurpleBuddy *buddy, PurpleGroup *group, const char *message);

    add_buddies_with_invite: procedure(gc: PPurpleConnection; buddies: PGList; groups: PGList; message: PChar);
    //void (*add_buddies_with_invite)(PurpleConnection *pc, GList *buddies, GList *groups, const char *message);
  end;

  TPurpleNotifyMsgType = (
    PURPLE_NOTIFY_MSG_ERROR   = 0, // Error notification.
    PURPLE_NOTIFY_MSG_WARNING,     // Warning notification.
    PURPLE_NOTIFY_MSG_INFO         // Information notification.
  );

  PPurpleNotifyCloseCallback = procedure(UserData: Pointer);





(********************************************
 *                                          *
 *   function imports from libpurple        *
 *                                          *
 *   All imported functions are declared    *
 *   as procedure variables and will be     *
 *   linked dnamically at runtime. Make     *
 *   sure that every new function that is   *
 *   added here will also be linked in our  *
 *   LoadImports() procedure below in the   *
 *   implementation section of this unit.   *
 *                                          *
 ********************************************)
{$calling cdecl}
var
  purple_plugin_register: function(var Plugin: TPurplePlugin): GBoolean;
  purple_timeout_add: function(Interval: Integer; cb: TGSourceFunc; UserData: Pointer): Integer;
  purple_timeout_remove: function(handle: Integer): GBoolean;
  purple_debug_info: procedure(category: PChar; format: PChar; args: array of const);
  purple_debug_warning: procedure(category: PChar; format: PChar; args: array of const);
  purple_debug_error: procedure(category: PChar; format: PChar; args: array of const);
  purple_notify_message: function(var Plugin: TPurplePlugin;
    typ: TPurpleNotifyMsgType; title: PChar; primary: PChar; secondary: PChar;
    cb: PPurpleNotifyCloseCallback; UserData: Pointer): GBoolean;

  (**
   * Creates a new status type.
   *
   * @param primitive     The primitive status type.
   * @param id            The ID of the status type, or @c NULL to use the id of
   *                      the primitive status type.
   * @param name          The name presented to the user, or @c NULL to use the
   *                      name of the primitive status type.
   * @param saveable      TRUE if the information set for this status by the
   *                      user can be saved for future sessions.
   * @param user_settable TRUE if this is a status the user can manually set.
   * @param independent   TRUE if this is an independent (non-exclusive)
   *                      status type.
   *
   * @return A new status type.
   *)
  purple_status_type_new_full: function(primitive: TPurpleStatusPrimitive;
    id: PChar; name: Pchar;
    saveable: GBoolean;
    user_settable: GBoolean;
    independent: GBoolean): PPurpleStatusType;
  //PurpleStatusType *purple_status_type_new_full(PurpleStatusPrimitive primitive,
  //                const char *id, const char *name,
  //                gboolean saveable,
  //                gboolean user_settable,
  //                gboolean independent);purple_status_type_new_full: function();

  (**
   * Sets the connection state.  PRPLs should call this and pass in
   * the state #PURPLE_CONNECTED when the account is completely
   * signed on.  What does it mean to be completely signed on?  If
   * the core can call prpl->set_status, and it successfully changes
   * your status, then the account is online.
   *
   * @param gc    The connection.
   * @param state The connection state.
   *)
  purple_connection_set_state: procedure(gc: PPurpleConnection; state: TPurpleConnectionState);
  //void purple_connection_set_state(PurpleConnection *gc, PurpleConnectionState state);

  (**
   * Returns the primitive type of a status type.
   *
   * @param status_type The status type.
   *
   * @return The primitive type of the status type.
   *)
  purple_status_type_get_primitive: function(status_type: PPurpleStatusType): TPurpleStatusPrimitive;
  //PurpleStatusPrimitive purple_status_type_get_primitive(
  //  const PurpleStatusType *status_type);

   (**
    * Returns the status's type.
    *
    * @param status The status.
    *
    * @return The status's type.
    *)
   purple_status_get_type: function(status: PPurpleStatus): PPurpleStatusType;
   //PurpleStatusType *purple_status_get_type(const PurpleStatus *status);

   (**
    * Returns the active exclusive status from a presence.
    *
    * @param presence The presence.
    *
    * @return The active exclusive status.
    *)
   purple_presence_get_active_status: function(presence: PPurplePresence): PPurpleStatus;
   //PurpleStatus *purple_presence_get_active_status(const PurplePresence *presence);



(****************************************
 *                                      *
 *   This is the only exported symbol   *
 *                                      *
 *   It is called when libpurple is     *
 *   probing all libs in the plugin     *
 *   folder. In the C examples this     *
 *   is hidden behind the macro         *
 *   PURPLE_INIT_PLUGIN but here we     *
 *   don't have macros to hide such     *
 *   things and the plugin library      *
 *   using this unit must explicitly    *
 *   export it.                         *
 *                                      *
 ****************************************)
function purple_init_plugin(var Plugin: TPurplePlugin): GBoolean;



(****************************************
 *                                      *
 *   Our own functions and helpers      *
 *                                      *
 *   from here on we also switch back   *
 *   to the default calling convention  *
 *                                      *
 ****************************************)
{$calling default}
type
  TDebugLevel = (
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

procedure _info(Msg: String);
procedure _warning(Msg: String);
procedure _error(Msg: String);

{ sometimes we need to allocate a char* for which
  libpurple will take ownership. This will allocate
  memory directly from the system and not from the
  FPC heap manager! }
function AllocPurpleString(Str: String): PChar;

var
  PluginInfo: TPurplePluginInfo;
  PluginProtocolInfo: TPurplePluginProtocolInfo;

implementation
uses
  dynlibs, sysutils, StreamIO;

var
  HPurple: TLibHandle = NilHandle;
  ImportsLoaded: Boolean = False;
  OldStdOut: Text;
  WritelnRedirect: TWritelnRedirect;

procedure _purple_debug(Level: TDebugLevel; Msg: String);
begin
  if ImportsLoaded then begin
    case Level of
      DEBUG_INFO: purple_debug_info(PluginInfo.id, PChar(Msg + LineEnding), []);
      DEBUG_WARNING: purple_debug_warning(PluginInfo.id, PChar(Msg + LineEnding), []);
      DEBUG_ERROR: purple_debug_error(PluginInfo.id, PChar(Msg + LineEnding), []);
    end;
  end;
  {$ifdef DebugToConsole}
  try
    WriteLn(OldStdOut, '[', PluginInfo.id, ' ', Level, '] ', Msg);
  except
    // There is no stdout on windows if pidgin is not run with --debug
  end;
  {$endif}
end;

procedure _info(Msg: String);
begin
  _purple_debug(DEBUG_INFO, Msg);
end;

procedure _warning(Msg: String);
begin
  _purple_debug(DEBUG_WARNING, Msg);
end;

procedure _error(Msg: String);
begin
  _purple_debug(DEBUG_ERROR, Msg);
end;

function AllocPurpleString(Str: String): PChar;
var
  L : Integer;
begin
  L := Length(Str);
  if L = 0 then
    Result := nil
  else begin
    // bypass the FPC heap manager and malloc() directly from the system.
    // Of course we do this only for strings that we pass to libpurple and
    // where libpurple promises to free it again.
    Result := g_malloc(L+1);
    Move(Str[1], Result[0], L);
    Result[L] := #0;
  end;
end;

{ TWritelnRedirect }
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
procedure InstallWritelnRedirect;
begin
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

procedure UnloadImports;
begin
  if HPurple <> NilHandle then begin
    FreeLibrary(HPurple);
    HPurple := NilHandle;
    ImportsLoaded := False;
  end;
end;

procedure LoadImports;
var
  PossibleNames : array[1..2] of string = ('libpurple', 'libpurple.so.0');
  LibName: String;
  Error: Boolean = False;

  procedure Link(const ProcVar; ProcName: String);
  var
    P : Pointer;
  begin
    P := GetProcAddress(HPurple, ProcName);
    if p <> nil then
      PPointer(@ProcVar)^ := P
    else begin
      _error('could not find symbol "' + ProcName + '"');
      Error := True;
    end;
  end;

begin
  if ImportsLoaded then
    exit;
  for LibName in PossibleNames do begin
    HPurple := LoadLibrary(LibName);
    if HPurple <> NilHandle then begin
      Link(purple_plugin_register, 'purple_plugin_register');
      Link(purple_timeout_add, 'purple_timeout_add');
      Link(purple_timeout_remove, 'purple_timeout_remove');
      Link(purple_debug_info, 'purple_debug_info');
      Link(purple_debug_warning, 'purple_debug_warning');
      Link(purple_debug_error, 'purple_debug_error');
      Link(purple_notify_message, 'purple_notify_message');
      Link(purple_status_type_new_full, 'purple_status_type_new_full');
      Link(purple_connection_set_state, 'purple_connection_set_state');
      Link(purple_status_type_get_primitive, 'purple_status_type_get_primitive');
      Link(purple_status_get_type, 'purple_status_get_type');
      Link(purple_presence_get_active_status, 'purple_presence_get_active_status');
      if Error then
        UnloadImports
      else
        ImportsLoaded := True;
      break;
    end;
  end;
  if HPurple = NilHandle then
    _error('could not load libpurple');
end;

{ This re-implements the stuff that is behind the PURPLE_INIT_PLUGIN macro.
  In C the macro would define the function and export it, here we only
  define it and the library must export it (because we can't export it
  from within a unit directly). }
function purple_init_plugin(var Plugin: TPurplePlugin): GBoolean; cdecl;
begin
  {$ifdef DebugToConsole}
  {$warning compiling with -dDebugToConsole. Not recommended for release.}
  _warning('Plugin has been compiled with -dDebugToConsole. Not recommended.');
  {$endif}
  LoadImports;
  Result := False;
  if ImportsLoaded then begin;
    Plugin.info := @PluginInfo;
    Result := purple_plugin_register(Plugin);
  end
  else begin
    _error('plugin will not load');
    Result := False;
  end;
end;

initialization
  InstallWritelnRedirect;
  FillByte(PluginInfo, Sizeof(PluginInfo), 0);
  FillByte(PluginProtocolInfo, SizeOf(PluginProtocolInfo), 0);
finalization
  UnloadImports;
  UninstallWritelnRedirect;
end.

