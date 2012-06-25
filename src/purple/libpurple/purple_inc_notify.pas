{$ifdef interface_type}
  PPurpleNotifyUserInfo     = Pointer;

  TPurpleNotifyMsgType = (
    PURPLE_NOTIFY_MSG_ERROR   = 0, // Error notification.
    PURPLE_NOTIFY_MSG_WARNING,     // Warning notification.
    PURPLE_NOTIFY_MSG_INFO         // Information notification.
  );

  PPurpleNotifyCloseCb = procedure(user_data: Pointer); cdecl;

{$endif}
