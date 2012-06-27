{$ifdef _type}
  PPurpleNotifyUserInfo = ^TPurpleNotifyUserInfo;
  TPurpleNotifyUserInfo = object
    procedure AddPair(Label_, Value: String); inline;
  end;

  TPurpleNotifyMsgType = (
    PURPLE_NOTIFY_MSG_ERROR   = 0, // Error notification.
    PURPLE_NOTIFY_MSG_WARNING,     // Warning notification.
    PURPLE_NOTIFY_MSG_INFO         // Information notification.
  );

  PPurpleNotifyCloseCb = procedure(user_data: Pointer); cdecl;
{$endif}

{$ifdef _func_public}
function  purple_notify_message(Plugin: PPurplePlugin;
 typ: TPurpleNotifyMsgType; title: PChar; primary: PChar; secondary: PChar;
 cb: PPurpleNotifyCloseCb; UserData: Pointer): GBoolean; cdecl; external LIBPURPLE;
{$endif}

{$ifdef _func}
procedure purple_notify_user_info_add_pair(user_info: PPurpleNotifyUserInfo;
  label_, value: PChar); cdecl; external LIBPURPLE;
{$endif}

{$ifdef _impl}
procedure TPurpleNotifyUserInfo.AddPair(Label_, Value: String);
begin
  purple_notify_user_info_add_pair(@Self, C(Label_), C(Value));
end;
{$endif}
