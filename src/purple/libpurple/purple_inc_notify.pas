{$ifdef interface_type}
  PPurpleNotifyUserInfo = ^TPurpleNotifyUserInfo;
  TPurpleNotifyUserInfo = object
    procedure AddPair(Label_, Value: String);
  end;

  TPurpleNotifyMsgType = (
    PURPLE_NOTIFY_MSG_ERROR   = 0, // Error notification.
    PURPLE_NOTIFY_MSG_WARNING,     // Warning notification.
    PURPLE_NOTIFY_MSG_INFO         // Information notification.
  );

  PPurpleNotifyCloseCb = procedure(user_data: Pointer); cdecl;
{$endif}

{$ifdef import_func_public}
function  purple_notify_message(Plugin: PPurplePlugin;
 typ: TPurpleNotifyMsgType; title: PChar; primary: PChar; secondary: PChar;
 cb: PPurpleNotifyCloseCb; UserData: Pointer): GBoolean; cdecl; external LIBPURPLE;
{$endif}

{$ifdef import_func}
procedure purple_notify_user_info_add_pair(user_info: PPurpleNotifyUserInfo;
  label_, value: PChar); cdecl; external LIBPURPLE;
{$endif}

{$ifdef implementation}
procedure TPurpleNotifyUserInfo.AddPair(Label_, Value: String);
begin
  purple_notify_user_info_add_pair(@Self, _PChar(Label_), _PChar(Value));
end;
{$endif}
