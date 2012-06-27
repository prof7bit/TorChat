{$ifdef _type_forward}
  PPurpleNotifyUserInfo = ^TPurpleNotifyUserInfo;
  PPurpleNotifyCloseCb = procedure(user_data: Pointer); cdecl;

  TPurpleNotifyMsgType = (
    PURPLE_NOTIFY_MSG_ERROR   = 0, // Error notification.
    PURPLE_NOTIFY_MSG_WARNING,     // Warning notification.
    PURPLE_NOTIFY_MSG_INFO         // Information notification.
  );
{$endif}

{$ifdef _type}
  { TPurpleNotifyUserInfo }
  TPurpleNotifyUserInfo = object
    class function Create: PPurpleNotifyUserInfo;
    procedure AddPair(ALabel, Value: String);
    procedure AddSectionBreak;
    procedure AddSectionHeader(ALabel: String);
  end;
{$endif}

{$ifdef _func}
function  purple_notify_message(Plugin: PPurplePlugin; typ: TPurpleNotifyMsgType; title: PChar; primary: PChar; secondary: PChar; cb: PPurpleNotifyCloseCb; UserData: Pointer): GBoolean; cdecl; external LIBPURPLE;
function  purple_notify_user_info_new: PPurpleNotifyUserInfo; cdecl; external LIBPURPLE;
procedure purple_notify_user_info_add_pair(user_info: PPurpleNotifyUserInfo; label_, value: PChar); cdecl; external LIBPURPLE;
procedure purple_notify_user_info_add_section_break(user_info: PPurpleNotifyUserInfo); cdecl; external LIBPURPLE;
procedure purple_notify_user_info_add_section_header(user_info: PPurpleNotifyUserInfo; label_: PChar); cdecl; external LIBPURPLE;
function  purple_notify_userinfo(gc: PPurpleConnection; who: PChar; user_info: PPurpleNotifyUserInfo; cb: PPurpleNotifyCloseCb; user_data: gpointer): Pointer; cdecl; external LIBPURPLE;
{$endif}

{$ifdef _impl}
class function TPurpleNotifyUserInfo.Create: PPurpleNotifyUserInfo;
begin
  Result := purple_notify_user_info_new;
end;

procedure TPurpleNotifyUserInfo.AddPair(ALabel, Value: String);
begin
  purple_notify_user_info_add_pair(@Self, C(ALabel), C(Value));
end;

procedure TPurpleNotifyUserInfo.AddSectionBreak;
begin
  purple_notify_user_info_add_section_break(@Self);
end;

procedure TPurpleNotifyUserInfo.AddSectionHeader(ALabel: String);
begin
  purple_notify_user_info_add_section_header(@Self, C(ALabel));
end;
{$endif}
