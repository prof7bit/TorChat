{$ifdef _type}
  PPurpleRequestFields = ^TPurpleRequestFields;
  PPurpleRequestFieldGroup = ^TPurpleRequestFieldGroup;
  PPurpleRequestField = ^TPurpleRequestField;

  PPurpleRequestDlgBtnCb = procedure(user_data: Pointer; fields: PPurpleRequestFields); cdecl;

  { TPurpleRequestFields }

  TPurpleRequestFields = object
    class function Create: PPurpleRequestFields;
    procedure AddGroup(Group: PPurpleRequestFieldGroup);
    function GetString(ID: String): String;
    function Request(Handle: Pointer; Title, Primary, Secondary: String;
      OkText: String; OkFunc: PPurpleRequestDlgBtnCb; CancelText: String;
      CancelFunc: PPurpleRequestDlgBtnCb; Account: PPurpleAccount;
      Who: String; Conv: PPurpleConversation; UserData: Pointer): Pointer;
  end;

  { TPurpleRequestFieldGroup }

  TPurpleRequestFieldGroup = object
    class function Create(Title: String): PPurpleRequestFieldGroup;
    procedure AddField(Field: PPurpleRequestField);
  end;

  TPurpleRequestField = object
    class function CreateString(Id, Text, DefaultValue: String; Multiline: Boolean): PPurpleRequestField;
  end;
{$endif}


{$ifdef _func}
function  purple_request_fields_new: PPurpleRequestFields; cdecl; external LIBPURPLE;
procedure purple_request_fields_add_group(fields: PPurpleRequestFields;
  group: PPurpleRequestFieldGroup); cdecl; external LIBPURPLE;
function  purple_request_fields(handle: Pointer;
  title, primary, secondary: PChar; fields: PPurpleRequestFields; ok_text: PChar;
  ok_cb:  PPurpleRequestDlgBtnCb; cancel_text: PChar; cancel_cb: PPurpleRequestDlgBtnCb;
  account: PPurpleAccount; who: PChar; conv: PPurpleConversation;
  user_data: Pointer): Pointer; cdecl; external LIBPURPLE; {<< this function
  differs from the C headers, they originally defined the callbacks as
  GCallback (which would take only one argument) but the callbacks will
  actually be given two aruments. Therefore I have made the following
  change: both callbacks now must be of type PPurpleRequestDlgBtnCb which
  receives two arguments: user_data and fields. }
function  purple_request_fields_get_string(fields: PPurpleRequestFields; id: PChar): PChar; cdecl; external LIBPURPLE;
procedure purple_request_field_group_add_field(group: PPurpleRequestFieldGroup;
  field: PPurpleRequestField); cdecl; external LIBPURPLE;
function  purple_request_field_group_new(title: PChar): PPurpleRequestFieldGroup; cdecl; external LIBPURPLE;
function  purple_request_field_string_new(id, text, default_value: PChar;
  multiline: gboolean): PPurpleRequestField; cdecl; external LIBPURPLE;
{$endif}


{$ifdef _impl}
{ TPurpleRequestFields }

class function TPurpleRequestFields.Create: PPurpleRequestFields;
begin
  Result := purple_request_fields_new;
end;

procedure TPurpleRequestFields.AddGroup(Group: PPurpleRequestFieldGroup);
begin
  purple_request_fields_add_group(@Self, Group);
end;

function TPurpleRequestFields.GetString(ID: String): String;
begin
  Result := purple_request_fields_get_string(@Self, C(ID));
end;

function TPurpleRequestFields.Request(Handle: Pointer; Title, Primary, Secondary: String; OkText: String; OkFunc: PPurpleRequestDlgBtnCb; CancelText: String; CancelFunc: PPurpleRequestDlgBtnCb; Account: PPurpleAccount; Who: String; Conv: PPurpleConversation; UserData: Pointer): Pointer;
begin
  Result := purple_request_fields(Handle, C(Title), C(Primary),
    C(Secondary), @Self, C(OkText), OkFunc, C(CancelText),
    CancelFunc, Account, C(Who), Conv, UserData);
end;

{ TPurpleRequestFieldGroup }

class function TPurpleRequestFieldGroup.Create(Title: String): PPurpleRequestFieldGroup;
begin
  Result := purple_request_field_group_new(C(Title));
end;

procedure TPurpleRequestFieldGroup.AddField(Field: PPurpleRequestField);
begin
  purple_request_field_group_add_field(@Self, Field);
end;

{ TPurpleRequestField }

class function TPurpleRequestField.CreateString(Id, Text, DefaultValue: String; Multiline: Boolean): PPurpleRequestField;
begin
  Result := purple_request_field_string_new(C(Id),
    C(Text), C(DefaultValue), Multiline);
end;

{$endif}
