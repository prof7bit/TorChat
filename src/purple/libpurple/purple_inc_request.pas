{$ifdef interface_type}
  PPurpleRequestFields = ^TPurpleRequestFields;
  PPurpleRequestFieldGroup = ^TPurpleRequestFieldGroup;
  PPurpleRequestField = ^TPurpleRequestField;

  PPurpleRequestDlgBtnCb = procedure(user_data: Pointer; fields: PPurpleRequestFields); cdecl;

  TPurpleRequestFields = object
  end;

  TPurpleRequestFieldGroup = object
  end;

  TPurpleRequestField = object
  end;
{$endif}
