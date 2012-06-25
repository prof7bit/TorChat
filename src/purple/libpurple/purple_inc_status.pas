{$ifdef _type_forward}
  PPurpleStatus = ^TPurpleStatus;
{$endif}

{$ifdef _type}
  TPurpleMood = cint;

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

  PPurpleStatusType = ^TPurpleStatusType;

  { TPurpleStatusType }

  TPurpleStatusType = object
    class function NewFull(APrimitive: TPurpleStatusPrimitive; ID: String;
      AName: String; Saveable: Boolean; UserSettable: Boolean;
      Independent: Boolean): PPurpleStatusType;
    function GetPrimitive: TPurpleStatusPrimitive;
  end;

  { TPurpleStatus }

  TPurpleStatus = object
    function GetType: PPurpleStatusType;
  end;
{$endif}

{$ifdef _func}
function  purple_status_get_type(status: PPurpleStatus): PPurpleStatusType; cdecl; external LIBPURPLE;
function  purple_status_type_get_primitive(status_type: PPurpleStatusType): TPurpleStatusPrimitive; cdecl; external LIBPURPLE;
function  purple_status_type_new_full(primitive: TPurpleStatusPrimitive;
  id: PChar; name: Pchar; saveable: GBoolean; user_settable: GBoolean;
  independent: GBoolean): PPurpleStatusType; cdecl; external LIBPURPLE;
{$endif}

{$ifdef _impl}
{ TPurpleStatusType }

class function TPurpleStatusType.NewFull(APrimitive: TPurpleStatusPrimitive;
  ID: String; AName: String; Saveable: Boolean; UserSettable: Boolean; Independent: Boolean): PPurpleStatusType;
begin
  Result := purple_status_type_new_full(APrimitive, _PChar(ID),
    _PChar(AName), Saveable, UserSettable, Independent);
end;

function TPurpleStatusType.GetPrimitive: TPurpleStatusPrimitive;
begin
  Result := purple_status_type_get_primitive(@self);
end;

{ TPurpleStatus }

function TPurpleStatus.GetType: PPurpleStatusType;
begin
  Result := purple_status_get_type(@Self);
end;

{$endif}
