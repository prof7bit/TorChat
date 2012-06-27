{$ifdef _type_forward}
  PPurplePresence = ^TPurplePresence;
{$endif}

{$ifdef _type}
  TPurplePresence = object
    function GetActiveStatus: PPurpleStatus;
    procedure SwitchStatus(StatusID: String);
  end;
{$endif}


{$ifdef _func}
function  purple_presence_get_active_status(presence: PPurplePresence): PPurpleStatus; cdecl; external LIBPURPLE;
procedure purple_presence_switch_status(presence: PPurplePresence; status_id: PChar); cdecl; external LIBPURPLE;
{$endif}


{$ifdef _impl}
{ TPurplePresence }

function TPurplePresence.GetActiveStatus: PPurpleStatus;
begin
  Result := purple_presence_get_active_status(@Self);
end;

procedure TPurplePresence.SwitchStatus(StatusID: String);
begin
  purple_presence_switch_status(@Self, Pointer(StatusID));
end;

{$endif}

