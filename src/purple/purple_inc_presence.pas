{$ifdef interface_type}
  PPurpleStatus = Pointer;

  { TPurplePresence }

  // PPurplePresence is already declared forward in purple.pas
  TPurplePresence = object
    function GetActiveStatus: PPurpleStatus;
    procedure SwitchStatus(StatusID: String);
  end;

{$endif}
{$ifdef implementation}

function  purple_presence_get_active_status(presence: PPurplePresence): PPurpleStatus; cdecl; external LIBPURPLE;
procedure purple_presence_switch_status(presence: PPurplePresence; status_id: PChar); cdecl; external LIBPURPLE;

{ TPurplePresence }

function TPurplePresence.GetActiveStatus: PPurpleStatus;
begin
  Result := purple_presence_get_active_status(@Self);
end;

procedure TPurplePresence.SwitchStatus(StatusID: String);
begin
  purple_presence_switch_status(@Self, _PChar(StatusID));
end;

{$endif}

