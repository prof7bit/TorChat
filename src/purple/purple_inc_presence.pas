{$ifdef purple_interface}
type
  PPurpleStatus = Pointer;

  { TPurplePresence }

  TPurplePresence = class(TWrapper)
    function GetActiveStatus: PPurpleStatus;
    procedure SwitchStatus(StatusID: String);
  end;

{$endif}
{$ifdef purple_implementation}

function  purple_presence_get_active_status(presence: TPurplePresence): PPurpleStatus; cdecl; external LIBPURPLE;
procedure purple_presence_switch_status(presence: TPurplePresence; status_id: PChar); cdecl; external LIBPURPLE;

{ TPurplePresence }

function TPurplePresence.GetActiveStatus: PPurpleStatus;
begin
  Result := purple_presence_get_active_status(Self);
end;

procedure TPurplePresence.SwitchStatus(StatusID: String);
begin
  purple_presence_switch_status(Self, _PChar(StatusID));
end;

{$endif}

