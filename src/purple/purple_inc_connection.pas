{$ifdef purple_interface}
type
  PPurplePresence               = Pointer;

  TPurpleConnectionState = (
  	PURPLE_DISCONNECTED = 0, // Disconnected.
  	PURPLE_CONNECTED,        // Connected.
  	PURPLE_CONNECTING        // Connecting.
  );

  TPurpleAccount = class; // forward will be solved in inc_account

  { TPurpleConnection }

  TPurpleConnection = class
    function GetAccount: TPurpleAccount;
    procedure SetState(State: TPurpleConnectionState);
  end;


{$endif}
{$ifdef purple_implementation}

function purple_connection_get_account(gc: TPurpleConnection): TPurpleAccount; cdecl; external LIBPURPLE;
procedure purple_connection_set_state(gc: TPurpleConnection; state: TPurpleConnectionState); cdecl; external LIBPURPLE;

{ TPurpleConnection }
function TPurpleConnection.GetAccount: TPurpleAccount;
begin
  Result := purple_connection_get_account(Self);
end;

procedure TPurpleConnection.SetState(State: TPurpleConnectionState);
begin
  purple_connection_set_state(Self, State);
end;


{$endif}
