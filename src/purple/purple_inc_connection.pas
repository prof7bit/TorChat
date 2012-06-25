{$ifdef purple_interface}
//type
  TPurplePresence = class;

  TPurpleConnectionState = (
  	PURPLE_DISCONNECTED = 0, // Disconnected.
  	PURPLE_CONNECTED,        // Connected.
  	PURPLE_CONNECTING        // Connecting.
  );

  { TPurpleConnection }

  TPurpleConnection = class(TWrapper)
    function GetAccount: PPurpleAccount;
    procedure SetState(State: TPurpleConnectionState);
  end;


{$endif}
{$ifdef purple_implementation}

function purple_connection_get_account(gc: TPurpleConnection): PPurpleAccount; cdecl; external LIBPURPLE;
procedure purple_connection_set_state(gc: TPurpleConnection; state: TPurpleConnectionState); cdecl; external LIBPURPLE;

{ TPurpleConnection }
function TPurpleConnection.GetAccount: PPurpleAccount;
begin
  Result := purple_connection_get_account(Self);
end;

procedure TPurpleConnection.SetState(State: TPurpleConnectionState);
begin
  purple_connection_set_state(Self, State);
end;

{$endif}

