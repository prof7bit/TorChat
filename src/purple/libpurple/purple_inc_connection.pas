{$ifdef _type_forward}
  PPurpleConnection = ^TPurpleConnection;
{$endif}

{$ifdef _type}
  TPurpleConnectionState = (
	  PURPLE_DISCONNECTED = 0, // Disconnected.
	  PURPLE_CONNECTED,        // Connected.
	  PURPLE_CONNECTING        // Connecting.
  );

  { TPurpleConnection }

  TPurpleConnection = object
    function GetAccount: PPurpleAccount;
    procedure SetState(State: TPurpleConnectionState);
    procedure GotAlias(Who, Alias_: String);
    procedure GotIM(Who, Msg: String; Flags: TPurpleMessageFlags; MsgTime: time_t);
  end;
{$endif}

{$ifdef _func}
function purple_connection_get_account(gc: PPurpleConnection): PPurpleAccount; cdecl; external LIBPURPLE;
procedure purple_connection_set_state(gc: PPurpleConnection; state: TPurpleConnectionState); cdecl; external LIBPURPLE;
{$endif}

{$ifdef _impl}
function TPurpleConnection.GetAccount: PPurpleAccount;
begin
  Result := purple_connection_get_account(@Self);
end;

procedure TPurpleConnection.SetState(State: TPurpleConnectionState);
begin
  purple_connection_set_state(@Self, State);
end;

procedure TPurpleConnection.GotAlias(Who, Alias_: String);
begin
  serv_got_alias(@Self, C(Who), C(Alias_));
end;

procedure TPurpleConnection.GotIM(Who, Msg: String; Flags: TPurpleMessageFlags; MsgTime: time_t);
begin
  serv_got_im(@Self, C(Who), C(Msg), Flags, MsgTime);
end;

{$endif}

