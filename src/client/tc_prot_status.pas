{ TorChat - Protocol message 'status' (status and keepalive)

  Copyright (C) 2012 Bernd Kreuss <prof7bit@gmail.com>

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 3 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}
unit tc_prot_status;

{$mode objfpc}{$H+}

interface

uses
  tc_interface,
  tc_protocol;

type
  { TMsgStatus

    The status message is sent after the handshake and repeated
    every 120 seconds as a keepalive message. The value of 120
    seconds is part of the protocol specification and may not be
    changed. The status message has the following structure:

      status <status>

    <status>  is an ascii string and can be one of the following:
              'available', 'away' or 'xa' (without the quotes)

    The status message is repeated every 120 seconds. When a client
    receives the status message it will update the status of the
    buddy accordingly and show this in the buddy list (colored
    icons for example), when no status message and also no other
    message is received on this connection for more than 240
    seconds (twice the keepalive interval) then the connections
    for this budy should (and will) be closed.

    The numbers 120 and 240 are part of the protocol specification,
    they may not be changed, a TorChat client will rely on the
    fact that all other clients will use the same timings to
    determine a timeout.
  }
  TMsgStatus = class(TMsg)
  strict protected
    FStatus: TTorchatStatus;
    procedure Serialize; override;
  public
    class function GetCommand: String; override;
    constructor Create(ABuddy: IBuddy); reintroduce;
    procedure Parse; override;
    procedure Execute; override;
  end;

implementation

{ TMsgPing }

class function TMsgStatus.GetCommand: String;
begin
  Result := 'status';
end;

constructor TMsgStatus.Create(ABuddy: IBuddy);
begin
  inherited Create(ABuddy);
  FStatus := FClient.Status;
end;

procedure TMsgStatus.Parse;
begin
  case FBinaryContent of
    'available': FStatus := TORCHAT_AVAILABLE;
    'away': FStatus := TORCHAT_AWAY;
    'xa': FStatus := TORCHAT_XA;
  else
    FStatus := TORCHAT_AVAILABLE;
  end;
end;

procedure TMsgStatus.Serialize;
begin
  case FStatus of
    TORCHAT_AVAILABLE: FBinaryContent := 'available';
    TORCHAT_AWAY: FBinaryContent := 'away';
    TORCHAT_XA: FBinaryContent := 'xa';
  else
    FBinaryContent := 'available';
  end;
end;

procedure TMsgStatus.Execute;
var
  Buddy: IBuddy;
begin
  Buddy := FConnection.Buddy;
  if Assigned(Buddy) then begin
    WriteLn('TMsgStatus.Execute() received status: ', FStatus, ' from ', Buddy.ID);
    Buddy.SetStatus(FStatus);
    Buddy.ResetTimeout;
  end
  else
    LogWarningAndClose();
end;

begin
  RegisterMessageClass(TMsgStatus);
end.

