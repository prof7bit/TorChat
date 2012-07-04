{ TorChat - Protocol message 'ping' (first message after connection)

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
unit tc_prot_ping;

{$mode objfpc}{$H+}

interface

uses
  tc_interface,
  tc_protocol;

type
  { TMsgPing

    The ping message is the first message that is sent when an
    outgoing connection is established. It has the following
    structure:

      ping <sender_id> <random_cookie>

    <sender_id>       is the .onion address without the .onion,
                      also known as the TorChat ID.
    <random_cookie>   is a randomly generated string that
                      belongs to this buddy. It is generated
                      for each buddy on application start.

    (Here and elsewhere I am using the same notation that is
    often found to document command line syntax, the angle
    brackets above are not actually part of the message, angle
    brackets <> denote an element that is mandatory, square
    brackets [] denote an element that is optional and the
    empty space is a space character 0x20.)

    Example (received on an incoming connection):
      ping lh7oap7zsprkypp5 575F54C4-096E-4DDD-8204-593A89223590

    When this message is received then the client must connect
    back to <sender_id> (TorChat always uses two connections)
    and respond with a pong message containing the exact same
    cookie (See also TMsgPong). The ping/pong mechanism with
    sending a random cookie on the outgoing connection and
    waiting for the same cookie to return on one of the incoming
    connections is used to identify the incoming connections
    (Incoming connections on Tor are anonymous but both ends
    must find a way to make sure that they are really connected
    to the right person and this is a simple but effective way
    to ensure this).

    The response in the above example would need to be sent
    through the outgoung connection to lh7oap7zsprkypp5:

      pong 575F54C4-096E-4DDD-8204-593A89223590

    Messages are always sent on the outgoing connection and
    always received on the incoming connection. The only
    exception from this rule are file transfer messages, all
    other messages will be ignored if they arrive on the
    outgoing connection.
  }
  TMsgPing = class(TMsg)
  strict protected
    FID: String;
    FCookie: String;
    procedure Serialize; override;
  public
    class function GetCommand: String; override;
    constructor Create(ABuddy: IBuddy; ACookie: String); reintroduce;
    procedure Parse; override;
    procedure Execute; override;
  end;

implementation
uses
  Classes,
  tc_misc,
  tc_buddy;

{ TMsgPing }

class function TMsgPing.GetCommand: String;
begin
  Result := 'ping';
end;

constructor TMsgPing.Create(ABuddy: IBuddy; ACookie: String);
begin
  inherited Create(ABuddy);
  FCookie := ACookie;
  FID := FClient.Roster.OwnID;  // the own ID, not the one of the buddy
end;

procedure TMsgPing.Parse;
begin
  FID := PopFirstWord(FBinaryContent);
  FCookie := FBinaryContent;
end;

procedure TMsgPing.Serialize;
begin
  FBinaryContent := FID + ' ' + FCookie;
end;

procedure TMsgPing.Execute;
var
  ID: String;
  BlackList: TStringArray;
begin
  WriteLn('TMsgPing.Execute() ', FID);
  FConnection.SetPingData(FID, FCookie);

  {$note maybe move this into TMsg somehow?}
  BlackList := FClient.Config.GetStringArray('BlackList');
  for ID in BlackList do begin
    if FID = ID then begin
      LogWarningAndIgnore(SF('%s is on the blacklist', [FID]));
      exit;
    end;
  end;

  FBuddy := FClient.Roster.ByID(FID);
  if not Assigned(FBuddy) then
    FBuddy := FClient.TempList.ByID(FID);

  if Assigned(FBuddy) then begin
    FBuddy.MustSendPong(FCookie)
  end
  else begin
    Writeln('got Ping from unknown Buddy, creating temporary buddy: ' + FID);
    FBuddy := TBuddy.Create(FClient);
    if FBuddy.InitID(FID) then begin // this will check if ID is valid and allowed
      FBuddy.MustSendPong(FCookie);
      FClient.TempList.AddBuddy(FBuddy);
    end
    else begin
      writeln('W cannot create a buddy with this id: ' + FID);
      LogWarningAndIgnore('malformed ping message');
      // ref counting will free the buddy and timeout
      // will free the connection.
    end;
  end;
end;

begin
  RegisterMessageClass(TMsgPing);
end.

