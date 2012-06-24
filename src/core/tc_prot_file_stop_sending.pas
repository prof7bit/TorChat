{ TorChat - TMsgFileStopSending

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

unit tc_prot_file_stop_sending;

{$mode objfpc}{$H+}

interface

uses
  tc_interface,
  tc_protocol;

type
  { TMsgFileStopSending

  }
  TMsgFileStopSending = class(TMsg)
  strict protected
    FTransferID: String;
    procedure Serialize; override;
    procedure ExecuteWithBuddy; override;
  public
    class function GetCommand: String; override;
    function GetSendConnection: IHiddenConnection; override;
    constructor Create(ABuddy: IBuddy; TransferID: String); reintroduce;
    procedure Parse; override;
  end;

implementation

{ TMsgFileStopSending }

class function TMsgFileStopSending.GetCommand: String;
begin
  Result := 'file_stop_sending';
end;

function TMsgFileStopSending.GetSendConnection: IHiddenConnection;
begin
  if Assigned(FBuddy) then
    Result := FBuddy.ConnIncoming
  else
    Result := nil;
end;

constructor TMsgFileStopSending.Create(ABuddy: IBuddy; TransferID: String);
begin
  inherited Create(ABuddy);
  FTransferID := TransferID;
end;

procedure TMsgFileStopSending.Parse;
begin
  FTransferID := FBinaryContent;
end;

procedure TMsgFileStopSending.Serialize;
begin
  FBinaryContent := FTransferID;
end;

procedure TMsgFileStopSending.ExecuteWithBuddy;
var
  Transfer: IFileTransfer;
begin
  Transfer := FClient.FindFileTransferSend(FTransferID);
  if Assigned(Transfer) then
    Transfer.ReceivedCancel
  else
    WriteLn('E received "file_stop_sending" that does not belong to any running transfer');
end;

begin
  RegisterMessageClass(TMsgFileStopSending);
end.


