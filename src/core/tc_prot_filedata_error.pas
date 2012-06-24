{ TorChat - TMsgFileDataError

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

unit tc_prot_filedata_error;

{$mode objfpc}{$H+}

interface

uses
  tc_interface,
  tc_protocol;

type
  { TMsgFileDataError

  }
  TMsgFileDataError = class(TMsg)
  strict protected
    FTransferID: String;
    FStartByte: Int64;
    procedure Serialize; override;
    procedure ExecuteWithBuddy; override;
  public
    class function GetCommand: String; override;
    function GetSendConnection: IHiddenConnection; override;
    constructor Create(ABuddy: IBuddy; TransferID: String; StartByte: Int64); reintroduce;
    procedure Parse; override;
  end;

implementation
uses
  sysutils,
  tc_misc;

{ TMsgFileDataError }

class function TMsgFileDataError.GetCommand: String;
begin
  Result := 'filedata_error';
end;

function TMsgFileDataError.GetSendConnection: IHiddenConnection;
begin
  if Assigned(FBuddy) then
    Result := FBuddy.ConnIncoming
  else
    Result := nil;
end;

constructor TMsgFileDataError.Create(ABuddy: IBuddy; TransferID: String; StartByte: Int64);
begin
  inherited Create(ABuddy);
  FTransferID := TransferID;
  FStartByte := StartByte;
end;

procedure TMsgFileDataError.Parse;
begin
  FTransferID := PopFirstWord(FBinaryContent);
  FStartByte := StrToInt64Def(FBinaryContent, 0);
end;

procedure TMsgFileDataError.Serialize;
begin
  FBinaryContent := _F('%s %d', [FTransferID, FStartByte]);
end;

procedure TMsgFileDataError.ExecuteWithBuddy;
var
  Transfer: IFileTransfer;
begin
  Transfer := FClient.FindFileTransferSend(FTransferID);
  if Assigned(Transfer) then
    Transfer.ReceivedError(FStartByte)
  else
    WriteLn('E received "filedata_error" that does not belong to any running transfer, ignoring.');
end;

begin
  RegisterMessageClass(TMsgFileDataError);
end.


