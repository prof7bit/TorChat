{ TorChat - TMsgFileData

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

unit tc_prot_filedata;

{$mode objfpc}{$H+}

interface

uses
  tc_interface,
  tc_protocol;

type
  { TMsgFileData

  }
  TMsgFileData = class(TMsg)
  strict protected
    FTransferID: String;
    FStartByte: Int64;
    FCheckSum: String;
    FFileChunk: String;
    FCheckSumOK: Boolean;
    procedure Serialize; override;
    procedure ExecuteWithBuddy; override;
  public
    class function GetCommand: String; override;
    function GetSendConnection: IHiddenConnection; override;
    constructor Create(Buddy: IBuddy; ID: String; StartByte: Int64; FileChunk: String); reintroduce;
    procedure Parse; override;
  end;

implementation
uses
  sysutils,
  tc_misc,
  tc_prot_file_stop_sending,
  md5;

function CheckSum(Data: String): String;
begin
  Result := MDPrint(MD5String(Data));
end;

{ TMsgFileData }

class function TMsgFileData.GetCommand: String;
begin
  Result := 'filedata';
end;

function TMsgFileData.GetSendConnection: IHiddenConnection;
begin
  if Assigned(FBuddy) then
    Result := FBuddy.ConnIncoming
  else
    Result := nil;
end;

constructor TMsgFileData.Create(Buddy: IBuddy; ID: String; StartByte: Int64; FileChunk: String);
begin
  inherited Create(Buddy);
  FTransferID := ID;
  FStartByte := StartByte;
  FCheckSum := CheckSum(FileChunk);
  FFileChunk := FileChunk;
end;

procedure TMsgFileData.Parse;
begin
  FTransferID := PopFirstWord(FBinaryContent);
  FStartByte := StrToInt64Def(PopFirstWord(FBinaryContent), 0);
  FCheckSum := PopFirstWord(FBinaryContent);
  FFileChunk := FBinaryContent;
  FCheckSumOK := (CheckSum(FFileChunk) = FCheckSum);
  if not FCheckSumOK then
    WriteLn('E received file data checksum wrong');
end;

procedure TMsgFileData.Serialize;
begin
  FBinaryContent := _F('%s %d %s %s',
    [FTransferID, FStartByte, FCheckSum, FFileChunk]);
end;

procedure TMsgFileData.ExecuteWithBuddy;
var
  Transfer: IFileTransfer;
  Msg: IProtocolMessage;
begin
  Transfer := FBuddy.Client.FindFileTransferRecv(FTransferID);
  if Assigned(Transfer) then begin
    if FCheckSumOK then
      Transfer.ReceivedFileChunk(FStartByte, FFileChunk)
    else
      Transfer.ReceivedBrokenChunk(FStartByte);
  end
  else begin
    WriteLn('E received file data that does not belong to any running transfer');
    Msg := TMsgFileStopSending.Create(FBuddy, FTransferID);
    Msg.Send;
  end;
end;

begin
  RegisterMessageClass(TMsgFileData);
end.

