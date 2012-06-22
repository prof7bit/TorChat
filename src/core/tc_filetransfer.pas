{ TorChat - TFileTransfer, represents an active file transfer

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

unit tc_filetransfer;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  tc_const,
  tc_interface;

type

  { TFileTransfer represents a file transfer. It can be either
    a sender or a receiver, this depends on which constructor
    has been used. The GUI will define a descendant that will
    implement the abstract event methods }
  TFileTransfer = class(TInterfacedObject, IFileTransfer)
  strict private
    FTransferID: String;
    FClient: IClient;
    FBuddy: IBuddy;
    FFileStream: TFileStream;
    FFileName: String;
    FFileSize: UInt64;
    FBlockSize: Int64;
    FIsSender: Boolean;
    FHasStarted: Boolean;
    FTimeLastReceive: TDateTime;
    FChunk: String;

    // these are used when sending a file
    FFileNameMessageSent: Boolean;
    FPosSendNext: Int64;  // start position (bytes) of next block to send
    FPosConfirmed: Int64; // start position of last confirmed block

    // these are used for receiving
    FTempFileName: String;
    FPosReceiveNext: Int64;

  public
    constructor Create(ABuddy: IBuddy);
    // create for sending
    constructor Create(ABuddy: IBuddy; AFileName: String);
    // create for receiving
    constructor Create(ABuddy: IBuddy; AFileName: String; ATransferID: String;
      AFileSize: Int64; ABlockSize: Integer);
    destructor Destroy; override;
    function ID: String;
    function Client: IClient;
    function Buddy: IBuddy;
    function IsSender: Boolean;
    function BytesCompleted: UInt64;
    procedure CheckState;
    procedure OnStart; virtual; abstract;
    procedure OnProgress; virtual; abstract;
    procedure OnCancel; virtual; abstract;
    procedure OnComplete; virtual; abstract;
    procedure ReceivedFileChunk(StartByte: UInt64; FileChunk: String);
    procedure ReceivedBrokenChunk;
    procedure ReceivedOk(StartByte: UInt64);
    procedure ReceivedError(StartByte: UInt64);
    procedure ReceivedCancel;
  end;

implementation
uses
  tc_prot_filename,
  tc_prot_filedata,
  tc_prot_filedata_ok,
  tc_prot_filedata_error,
  tc_prot_file_stop_sending,
  tc_prot_file_stop_receiving,
  tc_misc;


{ TFileTransfer }

constructor TFileTransfer.Create(ABuddy: IBuddy);
begin
  inherited Create;
  FBuddy := ABuddy;
  FClient := ABuddy.Client;
  FHasStarted := False;
  FTimeLastReceive := Now;
end;

constructor TFileTransfer.Create(ABuddy: IBuddy; AFileName: String);
var
  GUID: TGuid;
begin
  Create(ABuddy);
  FIsSender := True;
  FFileName := AFileName;
  FFileStream := nil;
  FFileNameMessageSent := False;
  CreateGUID(GUID);
  FTransferID := GUIDToString(GUID);
  FBlockSize := FILE_TRANSFER_BLOCK_SIZE;
  try
    FFileStream := TFileStream.Create(FFileName, fmOpenRead);
    FPosSendNext := 0;
    FPosConfirmed := -FBlockSize;
    FFileSize := FFileStream.Size;
  except
    WriteLn(_F('E could not open %s for reading', [FFileName]));
  end;
end;

constructor TFileTransfer.Create(ABuddy: IBuddy; AFileName: String; ATransferID: String; AFileSize: Int64; ABlockSize: Integer);
begin
  Create(ABuddy);
  FIsSender := False;
  FFileName := AFileName;
  FTransferID := ATransferID;
  FFileSize := AFileSize;
  FBlockSize := ABlockSize;
  FPosReceiveNext := 0;
  FTempFileName := GetTempFileName(Client.Config.DataDir, Buddy.ID + '_recv_' + AFileName + '_');
  FFileStream := TFileStream.Create(FTempFileName, fmCreate or fmOpenReadWrite);
end;

destructor TFileTransfer.Destroy;
begin
  WriteLn('TFileTransfer.Destroy() ', ExtractFileName(FFileName));
  if Assigned(FFileStream) then FFileStream.Free;
  inherited Destroy;
end;

function TFileTransfer.ID: String;
begin
  Result := FTransferID;
end;

function TFileTransfer.Client: IClient;
begin
  Result := FClient;
end;

function TFileTransfer.Buddy: IBuddy;
begin
  Result := FBuddy;
end;

function TFileTransfer.IsSender: Boolean;
begin
  Result := FIsSender;
end;

function TFileTransfer.BytesCompleted: UInt64;
begin
  if IsSender then begin
    Result := FPosConfirmed + FBlockSize;
    if Result >= FFileSize then
      Result := FFileSize;
  end
  else
    result := FPosReceiveNext;
end;

procedure TFileTransfer.CheckState;
var
  ChunkSize: UInt64;
  Msg: IProtocolMessage;
begin
  if FIsSender then begin
    if not Assigned(FFileStream) then
      exit;
    if TimeSince(FTimeLastReceive) > 60 then begin
      WriteLn(_F('W transfer to %s: long time without ack, resetting to last confirmed position',
        [FBuddy.ID]));
      FPosSendNext := FPosConfirmed + FBlockSize;
      FTimeLastReceive := Now;
    end;
    if not Buddy.MaySendText then
      exit;
    if FPosSendNext = FFileSize then
      exit; // nothing to send anymore
    if (FPosSendNext - FPosConfirmed) > FILE_TRANSFER_BLOCKS_WAIT * FBlockSize then
      exit;

    if not FFileNameMessageSent then begin
      Msg := TMsgFileName.Create(Buddy, FTransferID, FFileSize,
        FBlockSize, ExtractFileName(FFileName));
      Msg.Send;
      FFileNameMessageSent := True;
    end;

    FFileStream.Seek(FPosSendNext, soBeginning);
    ChunkSize := FFileStream.Size - FPosSendNext;
    if ChunkSize > FBlockSize then
      ChunkSize := FBlockSize;
    if Length(FChunk) <> ChunkSize then
      SetLength(FChunk, ChunkSize);
    FFileStream.Read(FChunk[1], ChunkSize);
    Msg := TMsgFileData.Create(Buddy, FTransferID, FPosSendNext, FChunk);
    Msg.Send;
    Client.OnNeedPump;
    Inc(FPosSendNext, FBlockSize);
    if FPosSendNext >= FFileSize then
      FPosSendNext := FFileSize;
  end;
end;

procedure TFileTransfer.ReceivedFileChunk(StartByte: UInt64; FileChunk: String);
var
  Msg: IProtocolMessage;
begin
  if StartByte >= FFileSize then begin
    Msg := TMsgFileStopSending.Create(Buddy, ID);
    Msg.Send;
    exit;
  end;
  if StartByte > FPosReceiveNext then begin
    Msg := TMsgFileDataError.Create(Buddy, ID, FPosReceiveNext);
    Msg.Send;
  end
  else begin
    if not FHasStarted then begin
      FHasStarted := True;
      OnStart;
    end;
    FFileStream.Seek(StartByte, soBeginning);
    FFileStream.Write(FileChunk[1], Length(FileChunk));
    Msg := TMsgFileDataOk.Create(Buddy, ID, StartByte);
    Msg.Send;
    OnProgress;
    Inc(FPosReceiveNext, Length(FileChunk));
    if FPosReceiveNext >= FFileSize then
      OnComplete;
  end;
end;

procedure TFileTransfer.ReceivedBrokenChunk;
var
  Msg: IProtocolMessage;
begin
  Msg := TMsgFileDataError.Create(Buddy, ID, FPosReceiveNext);
  Msg.Send;
end;

procedure TFileTransfer.ReceivedOk(StartByte: UInt64);
begin
  FPosConfirmed := StartByte;
  FTimeLastReceive := Now;
  if not FHasStarted then begin
    OnStart;
    FHasStarted := True;
  end;
  if FPosConfirmed + FILE_TRANSFER_BLOCK_SIZE >= FFileSize then
    OnComplete
  else
    OnProgress;
end;

procedure TFileTransfer.ReceivedError(StartByte: UInt64);
begin
  FTimeLastReceive := Now;
  FHasStarted := True;
  FPosSendNext := StartByte;
  if not FHasStarted then begin
    FHasStarted := True;
    OnStart;
  end;
end;

procedure TFileTransfer.ReceivedCancel;
begin
  if IsSender then
    FPosSendNext := FFileSize;
  OnCancel;
end;

end.

