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
    FGuiID: Pointer; // optional, a related GUI handle or something
    FClient: IClient;
    FBuddy: IBuddy;
    FFileStream: TFileStream;
    FFileName: String;
    FFileSize: Int64;
    FBlockSize: Int64;
    FIsSender: Boolean;
    FHasStarted: Boolean;
    FTimeLastReceive: TDateTime;
    FChunk: String;
    FComplete: Boolean;

    // these are used when sending a file
    FFileNameMessageSent: Boolean;
    FPosSendNext: Int64;  // start position (bytes) of next block to send
    FPosConfirmed: Int64; // start position of last confirmed block

    // these are used for receiving
    FTempFileName: String;
    FPosReceiveNext: Int64;
    FPosLastError: Int64;

    procedure SendErrorMessage(Position: Int64; Always: Boolean=False);
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
    function IsComplete: Boolean;
    function BytesCompleted: Int64;
    function FileSize: Int64;
    function GuiID: Pointer;
    procedure SetGuiID(AGuiID: Pointer);
    procedure CheckState;
    procedure MoveReceivedFile(DestName: String);
    procedure OnStartSending; virtual; abstract;
    procedure OnProgressSending; virtual; abstract;
    procedure OnCancelSending; virtual; abstract;
    procedure OnCompleteSending; virtual; abstract;
    procedure OnStartReceiving; virtual; abstract;
    procedure OnProgressReceiving; virtual; abstract;
    procedure OnCancelReceiving; virtual; abstract;
    procedure OnCompleteReceiving; virtual; abstract;
    procedure ReceivedFileChunk(StartByte: Int64; FileChunk: String);
    procedure ReceivedBrokenChunk(StartByte: Int64);
    procedure ReceivedOk(StartByte: Int64);
    procedure ReceivedError(StartByte: Int64);
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

const
  NO_ERROR = -1;

{ TFileTransfer }

procedure TFileTransfer.SendErrorMessage(Position: Int64; Always: Boolean);
var
  Msg: IProtocolMessage;
begin
  // send errors for the same position only once
  if Always or (Position <> FPosLastError) then begin;
    Msg := TMsgFileDataError.Create(Buddy, ID, Position);
    Msg.Send;
    FPosLastError := Position;
  end;
end;

constructor TFileTransfer.Create(ABuddy: IBuddy);
begin
  inherited Create;
  FBuddy := ABuddy;
  FClient := ABuddy.Client;
  FHasStarted := False;
  FTimeLastReceive := Now;
  FComplete := False;
  FPosLastError := NO_ERROR;
end;

{ sending constructor }
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
    FFileStream := TFileStream.Create(FFileName, fmOpenRead or fmShareDenyNone);
    FPosSendNext := 0;
    FPosConfirmed := -FBlockSize;
    FFileSize := FFileStream.Size;
  except
    WriteLn(SF('E could not open %s for reading', [FFileName]));
  end;
end;

{ receiving constructor }
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
  FileClose(FileCreate(FTempFileName));
  FFileStream := TFileStream.Create(FTempFileName, fmOpenReadWrite);
end;

destructor TFileTransfer.Destroy;
var
  Msg: IProtocolMessage;
begin
  WriteLn('TFileTransfer.Destroy() ', ExtractFileName(FFileName));
  if Assigned(FFileStream) then FFileStream.Free;

  if not FComplete then begin
    if IsSender then
      Msg := TMsgFileStopReceiving.Create(Buddy, ID)
    else
      Msg := TMsgFileStopSending.Create(Buddy, ID);
    Msg.Send;
  end;

  if not IsSender then
    SafeDelete(FTempFileName);

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

function TFileTransfer.IsComplete: Boolean;
begin
  Result := FComplete;
end;

function TFileTransfer.BytesCompleted: Int64;
begin
  if IsSender then begin
    Result := FPosConfirmed + FBlockSize;
    if Result >= FFileSize then
      Result := FFileSize;
  end
  else
    result := FPosReceiveNext;
end;

function TFileTransfer.FileSize: Int64;
begin
  Result := FFileSize;
end;

function TFileTransfer.GuiID: Pointer;
begin
  Result := FGuiID;
end;

procedure TFileTransfer.SetGuiID(AGuiID: Pointer);
begin
  FGuiID := AGuiID;
end;

procedure TFileTransfer.CheckState;
var
  ChunkSize: UInt64;
  Msg: IProtocolMessage;
begin
  if FIsSender then begin
    if not Assigned(FFileStream) then
      exit;
    if TimeSince(FTimeLastReceive) > FILE_TRANSFER_SECONDS_WAIT then begin
      WriteLnF('W transfer to %s: long time without ack, resetting to last confirmed position',
        [FBuddy.ID]);
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
  end
  else begin
    if TimeSince(FTimeLastReceive) > FILE_TRANSFER_SECONDS_WAIT then begin
      WriteLn('long time without receiving file chunks, sending error message.');
      FTimeLastReceive := Now;
      SendErrorMessage(FPosReceiveNext, True);
    end;
  end;
end;

procedure TFileTransfer.MoveReceivedFile(DestName: String);
var
  F: TFileStream = nil;
begin
  DeleteFile(DestName);
  FFileStream.Seek(0, soBeginning);
  try
    F := TFileStream.Create(DestName, fmCreate);
    F.CopyFrom(FFileStream, FFileSize);
  except
    on E: Exception do begin
      writeln('E ', E.Message);
    end;
  end;
  if Assigned(F) then F.Free;
end;

procedure TFileTransfer.ReceivedFileChunk(StartByte: Int64; FileChunk: String);
var
  Msg: IProtocolMessage;
begin
  FTimeLastReceive := Now;
  if FPosLastError <> NO_ERROR then
    if StartByte <> FPosLastError then
      exit // ignore this, still waiting for chunk at FPosLastError;
    else
      FPosLastError := NO_ERROR;

  if (StartByte >= FFileSize) then begin
    WriteLnF('W received chunk with startbyte %d but total size is only %d',
      [StartByte, FileSize]);
    Msg := TMsgFileStopSending.Create(Buddy, ID);
    Msg.Send;
    exit;
  end;

  if StartByte > FPosReceiveNext then begin
    WriteLn('W received chunk with wrong position');
    SendErrorMessage(FPosReceiveNext);
    exit;
  end;

  if not FHasStarted then begin
    FHasStarted := True;
    OnStartReceiving;
  end;

  FFileStream.Seek(StartByte, soBeginning);
  FFileStream.Write(FileChunk[1], Length(FileChunk));
  Msg := TMsgFileDataOk.Create(Buddy, ID, StartByte);
  Msg.Send;
  OnProgressReceiving;
  Inc(FPosReceiveNext, Length(FileChunk));
  if FPosReceiveNext >= FFileSize then begin
    FComplete := True;
    OnCompleteReceiving;
  end;
end;

procedure TFileTransfer.ReceivedBrokenChunk(StartByte: Int64);
begin
  FTimeLastReceive := Now;
  if FPosLastError <> NO_ERROR then
    if StartByte <> FPosLastError then
      exit; // ignore this, still waiting for chunk at FPosLastError;

  SendErrorMessage(FPosReceiveNext, True);
end;

procedure TFileTransfer.ReceivedOk(StartByte: Int64);
begin
  FPosConfirmed := StartByte;
  FTimeLastReceive := Now;

  if not FHasStarted then begin
    OnStartSending;
    FHasStarted := True;
  end;
  if FPosConfirmed + FILE_TRANSFER_BLOCK_SIZE >= FFileSize then begin
    FComplete := True;
    OnCompleteSending
  end
  else
    OnProgressSending;
end;

procedure TFileTransfer.ReceivedError(StartByte: Int64);
begin
  FTimeLastReceive := Now;
  FHasStarted := True;
  FPosSendNext := StartByte;
  if not FHasStarted then begin
    FHasStarted := True;
    OnStartSending;
  end;
end;

procedure TFileTransfer.ReceivedCancel;
begin
  if IsSender then begin
    FPosSendNext := FFileSize;
    OnCancelSending;
  end
  else
    OnCancelReceiving;
end;

end.

