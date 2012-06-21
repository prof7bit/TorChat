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

  { TFileTransfer represents a file transfer. It can be either a sender
    or a receiver, this depends on what methods have been used after
    construction. The GUI will define a descendant that will implement
    the abstract event methods }
  TFileTransfer = class(TInterfacedObject, IFileTransfer)
  strict private
    FTransferID: String;
    FGuiHandle: Pointer;
    FClient: IClient;
    FBuddy: IBuddy;
    FSendFile: TFileStream;
    FFileName: String;
    FFileSize: UInt64;
    FIsSender: Boolean;
    FChunk: String;

    // these are used when sending a file
    FSendNext: UInt64;  // start position (bytes) of next block to send
    FConfirmed: UInt64; // start position of last confirmed block

  public
    constructor Create(ABuddy: IBuddy; AFileName: String);
    destructor Destroy; override;
    function ID: String;
    function Client: IClient;
    function Buddy: IBuddy;
    function IsSender: Boolean;
    procedure SetGuiHandle(AHandle: Pointer);
    function GuiHandle: Pointer;
    function BytesCompleted: UInt64;
    procedure StartSending;
    procedure CheckState;
    procedure OnProgress; virtual; abstract;
    procedure OnCancel; virtual; abstract;
    procedure OnComplete; virtual; abstract;
    procedure ReceivedFileChunk(StartByte: UInt64; FileChunk: String);
    procedure ReceivedBrokenChunk(StartByte: UInt64);
    procedure ReceivedOk(StartByte: UInt64);
    procedure ReceivedError(StartByte: UInt64);
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

constructor TFileTransfer.Create(ABuddy: IBuddy; AFileName: String);
begin
  inherited Create;
  FIsSender := False;
  FBuddy := ABuddy;
  FClient := ABuddy.Client;
  FFileName := AFileName;
  FSendFile := nil;
end;

destructor TFileTransfer.Destroy;
begin
  WriteLn('TFileTransfer.Destroy() ', ExtractFileName(FFileName));
  if Assigned(FSendFile) then FSendFile.Free;
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

procedure TFileTransfer.SetGuiHandle(AHandle: Pointer);
begin
  FGuiHandle := AHandle;
end;

function TFileTransfer.GuiHandle: Pointer;
begin
  Result := FGuiHandle;
end;

function TFileTransfer.BytesCompleted: UInt64;
begin
  Result := FConfirmed + FILE_TRANSFER_BLOCK_SIZE;
  if Result >= FFileSize then
    Result := FFileSize;
end;

procedure TFileTransfer.StartSending;
var
  GUID: TGuid;
  Msg: IProtocolMessage;
  FileNameOnly: String;
begin
  FIsSender := True;
  CreateGUID(GUID);
  FTransferID := GUIDToString(GUID);
  try
    FSendFile := TFileStream.Create(FFileName, fmOpenRead);
    FSendNext := 0;
    FConfirmed := 0;
    FFileSize := FSendFile.Size;
    FileNameOnly := ExtractFileName(FFileName);
    Msg := TMsgFileName.Create(
      Buddy, FTransferID, FFileSize, FILE_TRANSFER_BLOCK_SIZE, FileNameOnly);
    Msg.Send;
    Client.OnNeedPump;
  except
    WriteLn(_F('E could not open %s for reading', [FFileName]));
  end;
end;

procedure TFileTransfer.CheckState;
var
  ChunkSize: UInt64;
  Msg: IProtocolMessage;
begin
  if FIsSender then begin
    if not Assigned(FSendFile) then
      exit;
    if not Buddy.MaySendText then
      exit;
    if FSendNext = FFileSize then
      exit; // nothing to send anymore
    if (FSendNext - FConfirmed) > FILE_TRANSFER_BLOCKS_WAIT * FILE_TRANSFER_BLOCK_SIZE then
      exit;
    FSendFile.Seek(FSendNext, soBeginning);
    ChunkSize := FSendFile.Size - FSendNext;
    if ChunkSize > FILE_TRANSFER_BLOCK_SIZE then
      ChunkSize := FILE_TRANSFER_BLOCK_SIZE;
    if Length(FChunk) <> ChunkSize then
      SetLength(FChunk, ChunkSize);
    FSendFile.Read(FChunk[1], ChunkSize);
    Msg := TMsgFileData.Create(Buddy, FTransferID, FSendNext, FChunk);
    Msg.Send;
    Client.OnNeedPump;
    Inc(FSendNext, FILE_TRANSFER_BLOCK_SIZE);
    if FSendNext >= FFileSize then
      FSendNext := FFileSize;
  end;
end;

procedure TFileTransfer.ReceivedFileChunk(StartByte: UInt64; FileChunk: String);
begin

end;

procedure TFileTransfer.ReceivedBrokenChunk(StartByte: UInt64);
begin

end;

procedure TFileTransfer.ReceivedOk(StartByte: UInt64);
begin
  FConfirmed := StartByte;
  if FConfirmed + FILE_TRANSFER_BLOCK_SIZE >= FFileSize then
    OnComplete
  else
    OnProgress;
end;

procedure TFileTransfer.ReceivedError(StartByte: UInt64);
begin
  FSendNext := StartByte;
end;

end.

