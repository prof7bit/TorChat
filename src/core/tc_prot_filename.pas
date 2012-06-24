{ TorChat - TMsgFileName

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

unit tc_prot_filename;

{$mode objfpc}{$H+}

interface

uses
  tc_interface,
  tc_protocol;

type
  { TMsgFileName

  }
  TMsgFileName = class(TMsg)
  strict protected
    FTransferID: String;
    FFileSize: Int64;
    FBlockSize: Integer;
    FFileName: String;
    procedure Serialize; override;
    procedure ExecuteWithBuddy; override;
  public
    class function GetCommand: String; override;
    class function ReceiveOnOutgoing: Boolean; override;
    constructor Create(Buddy: IBuddy; TransferID: String; FileSize: Int64; BlockSize: Integer; FileName: String); reintroduce;
    procedure Parse; override;
  end;

implementation
uses
  sysutils,
  tc_misc;

{ TMsgFileName }

class function TMsgFileName.GetCommand: String;
begin
  Result := 'filename';
end;

class function TMsgFileName.ReceiveOnOutgoing: Boolean;
begin
  Result := True;
end;

constructor TMsgFileName.Create(Buddy: IBuddy; TransferID: String; FileSize: Int64; BlockSize: Integer; FileName: String);
begin
  inherited Create(Buddy);
  FTransferID := TransferID;
  FFileSize := FileSize;
  FBlockSize := BlockSize;
  FFileName := FileName; // only the name, without path!
end;

procedure TMsgFileName.Parse;
begin
  FTransferID := PopFirstWord(FBinaryContent);
  FFileSize := StrToInt64Def(PopFirstWord(FBinaryContent), 0);
  FBlockSize := StrToIntDef(PopFirstWord(FBinaryContent), 0);
  FFileName := SanitizeFileName(FBinaryContent);
end;

procedure TMsgFileName.Serialize;
begin
  FBinaryContent := _F('%s %d %d %s',
    [FTransferID, FFileSize, FBlockSize, FFileName]);
end;

procedure TMsgFileName.ExecuteWithBuddy;
begin
  FBuddy.Client.OnIncomingFileTransfer(FBuddy, FTransferID, FFileName, FFileSize, FBlockSize)
end;

begin
  RegisterMessageClass(TMsgFileName);
end.

