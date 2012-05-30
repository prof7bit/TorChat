{ TorChat - Some small helper functions

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
unit tc_misc;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef windows}
  windows,
  {$else}
  unix,
  {$endif}
  Sockets,
  Classes,
  SysUtils;

type
  EEndOfString = class(Exception)
  end;

function SecondsSince(Start: TDateTime): Int64;

{ split the string Line at the first occurrence of Sep, return the left part
  in the function result and the right part in var Line. If no separator is
  found then an EEndOfString exception is generated }
function Split(var Line: String; Sep: Char): String;

{ This function exists only to make the compiler happy when we have unused
  function arguments, we can then just Ignore() them and the compiler will not
  emit a warning anymore. Unneccessary warnings only spam the console when
  compiling and bury legitimate problems. This function is declared inline,
  it does absolutely nothing and will therefore be optimized away completely. }
function Ignore(P: Pointer): Pointer; Inline;

{ works like Format() but will catch exceptions at runtime }
function _F(S: String; Args: array of const): String;

{ Delete a file by overwriting it }
procedure SafeDelete(AFileName: String);


procedure AddPortToList(APort: DWord);
procedure RemovePortFromList(APort: DWord);

{ Test if we can open this port for listening }
function IsPortAvailable(APort: DWord): Boolean;

function IsValidOnionName(AName: String): Boolean;

implementation

type

  { TSafeDeleteThread }

  TSafeDeleteThread = class(TThread)
    FFileStream: TFileStream;
    constructor Create(AFileStream: TFileStream);
    procedure Execute; override;
  end;

var
  PortList: array of DWord;

function SecondsSince(Start: TDateTime): Int64;
begin
  Result := Round((Now - Start) * 24 * 60 * 60);
end;

function Split(var Line: String; Sep: Char): String;
var
  P : Integer;
begin
  P := Pos(Sep, Line);
  if P > 0 then begin
    Result := LeftStr(Line, P-1);
    Line := RightStr(Line, Length(Line) - P);
  end
  else
    raise EEndOfString.Create('no more separator found');
end;

function Ignore(P: Pointer): Pointer; inline;
begin
  Result := P;
end;

function _F(S: String; Args: array of const): String;
begin
  try
    Result := Format(S, Args);
  except
    Result := 'E Eror while formatting: "' + S + '"';
  end;
end;

procedure SafeDelete(AFileName: String);
var
  Path, TempName: String;
  FS: TFileStream;
begin
  if FileExists(AFileName) then begin
    Path := ExtractFileDir(AFileName);
    TempName := GetTempFileName(Path, 'delete_');
    try
      RenameFile(AFileName, TempName);
      FS := TFileStream.Create(TempName, fmOpenReadWrite);
      TSafeDeleteThread.Create(FS);
    except
      on E: Exception do begin
        WriteLn('E ', E.ToString, ': ', E.Message);
      end;
    end;
  end;
end;

function IsPortInList(APort: DWord): Boolean;
var
  Port: DWord;
begin
  Result := False;
  for Port in PortList do
    if Port = APort then
      exit(True);
end;

procedure AddPortToList(APort: DWord);
var
  L : Integer;
begin
  L := Length(PortList);
  SetLength(PortList, L+1);
  PortList[L] := APort;
end;

procedure RemovePortFromList(APort: DWord);
var
  P,L : Integer;
begin
  L := Length(PortList) - 1;
  for P := 0 to L do begin
    if PortList[P] = APort then begin
      PortList[P] := PortList[L];
      SetLength(PortList, L);
      break;
    end;
  end;
end;

function IsPortAvailable(APort: DWord): Boolean;
var
  HSocket: Integer;
  SockAddr  : TInetSockAddr;
begin
  if IsPortInList(APort) then begin
    WriteLn(_F('I Port %d is already used by TorChat', [APort]));
    exit(False);
  end;

  Result := False;
  HSocket := Sockets.FPSocket(AF_INET, SOCK_STREAM, 0);
  if HSocket >= 0 then begin
    SockAddr.sin_family := AF_INET;
    SockAddr.sin_port := ShortHostToNet(APort);
    SockAddr.sin_addr.s_addr := 0;
    if fpbind(HSocket, @SockAddr, SizeOf(SockAddr)) = 0 then begin
      WriteLn(_F('Port %d is available', [APort]));
      Result := True;
    end;
    Sockets.CloseSocket(HSocket);
  end;
  if Result = False then
    WriteLn(_F('I Port %d is NOT available', [APort]));
end;

function IsValidOnionName(AName: String): Boolean;
var
  C: Char;
begin
  Result := False;
  if Length(AName) <> 16 then
    exit;
  for C in AName do
    if Pos(C, 'abcdefghijklmnopqrstuvwxyz234567') = 0 then
      exit;
  Result := True;
end;

{ TSafeDeleteThread }

constructor TSafeDeleteThread.Create(AFileStream: TFileStream);
begin
  FFileStream := AFileStream;
  FreeOnTerminate := True;
  Inherited Create(False);
end;

procedure TSafeDeleteThread.Execute;
const
  BLOCK = 1024;
var
  I,S : Integer;
  B : PByte;
begin
  B := GetMem(BLOCK);
  FillByte(B[0], BLOCK, 0);
  S := FFileStream.Size div BLOCK;
  for I := 0 to S do
    FFileStream.WriteBuffer(B[0], BLOCK);
  {$ifdef windows}
  FlushFileBuffers(FFileStream.Handle);
  {$else}
  fpfsync(FFileStream.Handle);
  {$endif}
  DeleteFile(FFileStream.FileName);
  FFileStream.Free;
  FreeMem(B);
end;

end.

