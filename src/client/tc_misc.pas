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

function SecondsSince(Start: TDateTime): QWord;

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

implementation

type

  { TSafeDeleteThread }

  TSafeDeleteThread = class(TThread)
    FFileName: String;
    constructor Create(AFileName: String);
    procedure Execute; override;
  end;

var
  PortList: array of DWord;

function SecondsSince(Start: TDateTime): QWord;
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
    raise EEndOfString.Create('');
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
begin
  if FileExists(AFileName) then begin
    Path := ExtractFileDir(AFileName);
    TempName := GetTempFileName(Path, 'delete_');
    RenameFile(AFileName, TempName);
    TSafeDeleteThread.Create(TempName);
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
  TrueValue: Integer;
begin
  if IsPortInList(APort) then begin
    WriteLn(_F('I Port %d is already used by TorChat', [APort]));
    exit(False);
  end;
  TrueValue := 1;
  Result := False;
  HSocket := Sockets.FPSocket(AF_INET, SOCK_STREAM, 0);
  if HSocket >= 0 then begin
    fpSetSockOpt(HSocket, SOL_SOCKET, 0, @TrueValue, SizeOf(TrueValue));
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

{ TSafeDeleteThread }

constructor TSafeDeleteThread.Create(AFileName: String);
begin
  FFileName := AFileName;
  FreeOnTerminate := True;
  Inherited Create(False);
end;

procedure TSafeDeleteThread.Execute;
const
  BLOCK = 100000;
var
  FS: TFileStream;
  I,S : Integer;
  B : PByte;
begin
  B := GetMem(BLOCK);
  FillByte(B[0], BLOCK, 0);
  FS := TFileStream.Create(FFileName, fmOpenReadWrite);
  S := FS.Seek(0, soEnd) div BLOCK;
  FS.Seek(0, soBeginning);
  for I := 0 to S do
    FS.WriteBuffer(B[0], BLOCK);
  {$ifdef windows}
  FlushFileBuffers(FS.Handle);
  {$else}
  fpfsync(FS.Handle);
  {$endif}
  FS.Free;
  DeleteFile(FFileName);
  FreeMem(B);
end;

end.

