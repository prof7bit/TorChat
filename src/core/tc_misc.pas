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

function TimeSince(Start: TDateTime): Double;

{ split the string Line at the first occurrence of Sep, return the left part
  in the function result and the right part in var Line. If no separator is
  found then an EEndOfString exception is generated }
function Split(var Line: String; Sep: Char): String;

{ works like Format() but will catch exceptions at runtime }
function SF(FormatString: String; Args: array of const): String;

{ Delete a file by overwriting it }
procedure SafeDelete(AFileName: String);


procedure AddPortToList(APort: DWord);
procedure RemovePortFromList(APort: DWord);

{ Test if we can open this port for listening }
function IsPortAvailable(AInterface: String; APort: DWord): Boolean;

function IsValidOnionName(AName: String): Boolean;

{ format someting printable for the binary string B }
function DebugFormatBinary(B: String): String;

{ convert any line breaks to the system native format }
function LineBreaksAnyToNative(AText: String): String;

{ convert any line breaks to LF (#$0a) }
function LineBreaksAnyToLF(AText: String): String;

{ convert any line breaks to Space (#$20) }
function LineBreaksAnyToSpace(AText: String): String;

function Plain2Html(APlain: String): String;
function Html2Plain(AHtml: String): String;

function SanitizeFileName(AFileName: String): String;

procedure WriteLnF(FormatStr: String; Args: array of const);

implementation

type

  { TSafeDeleteThread }

  TSafeDeleteThread = class(TThread)
    FOutput: Text;
    FFileStream: TFileStream;
    constructor Create(AFileStream: TFileStream);
    procedure Execute; override;
  end;

var
  PortList: array of DWord;

function TimeSince(Start: TDateTime): Double;
begin
  Result := (Now - Start) * SecsPerDay;
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

function SF(FormatString: String; Args: array of const): String;
begin
  try
    Result := Format(FormatString, Args);
  except
    // replace all % to avoid unwanted surprise in following printf()
    FormatString := StringReplace(FormatString, '%', '$', [rfReplaceAll]);
    Result := FormatString;
    WriteLnF('E Eror while formatting: "%s"', [FormatString]);
  end;
end;

procedure SafeDelete(AFileName: String);
var
  Path, TempName: String;
  FS: TFileStream;
begin
  WriteLnF('wipe file "%s"', [AFileName]);
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
  end
  else
    WriteLnF('file "%s" does not exist', [AFileName]);
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

function IsPortAvailable(AInterface: String; APort: DWord): Boolean;
var
  HSocket: Integer;
  SockAddr  : TInetSockAddr;
begin
  if IsPortInList(APort) then begin
    WriteLn(SF('I Port %d is already used by TorChat', [APort]));
    exit(False);
  end;

  Result := False;
  HSocket := Sockets.FPSocket(AF_INET, SOCK_STREAM, 0);
  if HSocket >= 0 then begin
    SockAddr.sin_family := AF_INET;
    SockAddr.sin_port := ShortHostToNet(APort);
    SockAddr.sin_addr := StrToNetAddr(AInterface);
    if fpbind(HSocket, @SockAddr, SizeOf(SockAddr)) = 0 then begin
      WriteLn(SF('Port %d is available', [APort]));
      Result := True;
    end;
    Sockets.CloseSocket(HSocket);
  end;
  if Result = False then
    WriteLn(SF('I Port %d is NOT available', [APort]));
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

function DebugFormatBinary(B: String): String;
var
  L,I: Integer;
  C: Char;
begin
  L := Length(B);
  if L > 16 then
    B := '"' + LeftStr(B, 32) + '..." (' + IntToStr(L) + ' bytes)'
  else
    B := '"' + B + '"';
  B := StringReplace(B, #$0a, '0x0a', [rfReplaceAll]);
  B := StringReplace(B, #$0d, '0x0d', [rfReplaceAll]);
  B := StringReplace(B, #$00, '0x00', [rfReplaceAll]);
  for I := 1 to Length(B) do begin
    C := B[i];
    if not (
      (C in ['a'..'z','A'..'Z','0'..'9']) or
      (Pos(C, '"´`|;:,.!? /\_-§$%&<>()[]{}~+*#=') > 0)
    ) then
      B[i] := '?';
  end;
  Result := B;
end;

function LineBreaksAnyToNative(AText: String): String;
begin
  Result :=
    StringReplace(
    StringReplace(
    StringReplace(
    StringReplace(
    StringReplace(
    Trim(AText),
    '<br>',     #$0a, [rfReplaceAll]),
    #$0d#$0a,   #$0a, [rfReplaceAll]),
    #$0d,       #$0a, [rfReplaceAll]),
    #$0b,       #$0a, [rfReplaceAll]), // 0x0b shift-enter on windows
    #$0a, LineEnding, [rfReplaceAll]);
end;

function LineBreaksAnyToLF(AText: String): String;
begin
  Result :=
    StringReplace(
    StringReplace(
    StringReplace(
    StringReplace(
    Trim(AText),
    '<br>',     #$0a, [rfReplaceAll]),
    #$0d#$0a,   #$0a, [rfReplaceAll]),
    #$0d,       #$0a, [rfReplaceAll]),
    #$0b,       #$0a, [rfReplaceAll]); // 0x0b shift-enter on windows
end;

function LineBreaksAnyToSpace(AText: String): String;
begin
  Result := StringReplace(
    LineBreaksAnyToNative(AText),
    LineEnding, ' ', [rfReplaceAll]);
end;

function Plain2Html(APlain: String): String;
begin
  Result := StringReplace(APlain, '&', '&amp;', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '&quot;', [rfReplaceAll]);
  Result := StringReplace(Result, '<', '&lt;', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
  Result := StringReplace(Result, LineEnding, '<br>', [rfReplaceAll]);
end;

function Html2Plain(AHtml: String): String;
begin
  Result := StringReplace(AHtml, '<br>', LineEnding, [rfReplaceAll]);
  Result := StringReplace(Result, '&lt;', '<', [rfReplaceAll]);
  Result := StringReplace(Result, '&gt;', '>', [rfReplaceAll]);
  Result := StringReplace(Result, '&quot;', '"', [rfReplaceAll]);
  Result := StringReplace(Result, '&amp;', '&', [rfReplaceAll]);
end;

function SanitizeFileName(AFileName: String): String;
begin
  Result := StringReplace(AFileName, '/', '', [rfReplaceAll]);
  Result := StringReplace(Result, '\', '', [rfReplaceAll]);
  Result := StringReplace(Result, ' ', '_', [rfReplaceAll]);
  While (Length(Result) > 0) and (Result[1] = '.') do
    Result := RightStr(Result, Length(Result) - 1);
  if Length(Result) = 0 then
    Result := 'unknown';
end;

procedure WriteLnF(FormatStr: String; Args: array of const);
begin
  WriteLn(SF(FormatStr, Args));
end;

{ TSafeDeleteThread }

constructor TSafeDeleteThread.Create(AFileStream: TFileStream);
begin
  FOutput := Output;
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
  TempName: String;
begin
  Output := FOutput;
  TempName := FFileStream.FileName;
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
  FFileStream.Free;
  FreeMem(B);
  DeleteFile(TempName);
  WriteLnF('wiping temp file %s completed', [TempName]);
end;

end.

