unit filetools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function FCopy(A,B: String): Boolean;
function FMkDir(D: String): Boolean;
function FDelete(D: String): Boolean;
function FRename(A, B: String): Boolean;
function FZip(A: String; F: array of AnsiString): String; // returns the archive name

implementation

function FCopy(A, B: String): Boolean;
var
  Fa: TFileStream = nil;
  Fb: TFileStream = nil;
  Buf: array[0..1023] of Byte;
  N: Integer;
begin
  Result := True;
  Buf[0] := 0; // make compiler happy ('not initialized')
  if DirectoryExists(B) then
    B := ConcatPaths([B, ExtractFileName(A)]);
  try
    Fa := TFileStream.Create(A, fmOpenRead);
    Fb := TFileStream.Create(B, fmCreate);
    WriteLn('  copy ', A, LineEnding, '    to ', B);
    repeat
      N := Fa.Read(Buf, 1024);
      Fb.Write(Buf, N);
    until N < 1024;
  except
    Result := False;
    if not Assigned(Fa) then
      WriteLn('FCopy: could not open ', A, ' for reading')
    else
      if not Assigned(Fb) then
        WriteLn('FCopy: could not open ', B, ' for writing')
      else
        Writeln('FCopy: error while copying', LineEnding,
          '  file ', A, LineEnding, '    to ', B);
  end;
  if Assigned(Fa) then Fa.Free;
  if Assigned(Fb) then Fb.Free;
end;

function FMkDir(D: String): Boolean;
begin
  WriteLn('create ', D);
  Result := ForceDirectories(D);
end;

function FDelete(D: String): Boolean;
var
  FileInfo: TSearchRec;
begin
  writeln('delete ', D);
  Result := True;
  if DirectoryExists(D) then begin
    if FindFirst(ConcatPaths([D, '*']), faAnyFile, FileInfo) = 0 then begin
      repeat
        if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='') then
          continue;
        if not FDelete(ConcatPaths([D, FileInfo.Name])) then
          Result := False;
      until FindNext(FileInfo) <> 0;
    end;
    try
      RmDir(D);
    except
      Result := False;
      WriteLn('FDelete: could not delete directory: ', D);
    end;
  end
  else begin
    if FileExists(D) then begin
      if not DeleteFile(D) then begin
        Result := False;
        WriteLn('FDelete: could not delete file: ', D);
      end;
    end;
  end;
end;

{$ifdef windows}
function FZip(A: String; F: array of AnsiString): Boolean;
begin
end;
{$else}
function FZip(A: String; F: array of AnsiString): String;
var
  Args: Array of AnsiString;
  I: Integer;
begin
  A := A + '.tar.bz2';
  WriteLn('create ', A);
  SetLength(Args, Length(F) + 2);
  Args[0] := '-cjf';
  Args[1] := A;
  for I := 0 to Length(F) - 1 do begin
    writeln('   add ', F[i]);
    Args[i+2] := F[i];
  end;
  if ExecuteProcess('/bin/tar', Args) = 0 then
    Result := A
  else
    Result := '';
end;
{$endif}

function FRename(A, B: String): Boolean;
begin
  writeln('rename ', A, LineEnding, '    to ', B);
  Result := RenameFile(A, B);
end;

end.

