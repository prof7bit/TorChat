unit filetools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure FCopy(A,B: String);
procedure FMkDir(D: String);
procedure FDelete(D: String);
procedure FRename(A, B: String);
procedure FZip(A: String; F: array of AnsiString);


implementation

const
  TAR_EXE = '/bin/tar';
  ZIP_EXE = '/usr/bin/zip';

procedure FCopy(A, B: String);
var
  Fa: TFileStream = nil;
  Fb: TFileStream = nil;
  Buf: array[0..1023] of Byte;
  N: Integer;
  E: String;
begin
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
    if not Assigned(Fa) then
      E := '!!! FCopy: could not open ' + A + ' for reading'
    else
      if not Assigned(Fb) then
        E := '!!! FCopy: could not open ' + B + ' for writing'
      else
        E := '!!! FCopy: error while copying' + LineEnding +
          '  file ' + A + LineEnding + '    to ' + B;
  end;
  if Assigned(Fa) then Fa.Free;
  if Assigned(Fb) then Fb.Free;
  if E <> '' then
    raise Exception.Create(E);
end;

procedure FMkDir(D: String);
begin
  WriteLn('create ', D);
  if not ForceDirectories(D) then
    raise Exception.Create('!!! could not create directory ' + D);
end;

procedure FDelete(D: String);
var
  FileInfo: TSearchRec;
begin
  writeln('delete ', D);
  if DirectoryExists(D) then begin
    if FindFirst(ConcatPaths([D, '*']), faAnyFile, FileInfo) = 0 then begin
      repeat
        if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='') then
          continue;
        FDelete(ConcatPaths([D, FileInfo.Name]));
      until FindNext(FileInfo) <> 0;
    end;
    try
      RmDir(D);
    except
      raise Exception.Create('!!! FDelete: could not delete directory: ' + D);
    end;
  end
  else begin
    if FileExists(D) then begin
      if not DeleteFile(D) then begin
        raise Exception.Create('!!! FDelete: could not delete file: ' + D);
      end;
    end;
    // ignore if it does not exist.
  end;
end;

function _FTar(A: String; F: array of AnsiString): String;
var
  Args: Array of AnsiString;
  I: Integer;
begin
  WriteLn('create ', A);
  SetLength(Args, Length(F) + 2);
  Args[0] := '-caf';
  Args[1] := A;
  for I := 0 to Length(F) - 1 do begin
    writeln('   add ', F[i]);
    Args[i+2] := F[i];
  end;
  if ExecuteProcess(TAR_EXE, Args) <> 0 then
    raise Exception.Create('!!! could not create archive: ' + A);
end;

procedure _FZip(A: String; F: array of AnsiString);
var
  Args: Array of AnsiString;
  I: Integer;
begin
  WriteLn('create ', A);
  SetLength(Args, Length(F) + 2);
  Args[0] := '-r';
  Args[1] := A;
  for I := 0 to Length(F) - 1 do begin
    writeln('   add ', F[i]);
    Args[i+2] := F[i];
  end;
  if ExecuteProcess(ZIP_EXE, Args) <> 0 then
    raise Exception.Create('!!! could not create archive: ' + A);
end;

procedure FRename(A, B: String);
begin
  writeln('rename ', A, LineEnding, '    to ', B);
  if not RenameFile(A, B) then
    raise Exception.Create('!!! could not rename ' + A + ' to ' + B);
end;

procedure FZip(A: String; F: array of AnsiString);
var
  Ext: String;
begin
  Ext := ExtractFileExt(A);
  if Ext = '.bz2' then
    _FTar(A, F);
  if Ext = '.zip' then
    _FZip(A, F);
end;

end.

