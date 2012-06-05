program makezip;

{$mode objfpc}{$H+}

uses
  Classes,
  sysutils,
  tc_config;

const
  BINDIR = '../../bin/';
  ZIPDIR = 'libpurpletorchat/';
  LIBNAME = 'libpurpletorchat.so';

var
  Readme: Text;
  ZipName: String;

procedure CP(S,D: String);
begin
  ExecuteProcess('/bin/cp', [S, D]);
end;

procedure RM(S: String);
begin
  ExecuteProcess('/bin/rm', ['-rf', S]);
end;

procedure TAR(A: String; F: array of AnsiString);
var
  Args: Array of AnsiString;
  I: Integer;
begin
  SetLength(Args, Length(F) + 2);
  Args[0] := '-cjf';
  Args[1] := A;
  for I := 0 to Length(F) - 1 do
    Args[i+2] := F[i];
  ExecuteProcess('/bin/tar', Args);
end;

begin
  ZipName := 'libpurpletorchat-' + SOFTWARE_VERSION + '-linux-i368.tar.bz2';
  RM(ZIPDIR);
  CreateDir(ZIPDIR);

  CP(BINDIR + LIBNAME, ZIPDIR);
  CP('readme_plugin.txt', ConcatPaths([ZIPDIR, 'README.txt']));
  TAR(ZipName, [ZIPDIR]);
  RenameFile(ZipName, ConcatPaths([BINDIR, ZipName]));
  RM(ZIPDIR);
end.

