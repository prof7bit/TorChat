program makezip;

{$mode objfpc}{$H+}

uses
  Classes,
  sysutils,
  tc_const,
  filetools;

const
  BINDIR = '../../bin/';
  ZIPDIR = 'libpurpletorchat/';
  LIBNAME_LINUX = 'libpurpletorchat.so';
  LIBNAME_LINUX64 = 'libpurpletorchat64.so';
  LIBNAME_WIN32 = 'purpletorchat.dll';
  README_NAME_LINUX = 'readme_plugin_lin.txt';
  README_NAME_WIN32 = 'readme_plugin_win.txt';

procedure MakeZipFile(ZipName, ReadmeName, LibNameA, LibNameB: String);
begin
  WriteLn;
  WriteLn('*** making ' + ZipName);
  FMkDir(ZIPDIR);
  FCopy(BINDIR + LibNameA, ZIPDIR + LibNameB);
  FCopy(ReadmeName, ConcatPaths([ZIPDIR, 'README.txt']));
  FZip(ZipName, [ZIPDIR]);
  FRename(ZipName, ConcatPaths([BINDIR, ZipName]));
  FDelete(ZIPDIR);
end;

begin
  FDelete(ZIPDIR);

  // this assumes that the binaries have ben built already
  // and are in the bin folder

  MakeZipFile(
    'libpurpletorchat-' + SOFTWARE_VERSION + '-linux-i368.tar.bz2',
    README_NAME_LINUX,
    LIBNAME_LINUX,
    LIBNAME_LINUX
  );

  MakeZipFile(
    'libpurpletorchat-' + SOFTWARE_VERSION + '-Windows-i368.zip',
    README_NAME_WIN32,
    LIBNAME_WIN32,
    LIBNAME_WIN32
  );

  MakeZipFile(
    'libpurpletorchat-' + SOFTWARE_VERSION + '-linux-x86-64.tar.bz2',
    README_NAME_LINUX,
    LIBNAME_LINUX64,
    LIBNAME_LINUX
  );
end.

