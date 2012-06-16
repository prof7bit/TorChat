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
  LIBNAME_WIN32 = 'purpletorchat.dll';

var
  ZipName: String;

begin
  FDelete(ZIPDIR);

  // this assumes that the binaries have ben built already
  // and are in the bin folder

  WriteLn;
  WriteLn('*** making linux archive');
  FMkDir(ZIPDIR);
  FCopy(BINDIR + LIBNAME_LINUX, ZIPDIR);
  FCopy('readme_plugin_lin.txt', ConcatPaths([ZIPDIR, 'README.txt']));
  ZipName := 'libpurpletorchat-' + SOFTWARE_VERSION + '-linux-i368';
  ZipName := FTar(ZipName, [ZIPDIR]);
  FRename(ZipName, ConcatPaths([BINDIR, ZipName]));
  FDelete(ZIPDIR);

  WriteLn;
  WriteLn('*** making windows archive');
  FMkDir(ZIPDIR);
  FCopy(BINDIR + LIBNAME_WIN32, ZIPDIR);
  FCopy('readme_plugin_win.txt', ConcatPaths([ZIPDIR, 'README.txt']));
  ZipName := 'purpletorchat-' + SOFTWARE_VERSION + '-win32';
  ZipName := FZip(ZipName, [ZIPDIR]);
  FRename(ZipName, ConcatPaths([BINDIR, ZipName]));
  FDelete(ZIPDIR);
end.

