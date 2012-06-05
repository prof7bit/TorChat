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
  LIBNAME = 'libpurpletorchat.so';

var
  ZipName: String;

begin
  ZipName := 'libpurpletorchat-' + SOFTWARE_VERSION + '-linux-i368';
  FDelete(ZIPDIR);
  FMkDir(ZIPDIR);

  FCopy(BINDIR + LIBNAME, ZIPDIR);
  FCopy('readme_plugin.txt', ConcatPaths([ZIPDIR, 'README.txt']));
  ZipName := FZip(ZipName, [ZIPDIR]);
  FRename(ZipName, ConcatPaths([BINDIR, ZipName]));
  FDelete(ZIPDIR);
end.

