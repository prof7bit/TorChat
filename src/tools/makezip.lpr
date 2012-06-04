program makezip;

{$mode objfpc}{$H+}

uses
  Classes,
  sysutils,
  tc_config;

const
  LE = LineEnding;
  PLUGIN_README_TXT =
    'libpurpletorchat.so' + LE +
    '===================' + LE +
    LE +
    '  Put this file into the plugin folder of libpurple which is' + LE +
    LE +
    '      ~/.purple/plugins/' + LE +
    '      (you might have to create this folder first)' + LE +
    LE +
    '  or alternatively put it into' + LE +
    LE +
    '      /usr/lib/purple-2/' + LE +
    LE +
    '  You also must have tor installed in /sbin/tor which should normally be' + LE +
    'the default location of tor if are on a debian based system.' + LE +
    LE +
    '  Now start pidgin from within a console window. You should see a lot of' + LE +
    'debugging output. Additionally there will be a file ~/purpletorchat.log' + LE +
    'which contains the same debug output.' + LE +
    LE +
    '  *** note that this is the debug version of the plugin, the' + LE +
    'final release version will not create a debug file. ***' + LE +
    LE +
    '  Create a new account, select "TorChat" for the protocol and when asked' + LE +
    'for a username just enter your name (or anything else), this is NOT the' + LE +
    'TorChat ID, its just an account name to tell different TorChat profiles' + LE +
    'apart with a human readable name. It will create a config folder' + LE +
    '~/.torchat2_accountname/ for each account (profile). You can create as' + LE +
    'many accounts (profiles) as you want and use them all simultanously.' + LE +
    LE +
    '  This early alpha version of the plugin does only support instant' + LE +
    'messaging and NONE of the other features like file transfer, profile' + LE +
    'texts, etc., these will all be implemented in later versions.' + LE +
    LE;

  BINDIR = '../../bin/';
  LIBNAME = 'libpurpletorchat.so';

var
  Readme: Text;
  FileName: String;

procedure CP(S,D: String);
begin
  ExecuteProcess('/bin/cp', [S, D]);
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
  FileName := 'libpurpletorchat-' + SOFTWARE_VERSION + '-linux-i368.tar.bz2';
  Assign(Readme, 'README.txt');
  Rewrite(Readme);
  write(Readme, PLUGIN_README_TXT);
  Close(Readme);

  CP(BINDIR + LIBNAME,  '.');
  TAR(FileName, [LIBNAME, 'README.txt']);
  DeleteFile(LIBNAME);
  DeleteFile('README.txt');
  RenameFile(FileName, BINDIR + FileName);
end.

