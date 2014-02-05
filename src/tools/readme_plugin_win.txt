libpurpletorchat.so
===================

  Put this .dll file into the plugin folder of libpurple which is

      %APPDATA%\.purple\plugins\
      (you might have to create this folder first)

  or *alternatively* (not recommended) you may put it to the other
plugin dlls in the Pidgin program folder instead (you will need admin
rights for this):

      %ProgramFiles%\Pidgin\plugins\

  You also must have Tor installed in the usual location, this must
be one of the following folders (this is where the official Tor
installer will install Tor.exe), TorChat will automatically search
these folders and use the one that is first found:

      %ProgramFiles%\Tor\
      %ProgramFiles%\Vidalia Bundle\Tor\

  Tor only needs to be installed on your computer, it does not need to
be running or configured in any special way to use TorChat. TorChat will
take care of all that automatically and without interfering with your
other usage of Tor.

  Now start pidgin from within a console window. You should see some
debugging output. You may also start it from the start menu, see below
for how to log the debug output into a file.

  Create a new account, select "TorChat" for the protocol and when asked
for a username just enter your name (or anything else), this is NOT the
TorChat ID, its just an account name to tell different TorChat profiles
apart with a human readable name. It will create a config folder
%APPDATA%\torchat2_accountname\ for each account (profile). You can create
as many accounts (profiles) as you want and use them all simultaneously.

If you need a log file for debugging purposes create the folder
%APPDATA%\torchatlogs before you start it, TorChat will detect this and
enable logging for the plugin and for tor, if you want to disable logging
simply remove this folder.

The TorChat plugin will recognize the following environment variables
on all platforms: PURPLEHOME, APPDATA, TOR_EXE to override the defaults
for the data directory (if you want to include it with portable Pidgn),
TOR_EXE is the path and name of the tor binary (not just the folder).
