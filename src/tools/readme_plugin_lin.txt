libpurpletorchat.so
===================

  Put this file into the plugin folder of libpurple which is

      ~/.purple/plugins/
      (you might have to create this folder first)

  or alternatively put it into

      /usr/lib/purple-2/

  You also must have tor installed. TorChat will search these paths
and use the one that is found first:

      /usr/local/sbin/
      /usr/local/bin/
      /usr/sbin/
      /usr/bin/
      /sbin/
      /bin/

  Tor does not necessarily need to be running or configured in any
special way to use TorChat, TorChat will care about all that automatically.
It will start a separate process with its own configuration that will not
and can not interfere with your existing tor configuration and usage.

  Now start pidgin from within a console window. You should see some
debugging output if you started it from the console.

  Create a new account, select "TorChat" for the protocol and when asked
for a username just enter your name (or anything else), this is NOT the
TorChat ID, its just an account name to tell different TorChat profiles
apart with a human readable name. It will create a config folder
~/.torchat2_accountname/ for each account (profile). You can create as
many accounts (profiles) as you want and use them all simultanously.

If you need a log file for debugging purposes create the folder
~/torchatlogs before you start it, TorChat will detect this and enable
logging for the plugin and for tor, if you want to disable logging simply
remove this folder.

The TorChat plugin will recognize the following environment variables
on all platforms: PURPLEHOME, APPDATA, TOR_EXE to override the defaults
for the data directory (if you want to include it with portable Pidgin),
TOR_EXE is the path and name of the tor binary (not just the folder).
