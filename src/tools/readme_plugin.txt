libpurpletorchat.so
===================

  Put this file into the plugin folder of libpurple which is

      ~/.purple/plugins/
      (you might have to create this folder first)

  or alternatively put it into

      /usr/lib/purple-2/

  You also must have tor installed in /usr/sbin/tor which should 
normally be the default location of tor if are on a debian based system.

  Now start pidgin from within a console window. You should see a lot of
debugging output. Additionally there will be a file ~/purpletorchat.log
which contains the same debug output.

  *** note that this is the debug version of the plugin, the
final release version will not create a debug file. ***

  Create a new account, select "TorChat" for the protocol and when asked
for a username just enter your name (or anything else), this is NOT the
TorChat ID, its just an account name to tell different TorChat profiles
apart with a human readable name. It will create a config folder
~/.torchat2_accountname/ for each account (profile). You can create as
many accounts (profiles) as you want and use them all simultanously.

  This early alpha version of the plugin does only support instant
messaging and NONE of the other features like file transfer, profile
texts, etc., these will all be implemented in later versions.
