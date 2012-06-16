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
be the following folder (this is where the offical Tor installer 
will install Tor.exe):
  
      %ProgramFiles%\Tor\

  Now start pidgin from within a console window. You should see some
debugging output. You may also start it from the start menu because it
will also create a file %APPDATA%\purpletorchat.log which contains the 
same debug output.

  *** note that this is the debug version of the plugin, the
final release version will not create a debug file. ***

  Create a new account, select "TorChat" for the protocol and when asked
for a username just enter your name (or anything else), this is NOT the
TorChat ID, its just an account name to tell different TorChat profiles
apart with a human readable name. It will create a config folder
%APPDATA%\torchat2_accountname\ for each account (profile). You can create 
as many accounts (profiles) as you want and use them all simultanously.

  This early alpha version of the plugin does only support instant
messaging and no file transfer, this will be implemented in later versions.
