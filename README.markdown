Don't use the Ubuntu/Mint Repositories
======================================
...until further notice.  Please use only the original .deb 
from github. Ubuntu version is broadcasting private information
in the version field. They tried to fix something (that was
not broken in the first place) regarding the version number
and broke the innermost workings of the client and the protocol 
because they confused two similar sounding variable names.

Update: After contacting the Debian maintainer it has been
fixed in the Debian repository very fast but Ubuntu is still
not reacting to anything, there does not seem to be anybody 
home during summer...


TorChat2
========
If you are looking for **TorChat 0.9.9.xxx** (the original Python
implementation) then please switch to the **torchat_py** branch.
For downloads of the latest versions please see the downloads
section: https://github.com/prof7bit/TorChat/downloads

Please don't use the master branch at all, it still points to 
torchat_py but will soon be moved, if you have own work based on 
master then please rebase it to torchat_py.

This branch **torchat2** is a rewrite from scratch, using Lazarus +
Free Pascal. This will make it easier to create plugins for 
existing IM applications and also allows to easily generate code 
for a wider range of platforms, especially mobile devices 
like Android and iPhone.

At the moment this branch is mainly used to develop a plugin
for libpurple (the Pidgin-IM) which will be the first thing
that you can expect to come out of this branch, later (once
this works) there will also be a standalone GUI that will 
completely replace the current Python implementation (Don't 
worry, the python version will stay here in the torchat_py
branch, it is the reference implementation of the protocol, 
I'm still using it myself as protocol documentation and to 
test other implementations against it, it will not go away)
