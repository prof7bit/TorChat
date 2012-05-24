TorChat
=======

TorChat is a peer to peer instant messenger with a completely decentralized design, 
built on top of Tor's location hidden services, giving you extremely strong anonymity 
while being very easy to use without the need to install or configure anything.

Please see the wiki page: https://github.com/prof7bit/TorChat/wiki for more information.

Downloads
---------
Downloads can be found in the section "Download Packages" on the following page: 
https://github.com/prof7bit/TorChat/downloads

The current stable version is 0.9.9.551 (released 2012-02-06)  
Change log: https://raw.github.com/prof7bit/TorChat/master/torchat/src/changelog.txt

(End users: please ignore the two buttons "download zip" and "download tar.gz" on
the top of the download page, these are only source code snapshots for developers 
and as an end user you most likely simply want to download one of the packages.)

Branches: master and torchat_py and torchat2
--------------------------------------------
if you want to fork torchat (the python version), then please base your work 
on the torchat_py branch because the master branch will be used for something 
else in a few weeks. If you already forked or cloned the old master then please 
check out the torchat_py branch and rebase your work on this branch. Currently
master and torchat_py are still identical but master will soon be pointed to 
torchat2 which is a totally unrelated code base and this might cause you some 
confusion.

Pidgin Plugin
-------------
As some of you might have noticed already there is another branch silently growing 
in the dark in this repository, the torchat2 branch. This is intended to become a 
complete rewrite of TorChat in FreePascal. The first result from this (and one of 
the reasons for the rewrite) will be a plugin for the instant messenger Pidgin.
Don't hold your breath yet, at the moment it's still not much more than a mostly 
empty skeleton but once this is complete and working I will also write a standalone 
GUI version which will eventually (in a far away future) replace the current Python 
TorChat.

Currently most development time is going into the torchat2 branch. The existing user 
contributed localizations for TorChat will not be lost, since most of the new GUI 
will be almost identical I can simply write a script to transform all the existing 
language files into .po files.

