If you use the Ubuntu Repositories then please update
=====================================================
There has been an ugly bug in the Debian and Ubuntu versions of
TorChat (the ones from their repositories, not the original .deb
file from here) that would have broadcasted the local alias you
gave to a buddy to that buddy in the version message. This bug has 
finally been fixed on 22 Jun 2012. Please update & upgrade (using 
synaptic or apt-get) to get these updated packages. The fixed 
packages are

* Oneiric: 0.9.9.534-1ubuntu0.1
* Precise: 0.9.9.550-2~build0.12.04.1

If you have one of the versions above you are safe. If you have 
installed the .deb file from github you are also safe. If you have
and older version (especially 550-1 or 534-1) then you should update.


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
