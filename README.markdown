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

Beer
====
If you want to buy me a beer (very much appreciated) you can do so via

* Bitcoin: 18hmynLnHC44XiGiiPqfuTL3M4xPeJ5KqW
* PayPal: 7bit@arcor.de

Please note that TorChat is produced *independently* from the Tor® anonymity software. I am not involved with or sponsored at all by torproject.org. TorChat is simply making use of the Tor® client software and the Windows version comes bundled with original Tor binaries but TorChat itself is a completely separate project developed by completely different people,. If you instead want to buy the developers of Tor® from torproject.org a beer, (they deserve it even more than me and without Tor software, my little program would not have been possible) then please consider doing so here:

* https://www.torproject.org/donate/donate.html.en
