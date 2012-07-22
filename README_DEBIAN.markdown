To the Debian packager
======================
There are not yet any special arrangements that make it easy to
package it for Debian but I like to add them and help making this
process as uncomplicated as possible. This also applies to other
package systems. I have not yet made a Debian source package myself,
I have read parts of the instructions, it seems there are a lot of
things to do to get it right, probably easy for someone who has done
this before but not for me unless someone helps me to do it.

You will probably make patches to the makefile or rearrange things,
please tell me what is needed to make this easier.

Build requirements
------------------
### Pidgin Plugin
* libpurple-dev
* fp-compiler 2.6 (Free Pascal 2.6 is the current stable release of fpc)

### Standalone version (2.0)
* gtk2
* fp-compiler 2.6
* lazarus 1.0

or alternatively

* qt4
* libqt4pas-dev
* fp-compiler 2.6
* lazarus 1.0

Runtime requirements
--------------------
### Pidgin Plugin
* tor
* libpurple

### Standalone version (2.0)
* tor
* gtk2

or alternatively

* tor
* qt4
* libqt4pas

Build system
-------------
This project does not use autotools or any other configuration tools,
it is built by a very simple handwritten makefile. The compilation
process is started with only one call of the compiler which will then
figure out on its own what needs to be built and how to link it.

There are some make targets in the top level makefile, the built
process should be started from the top level makefile. make purple
would build the plugin and put the .so file into the bin folder, make
gui would build the standalone version of TorChat and also put it
into bin. There is not yet a working install target. Also there are
still some icons missing for the plugin, libpurple wants icons of
several different resolutions in its pixmaps folder, the installer
would need to install them there.

There are also a few other targets in the makefile that are only for
experimental use and should not be relied upon. Packagers please send
me patches and/or suggestions so can better prepare it for automating
the Debian (and also others like rpm) package build process, tell me
what is needed and I could commit appropriate changes and needed
precautions so we can all make our lifes easier.

Building the plugin
-------------------
make purple

Installing the plugin
---------------------
Copy the file libpurpletorchat.so into the libpurple plugin directory
and also the torchat icons (the blue globe in several different
sizes) into libpurples appropriate pixmap folders (this is optional,
it will also run without the icons but a full install should include
them).

Please submit patches of how the makefile should do this in order to
make it properly work for creating a deban installer (do we need a
special make target for Debian or does the debian installer use the
install target?)
