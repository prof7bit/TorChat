To the Debian packager
======================

Build requirements
------------------
### Pidgin Plugin
* libpurple-dev
* fpc (>= 2.6.0) (Free Pascal 2.6 is the current stable release of fpc)

### Standalone version (2.0) (does not exist yet)
* gtk2
* fpc (>= 2.6.0)
* lazarus (>= 1.0.0), lcl (>= 1.0.0), lcl-utils (>= 1.0.0)

or alternatively

* qt4
* libqt4pas-dev
* fpc (>= 2.6.0)
* lazarus (>= 1.0.0), lcl (>= 1.0.0), lcl-utils (>= 1.0.0)

Runtime requirements
--------------------
### Pidgin Plugin
* tor
* libpurple

### Standalone version (2.0) (does not exist yet)
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
