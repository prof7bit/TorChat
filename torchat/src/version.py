NAME = "TorChat"
VERSION_MAJOR = "0.9.9"
VERSION_SVN = 552
EXPERIMENTAL = False

# here used to be code that scanned the .svn dirs for the revision
# number, generated a cache file if no svn was found, etc.
# now I'm on git and for simplicity just manually increment 
# VERSION_SVN after each official release.

VERSION = VERSION_MAJOR + "." + str(VERSION_SVN)
VERSION_ONLY = VERSION
if EXPERIMENTAL:
    VERSION += "-experimental"
