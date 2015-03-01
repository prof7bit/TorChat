# Installation / Usage Instructions for TorChat on Mac OS X #

***WARNING: THESE INSTRUCTIONS CURRENTLY HAVE BEEN TESTED ONLY ON MAC OS X
   VERSION 10.8.  ALL OTHER VERSIONS USE CAUTION.***

## Download and Install Tor ##

Unfortunately, TorChat currently requires a `tor` executable in order to
function.  This severely complicates the install, as the Tor project
tries to helpfully package Tor into a one-stop bundle for browsing the
web.

There are three ways in which to install Tor on Mac OS X: downloading
a pre-built image from upstream TorProject, installing through
[MacPorts](https://www.macports.org/), or building from upstream
TorProject source.

### Web Package Download ###

If you're not running MacPorts (if you don't know what that is, you're
not running it), go
[here](https://www.torproject.org/download/download.html.en#apple) and
download either the i686 version (32-bit) or x86_64 (64-bit) version.
If you're not sure which one you need, go with the i686 (32-bit)
version.

This will create a file in your downloads folder with a name like
**TorBrowser-2.3.25-10-osx-x86_64-en-US.zip**.  If it doesn't create
an application with a name like **TorBrowser_en-US** as well,
double-click on the aforementioned file.  An application named
**TorBrowser_en-US** should now appear.  Move the **TorBrowser_en-US**
file into your Applications folder.

The key to installing this way is that there is a `tor` executable
within the **TorBrowser_en-US** application.

### MacPorts ###

If you already have MacPorts installed, you can simply use the command:

    $ sudo port install tor torsocks

and Tor will be installed, regardless of platform.

This method creates a `tor` executable wherever your MacPorts is set
to install to (typically */opt/local/tor*).

### Building from Upstream Source ###

Source can be found
[here](https://www.torproject.org/download/download.html.en#source).
I recommend using the stable version.  Build instructions can also be
found in that location.

## Download and Install Python 2.x ##

If you're using Mac OS X 10.8, you're in luck!  Python 2.7 comes by
default.

TODO: other versions of Mac OS X.  Need at least Python 2.6 for
wxPython (unless MacPorts deals with earlier versions).

## Download and Install wxPython 2.x ##

Which wxPython you use will depend on which Python you are using.  If
you installed Python from MacPorts, you should also install wxPython
from MacPorts; if you downloaded a binary of Python or used the
built-in Mac OS Python, you should download a binary of wxPython; and
if you want to build things from source, follow the instructions below.

### Download a Pre-Built Binary ###

Pre-built binaries can be found
[here](http://wxpython.org/download.php#stable).  I cannot think of a
reason to use the binaries with "ansi" in their names.  You should use
the version of wxPython appropriate to your Python installation (so,
for example, if you're using Python 2.7, you will want the link
**wxPython2.8-osx-unicode-py2.7**, or for Python 2.6, you would want
**wxPython2.8-osx-unicode-py2.6**).

When you download the appropriate wxPython, a disk image should be
mounted (with a name like
**wxPython2.8-osx-unicode-2.8.12.1-universal-py27**).  If it is not,
check your Downloads folder for a file with name like
**wxPython2.8-osx-unicode-2.8.12.1-universal-py2.7.dmg** and
double-click it.

In the disk image, there will be an installer named something like
**wxPython2.8-osx-unicode-universal-py2.7.pkg**.  Double-click it, and
the installer will begin.

Click "Continue" to begin the installation, read the Software License
Agreement (click "Continue", then "Agree" when done if and only if you
agree to abide by it), then click "Install".  (You may be prompted for
a password at this point.)  When the installation finishes, click
"close" on the installer.

### MacPorts ###

(TODO: once bug is resolved, write up how to do this)

### Building from Source ###

It's important to note that the benefit here is not from building
Python binaries (because Python binaries aren't a thing), but from
building wxWidgets from source.  Anyway, take a deep breath,
reconsider your life choices, and follow the instructions
[here](http://wxpython.org/builddoc.php).

## TorChat ##

For purposes of this guide, we will install torchat-source, which is a
source release of TorChat.  It is also totally workable to substitute
another version from git instead; however, it is assumed that if you
do this, you know what you are doing.

Releases can be downloaded
[here](https://github.com/prof7bit/TorChat/downloads).  You probably
want to download the highest entry whose name resembles
**torchat-source-0.9.9.553.zip**.

This will create a file in your Downloads folder with name something
like **torchat-source-0.9.9.553.zip**.  If it does not also create a
folder with name like **torchat-source-0.9.9.553**, double click on
the aforementioned file and it should appear.

Okay, so now we're going to launch TorChat.  Take a deep breath, and
let's make a launcher.  Open TextEdit (or the text editor of your
choice).  Ensure the document is plain text by going to the "Format"
menu.  If you see an option "Make Plain Text", choose it; otherwise,
your document is already plain text so don't select anything.  Type
the following:

    #!/bin/bash
    PATH=/Applications/TorBrowser_en-US.app/Contents/MacOS:$PATH
    arch -i386 python ~/Downloads/torchat-source-*/src/torchat.py &
    exit

Be sure to leave a blank line at the end of the file, then save it on
the Desktop (or Applications folder or anywhere you like) with a
clever name (I like **TorChat Launcher**, myself).  When saving, be
sure to uncheck the box that says "If no extension is provided, use
'.txt'.", and also ensure that there are no periods ('.') in the name
that you save it as.

Select the file you just made in the finder.  Then open
**Terminal.app** (it can be found in the **Utilities** folder in your
**Applications** folder).  Type `chmod +x ` (with a space at the end)
and then, *before hitting enter*, drag your TorChat launcher into the
Terminal.app window.  Then press the Return key.  You should see the
icon of your launcher change in the Finder.

Simply double-click on the launcher to launch TorChat.  After a
moment, another window will open: your TorChat contact list!

## Upgrading components ##

All components of the setup we just made are upgradable.  Here's how:

### Tor ###

If you installed Tor through the TorBrowser pre-built binary, update
as instructed by TorProject.  Your TorChat launcher will continue to
work.

If you installed Tor through MacPorts, `sudo port upgrade tor
torsocks` will automatically update you, and your TorChat launcher
will continue to work.

If you built Tor from source, you will need to rebuild and re-install
to the same location you did initially in order for the TorChat
launcher to continue to work.

### Python ###

If you used built-in Python, Python will update through Software
Update/App Store.  The launcher script will continue to function.

If you installed Python from MacPorts, `sudo port upgrade outdated`
will automatically update you, and your TorChat launcher will continue
to work.

If you downloaded a pre-built binary of Python, just download and run
the appropriate newer installer.  As long as it installs to the same
location as it did the first time, your TorChat launcher will continue
to work.

### wxPython ###

If you downloaded a pre-built binary of wxPython, just download and
run the appropriate newer installer.  As long as it installs to the
same location as it did the first time, your TorChat launcher will
continue to work.

If you installed wxPython from MacPorts, `sudo port upgrade outdated`
will automatically update you, and your TorChat launcher will continue
to work.

If you built wxPython from source, you will need to build the new
version and install it to the same location you did initially.  If you
do so, your TorChat launcher will continue to work.

### TorChat ###

When a new TorChat is released, a new torchat-source entry will appear
on [the Downloads
page](https://github.com/prof7bit/TorChat/downloads).  Before
downloading, remove the old torchat-source files from your Downloads
folder, then follow instructions as if you were installing TorChat
above.  You do not need to recreate the launcher; it will continue to
work.
