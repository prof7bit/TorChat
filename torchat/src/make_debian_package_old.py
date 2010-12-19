#!/usr/bin/python

import os
import version

TMP_ROOT = "debroot"

version = version.VERSION_ONLY
deb_name = "torchat-%s.deb" % version

control_file = """Package: torchat
Version: %s
Section: internet
Priority: optional
Architecture: all
Essential: no
Depends: tor, python2.5, python-wxgtk2.8
Pre-Depends: python2.5
Maintainer: Bernd Kreuss <prof7bit@cooglemail.com>
Provides: torchat
Description: Instant Messenger for Tor
""" % version

copyright="""TorChat is copyright (C) 2007, 2008 Bernd Kreuss <prof7bit@googlemail.com>

TorChat is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

TorChat is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License with
the Debian GNU/Linux distribution in file /usr/share/common-licenses/GPL;
if not, write to the Free Software Foundation, Inc., 59 Temple Place,
Suite 330, Boston, MA  02111-1307  USA

On Debian systems, the complete text of the GNU General Public
License, version 3, can be found in /usr/share/common-licenses/GPL-3.

--

The files in /usr/lib/torchat/SocksiPy are 
Copyright 2006 Dan-Haim. All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:
1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.
3. Neither the name of Dan Haim nor the names of his contributors may be used
   to endorse or promote products derived from this software without specific
   prior written permission.
   
THIS SOFTWARE IS PROVIDED BY DAN HAIM "AS IS" AND ANY EXPRESS OR IMPLIED
WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
EVENT SHALL DAN HAIM OR HIS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA
OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMANGE.

--

The files in /usr/share/pixmaps/torchat are partially derived from
the gajim buddy status icons (c) The Gajim Team (GNU-GPL) 
"""

dirs = ["DEBIAN",
        "usr",
        "usr/bin",
        "usr/share",
        "usr/share/doc",
        "usr/share/doc/torchat",
        "usr/share/doc/torchat/html",
        "usr/share/applications",
        "usr/share/pixmaps",
        "usr/share/pixmaps/torchat",
        "usr/lib",
        "usr/lib/torchat",
        "usr/lib/torchat/SocksiPy",
        "usr/lib/torchat/translations",
        "usr/lib/torchat/Tor",
        ]

files = [("translations/*.py", "usr/lib/torchat/translations"),
         ("translations/*.txt", "usr/lib/torchat/translations"),
         ("icons/*", "usr/share/pixmaps/torchat"),
         ("SocksiPy/__init__.py", "usr/lib/torchat/SocksiPy"),
         ("SocksiPy/socks.py", "usr/lib/torchat/SocksiPy"),
         ("SocksiPy/BUGS", "usr/lib/torchat/SocksiPy"),
         ("SocksiPy/LICENSE", "usr/lib/torchat/SocksiPy"),
         ("SocksiPy/README", "usr/lib/torchat/SocksiPy"),
         ("Tor/tor.sh", "usr/lib/torchat/Tor"),
         ("Tor/torrc.txt", "usr/lib/torchat/Tor"),
         ("torchat.py", "usr/lib/torchat"),
         ("config.py", "usr/lib/torchat"),
         ("version*.py", "usr/lib/torchat"),
         ("tc_*.py", "usr/lib/torchat"),
         ("dlg*.py", "usr/lib/torchat"),
         ("LICENSE", "usr/share/doc/torchat"),
         ("changelog.txt", "usr/share/doc/torchat"),
         ("../doc/howto_second_instance.html", "usr/share/doc/torchat/html"),
         ]

postinst = """#!/bin/sh

cd /usr/lib/torchat
echo creating symbolic links...
ln -s /usr/share/pixmaps/torchat icons

echo compiling to bytecode...
python -OOc "import torchat"

echo TorChat installed.
"""

prerm = """#!/bin/sh

rm -rf  /usr/lib/torchat

"""

start_script = """#!/bin/sh

cd /usr/lib/torchat
./torchat.py $*
"""

desktop = """[Desktop Entry]
Categories=Network;InstantMessaging;
Name=TorChat Instant Messenger
Comment=Anonymous Instant Messenger for Tor
Version=%s
Exec=/usr/bin/torchat
Path=/usr/lib/torchat/
Icon=/usr/share/pixmaps/torchat/torchat.png
StartupNotify=true
StartupWMClass=TorChat
Terminal=false
Type=Application
""" % version

def chmod(mode, dest):
    dest_full = os.path.join(TMP_ROOT, dest)
    os.system("chmod %s %s" % (mode, dest_full))

def mkdir(dir):
    print "creating directory %s" % dir
    path = os.path.join(TMP_ROOT, dir)
    os.system("mkdir %s" % path)
    chmod(755, dir)

def copy(file, dest):
    print "copying %s to %s" % (file, dest)
    dest_full = os.path.join(TMP_ROOT, dest)
    os.system("cp %s %s" % (file, dest_full))
    chmod(644, os.path.join(dest, os.path.basename(file)))

def create(content, dest):
    print "creating file %s" % (dest)
    dest_full = os.path.join(TMP_ROOT, dest)
    os.system("echo '%s' > %s" % (content, dest_full))
    chmod(644, dest)

print "creating temporary root dir"
os.system("rm -rf %s" % TMP_ROOT)
mkdir("") #create empty TMP_ROOT

for dir in dirs:
    mkdir(dir)

for file, dest in files:
    copy(file, dest)

create(control_file, "DEBIAN/control")
create(copyright, "DEBIAN/copyright")
create(copyright, "usr/share/doc/torchat/copyright")
create(postinst, "DEBIAN/postinst")
chmod(755, "DEBIAN/postinst")
create(prerm, "DEBIAN/prerm")
chmod(755, "DEBIAN/prerm")


create(desktop, "usr/share/applications/torchat.desktop")

create(start_script, "usr/bin/torchat")
chmod(755, "usr/bin/torchat")
chmod(755, "usr/lib/torchat/torchat.py")
chmod(755, "usr/lib/torchat/Tor/tor.sh")

#now build the package using dpkg -b
os.system("fakeroot dpkg -b %s %s" % (TMP_ROOT, deb_name))

os.system("mv %s ../release" % deb_name)
os.system("rm -rf %s" % TMP_ROOT)
print "done."
