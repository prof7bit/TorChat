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
Depends: tor
Pre-Depends: python2.5
Maintainer: Bernd Kreuss <prof7bit@cooglemail.com>
Provides: torchat
Description: Instant Messenger for Tor
""" % version

copyright="""TorChat is copyright (C) 2007-2010 Bernd Kreuss <prof7bit@googlemail.com>

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

The files in /usr/share/torchat/SocksiPy are 
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
        "usr/share/torchat",
        "usr/share/torchat/Tor",
        ]

files = [("icons/*", "usr/share/pixmaps/torchat"),
         ("Tor/tor.sh", "usr/share/torchat/Tor"),
         ("Tor/torrc.txt", "usr/share/torchat/Tor"),
         ("dist/torchat", "usr/share/torchat"),
         ("LICENSE", "usr/share/torchat"),
         ("changelog.txt", "usr/share/doc/torchat"),
         ("../doc/howto_second_instance.html", "usr/share/doc/torchat/html"),
         ]

postinst = """#!/bin/sh

cd /usr/share/torchat
echo creating symbolic links...
ln -s /usr/share/pixmaps/torchat icons
"""

prerm = """#!/bin/sh

rm -rf  /usr/share/torchat
rm -rf  /usr/share/pixmaps/torchat
rm -rf  /usr/share/doc/torchat

"""

start_script = """#!/bin/sh

cd /usr/share/torchat
./torchat $*
"""

desktop = """[Desktop Entry]
Categories=Network;InstantMessaging;
Name=TorChat Instant Messenger
Comment=Anonymous Instant Messenger for Tor
Version=%s
Exec=/usr/bin/torchat
Path=/usr/share/torchat/
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


print "running pyinstaller to create binary"
os.system("python ~/pyinstaller/Build.py torchat_linux.spec")

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
chmod(755, "usr/share/torchat/torchat")
chmod(755, "usr/share/torchat/Tor/tor.sh")

#now build the package using dpkg -b
os.system("fakeroot dpkg -b %s %s" % (TMP_ROOT, deb_name))

os.system("mv %s ../release" % deb_name)
os.system("rm -rf %s" % TMP_ROOT)
print "done."
