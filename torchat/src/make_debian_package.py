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

dirs = ["DEBIAN",
        "usr",
        "usr/bin",
        "usr/share",
        "usr/share/applications",
        "usr/share/pixmaps",
        "usr/share/pixmaps/torchat",
        "usr/share/torchat",
        "usr/share/torchat/SocksiPy",
        "usr/share/torchat/translations",
        "usr/share/torchat/Tor",
        ]

files = [("translations/*.py", "usr/share/torchat/translations"),
         ("translations/*.txt", "usr/share/torchat/translations"),
         ("icons/*", "usr/share/pixmaps/torchat"),
         ("SocksiPy/__init__.py", "usr/share/torchat/SocksiPy"),
         ("SocksiPy/socks.py", "usr/share/torchat/SocksiPy"),
         ("SocksiPy/BUGS", "usr/share/torchat/SocksiPy"),
         ("SocksiPy/LICENSE", "usr/share/torchat/SocksiPy"),
         ("SocksiPy/README", "usr/share/torchat/SocksiPy"),
         ("Tor/tor.sh", "usr/share/torchat/Tor"),
         ("Tor/torrc.txt", "usr/share/torchat/Tor"),
         ("torchat.py", "usr/share/torchat"),
         ("config.py", "usr/share/torchat"),
         ("version*.py", "usr/share/torchat"),
         ("tc_*.py", "usr/share/torchat"),
         ("dlg*.py", "usr/share/torchat"),
         ("LICENSE", "usr/share/torchat"),
         ]

postinst = """#!/bin/sh

cd /usr/share/torchat
echo creating symbolic links...
ln -s /usr/share/pixmaps/torchat icons

echo compiling to bytecode...
python -OOc "import torchat"

echo done.
"""

prerm = """#!/bin/sh

rm -rf  /usr/share/torchat

echo done
"""

start_script = """#!/bin/sh

cd /usr/share/torchat
./torchat.py
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
    path = os.path.join(TMP_ROOT, dir)
    os.system("mkdir %s" % path)
    chmod(755, dir)

def copy(file, dest):
    dest_full = os.path.join(TMP_ROOT, dest)
    os.system("cp %s %s" % (file, dest_full))
    chmod(644, os.path.join(dest, os.path.basename(file)))

def create(content, dest):
    dest_full = os.path.join(TMP_ROOT, dest)
    os.system("echo '%s' > %s" % (content, dest_full))
    chmod(644, dest)

os.system("rm -rf %s" % TMP_ROOT)
mkdir("") #create empty TMP_ROOT

for dir in dirs:
    mkdir(dir)

for file, dest in files:
    copy(file, dest)

create(control_file, "DEBIAN/control")
create(postinst, "DEBIAN/postinst")
chmod(755, "DEBIAN/postinst")
create(prerm, "DEBIAN/prerm")
chmod(755, "DEBIAN/prerm")

create(desktop, "usr/share/applications/torchat.desktop")

create(start_script, "usr/bin/torchat")
chmod(755, "usr/bin/torchat")
chmod(755, "usr/share/torchat/torchat.py")
chmod(755, "usr/share/torchat/Tor/tor.sh")

os.system("dpkg -b %s %s" % (TMP_ROOT, deb_name))
os.system("rm -rf %s" % TMP_ROOT)