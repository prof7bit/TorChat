#!/usr/bin/python

import os
import version

TMP_ROOT = "debroot"

dirs = ["DEBIAN",
        "usr",
        "usr/bin",
        "usr/share",
        "usr/share/torchat",
        "usr/share/torchat/icons",
        "usr/share/torchat/SocksiPy",
        "usr/share/torchat/translations",
        "usr/share/torchat/Tor",
        ]

files = [("translations/*.py", "usr/share/torchat/translations"),
         ("translations/*.txt", "usr/share/torchat/translations"),
         ("icons/*", "usr/share/torchat/icons"),
         ("SocksiPy/*", "usr/share/torchat/SocksiPy"),
         ("Tor/tor.sh", "usr/share/torchat/Tor"),
         ("Tor/torrc.txt", "usr/share/torchat/Tor"),
         ("torchat.py", "usr/share/torchat"),
         ("config.py", "usr/share/torchat"),
         ("version*.py", "usr/share/torchat"),
         ("tc_*.py", "usr/share/torchat"),
         ("dlg*.py", "usr/share/torchat"),
         ("LICENSE", "usr/share/torchat"),
         ]

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
""" % version.VERSION_ONLY

start_script = """#!/bin/sh

cd /usr/share/torchat
./torchat.py
"""

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

os.system("rm -r %s" % TMP_ROOT)
mkdir("") #create empty TMP_ROOT

for dir in dirs:
    mkdir(dir)

for file, dest in files:
    copy(file, dest)

create(control_file, "DEBIAN/control")
create(start_script, "usr/bin/torchat")
chmod(755, "usr/bin/torchat")
chmod(755, "usr/share/torchat/torchat.py")
chmod(755, "usr/share/torchat/Tor/tor.sh")

os.system("dpkg -b %s %s" % (TMP_ROOT, "torchat-%s.deb" % version.VERSION_ONLY))
#os.system("rm -r %s" % TMP_ROOT)