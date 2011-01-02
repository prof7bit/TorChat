# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# !!!                                          !!!
# !!!  THIS SCRIPT WILL DELETE THE BIN FOLDER  !!!
# !!!                                          !!!
# !!!  and all its contents and then it will   !!!
# !!!      create a new one from scratch.      !!!
# !!!          You have been warned.           !!!
# !!!                                          !!!
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# this is RUN ON WINDOWS and
# needs some tools installed:
#
#   python 2.7.x
#   wxpython 2.8 (the unicode version)
#   upx.exe (must be in the PATH)
#   pythonwin extensions (needed by pyinstaller)
#   pyinstaller latest trunk (in c:\pyinst\)
#
#   also put a copy of tor.exe into src/Tor
#
# run make_windows_binary.py from within the src directory and
# you should end up with the zip files in the ../release folder
# and a fresh bin folder to test the newly created torchat.exe

import version
import os
import sys
import shutil
import zlib
import zipfile
import glob

# pyinstaller must have been configured aleady
PYINSTALLER_PATH = "c:\\pyinst"


# ----------------------------------------------------
# first some useful helpers to zip, copy, delete files
# ----------------------------------------------------

class MyZip(zipfile.ZipFile):
    def add(self, pattern):
        for name in glob.glob(pattern):
            print "adding %s" % name
            self.write(name, name, zipfile.ZIP_DEFLATED)

def unlink(patterns):
    """delete files. takes a list of names or patterns"""
    for pattern in patterns:
        print "trying to unlink %s" % pattern
        for name in glob.glob(pattern):
            try:
                os.unlink(name)
                print "unlinked %s" % name
            except:
                print "could not unlink %s" % name

def copy(patterns):
    """copy files. takes a list of tuples of which
    the first element is a name or pattern and the
    second one is is the destination directory"""
    for pattern, dest_dir in patterns:
        print "trying to copy %s to %s" % (pattern, dest_dir)
        for name in glob.glob(pattern):
            try:
                shutil.copy(name, dest_dir)
                print "copied %s" % name
            except:
                print "could not copy %s" % name

def zip(zipfile_name, patterns):
    """add files to the archive zipfile_name.
    Takes a list of filenames or wildcard patterns"""
    print "\nopening %s" % zipfile_name
    archive = MyZip(zipfile_name, "a")
    zlib.Z_DEFAULT_COMPRESSION = zlib.Z_BEST_COMPRESSION
    for pattern in patterns:
        archive.add(pattern)
    print "closing %s" % zipfile_name
    archive.close()

def zipSrc(zipfile_name):
    zip(zipfile_name, [
        "doc\\*",
        "src\\*.py",
        "src\\*.spec",
        "src\\*.bat",
        "src\\portable.txt",
        "src\\changelog.txt",
        "src\\LICENSE",
        "src\\icons\\*",
        "src\\Tor\\tor.sh",
        "src\\Tor\\torrc.txt",
        "src\\translations\\*.txt",
        "src\\translations\\lang_*.py",
        "src\\translations\\:__init__.py",
        "src\\SocksiPy\\*"
    ])

def zipBin(zipfile_name):
    zip(zipfile_name, [
        "bin\\*",
        "bin\\Tor\\*",
        "bin\\icons\\*"
    ])

def clean(folder):
    print "\ncleaning %s" % folder
    unlink([
        "%s\\*.pyo" % folder,
        "%s\\*.pyc" % folder,
        "%s\\*.log" % folder,
        "%s\\*.tmp" % folder,
        "%s\\*~" % folder,
        "%s\\*offline*" % folder,
        "%s\\DEADJOE" % folder
    ])


# ------------------
# and here it begins
# ------------------

try:
    os.mkdir("../release")
except:
    pass

if not os.path.exists("Tor\\tor.exe"):
    print "!!! need a copy of tor.exe in the src\\Tor folder"
    sys.exit()

# clean up the src directory
clean(".")
clean("translations")
clean("SocksiPy")
#os.system("rmdir /S /Q dist")
#os.system("rmdir /S /Q build")

# build the .exe with pyinstaller
# the following will result in a command line like this::
# "c:\Python27\python.exe c:\pyinst\Build.py torchat_windows.spec"
cmd = sys.executable \
    + " " + os.path.join(PYINSTALLER_PATH, 'Build.py') \
    + " torchat_windows.spec"

print "\n\n" + cmd
os.system(cmd)

# check for success
if not os.path.exists("dist\\torchat.exe"):
    print "!!! pyinstaller did not run properly"
    sys.exit()

# now we have all files we need. We now put together
# the contents of the bin folder exactly as it will be
# in the final zip file. First make an empty bin folder.
print "\n\nputting together the contents of the bin folder"
os.system("rmdir /S /Q ..\\bin")
os.system("mkdir ..\\bin")
os.system("mkdir ..\\bin\\icons")
os.system("mkdir ..\\bin\\Tor")

#now copy the needed files to bin
copy([
    ("dist\\torchat.exe", "..\\bin"),
    ("portable.txt", "..\\bin"),
    ("Tor\\tor.exe", "..\\bin\\Tor"),
    ("Tor\\torrc.txt", "..\\bin\\Tor"),
    ("icons\\*.png", "..\\bin\\icons"),
    ("icons\\*.ico", "..\\bin\\icons"),
])

print "\n\ncreating the zip files"
os.chdir("..")
bin_zip_filename = "release\\torchat-windows-%s.zip" % version.VERSION
src_zip_filename = "release\\torchat-source-%s.zip" % version.VERSION
unlink([bin_zip_filename, src_zip_filename])

# torchat-windows-x.x.x.x.zip
zipBin(bin_zip_filename)
zipSrc(bin_zip_filename)

# torchat-source-x.x.x.x.zip
zipSrc(src_zip_filename)

