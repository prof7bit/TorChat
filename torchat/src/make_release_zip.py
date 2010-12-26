# this is RUN ON WINDOWS and
# needs some tools installed:
# python 2.7.x
# zip.exe (must be in the PATH)
# upx.exe (must be in the PATH)
# pyinstaller latest trunk (in c:\pyinst\)
# run make_windows_binary.py from within the src directory

import version
import os

def zipSource(zip_filename, zip_options):
    os.system("zip %s %s doc\\*" % (zip_options, zip_filename))
    os.system("zip %s %s src\\* -x torchat_incoming* *offline.txt *.ini *.pyc *.pyo *.log src/buddy-list.txt src/test*" % (zip_options, zip_filename))
    os.system("zip %s %s src\\icons\\*" % (zip_options, zip_filename))
    os.system("zip %s %s src\\Tor\\tor.sh" % (zip_options, zip_filename))
    os.system("zip %s %s src\\Tor\\torrc.txt" % (zip_options, zip_filename))
    os.system("zip %s %s src\\translations\\* -x *.pyc *.pyo" % (zip_options, zip_filename))
    os.system("zip %s %s src\\SocksiPy\\* -x *.pyc *.pyo" % (zip_options, zip_filename))

def zipWindowsBin(zip_filename, zip_options):
    os.system("zip %s %s bin\\* -x bin\\buddy-list.txt *.log *.ini" % (zip_options, zip_filename))
    os.system("zip %s %s bin\\Tor\\* -x *.log" % (zip_options, zip_filename))
    os.system("zip %s %s bin\\icons\\*" % (zip_options, zip_filename))

def clean(dir):
    print "cleaning %s" % dir
    os.system ("del %s\\*.pyo" % dir)
    os.system ("del %s\\*.pyc" % dir)
    os.system ("del %s\\*.log" % dir)
    os.system ("del %s\\*.tmp" % dir)
    os.system ("del %s\\*~" % dir)
    os.system ("del %s\\DEADJOE" % dir)

dir = os.getcwd()
try:
    os.mkdir("../release")
except:
    pass

# clean up the src directory
clean(".")
clean("translations")
clean("SocksiPy")

# build the .exe with pyinstaller
os.system("c:\\pyinst\\Build.py torchat_windows.spec")
os.system("copy /Y dist\\torchat.exe ..\\bin")

#copy some files
os.system("copy /Y portable.txt ..\\bin")
os.system("copy /Y Tor\\torrc.txt ..\\bin\\Tor")

os.chdir("..")
zip_options = "-9"

zip_filename = "release\\torchat-windows-%s.zip" % version.VERSION
os.system("del %s" % zip_filename)
zipWindowsBin(zip_filename, zip_options)
zipSource(zip_filename, zip_options)

zip_filename = "release\\torchat-source-%s.zip" % version.VERSION
os.system("del %s" % zip_filename)
zipSource(zip_filename, zip_options)

