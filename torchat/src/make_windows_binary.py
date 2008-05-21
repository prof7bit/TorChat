# -*- coding: cp1252 -*-

from distutils.core import setup
import py2exe
import sys
import os
import time
import version
import wx

wx_path = wx.__path__[0]
py_path = "c:\python25"

app_name = "TorChat" 
app_descr = "Messenger on top of the TOR-network" 
app_version = version.VERSION
app_company = "Bernd Kreuß"
app_copyright = "© 2007 Bernd Kreuß"

script_name = "torchat.py"
icon_name = "icons/torchat.ico"

# If run without args, build executables, in quiet mode.
if len(sys.argv) == 1:
    sys.argv.append("py2exe")
    sys.argv.append("-q")

manifest = '''
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<assembly xmlns="urn:schemas-microsoft-com:asm.v1" manifestVersion="1.0">
<assemblyIdentity
    version="5.0.0.0"
    processorArchitecture="x86"
    name="%(name)s"
    type="win32"
/>
<description>%(descr)s</description>
<dependency>
    <dependentAssembly>
        <assemblyIdentity
            type="win32"
            name="Microsoft.Windows.Common-Controls"
            version="6.0.0.0"
            processorArchitecture="X86"
            publicKeyToken="6595b64144ccf1df"
            language="*"
        />
    </dependentAssembly>
</dependency>
</assembly>
'''

setup(
    options = {
        "py2exe": {
#            "bundle_files": 1, # create singlefile exe
            "bundle_files": 3, # windows 9x needs this
            "compressed": 1, # compress the library archive
            "excludes": [],
            "dll_excludes": ["w9xpopen.exe"]
        }
    },
    zipfile = None, # append zip-archive to the executable.
    
    #console = [
    windows = [
        {
            'description': app_descr,
            'version': app_version,
            'company_name': app_company,
            'copyright': app_copyright,
            'name': app_name,
            
            'script': script_name,
            
            'icon_resources': [(1, icon_name)],
            'other_resources': [(24,1,manifest % dict(name=app_name, descr=app_descr))]
        }
    ]
)

os.system("copy %s\gdiplus.dll dist" % wx_path)
os.system("copy %s\msvcp71.dll dist" % wx_path)
os.system("copy %s\unicows.dll dist" % py_path) #needed for win9x
os.system("upx dist\\*.*")

os.system("copy dist\\*.* ..\\bin")
os.system("rmdir /S /Q dist build")

time.sleep(2)
