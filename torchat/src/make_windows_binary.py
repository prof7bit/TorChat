# -*- coding: cp1252 -*-

from distutils.core import setup
import py2exe
import sys
import os
import time
import version
import wx

wx_path = wx.__path__[0]
py_path = "c:\python27"

print(wx_path)

app_name = "TorChat" 
app_descr = "Messenger on top of the Tor-network" 
app_version = version.VERSION
app_company = "Bernd Kreuß"
app_copyright = "© 2007 - 2010 Bernd Kreuß"

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
<trustInfo xmlns="urn:schemas-microsoft-com:asm.v3">
  <security>
    <requestedPrivileges>
      <requestedExecutionLevel
			level="asInvoker"
			uiAccess="false">
      </requestedExecutionLevel>
    </requestedPrivileges>
  </security>
</trustInfo>
<dependency>
  <dependentAssembly>
    <assemblyIdentity
          type="win32"
          name="Microsoft.VC90.CRT"
          version="9.0.30729.4148"
          processorArchitecture="x86"
          publicKeyToken="1fc8b3b9a1e18e3b">
    </assemblyIdentity>
  </dependentAssembly>
</dependency>
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
            "bundle_files": 1, # create singlefile exe
            #"bundle_files": 3, # windows 9x needs this
            "compressed": 1, # compress the library archive
            "excludes": [],
            "dll_excludes": ["w9xpopen.exe", "MSVCP90.dll"]
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

def deleteDirectory(dir):
    if sys.getwindowsversion()[0] == 4:
        #windows 98
        os.system("deltree /Y %s" % dir)
    else:
        #all other windows
        os.system("rmdir /S /Q %s" % dir)

os.system("copy %s\gdiplus.dll dist" % wx_path)
#os.system("copy %s\msvcp71.dll dist" % wx_path)
os.system("copy %s\unicows.dll dist" % py_path) #needed for win9x
os.system("upx dist\\*.*")
os.system("copy dist\\*.* ..\\bin")
deleteDirectory("dist")
deleteDirectory("build")

time.sleep(2)
