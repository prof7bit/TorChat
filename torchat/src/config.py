import sys, os

ICON_DIR = "icons" #can be absolute or relative to script dir

def getScriptDir():
    #must be called at least once before working dir is changed
    #because after that abspath won't work correctly anymore.
    #this is the reason why this function uses a cache.
    global _script_dir
    try:
        return _script_dir
    except:
        #first call, _script_dir not yet defined
        _script_dir = os.path.abspath(os.path.dirname(sys.argv[0]))
        return _script_dir

def isPortable():
    #if the file portable.txt exists in the same directory
    #then we know that we are running in portable mode.
    dir = getScriptDir()
    try:
        f = open(os.path.join(dir, "portable.txt"), "r")
        f.close()
        return True
    except:
        return False
    
def getDataDir():
    if isPortable():
        data_dir = getScriptDir()
    else:
        if "win" in sys.platform:
            appdata = os.environ["APPDATA"]
            data_dir = os.path.join(appdata, "torchat")
        else:
            home = os.path.expanduser("~")
            data_dir = os.path.join(home, ".torchat")
    
    if not os.path.exists(data_dir):
        os.mkdir(data_dir)
        
    return data_dir

def readConfig():
    pass

def writeConfig():
    pass

def get(name):
    pass

def set(name, value):
    writeConfig()

readConfig()

