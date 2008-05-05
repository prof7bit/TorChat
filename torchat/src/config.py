import sys, os
import ConfigParser

config_defaults = {
    ("tor", "tor_server") : "127.0.0.1",
    ("tor", "tor_server_socks_port") : 9050,
    ("tor", "tor_server_control_port") : 9051,
    ("tor_portable", "tor_server") : "127.0.0.1",
    ("tor_portable", "tor_server_socks_port") : 11109,
    ("tor_portable", "tor_server_control_port") : 11119,
    ("client", "own_hostname") : "0000000000000000",
    ("client", "listen_interface") : "127.0.0.1",
    ("client", "listen_port") : 11009,
    ("logging", "log_file") : "",
    ("logging", "log_level") : 0,
    ("gui", "notification_popup") : 1,
    ("gui", "notification_flash_window") : 1,
    ("gui", "open_main_window_hidden") : 0,
    ("gui", "open_chat_window_hidden") : 0,
    ("gui", "time_stamp_format") : "(%H:%M:%S)",
}

ICON_DIR = "icons" #can be absolute or relative to script dir

AUTHORS_ID = "utvrla6mjdypbyw6"
AUTHORS_NAME = "Bernd"

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
    global file_name
    global config
    dir = getDataDir()
    if not os.path.isdir(dir):
        os.mkdir(dir)        
    file_name = dir + "/torchat.ini"
    config = ConfigParser.ConfigParser()
    config.read(file_name)
    #try to read all known options once. This will add 
    #all the missing options to the config file 
    for section, option in config_defaults:
        get(section, option)
    
def writeConfig():
    fp = open(file_name, "w")
    config.write(fp)
    fp.close()

def get(section, option):
    if not config.has_section(section):
        config.add_section(section)
    if not config.has_option(section, option):
        value = config_defaults[section, option]
        set(section, option, value)
    return config.get(section, option, True)

def getint(section, option):
    value = get(section, option)
    try:
        return int(value)
    except:
        return 0

def set(section, option, value):
    if not config.has_section(section):
        config.add_section(section)
    config.set(section, option, value)
    writeConfig()

readConfig()
