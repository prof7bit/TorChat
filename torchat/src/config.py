# -*- coding: UTF-8 -*-

##############################################################################
#                                                                            #
# Copyright (c) 2007-2010 Bernd Kreuss <prof7bit@gmail.com>                  #
#                                                                            #
# This program is licensed under the GNU General Public License V3,          #
# the full source code is included in the binary distribution.               #
#                                                                            #
# Included in the distribution are files from other open source projects:    #
# - TOR Onion Router (c) The Tor Project, 3-clause-BSD                       #
# - SocksiPy (c) Dan Haim, BSD Style License                                 #
# - Gajim buddy status icons (c) The Gajim Team, GNU GPL                     #
#                                                                            #
##############################################################################

import sys, os
import locale
import ConfigParser
import traceback
import inspect
import translations
import shutil

def isWindows():
    return sys.platform.startswith("win")

if isWindows():
    import ctypes

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
    ("files", "temp_files_in_data_dir") : 1,
    ("files", "temp_files_custom_dir") : "",
    ("gui", "language") : "en",
    ("gui", "notification_popup") : 1,
    ("gui", "notification_method") : "generic",
    ("gui", "notification_flash_window") : 1,
    ("gui", "open_main_window_hidden") : 0,
    ("gui", "open_chat_window_hidden") : 0,
    ("gui", "time_stamp_format") : "(%H:%M:%S)",
    ("gui", "color_time_stamp") : "#808080",
    ("gui", "color_nick_myself") : "#0000c0",
    ("gui", "color_nick_buddy") : "#c00000",
    ("gui", "color_text_back") : "#ffffff",
    ("gui", "color_text_fore") : "#000000",
    ("gui", "color_text_use_system_colors") : 1,
    ("gui", "chat_font_name") : "Arial",
    ("gui", "chat_font_size") : 10,
    ("gui", "chat_window_width") : 400,
    ("gui", "chat_window_height") : 400,
    ("gui", "chat_window_height_lower") : 50,
    ("gui", "main_window_width") : 260,
    ("gui", "main_window_height") : 350,
    ("branding", "support_id") : "utvrla6mjdypbyw6",
    ("branding", "support_name") : "Bernd, author of TorChat",
    ("profile", "name") : "",
    ("profile", "text") : "",
}

LOCALE_ENC = locale.getpreferredencoding()
try:
    CONSOLE_ENC = sys.stdout.encoding
except:
    CONSOLE_ENC = None

def toUnicode(unknownstr):
    # some things like sys.argv[] and also functions from os.path
    # return bytestrings. Since I don't know if this might change
    # eventually in some future Python version I need something to
    # decode them only if needed. (I try to decode everything as
    # soon as possible and only work with unicode everywhere)
    # Note: it seems none of these strings I have come across so far
    # was ever encoded in the console encoding, they all seem to use
    # the locale encoding.
    if isinstance(unknownstr, str):
        return unknownstr.decode(LOCALE_ENC)
    else:
        return unknownstr

COPYRIGHT = u"Copyright (c) 2007-2011 Bernd Kreuß <prof7bit@googlemail.com>"

DEAD_CONNECTION_TIMEOUT = 240
KEEPALIVE_INTERVAL = 120
MAX_UNANSWERED_PINGS = 4

SCRIPT_DIR = os.path.abspath(os.path.dirname(toUnicode(sys.argv[0])))
ICON_DIR = os.path.join(SCRIPT_DIR, "icons")
log_writer = None

def isWindows98():
    if isWindows():
        return sys.getwindowsversion()[0] == 4 #@UndefinedVariable (make PyDev happy)
    else:
        return False
        
def isMac():
    return sys.platform == 'darwin'

def killProcess(pid):
    try:
        if isWindows():
            PROCESS_TERMINATE = 1
            handle = ctypes.windll.kernel32.OpenProcess(PROCESS_TERMINATE, #@UndefinedVariable
                                                        False,
                                                        pid)
            print handle
            ctypes.windll.kernel32.TerminateProcess(handle, -1) #@UndefinedVariable
            ctypes.windll.kernel32.CloseHandle(handle) #@UndefinedVariable
        else:
            os.kill(pid, 15)
    except:
        print "(1) could not kill process %i" % pid
        tb()


def isPortable():
    #if the file portable.txt exists in the same directory
    #then we know that we are running in portable mode.
    dir = SCRIPT_DIR
    try:
        f = open(os.path.join(dir, "portable.txt"), "r")
        f.close()
        return True
    except:
        return False

def getHomeDir():
    if isWindows():
        CSIDL_PERSONAL = 0x0005
        buf = ctypes.create_unicode_buffer(256)
        ctypes.windll.shell32.SHGetSpecialFolderPathW(None, buf, CSIDL_PERSONAL, 0)
        return buf.value
    else:
        return toUnicode(os.path.expanduser("~"))

def getDataDir():
    if isPortable():
        return SCRIPT_DIR
    else:
        if isWindows():
            CSIDL_APPDATA = 0x001a
            buf = ctypes.create_unicode_buffer(256)
            ctypes.windll.shell32.SHGetSpecialFolderPathW(None, buf, CSIDL_APPDATA, 0)
            appdata = buf.value
            data_dir = os.path.join(appdata, "torchat")
        else:
            home = toUnicode(os.path.expanduser("~"))
            data_dir = os.path.join(home, ".torchat")

    #test for optional profile name in command line
    try:
        data_dir += "_" + toUnicode(sys.argv[1])
    except:
        pass

    #create it if necessary
    if not os.path.exists(data_dir):
        os.mkdir(data_dir)

    #and create the folder 'Tor' with tor.exe and torrc.txt in it if necessary
    data_dir_tor = os.path.join(data_dir, "Tor")
    if not os.path.exists(data_dir_tor):
        os.mkdir(data_dir_tor)
        if isWindows():
            tor_exe =  "tor.exe"
        else:
            tor_exe = "tor.sh"
        shutil.copy(os.path.join("Tor", tor_exe), data_dir_tor)
        shutil.copy(os.path.join("Tor", "torrc.txt"), data_dir_tor)

    return data_dir

def getProfileLongName():
    try:
        return "%s - %s" % (toUnicode(sys.argv[1]), get("client", "own_hostname"))
    except:
        return get("client", "own_hostname")

class OrderedRawConfigParser(ConfigParser.RawConfigParser):
    def __init__(self, defaults = None):
        ConfigParser.RawConfigParser.__init__(self, defaults = None)

    def write(self, fp):
        """Write an .ini-format representation of the configuration state."""
        if self._defaults:
            fp.write("[%s]\n" % ConfigParser.DEFAULTSECT)
            for key in sorted(self._defaults):
                fp.write( "%s = %s\n" % (key, str(self._defaults[key]).replace('\n', '\n\t')))
            fp.write("\n")
        for section in sorted(self._sections):
            fp.write("[%s]\n" % section)
            for key in sorted(self._sections[section]):
                if key != "__name__":
                    fp.write("%s = %s\n" %
                             (key, str(self._sections[section][key]).replace('\n', '\n\t')))
            fp.write("\n")

def readConfig():
    global file_name
    global config
    dir = getDataDir()
    if not os.path.isdir(dir):
        os.mkdir(dir)
    file_name = dir + "/torchat.ini"
    config = OrderedRawConfigParser()

    #remove the BOM (notepad saves with BOM)
    if os.path.exists(file_name):
        f = file(file_name,'r+b')
        try:
            header = f.read(3)
            if header == "\xef\xbb\xbf":
                print "found UTF8 BOM in torchat.ini, removing it"
                f.seek(0)
                f.write("\x20\x0d\x0a")
        except:
            pass
        f.close()

    try:
        config.read(file_name)
    except ConfigParser.MissingSectionHeaderError:
        print ""
        print "*** torchat.ini must be saved as UTF-8 ***"
        sys.exit()

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
    value = config.get(section, option)
    if type(value) == str:
        try:
            value = value.decode("UTF-8")
            value = value.rstrip(" \"'").lstrip(" \"'")
        except:
            print "*** config file torchat.ini is not UTF-8 ***"
            print "*** this will most likely break things   ***"
    elif type(value) == int:
        value = str(value)
    elif type(value) == float:
        value = str(value)

    return value # this should now be a unicode string

def getint(section, option):
    value = get(section, option).lower()
    if value in ["yes", "on", "true"]:
        return 1
    if value in ["no", "off", "false"]:
        return 0
    try:
        return int(value)
    except:
        return 0

def set(section, option, value):
    if not config.has_section(section):
        config.add_section(section)
    if type(value) == bool:
        value = int(value)
    if type(value) == unicode:
        value = value.encode("UTF-8")
    config.set(section, option, value)
    writeConfig()

def tb(level=0):
    print "(%i) ----- start traceback -----\n%s   ----- end traceback -----\n" % (level, traceback.format_exc())

def tb1():
    print "---- BEGIN DEBUG CALLSTACK"
    traceback.print_stack()
    print "---- END DEBUG CALLSTACK"

def getTranslators():
    translators = []
    for mname in translations.__dict__: #@UndefinedVariable
        if mname[:5] == "lang_":
            m = translations.__dict__[mname] #@UndefinedVariable
            try:
                lcode = m.LANGUAGE_CODE
                lname = m.LANGUAGE_NAME
                ltrans = m.TRANSLATOR_NAMES
                for person in ltrans:
                    new_entry = "%s (%s [%s])" % (person, lname, lcode)
                    if not new_entry in translators:
                        translators.append(new_entry)
            except:
                pass
    return ", ".join(translators)

def importLanguage():
    """switch the language by redefining all the variables that will be 
    available in the lang.* namespace, using the namespace __dict__
    and making use of the wonderful dynamic nature of the Python language"""
    # (The many undefinedvariable comments below are there to make
    # the code analysis of Eclipse-PyDev happy, which would not be able
    # to recognize that these are perfectly valid at *runtime*)
     
    #if the strings in the language module have already been changed then
    if translations.lang_en.LANGUAGE_CODE != "en":
        #restore the original values from our backup to have
        #all strings reset to english. This helps when switching
        #between incomplete translations.
        for key in standard_dict:
            translations.lang_en.__dict__[key] = standard_dict[key] #@UndefinedVariable

    lang_xx = "lang_" + get("gui", "language")
    if lang_xx == "lang_en":
        #lang_en is the standard translation. nothing to replace.
        return

    if not SCRIPT_DIR in sys.path:
        #make sure that script dir is in sys.path (py2exe etc.)
        print "(1) putting script directory into module search path"
        sys.path.insert(0, SCRIPT_DIR)

    dict_std = translations.lang_en.__dict__ #@UndefinedVariable
    print "(1) trying to import language module %s" % lang_xx
    try:
        #first we try to find a language module in the script dir
        dict_trans = __import__(lang_xx).__dict__
        print "(1) found custom language module %s.py" % lang_xx
    except:
        #nothing found, so we try the built in translations
        if lang_xx in translations.__dict__: #@UndefinedVariable
            print "(1) found built in language module %s" % lang_xx
            dict_trans = translations.__dict__[lang_xx].__dict__
        else:
            print "(0) translation module %s not found"
            dict_trans = None

    if dict_trans:
        #dict_std is the __dict__ of the standard lang module
        #dict_trans is the __dict__ of the translation
        #find missing translations and report them in the log
        for key in dict_std:
            if not key in dict_trans:
                print "(2) %s is missing translation for %s" % (lang_xx, key)
        #replace the bindings in lang_en with those from lang_xx
        for key in dict_trans:
            if not key in dict_std:
                print "(2) unused %s in %s" % (key, lang_xx)
            else:
                dict_std[key] = dict_trans[key]


class LogWriter:
    def __init__(self):
        old_dir = os.getcwd()
        os.chdir(getDataDir())
        self.encoding = LOCALE_ENC

        #if log_file is a relative path then let it be relative to DataDir()
        self.file_name = os.path.abspath(get("logging", "log_file"))
        os.chdir(old_dir)
        self.stdout = sys.stdout
        sys.stdout = self
        sys.stderr = self
        self.level = getint("logging", "log_level")
        if  self.level and get("logging", "log_file"):
            try:
                self.logfile = open(self.file_name, 'w')
                print "(0) started logging to file '%s'" % self.file_name
                print "(0) logging to file might leave sensitive information on disk"
            except:
                self.logfile = None
                print "(0) could not open logfile '%s'" % self.file_name
                print "(0) logging only to stdout"

        else:
            self.logfile = None
            print "(1) logging to file is disabled"
            
        print "(1) current log level is %i" % self.level
        print "(1) locale encoding is %s" % LOCALE_ENC
        print "(1) console encoding is %s" % CONSOLE_ENC
        print "(1) LogWriter initialized"

    def write(self, text):
        text = text.rstrip()
        if text == "":        
            return
            
        # If something prints a string that is not unicode then we simply
        # assume it is encoded in the encoding of the current locale.
        if isinstance(text, str):
            text = text.decode(self.encoding, 'replace')
            
        text += "\n"
        try:
            x = text[0]
            y = text[2]
            if x == "(" and y == ")":
                level = int(text[1])
            else:
                text = "(0) " + text
                level = 0
        except:
            text = "(0) " + text
            level = 0

        if level <= self.level:
            try:
                frame = inspect.getframeinfo(inspect.currentframe(1))
                module = os.path.basename(frame[0])
                module = module.split(".")[0]
                line = frame[1]
                func = frame[2]
                pos = "[%s,%i,%s]" % (module, line, func)
                text = text[0:4] + pos + text[3:]
            except:
                pass
            if CONSOLE_ENC:
                self.stdout.write(text.encode(CONSOLE_ENC, 'replace'))
                self.stdout.flush()
            if self.logfile:
                # logfile like all other TorChat related files always UTF-8
                self.logfile.write(text.encode("UTF-8"))
                self.logfile.flush()

    def close(self):
        self.stdout.close()
        self.logfile.close()

def main():
    global standard_dict
    global log_writer

    #many things are relative to the script directory, so set is as the cwd
    os.chdir(SCRIPT_DIR)
    readConfig()
    log_writer = LogWriter()
    
    print "(0) python version %s" % sys.version.replace("\n", "").replace("\r", "")

    if isPortable():
        print "(0) running in portable mode, all data is kept inside the bin folder."
        if (len(sys.argv) > 1):
            print "(0) ignoring requested profile '%s' because profiles do not exist in portable mode" % toUnicode(sys.argv[1])

    print "(0) script directory is %s" % SCRIPT_DIR
    print "(0) data directory is %s" % getDataDir()

    #make a backup of all strings that are in the standard language file
    #because we could need them when switching between incomplete languages
    standard_dict = {}
    for key in translations.lang_en.__dict__: #@UndefinedVariable
        standard_dict[key] = translations.lang_en.__dict__[key] #@UndefinedVariable

    #now switch to the configured translation
    importLanguage()

main()
