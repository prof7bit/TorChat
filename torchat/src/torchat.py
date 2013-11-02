#!/usr/bin/env python
# -*- coding: UTF-8 -*-
# vim: set sw=4 sts=4 expandtab:

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

import config
import wxversion
if config.isMac():
    if wxversion.checkInstalled('2.9'):
        wxversion.select('2.9') # For Mac it is tweaked and optimized with 2.9
    else:
        print "(1) wxPython-2.9 is not installed"
        
else:
    try:
        if wxversion.checkInstalled('2.8'):
            wxversion.select('2.8') # On MSW and GTK we stick with 2.8 for now
        else:
            print "(1) wxPython-2.8 is not installed"
        
    except:
        # continue anyways. 
        # in the pyinstaller binary wxversion can screw up and throw exceptions 
        # so we ignore the error and just use the wx that happens to be available.
        # TODO: Does this still happen since we now use checkInstalled()?
        print "(2) wxversion screwed up, this is harmless, ignoring it."

import wx
import re
import os
import sys

import tc_client
import tc_gui
import dlg
import dlg_settings
import translations

# xx to module
TRANSLATIONS = {}
translations_dir = os.path.join(os.path.dirname(__file__), 'translations')
for translation_file in os.listdir(translations_dir):
    if re.match(r'^lang_..\.py$', translation_file):
        translation_name = translation_file[:-3]
        xx = translation_name[-2:]
        translation_path = os.path.join(translations_dir, translation_file)
        TRANSLATIONS[xx] = getattr(translations, translation_name)

PLUGINS = {} # shortname to python modules
# plugin module must have NAME_en member (string, long name)
# plugin module must have load(torchat) member, torchat is this module
# plugin module should have NAME_<LANG> with translation of name
# plugins are searched in app's plugins/ dir and in getDataDir()/plugins
# plugins with same name in getDataDir()/plugins win over app's plugins/ dir
plugins_parent_dirs = [config.getDataDir(), os.path.dirname(__file__)]
for plugins_parent_dir in plugins_parent_dirs:
    plugins_dir = os.path.join(plugins_parent_dir, 'plugins')
    if os.path.exists(plugins_dir):
        sys.path.append(plugins_dir)
        for plugin_file in os.listdir(plugins_dir):
            if plugin_file.endswith('.py'):
                plugin_name = plugin_file[:-3]
                PLUGINS[plugin_name] = __import__(plugin_name)
                for xx in TRANSLATIONS:
                    if hasattr(PLUGINS[plugin_name], 'NAME_' + xx):
                        dscr = getattr(PLUGINS[plugin_name], 'NAME_' + xx)
                        var_name = 'DSET_PLUGIN_' + plugin_name.upper()
                        setattr(TRANSLATIONS[xx], var_name, dscr)

def load_plugins():
    enabled_plugins = config.get('plugin', 'enabled_plugins').split(',')
    for plugin_name in enabled_plugins:
        if plugin_name in PLUGINS:
            PLUGINS[plugin_name].load(sys.modules[__name__])

def main():
    load_plugins()
    global app
    print "(2) wxPython version %s" % wx.version()
    #create the mandatory wx application object
    if config.isMac():
        import tc_mac
        app = tc_mac.App(redirect=False)
    else:
        app = wx.App(redirect=False)
    
    #test for availability of our listening port
    interface = config.get("client", "listen_interface")
    port = config.getint("client", "listen_port")
    print "(1) opening TorChat listener on %s:%s" % (interface, port)
    listen_socket = tc_client.tryBindPort(interface, port)
    if not listen_socket:
        print "(1) %s:%s is already in use" % (interface, port)
        wx.MessageBox(tc_gui.lang.D_WARN_USED_PORT_MESSAGE % (interface, port),
                      tc_gui.lang.D_WARN_USED_PORT_TITLE)
        return
    else:
        print "(1) TorChat is listening on %s:%s" % (interface, port)
    
    #now continue with normal program startup 
    print "(1) start initializing main window"
    app.mw = tc_gui.MainWindow(listen_socket)
    app.SetTopWindow(app.mw)
    print "(1) main window initialized"
    print "(1) entering main loop"
    app.MainLoop()

if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        app.mw.buddy_list.stopPortableTor()
