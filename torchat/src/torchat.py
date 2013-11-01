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
import os
import sys
import imp

import tc_client
import tc_gui
import dlg
import dlg_settings

CORE_MODULES = ('config', 'dlg', 'dlg_settings', 'tc_client', 'tc_gui')
# translations are not restored, plugins should not change old members
ORIG_MEMBERS = {}
for module in CORE_MODULES:
    ORIG_MEMBERS[module] = dict(vars(__import__(module)))

def restore_orig_members():
    for module in CORE_MODULES:
        vars(__import__(module)).update(ORIG_MEMBERS[module])

plugins = {} # shortname to python modules
# plugin module must have NAME member (string, long name)
# plugin module must have load(torchat) member, torchat is this module
# plugin module should have NAME_<LANG> with translation of name
# plugins are searched in app's plugins/ dir and in getDataDir()/plugins
# plugins with same name in getDataDir()/plugins win over app's plugins/ dir
plugins_parent_dirs = []
try:
    plugins_parent_dirs.append(os.path.dirname(__file__))
except:
    pass
plugins_parent_dirs.append(config.getDataDir())
for plugins_parent_dir in plugins_parent_dirs:
    plugins_dir = os.path.join(plugins_parent_dir, 'plugins')
    if os.path.exists(plugins_dir):
        for plugin_file in os.listdir(plugins_dir):
            if plugin_file.endswith('.py'):
                plugin_name = plugin_file[:-3]
                plugin_path = os.path.join(plugins_dir, plugin_file)
                plugins[plugin_name] = imp.load_source(plugin_name, plugin_path)

def reload_plugins():
    restore_orig_members()
    enabled_plugins = config.get('plugin', 'enabled_plugins').split(',')
    for plugin_name in enabled_plugins:
        if plugin_name in plugins:
            plugins[plugin_name].load(sys.modules[__name__])

reload_plugins()

def main():
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
