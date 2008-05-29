#!/usr/bin/python
# -*- coding: UTF-8 -*-

##############################################################################
#                                                                            #
# Copyright (c) 2007-2008 Bernd Kreuss <prof7bit@gmail.com>                  #
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
import os
import wx
import tc_client
import tc_gui
        
def main():
    #initialize the configuration
    config.main()
    
    #create the mandatory wx application object
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
        tc_client.stopPortableTor()
