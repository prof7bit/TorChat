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
import tc_gui
        
def main():
    config.main() 
    app = wx.App(redirect=False)
    print "(1) start initializing main window"
    app.mw = tc_gui.MainWindow()
    app.SetTopWindow(app.mw)
    print "(1) main window initialized"
    print "(1) entering main loop"
    app.MainLoop()

if __name__ == "__main__":
    main()
