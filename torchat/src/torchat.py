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

import os
import wx
import tc_gui
import config
        
def main():
    os.chdir(config.getScriptDir())
    app = wx.App(redirect=False)
    app.mw = tc_gui.MainWindow()
    app.SetTopWindow(app.mw)
    app.MainLoop()


if __name__ == "__main__":
    main()
