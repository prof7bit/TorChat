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

# This is the settings dialog

import wx
import dlg
import config
import tc_client
import translations
lang = translations.lang_en


class Dialog(wx.Dialog):
    def __init__(self, main_window):
        wx.Dialog.__init__(self, main_window, wx.ID_ANY, lang.DSET_TITLE)
        self.mw = main_window
        
        #1 outer panel and vertical sizer
        self.outer_panel = wx.Panel(self)
        outer_sizer = wx.BoxSizer(wx.VERTICAL)
        self.outer_panel.SetSizer(outer_sizer)

        #1.1 the notebook on the top
        self.notebook = wx.Notebook(self.outer_panel)
        outer_sizer.Add(self.notebook, 1, wx.EXPAND|wx.LEFT|wx.TOP|wx.RIGHT, border=5)
        
        #1.2 the button_sizer at the bottom
        button_sizer = wx.BoxSizer(wx.HORIZONTAL)
        outer_sizer.Add(button_sizer, 0, wx.ALIGN_RIGHT | wx.ALL, border=5)
        
        #1.2.1 cancel button
        btn_cancel = wx.Button(self.outer_panel, wx.ID_CANCEL, lang.BTN_CANCEL)
        button_sizer.Add(btn_cancel)
        
        #1.2.2 ok button
        btn_ok = wx.Button(self.outer_panel, wx.ID_OK, lang.BTN_OK)
        button_sizer.Add(btn_ok, 0, wx.LEFT, 5)
        
        #2 the button bindings
        btn_ok.Bind(wx.EVT_BUTTON, self.onOk)
        btn_cancel.Bind(wx.EVT_BUTTON, self.onCancel)
        
        #3 now we create the notebook tabs
        #3.1 network
        self.p1 = dlg.Panel(self.notebook)
        self.notebook.AddPage(self.p1, lang.DSET_NET_TITLE)

        self.s_tor_portable = dlg.Separator(self.p1, "Tor portable")
        dlg.Text(self.p1, lang.DSET_NET_TOR_ADDRESS, ("tor_portable", "tor_server"), width=150)
        dlg.Text(self.p1, lang.DSET_NET_TOR_SOCKS, ("tor_portable", "tor_server_socks_port"))
        dlg.Text(self.p1, lang.DSET_NET_TOR_CONTROL, ("tor_portable", "tor_server_control_port"))
        self.s_tor = dlg.Separator(self.p1, "Tor")
        dlg.Text(self.p1, lang.DSET_NET_TOR_ADDRESS, ("tor", "tor_server"), width=150)
        dlg.Text(self.p1, lang.DSET_NET_TOR_SOCKS, ("tor", "tor_server_socks_port"))
        dlg.Text(self.p1, lang.DSET_NET_TOR_CONTROL, ("tor", "tor_server_control_port"))
        dlg.Text(self.p1, lang.DSET_NET_OWN_HOSTNAME, ("client", "own_hostname"), width=150)
        dlg.Separator(self.p1, "Client")
        dlg.Text(self.p1, lang.DSET_NET_LISTEN_INTERFACE, ("client", "listen_interface"), width=150)
        dlg.Text(self.p1, lang.DSET_NET_LISTEN_PORT, ("client", "listen_port"))
        self.p1.fit()
        
        portable = (tc_client.TOR_CONFIG == "tor_portable")
        if portable:
            self.s_tor.setEnabled(False)
        else:
            self.s_tor_portable.setEnabled(False)
        
        #3.2 user interface
        self.p2 = dlg.Panel(self.notebook)
        self.notebook.AddPage(self.p2, lang.DSET_GUI_TITLE)
        
        #4 fit the sizers
        outer_sizer.Fit(self)
        
    def onCancel(self, evt):
        evt.Skip() #let the frame now process the Cancel event
        
    def onOk(self, evt):
        self.p1.saveAllData()
        evt.Skip() #let the frame now process the Ok event

