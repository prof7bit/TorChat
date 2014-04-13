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

# This is the settings dialog

import wx
import dlg
import config
import translations
lang = translations.lang_en


class Dialog(wx.Dialog):
    def __init__(self, main_window):
        wx.Dialog.__init__(self, main_window, wx.ID_ANY, lang.DSET_TITLE)
        self.mw = main_window
        
        #1 outer panel and vertical sizer
        self.outer_panel = wx.Panel(self)
        self.outer_sizer = wx.BoxSizer(wx.VERTICAL)
        self.outer_panel.SetSizer(self.outer_sizer)

        #1.1 the notebook on the top
        self.notebook = wx.Notebook(self.outer_panel)
        self.outer_sizer.Add(self.notebook, 1, wx.EXPAND|wx.LEFT|wx.TOP|wx.RIGHT, border=5)
        
        #1.2 the button_sizer at the bottom
        button_sizer = wx.BoxSizer(wx.HORIZONTAL)
        self.outer_sizer.Add(button_sizer, 0, wx.ALIGN_RIGHT | wx.ALL, border=5)
        
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

        portable = (self.mw.buddy_list.tor_config == "tor_portable")
        self.tor_portable = dlg.Check(self.p1, lang.DSET_GUI_TOR_PORTABLE, int(portable))

        self.s_tor_portable = dlg.Separator(self.p1, "Tor portable")
        dlg.Text(self.p1, lang.DSET_NET_TOR_ADDRESS, ("tor_portable", "tor_server"), True)
        dlg.Text(self.p1, lang.DSET_NET_TOR_SOCKS, ("tor_portable", "tor_server_socks_port"))
        dlg.Text(self.p1, lang.DSET_NET_TOR_CONTROL, ("tor_portable", "tor_server_control_port"))
        self.s_tor = dlg.Separator(self.p1, "Tor")
        dlg.Text(self.p1, lang.DSET_NET_TOR_ADDRESS, ("tor", "tor_server"), True)
        dlg.Text(self.p1, lang.DSET_NET_TOR_SOCKS, ("tor", "tor_server_socks_port"))
        dlg.Text(self.p1, lang.DSET_NET_TOR_CONTROL, ("tor", "tor_server_control_port"))
        dlg.Text(self.p1, lang.DSET_NET_OWN_HOSTNAME, ("client", "own_hostname"), True)
        dlg.Separator(self.p1, "Client")
        dlg.Text(self.p1, lang.DSET_NET_LISTEN_INTERFACE, ("client", "listen_interface"), True)
        dlg.Text(self.p1, lang.DSET_NET_LISTEN_PORT, ("client", "listen_port"))
        
        if portable:
            self.s_tor.setEnabled(False)
        else:
            self.s_tor_portable.setEnabled(False)
        
        #3.2 user interface
        self.p2 = dlg.Panel(self.notebook)
        self.notebook.AddPage(self.p2, lang.DSET_GUI_TITLE)

        self.lang = dlg.Text(self.p2, lang.DSET_GUI_LANGUAGE, ("gui", "language"))
        self.lang_old = self.lang.getValue()
        dlg.Check(self.p2, lang.DSET_GUI_OPEN_MAIN_HIDDEN, ("gui", "open_main_window_hidden"))
        dlg.Check(self.p2, lang.DSET_GUI_OPEN_CHAT_HIDDEN, ("gui", "open_chat_window_hidden"))
        dlg.Check(self.p2, lang.DSET_GUI_NOTIFICATION_POPUP, ("gui", "notification_popup"))
        dlg.Text(self.p2, lang.DSET_GUI_NOTIFICATION_METHOD, ("gui", "notification_method"))
        dlg.Check(self.p2, lang.DSET_GUI_FLASH_WINDOW, ("gui", "notification_flash_window"))
        
        #3.3 misc options
        self.p3 = dlg.Panel(self.notebook)
        self.notebook.AddPage(self.p3, lang.DSET_MISC_TITLE)
        self.chk_tmp = dlg.Check(self.p3, lang.DSET_MISC_TEMP_IN_DATA, ("files", "temp_files_in_data_dir"))
        self.dir_tmp = dlg.Dir(self.p3, lang.DSET_MISC_TEMP_CUSTOM_DIR, ("files", "temp_files_custom_dir"))
        self.dir_tmp.setEnabled(not self.chk_tmp.getValue())
        self.chk_tmp.wx_ctrl.Bind(wx.EVT_CHECKBOX, self.onChkTmp)

        #3.4 plugins
        self.p4 = dlg.Panel(self.notebook)
        self.notebook.AddPage(self.p4, lang.DSET_PLUGINS_TITLE)
        self.plugins = {}
        enabled_plugins = set(config.get('plugin', 'enabled_plugins').split(','))
        import torchat
        for plugin_name in sorted(torchat.PLUGINS.keys()):
            plugin_dscr = getattr(lang, 'DSET_PLUGIN_' + plugin_name.upper(), plugin_name)
            enabled = int(bool(plugin_name in enabled_plugins))
            self.plugins[plugin_name] = dlg.Check(self.p4, plugin_dscr, enabled)

        # add plugins' settings
        self.addPluginSettings(main_window)

        #4 fit the sizers
        self.p1.fit()
        self.p2.fit()
        self.p3.fit()
        self.p4.fit()
        self.outer_sizer.Fit(self)

    def addPluginSettings(self, main_window):
        pass

    def onChkTmp(self, evt):
        self.dir_tmp.setEnabled(not self.chk_tmp.getValue())

    def onCancel(self, evt):
        evt.Skip() #let the frame now process the Cancel event
        
    def onOk(self, evt):
        self.p1.saveAllData()
        self.p2.saveAllData()
        self.p3.saveAllData()
        #enabled_plugins = set(config.get('plugin', 'enabled_plugins').split(','))
        import torchat
        if self.tor_portable.getValue() == 1:
            config.set('client', 'tor_config', 'tor_portable')
        else:
            config.set('client', 'tor_config', 'tor')
        enabled_plugins = []
        for plugin_name in torchat.PLUGINS:
            if self.plugins[plugin_name].getValue():
                enabled_plugins.append(plugin_name)
        config.set('plugin', 'enabled_plugins', ','.join(enabled_plugins))
        if self.lang.getValue() != self.lang_old:
            config.importLanguage()
        evt.Skip() #let the frame now process the Ok event

