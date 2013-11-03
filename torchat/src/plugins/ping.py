# -*- coding: UTF-8 -*-

from functools import partial
from datetime import datetime

import wx

NAME_en = u'Add "Ping" button'
NAME_ru = u'Добавляет кнопку "Ping"'

def load(torchat):
    def set_tr(lang, option, translation):
        setattr(torchat.TRANSLATIONS[lang],
                'PING_' + option.upper(), translation)
    set_tr('en', 'ping', u'Ping')
    torchat.config.importLanguage()

    def onPing(chat_window, evt):
        chat_window.last_ping = datetime.now()
        chat_window.buddy.sendPing()

    _ChatWindow_PopupMenu = torchat.tc_gui.ChatWindow.PopupMenu
    def ChatWindow_PopupMenu(self, menu):
        # add Ping item
        id = wx.NewId()
        item = wx.MenuItem(menu, id, torchat.dlg_settings.lang.PING_PING)
        self.Bind(wx.EVT_MENU, partial(onPing, self), id=id)
        menu.AppendItem(item)
        # show
        _ChatWindow_PopupMenu(self, menu)
    torchat.tc_gui.ChatWindow.PopupMenu = ChatWindow_PopupMenu

    _Buddy_onInConnectionFound = torchat.tc_client.Buddy.onInConnectionFound
    def Buddy_onInConnectionFound(self, conn):
        _Buddy_onInConnectionFound(self, conn)
        for window in torchat.app.mw.chat_windows:
            if window.buddy == self and hasattr(window, 'last_ping'):
                delay = datetime.now() - window.last_ping
                del window.last_ping
                ms = delay.seconds * 1000 + delay.microseconds // 1000
                window.writeHintLine('time=%i ms' % ms)
    torchat.tc_client.Buddy.onInConnectionFound = Buddy_onInConnectionFound

