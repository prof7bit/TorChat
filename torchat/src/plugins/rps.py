# -*- coding: UTF-8 -*-

import os
import re
import random
from hashlib import sha1
from functools import partial

import wx

NAME_en = u'Rock-paper-scissors (see popup menu)'
NAME_ru = u'Камень-ножницы-бумага (см. контекстное меню)'

FULL = {
    'r': 'rock',
    'p': 'paper',
    's': 'scissors',
}

HASH_RE = r'\b[0-9a-f]{40}$'
CLEARTEXT_RE = r'\b\S{10,}-[rps]$'

def winner_of(a, b):
    if a == b:
        return 'draw'
    if a == 'rock' and b == 'scissors':
        return a
    if a == 'scissors' and b == 'paper':
        return a
    if a == 'paper' and b == 'rock':
        return a
    return b

def load(torchat):
    def get_lang():
        return torchat.dlg_settings.lang
    def set_tr(lang, option, translation):
        setattr(torchat.TRANSLATIONS[lang],
                'RPS_' + option.upper(), translation)
    def tr(option):
        return getattr(get_lang(), 'RPS_' + option.upper(), option)
    set_tr('en', 'rock', u'Rock')
    set_tr('ru', 'rock', u'Камень')
    set_tr('en', 'paper', u'Paper')
    set_tr('ru', 'paper', u'Бумага')
    set_tr('en', 'scissors', u'Scissors')
    set_tr('ru', 'scissors', u'Ножницы')
    set_tr('en', 'random', u'Random')
    set_tr('ru', 'random', u'Наугад')
    set_tr('en', 'reset', u'Reset')
    set_tr('ru', 'reset', u'Сброс')
    set_tr('en', 'draw', u'Draw')
    set_tr('ru', 'draw', u'Ничья')
    set_tr('en', 'hash_sent', u'Hash sent')
    set_tr('ru', 'hash_sent', u'Хеш отправлен')
    set_tr('en', 'cleartext_sent', u'Cleartext sent')
    set_tr('ru', 'cleartext_sent', u'Открытый текст отправлен')
    set_tr('en', 'you_won', u'You won')
    set_tr('ru', 'you_won', u'Вы победили')
    set_tr('en', 'opponent_won', u'Opponent won')
    set_tr('ru', 'opponent_won', u'Соперник победил')
    set_tr('en', 'cheat', u'Hash mismatch. Cheat!')
    set_tr('ru', 'cheat', u'Хеш не совпадает. Жулик!')
    set_tr('en', 'unknown_choice', u'Unknown choice')
    set_tr('ru', 'unknown_choice', u'Неизвестный выбор')
    torchat.config.importLanguage()

    def make_cleartext(salt, choice):
        return '%s-%s' % (salt, choice[0])

    def write_hint(window, hint):
        wx.CallAfter(torchat.tc_gui.ChatWindow.writeHintLine, window, hint)

    def send_hash(window):
        if hasattr(window, 'rps_hash_sent') and window.rps_hash_sent:
            return
        window.rps_hash_sent = True
        cleartext = make_cleartext(window.rps_salt, window.rps_choice)
        hash = sha1(cleartext).hexdigest()
        window.buddy.sendChatMessage(hash)
        write_hint(window, tr('hash_sent'))

    def send_cleartext(window):
        if hasattr(window, 'rps_cleartext_sent') and window.rps_cleartext_sent:
            return
        window.rps_cleartext_sent = True
        cleartext = make_cleartext(window.rps_salt, window.rps_choice)
        window.buddy.sendChatMessage(cleartext)
        write_hint(window, tr('cleartext_sent'))

    def reset(window):
        window.rps_choice = ''
        window.rps_salt = ''
        window.rps_cleartext_sent = ''
        window.rps_hash_sent = ''

    def onRps(window, what, evt):
        reset(window)
        if what == 'reset':
            return
        if what == 'random':
            what = random.choice(['rock', 'paper', 'scissors'])
        window.rps_salt = sha1(str(random.getrandbits(256))).hexdigest()
        window.rps_choice = what
        write_hint(window, tr(window.rps_choice))
        cleartext = make_cleartext(window.rps_salt, window.rps_choice)
        write_hint(window, cleartext)
        send_hash(window)

    _ChatWindow_PopupMenu = torchat.tc_gui.ChatWindow.PopupMenu
    def ChatWindow_PopupMenu(self, menu):
        # rps submenu
        self.rps_submenu = wx.Menu()
        menu.AppendMenu(-1, get_lang().DSET_PLUGIN_RPS, self.rps_submenu)
        def add_item(what):
            id = wx.NewId()
            item = wx.MenuItem(self.rps_submenu, id, tr(what))
            self.Bind(wx.EVT_MENU, partial(onRps, self, what), id=id)
            self.rps_submenu.AppendItem(item)
        add_item('rock')
        add_item('paper')
        add_item('scissors')
        add_item('random')
        add_item('reset')
        # show
        _ChatWindow_PopupMenu(self, menu)
    torchat.tc_gui.ChatWindow.PopupMenu = ChatWindow_PopupMenu

    _message_execute = torchat.tc_client.ProtocolMsg_message.execute
    def message_execute(self):
        _message_execute(self)
        goood_message = self.buddy and self.buddy in self.bl.list
        if goood_message:
            hash_match = re.search(HASH_RE, self.text)
            if hash_match:
                hash = hash_match.group()
                for window in torchat.app.mw.chat_windows:
                    if window.buddy == self.buddy:
                        window.rps_opponent_hash = hash
                        if hasattr(window, 'rps_choice') and window.rps_choice:
                            send_cleartext(window)
            cleartext_match = re.search(CLEARTEXT_RE, self.text)
            if cleartext_match:
                cleartext = cleartext_match.group()
                for window in torchat.app.mw.chat_windows:
                    if window.buddy == self.buddy:
                        send_cleartext(window)
                        hash = window.rps_opponent_hash
                        if sha1(cleartext).hexdigest() != hash:
                            self.buddy.sendChatMessage('Hash mismatch. Cheat!')
                            write_hint(window, tr('cheat'))
                            return
                        choice = cleartext.split('-')[-1]
                        if choice not in 'rps':
                            self.buddy.sendChatMessage('Unknown choice')
                            write_hint(window, tr('cheat'))
                            return
                        choice_full = FULL[choice]
                        winner = winner_of(choice_full, window.rps_choice)
                        tr_my = tr(window.rps_choice).lower()
                        tr_his = tr(choice_full).lower()
                        if winner == 'draw':
                            write_hint(window, tr('draw'))
                        elif winner == choice_full:
                            write_hint(window, tr('opponent_won') +
                                    ': %s > %s' % (tr_his, tr_my))
                        elif winner == window.rps_choice:
                            write_hint(window, tr('you_won') +
                                    ': %s > %s' % (tr_my, tr_his))
                        reset(window)
    torchat.tc_client.ProtocolMsg_message.execute = message_execute

