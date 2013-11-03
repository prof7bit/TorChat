# -*- coding: UTF-8 -*-

import os

NAME_en = u'Do not show self-echo messages'
NAME_ru = u'Не отображать эхо-ответы от самого себя'

def load(torchat):
    _onSend = torchat.tc_gui.ChatWindow.onSend
    text_last = None
    def onSend(self, evt):
        global text_last
        text_last = self.txt_out.GetValue().rstrip().lstrip().replace("\x0b", os.linesep)
        _onSend(self, evt)
    torchat.tc_gui.ChatWindow.onSend = onSend

    _onChatMessage = torchat.tc_client.Buddy.onChatMessage
    def onChatMessage(self, text):
        global text_last
        if self == self.bl.own_buddy and text == text_last:
            return
        _onChatMessage(self, text)
    torchat.tc_client.Buddy.onChatMessage = onChatMessage
