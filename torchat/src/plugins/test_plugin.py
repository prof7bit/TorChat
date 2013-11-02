# -*- coding: UTF-8 -*-

NAME_en = u'Test plugin, replaces all sent messages with "test"'
NAME_ru = u'Проверочный плагин, заменяет все отправляемые сообщения на "test"'
def load(torchat):
    _sendChatMessage = torchat.tc_client.Buddy.sendChatMessage
    def sendChatMessage(self, text):
        text = u'test'
        _sendChatMessage(self, text)
    torchat.tc_client.Buddy.sendChatMessage = sendChatMessage

