# -*- coding: UTF-8 -*-

NAME_en = u'Test plugin, replaces all sent messages with "test"'
NAME_ru = u'Проверочный плагин, заменяет все отправляемые сообщения на "test"'
def load(torchat):
    sendChatMessage = torchat.tc_client.Buddy.sendChatMessage
    class Buddy(torchat.tc_client.Buddy):
        def sendChatMessage(self, text):
            text = u'test'
            sendChatMessage(self, text)
    torchat.tc_client.Buddy = Buddy

