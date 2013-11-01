NAME = u'Test plugin, replaces all sent messages with "test"'
def load(torchat):
    sendChatMessage = torchat.tc_client.Buddy.sendChatMessage
    class Buddy(torchat.tc_client.Buddy):
        def sendChatMessage(self, text):
            text = u'test'
            sendChatMessage(self, text)
    torchat.tc_client.Buddy = Buddy

