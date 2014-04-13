# -*- coding: UTF-8 -*-

NAME_en = u'Test plugin, replaces all sent messages with "test"'
NAME_ru = u'Проверочный плагин, заменяет все отправляемые сообщения на "test"'

def load(torchat):
    _sendChatMessage = torchat.tc_client.Buddy.sendChatMessage
    def sendChatMessage(self, text):
        text = torchat.config.get("test_plugin", "text")
        _sendChatMessage(self, text)
    torchat.tc_client.Buddy.sendChatMessage = sendChatMessage

    torchat.config.config_defaults['test_plugin', 'text'] = 'test'

    torchat.TRANSLATIONS['en'].DSET_TEST_PLUGIN_TEXT = u'Replace any message with text'
    torchat.TRANSLATIONS['ru'].DSET_TEST_PLUGIN_TEXT = u'Заменять сообщения текстом'
    torchat.config.importLanguage()

    _addPluginSettings = torchat.dlg_settings.Dialog.addPluginSettings
    def addPluginSettings(self, main_window):
        _addPluginSettings(self, main_window)
        torchat.dlg.Text(self.p3, torchat.dlg_settings.lang.DSET_TEST_PLUGIN_TEXT,
                ("test_plugin", "text"))
    torchat.dlg_settings.Dialog.addPluginSettings = addPluginSettings

