# -*- coding: UTF-8 -*-

NAME_en = u'Turn this account into a conference'
NAME_ru = u'Превращает аккаунт в конференцию'

def load(torchat):
    _execute = torchat.tc_client.ProtocolMsg_message.execute
    def execute(self):
        goood_message = self.buddy and self.buddy in self.bl.list
        if int(torchat.config.get("conference", "no_gui")) == 0 or not goood_message:
            _execute(self)
        if goood_message:
            nick = self.buddy.address
            if int(torchat.config.get("conference", "prefer_nicks")) == 1 \
                    and self.buddy.profile_name \
                    and not torchat.tc_client.isValidAddress(self.buddy.profile_name):
                nick = self.buddy.profile_name
            resent_message = '%s: %s' % (nick, self.text)
            for buddy in self.bl.list:
                if buddy != self.buddy and buddy != self.bl.own_buddy:
                    buddy.sendChatMessage(resent_message)
    torchat.tc_client.ProtocolMsg_message.execute = execute

    torchat.config.config_defaults['conference', 'prefer_nicks'] = 1
    torchat.config.config_defaults['conference', 'no_gui'] = 0

    torchat.TRANSLATIONS['en'].DSET_MISC_CONFERENCE_PREFER_NICKS = \
            u'Show torchat nick if available instead of id to conference members'
    torchat.TRANSLATIONS['ru'].DSET_MISC_CONFERENCE_PREFER_NICKS = \
            u'Показывать ник отправителя вместо id, если ник выставлен'
    torchat.TRANSLATIONS['en'].DSET_MISC_CONFERENCE_NO_GUI = \
            u'Do not reflect new messages in GUI'
    torchat.TRANSLATIONS['ru'].DSET_MISC_CONFERENCE_NO_GUI = \
            u'Не отображать новые сообщения в графическом интерфейсе'
    torchat.config.importLanguage()

    _constructor = torchat.dlg_settings.Dialog.__init__
    def constructor(self, main_window):
        _constructor(self, main_window)
        torchat.dlg.Check(self.p3, torchat.dlg_settings.lang.DSET_MISC_CONFERENCE_PREFER_NICKS,
                ("conference", "prefer_nicks"))
        torchat.dlg.Check(self.p3, torchat.dlg_settings.lang.DSET_MISC_CONFERENCE_NO_GUI,
                ("conference", "no_gui"))
    torchat.dlg_settings.Dialog.__init__ = constructor

