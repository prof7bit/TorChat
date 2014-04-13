# -*- coding: UTF-8 -*-

NAME_en = u'Password protection against new contacts'
NAME_ru = u'Защита паролем от новых контактов'

def load(torchat):
    def get(option):
        return torchat.config.get("password", option)

    torchat.config.config_defaults['password', 'password'] = ''
    torchat.config.config_defaults['password', 'password_tip'] = ''

    _message_execute = torchat.tc_client.ProtocolMsg_message.execute
    def message_execute(self):
        if self.buddy and self.buddy not in self.bl.list and get('password'):
            if get('password') in self.text:
                self.buddy.sendChatMessage('Good password')
                self.__class__ = torchat.tc_client.ProtocolMsg_add_me
                _add_me_execute(self)
            else:
                self.buddy.sendChatMessage('Bad password')
            return # to suppress warning sent by torchat and password passing
        _message_execute(self)
    torchat.tc_client.ProtocolMsg_message.execute = message_execute

    _add_me_execute = torchat.tc_client.ProtocolMsg_add_me.execute
    def add_me_execute(self):
        if self.buddy and self.buddy not in self.bl.list and get('password'):
            message = u'Enter password'
            if get('password_tip'):
                message += u'. Password tip: %s' % get('password_tip')
            self.buddy.sendChatMessage(message)
        else:
            _add_me_execute(self)
    torchat.tc_client.ProtocolMsg_add_me.execute = add_me_execute

    def set_tr(lang, option, translation):
        setattr(torchat.TRANSLATIONS[lang],
                'DSET_PASSWORD_' + option.upper(), translation)
    set_tr('en', 'password', u'Password for new contacts')
    set_tr('ru', 'password', u'Пароль для новых контактов')
    set_tr('en', 'password_tip', u'Password tip')
    set_tr('ru', 'password_tip', u'Подсказка о пароле')
    torchat.config.importLanguage()

    _addPluginSettings = torchat.dlg_settings.Dialog.addPluginSettings
    def addPluginSettings(self, main_window):
        _addPluginSettings(self, main_window)
        def tr(option):
            attr_name = 'DSET_PASSWORD_' + option.upper()
            if hasattr(torchat.dlg_settings.lang, attr_name):
                return getattr(torchat.dlg_settings.lang, attr_name)
            else:
                return option
        def text(self, option):
            torchat.dlg.Text(self.p3, tr(option),
                    ("password", option), expand=True)
        text(self, 'password')
        text(self, 'password_tip')
    torchat.dlg_settings.Dialog.addPluginSettings = addPluginSettings
