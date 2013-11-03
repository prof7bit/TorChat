# -*- coding: UTF-8 -*-

NAME_en = u'Fake client and version'
NAME_ru = u'Подделывает отсылаемый клиент и версию'

def load(torchat):
    def get(option):
        return torchat.config.get("fake_user_agent", option)

    torchat.config.config_defaults['fake_user_agent', 'client'] = \
            torchat.version.NAME
    torchat.config.config_defaults['fake_user_agent', 'version'] = \
            torchat.version.VERSION

    _sendVersion = torchat.tc_client.Buddy.sendVersion
    def sendVersion(self):
        if self.isAlreadyPonged():
            msg = torchat.tc_client.ProtocolMsg_client(self, get('client'))
            msg.send()
            msg = torchat.tc_client.ProtocolMsg_version(self, get('version'))
            msg.send()
        else:
            _sendVersion(self)
    torchat.tc_client.Buddy.sendVersion = sendVersion

    def set_tr(lang, option, translation):
        setattr(torchat.TRANSLATIONS[lang],
                'DSET_FAKEUA_' + option.upper(), translation)
    set_tr('en', 'client', u'Client reported')
    set_tr('ru', 'client', u'Отсылаемое название клиента')
    set_tr('en', 'version', u'Version reported')
    set_tr('ru', 'version', u'Отсылаемая версия клиента')
    torchat.config.importLanguage()

    _dlg_settings_constructor = torchat.dlg_settings.Dialog.__init__
    def dlg_settings_constructor(self, main_window):
        _dlg_settings_constructor(self, main_window)
        def tr(option):
            attr_name = 'DSET_FAKEUA_' + option.upper()
            if hasattr(torchat.dlg_settings.lang, attr_name):
                return getattr(torchat.dlg_settings.lang, attr_name)
            else:
                return option
        def text(self, option):
            torchat.dlg.Text(self.p3, tr(option),
                    ("fake_user_agent", option), expand=True)
        text(self, 'client')
        text(self, 'version')
        self.p3.fit()
        self.outer_sizer.Fit(self)
    torchat.dlg_settings.Dialog.__init__ = dlg_settings_constructor

    _dlg_settings_onOk = torchat.dlg_settings.Dialog.onOk
    def dlg_settings_onOk(self, evt):
        old_client = get("client")
        old_version = get("version")
        _dlg_settings_onOk(self, evt)
        if get("client") != old_client or get("version") != old_version:
            for buddy in self.mw.buddy_list.list:
                buddy.sendVersion()
    torchat.dlg_settings.Dialog.onOk = dlg_settings_onOk
