# -*- coding: UTF-8 -*-

import os
import wx
import json

NAME_en = u'Turn this account into a conference'
NAME_ru = u'Превращает аккаунт в конференцию'

NOBODY = set(['help'])
GUEST = NOBODY | set(['read_actions', 'read', 'list', 'ignore', 'unignore'])
USER = GUEST | set(['write', 'pm'])
MODER = USER | set(['mute', 'unmute', 'kick', 'invite', 'ban', 'unban',
    'topic', 'description', 'set_avatar', 'list_for_moder'])
ADMIN = MODER | set(['role', 'prefer_nicks',
    'allow_list', 'allow_pm', 'list_status', 'list_role', 'default_role',
    'show_admin_actions', 'show_enter_leave', 'welcome_help'])
OWNER = ADMIN | set(['add_admin', 'remove_admin'])

ROLES = {'nobody': NOBODY, 'guest': GUEST, 'user': USER,
    'moder': MODER, 'admin': ADMIN, 'owner': OWNER}

HELP = {}
HELP['nobody'] = '''
!help             get help
'''
HELP['guest'] = HELP['nobody'] + '''
!list             get list of room members
!ignore nick      ignore messages from nick
!unignore nick    undo ignore
'''
HELP['user'] = HELP['guest'] + '''
!pm nick message  private message to nick
'''
HELP['moder'] = HELP['user'] + '''
!mute nick        mute nick (role: user->guest)
!unmute nick      undo mute (role: guest,nobody->user)
!kick nick        kick nick from room
!invite nick      invite nick to room
!ban nick         ban nick (needs Ban plugin)
!unban nick       undo ban
!topic text       change topic of room
!description text change description of room
!set_avatar nick  set avatar of nick as room's avatar
'''
HELP['admin'] = HELP['moder'] + '''
!role [nobody|guest|user|moder] nick get or set role of nick
!prefer_nicks [yes|no]               get or set prefer_nicks
!allow_list [yes|no]                 get or set allow_list
!allow_pm [yes|no]                   get or set allow_pm
!list_status [yes|no]                get or set list_status
!list_role [yes|no]                  get or set list_role
!default_role [nobody|guest|user]    get or set default_role
!show_admin_actions [yes|no]         get or set show_admin_actions
!show_enter_leave [yes|no]           get or set show_enter_leave
!welcome_help [yes|no]               get or set welcome_help
'''
HELP['owner'] = HELP['admin'] + '''
!add_admin nick                      set nick's role to admin
!remove_admin nick                   set nick's role to user
'''

def load(torchat):
    torchat.config.config_defaults['conference', 'prefer_nicks'] = 1
    torchat.config.config_defaults['conference', 'no_gui'] = 0
    torchat.config.config_defaults['conference', 'roles'] = '{}'
    torchat.config.config_defaults['conference', 'default_role'] = 'user'
    torchat.config.config_defaults['conference', 'allow_list'] = 1
    torchat.config.config_defaults['conference', 'allow_pm'] = 1
    torchat.config.config_defaults['conference', 'list_status'] = 1
    torchat.config.config_defaults['conference', 'list_role'] = 1
    torchat.config.config_defaults['conference', 'show_admin_actions'] = 1
    torchat.config.config_defaults['conference', 'show_enter_leave'] = 1
    torchat.config.config_defaults['conference', 'welcome_help'] = 1
    torchat.config.config_defaults['conference', 'ignored'] = '{"who-bywho":1}'

    _execute = torchat.tc_client.ProtocolMsg_message.execute

    def sstatus(status):
        if status == torchat.tc_client.STATUS_OFFLINE:
            return 'offline'
        if status == torchat.tc_client.STATUS_HANDSHAKE:
            return 'handshake'
        if status == torchat.tc_client.STATUS_ONLINE:
            return 'online'
        if status == torchat.tc_client.STATUS_AWAY:
            return 'away'
        if status == torchat.tc_client.STATUS_XA:
            return 'busy'
    def buddy_list():
        return torchat.app.mw.buddy_list
    def get(option):
        return torchat.config.get("conference", option)
    def set_option(option, value):
        torchat.config.set("conference", option, value)
    roles = json.loads(get("roles"))
    def role_of(torchat_id):
        if torchat_id == buddy_list().own_buddy.address:
            return 'owner'
        default_role = get("default_role")
        return roles.get(torchat_id, default_role)
    def set_role(torchat_id, role):
        roles[torchat_id] = role
        set_option("roles", json.dumps(roles))
    ignored = json.loads(get("ignored"))
    def is_ignored(who, bywho):
        return ('%s-%s' % (who, bywho)) in ignored
    def ignore(who, bywho):
        ignored[('%s-%s' % (who, bywho))] = 1
        set_option("ignored", json.dumps(ignored))
    def unignore(who, bywho):
        ignored.pop(('%s-%s' % (who, bywho)), None)
        set_option("ignored", json.dumps(ignored))
    def role_can(role, action):
        return role in ROLES and action in ROLES[role]
    def is_moder(torchat_id):
        return role_of(torchat_id) in ('moder', 'admin', 'owner')
    def can(torchat_id, action):
        return role_can(role_of(torchat_id), action)
    def buddy_from_nick(nick, me):
        if nick == 'me':
            return me
        for buddy in buddy_list().list:
            if buddy.address == nick or buddy.profile_name == nick:
                return buddy
    def nick_repr(buddy, moder=False):
        nick = buddy.address
        if int(get("prefer_nicks")) == 1 \
                and buddy.profile_name \
                and not torchat.tc_client.isValidAddress(buddy.profile_name):
            nick = buddy.profile_name
            if moder:
                nick += ' (%s)' % buddy.address
        if moder or get('list_role'):
            buddy_role = role_of(buddy.address)
            if buddy_role in ('admin', 'owner'):
                nick = '@' + nick
            if buddy_role == 'moder':
                nick = '%' + nick
        if get('list_status') and buddy.status != torchat.tc_client.STATUS_ONLINE:
            nick += ' [%s]' % sstatus(buddy.status)
        return nick
    def send_help(buddy, buddy_role):
        buddy.sendChatMessage(HELP[buddy_role])
    def announce(text, moder):
        text = '[room] %s' % text
        for buddy in buddy_list().list:
            if not can(buddy.address, 'read'):
                continue
            if moder and not is_moder(buddy.address) \
                    and int(get("show_admin_actions")) != 1:
                continue
            if not moder and not is_moder(buddy.address) \
                    and int(get("show_enter_leave")) != 1:
                continue
            buddy.sendChatMessage(text)
    def splitLine(line):
        return torchat.tc_client.splitLine(line)

    def execute(self):
        goood_message = self.buddy and self.buddy in self.bl.list
        if int(get("no_gui")) == 0 or not goood_message:
            _execute(self)
        if goood_message:
            me = self.buddy
            my_address = me.address
            my_role = role_of(my_address)
            if self.text.startswith('!'):
                command, argument = splitLine(self.text)
                command = command[1:] # pop "!"
                if not role_can(my_role, command):
                    me.sendChatMessage('[room] Action not allowed')
                    return
                if command == 'help':
                    send_help(me, my_role)
                if command == 'list':
                    list_for_moder = role_can(my_role, 'list_for_moder')
                    if int(get('allow_list')) != 1 and not list_for_moder:
                        me.sendChatMessage('[room] Action not allowed')
                        return
                    nicks = []
                    for buddy in self.bl.list:
                        nicks.append(nick_repr(buddy, list_for_moder))
                    self.buddy.sendChatMessage('[room]\n' + '\n'.join(nicks))
                if command == 'ignore':
                    buddy = buddy_from_nick(argument, me)
                    if buddy:
                        ignore(buddy, me)
                    else:
                        me.sendChatMessage('[room] Unknown nick')
                if command == 'unignore':
                    buddy = buddy_from_nick(argument, me)
                    if buddy:
                        unignore(buddy, me)
                    else:
                        me.sendChatMessage('[room] Unknown nick')
                if command == 'pm':
                    if int(get('allow_pm')) != 1:
                        me.sendChatMessage('[room] Private messages disabled')
                        return
                    dest, message = splitLine(argument)
                    dest = buddy_from_nick(dest, me)
                    if not dest:
                        me.sendChatMessage('[room] Unknown nick')
                        return
                    if is_ignored(me, dest):
                        me.sendChatMessage('[room] Buddy ignores you')
                        return
                    my_nick = nick_repr(me, is_moder(dest.address))
                    dest.sendChatMessage('[private] %s: %s' % (my_nick, message))
                if command == 'mute':
                    buddy = buddy_from_nick(argument, me)
                    if not buddy:
                        me.sendChatMessage('[room] Unknown nick')
                        return
                    if role_of(argument) != 'user':
                        me.sendChatMessage('[room] You can mute only users')
                        return
                    set_role(argument, 'guest')
                    announce('%s muted %s' % (nick_repr(me), nick_repr(buddy)), True)
                if command == 'unmute':
                    buddy = buddy_from_nick(argument, me)
                    if not buddy:
                        me.sendChatMessage('[room] Unknown nick')
                        return
                    if role_of(argument) not in ('nobody', 'guest'):
                        me.sendChatMessage('[room] You can unmute nobody,guest')
                        return
                    announce('%s unmuted %s' % (nick_repr(me), nick_repr(buddy)), True)
                    set_role(argument, 'user')
                if command == 'kick':
                    buddy = buddy_from_nick(argument, me)
                    if not buddy:
                        me.sendChatMessage('[room] Unknown nick')
                        return
                    if role_of(argument) not in ('nobody', 'guest', 'user'):
                        me.sendChatMessage('[room] You can kick nobody,guest,user')
                        return
                    buddy_list().removeBuddy(buddy, disconnect=False)
                    announce('%s kicked %s' % (nick_repr(me), nick_repr(buddy)), True)
                if command == 'invite':
                    if not torchat.tc_client.isValidAddress(argument):
                        me.sendChatMessage('[room] Bad address')
                        return
                    buddy = buddy_from_nick(argument, me)
                    if buddy:
                        me.sendChatMessage('[room] Already in room')
                        return
                    buddy = torchat.tc_client.Buddy(argument, buddy_list(), '')
                    self.bl.addBuddy(buddy)
                    announce('%s invited %s' % (nick_repr(me), argument), True)
                if command == 'ban':
                    buddy = buddy_from_nick(argument, me)
                    if not buddy:
                        me.sendChatMessage('[room] Unknown nick')
                        return
                    if role_of(argument) not in ('nobody', 'guest', 'user'):
                        me.sendChatMessage('[room] You can ban nobody,guest,user')
                        return
                    ban_list = json.loads(torchat.config.get("ban", 'list'))
                    ban_list[buddy.address] = 1
                    torchat.config.set("ban", 'list', json.dumps(ban_list))
                    buddy_list().removeBuddy(buddy, disconnect=False)
                    announce('%s banned %s' % (nick_repr(me), nick_repr(buddy)), True)
                if command == 'unban':
                    announce('%s unbanned %s' % (nick_repr(me), argument), True)
                    ban_list = json.loads(torchat.config.get("ban", 'list'))
                    ban_list.pop(argument, None)
                    torchat.config.set("ban", 'list', json.dumps(ban_list))
                if command == 'topic':
                    torchat.config.set("profile", "name", argument)
                    for buddy in buddy_list().list:
                        buddy.sendProfile()
                    announce('%s changed the topic to %s' % (nick_repr(me), argument), True)
                if command == 'description':
                    torchat.config.set("profile", "text", argument)
                    for buddy in buddy_list().list:
                        buddy.sendProfile()
                    announce('%s changed the description to %s' % (nick_repr(me), argument), True)
                if command == 'set_avatar':
                    buddy = buddy_from_nick(argument, me)
                    if not buddy:
                        me.sendChatMessage('[room] Unknown nick')
                        return
                    alpha = buddy.profile_avatar_data_alpha
                    avatar = buddy.profile_avatar_data
                    buddy_list().own_avatar_data_alpha = alpha
                    buddy_list().own_avatar_data = avatar
                    file_name = os.path.join(torchat.config.getDataDir(), "avatar.png")
                    if not avatar:
                        torchat.tc_client.wipeFile(file_name)
                    else:
                        image = wx.ImageFromData(64, 64, avatar)
                        if alpha:
                            image.SetAlphaData(alpha)
                        image.SaveFile(file_name, wx.BITMAP_TYPE_PNG)
                    for buddy in buddy_list().list:
                        buddy.sendAvatar(send_empty=True)
                    announce('%s changed image of the room' % nick_repr(me), True)
                if command == 'role':
                    arg1, arg2 = splitLine(argument)
                    if arg2:
                        # set
                        new_role, nick = arg1, arg2
                        buddy = buddy_from_nick(nick, me)
                        if not buddy:
                            me.sendChatMessage('[room] Unknown nick')
                            return
                        if role_of(buddy.address) in ('admin', 'owner'):
                            me.sendChatMessage('[room] Can not change role of admin,owner')
                            return
                        if new_role not in ('nobody', 'guest', 'user', 'moder'):
                            me.sendChatMessage('[room] Can change role to nobody,guest,user,moder')
                            return
                        set_role(buddy.address, new_role)
                        announce('%s set role of %s to %s' %
                                (nick_repr(me), nick_repr(buddy), new_role), True)
                    else :
                        # get
                        nick = arg1
                        buddy = buddy_from_nick(nick, me)
                        if not buddy:
                            me.sendChatMessage('[room] Unknown nick')
                            return
                        me.sendChatMessage('[room] role(%s)=%s' %
                                (nick, role_of(buddy.address)))
                # TODO admin settings
            else:
                if not role_can(my_role, 'write'):
                    self.buddy.sendChatMessage('[room] You can not send messages')
                    return
                # resend message
                for buddy in self.bl.list:
                    if not can(buddy.address, 'read'):
                        continue
                    if buddy == self.buddy:
                        continue
                    if buddy == self.bl.own_buddy:
                        continue
                    if self.buddy == self.bl.own_buddy \
                            and self.text.startswith('[room]'):
                        continue
                    sender_nick = nick_repr(self.buddy, is_moder(buddy.address))
                    resent_message = '%s: %s' % (sender_nick, self.text)
                    buddy.sendChatMessage(resent_message)
    torchat.tc_client.ProtocolMsg_message.execute = execute

    torchat.TRANSLATIONS['en'].DSET_MISC_CONFERENCE_PREFER_NICKS = \
            u'Show torchat nick if available instead of id to conference members'
    torchat.TRANSLATIONS['ru'].DSET_MISC_CONFERENCE_PREFER_NICKS = \
            u'Показывать ник отправителя вместо id, если ник выставлен'
    torchat.TRANSLATIONS['en'].DSET_MISC_CONFERENCE_NO_GUI = \
            u'Do not reflect new messages in GUI'
    torchat.TRANSLATIONS['ru'].DSET_MISC_CONFERENCE_NO_GUI = \
            u'Не отображать новые сообщения в графическом интерфейсе'
    torchat.TRANSLATIONS['en'].DSET_MISC_CONFERENCE_DEFAULT_ROLE = \
            u'Default role for new members (nobody|guest|user)'
    torchat.TRANSLATIONS['ru'].DSET_MISC_CONFERENCE_DEFAULT_ROLE = \
            u'Роль по умолчанию для новых членов (nobody|guest|user)'
    torchat.TRANSLATIONS['en'].DSET_MISC_CONFERENCE_ALLOW_LIST = \
            u'Regular users can get list of users'
    torchat.TRANSLATIONS['ru'].DSET_MISC_CONFERENCE_ALLOW_LIST = \
            u'Обычные пользователи могут смотреть список пользователей'
    torchat.TRANSLATIONS['en'].DSET_MISC_CONFERENCE_ALLOW_PM = \
            u'Allow private messages'
    torchat.TRANSLATIONS['ru'].DSET_MISC_CONFERENCE_ALLOW_PM = \
            u'Разрешить личные сообщения'
    torchat.TRANSLATIONS['en'].DSET_MISC_CONFERENCE_LIST_STATUS = \
            u'Regular users view status of users'
    torchat.TRANSLATIONS['ru'].DSET_MISC_CONFERENCE_LIST_STATUS = \
            u'Обычные пользователи видят статус пользователей'
    torchat.TRANSLATIONS['en'].DSET_MISC_CONFERENCE_LIST_ROLE = \
            u'Regular users view role of users'
    torchat.TRANSLATIONS['ru'].DSET_MISC_CONFERENCE_LIST_ROLE = \
            u'Обычные пользователи видят роль пользователей'
    torchat.TRANSLATIONS['en'].DSET_MISC_CONFERENCE_SHOW_ADMIN_ACTIONS = \
            u"Regular users view administrators' actions"
    torchat.TRANSLATIONS['ru'].DSET_MISC_CONFERENCE_SHOW_ADMIN_ACTIONS = \
            u'Обычные пользователи видят действия администраторов'
    torchat.TRANSLATIONS['en'].DSET_MISC_CONFERENCE_SHOW_ENTER_LEAVE = \
            u"Regular users view somebody enter or leave room"
    torchat.TRANSLATIONS['ru'].DSET_MISC_CONFERENCE_SHOW_ADMIN_ACTIONS = \
            u'Обычные пользователи видят, когда кто-то входит или выходит'
    torchat.TRANSLATIONS['en'].DSET_MISC_CONFERENCE_WELCOME_HELP = \
            u"Welcome new users with help message"
    torchat.TRANSLATIONS['ru'].DSET_MISC_CONFERENCE_WELCOME_HELP = \
            u'Приветствовать новых пользователей справкой'
    torchat.config.importLanguage()

    _constructor = torchat.dlg_settings.Dialog.__init__
    def constructor(self, main_window):
        _constructor(self, main_window)
        torchat.dlg.Check(self.p3, torchat.dlg_settings.lang.DSET_MISC_CONFERENCE_PREFER_NICKS,
                ("conference", "prefer_nicks"))
        torchat.dlg.Check(self.p3, torchat.dlg_settings.lang.DSET_MISC_CONFERENCE_NO_GUI,
                ("conference", "no_gui"))
        # TODO
    torchat.dlg_settings.Dialog.__init__ = constructor

