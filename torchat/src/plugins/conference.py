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
    'topic', 'description', 'set_avatar'])
ADMIN = MODER | set(['role', 'prefer_nicks',
    'allow_list', 'allow_pm', 'list_status', 'list_role', 'default_role',
    'show_admin_actions', 'show_enter_leave', 'welcome_help', 'password'])
OWNER = ADMIN | set(['add_admin', 'remove_admin'])

ROLES = {'banned': set(), 'nobody': NOBODY, 'guest': GUEST, 'user': USER,
    'moder': MODER, 'admin': ADMIN, 'owner': OWNER}

HELP = {}
HELP['banned'] = 'You are banned'
HELP['nobody'] = '''[help]
!help             get help
'''
HELP['guest'] = HELP['nobody'] + '''
You can refer nick or torchat id, with or without % and @.
You can refer part of nick.

!list             get list of room members
!ignore nick      ignore messages from nick
!unignore nick    undo ignore
'''
HELP['user'] = HELP['guest'] + '''
You can send messages to chat. Just send regular messages!
To prevent !smth from interpreting as a command, prepend it with "_ ".

!pm nick message  private message to nick
'''
HELP['moder'] = HELP['user'] + '''
!mute nick        mute nick (role: user->guest)
!unmute nick      undo mute (role: guest,nobody->user)
!kick nick        kick nick from room
!invite nick      invite nick to room
!ban nick         ban nick
!unban nick       undo ban
!topic text       change topic of room
!description text change description of room
!set_avatar nick  set avatar of nick as room's avatar
'''
HELP['admin'] = HELP['moder'] + '''
!role [nobody|guest|user|moder] nick     get or set role of nick
!prefer_nicks [yes|no]                   get or set prefer_nicks
!allow_list [yes|no]                     get or set allow_list
!allow_pm [yes|no]                       get or set allow_pm
!list_status [yes|no]                    get or set list_status
!list_role [yes|no]                      get or set list_role
!default_role [banned|nobody|guest|user] get or set default_role
!show_admin_actions [yes|no]             get or set show_admin_actions
!show_enter_leave [yes|no]               get or set show_enter_leave
!welcome_help [yes|no]                   get or set welcome_help
!password [password]                     get or set room password

To make closed conference, set role of each user to 'user' explicitly
and then set default_role=banned.
'''
HELP['owner'] = HELP['admin'] + '''
!add_admin nick                      set nick's role to admin
!remove_admin nick                   set nick's role to user
'''
WELCOME = '''%s
Topic: %s

Description: %s'''

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
    torchat.config.config_defaults['conference', 'password'] = ''
    torchat.config.config_defaults['conference', 'ignored'] = '{"who-bywho":1}'

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
    def is_moder(torchat_id):
        return role_of(torchat_id) in ('moder', 'admin', 'owner')
    def can(torchat_id, action):
        role = role_of(torchat_id)
        return role in ROLES and action in ROLES[role]
    def buddy_from_nick(nick, me):
        if nick == 'me':
            return me
        for buddy in buddy_list().list:
            if buddy.address == nick or buddy.profile_name == nick:
                return buddy
        if nick.startswith('@') or nick.startswith('%'):
            nick = nick[1:]
            for buddy in buddy_list().list:
                if buddy.address == nick or buddy.profile_name == nick:
                    return buddy
        i_am_moder = is_moder(me.address)
        for buddy in buddy_list().list:
            if nick in nick_repr(buddy, i_am_moder):
                return buddy
    def nick_repr(buddy, moder=False):
        nick = buddy.address
        if int(get("prefer_nicks")) == 1 \
                and buddy.profile_name \
                and not torchat.tc_client.isValidAddress(buddy.profile_name):
            nick = buddy.profile_name
            if moder:
                nick += ' (%s)' % buddy.address
        if moder or int(get('list_role')) == 1:
            buddy_role = role_of(buddy.address)
            if buddy_role in ('admin', 'owner'):
                nick = '@' + nick
            if buddy_role == 'moder':
                nick = '%' + nick
        if int(get('list_status')) == 1 and \
                buddy.status != torchat.tc_client.STATUS_ONLINE:
            nick += ' [%s]' % sstatus(buddy.status)
        return nick
    def announce(text, moder):
        prefix = '[admin]' if moder else '[user]'
        text = '%s %s' % (prefix, text)
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

    def do_help(me, _):
        topic = buddy_list().own_buddy.profile_name
        description = buddy_list().own_buddy.profile_text
        text = WELCOME % (HELP[role_of(me.address)], topic, description)
        me.sendChatMessage(text)
    def do_list(me, _):
        if int(get('allow_list')) != 1 and not is_moder(me.address):
            me.sendChatMessage('[room] Action not allowed')
            return
        nicks = []
        for buddy in buddy_list().list:
            nick = nick_repr(buddy, is_moder(me.address))
            if int(get('list_role')) == 1 or is_moder(me.address):
                nick += ' /%s/' % role_of(buddy.address)
            nicks.append(nick)
        me.sendChatMessage('[room]\n' + '\n'.join(nicks))
    def do_ignore(me, nick):
        buddy = buddy_from_nick(nick, me)
        if buddy:
            if is_moder(buddy.address):
                me.sendChatMessage('[room] Can not ignore moderator')
            else:
                ignore(buddy.address, me.address)
        else:
            me.sendChatMessage('[room] Unknown nick')
    def do_unignore(me, nick):
        buddy = buddy_from_nick(nick, me)
        if buddy:
            unignore(buddy.address, me.address)
        else:
            me.sendChatMessage('[room] Unknown nick')
    def do_pm(me, argument):
        if int(get('allow_pm')) != 1:
            me.sendChatMessage('[room] Private messages disabled')
            return
        dest, message = splitLine(argument)
        dest = buddy_from_nick(dest, me)
        if not dest:
            me.sendChatMessage('[room] Unknown nick')
            return
        if is_ignored(me.address, dest.address):
            me.sendChatMessage('[room] Buddy ignores you')
            return
        my_nick = nick_repr(me, is_moder(dest.address))
        dest.sendChatMessage('[private] %s: %s' % (my_nick, message))
    def do_mute(me, nick):
        buddy = buddy_from_nick(nick, me)
        if not buddy:
            me.sendChatMessage('[room] Unknown nick')
            return
        if role_of(buddy.address) != 'user':
            me.sendChatMessage('[room] You can mute only users')
            return
        set_role(buddy.address, 'guest')
        announce('%s muted %s' % (nick_repr(me), nick_repr(buddy)), True)
    def do_unmute(me, nick):
        buddy = buddy_from_nick(nick, me)
        if not buddy:
            me.sendChatMessage('[room] Unknown nick')
            return
        if role_of(buddy.address) not in ('nobody', 'guest'):
            me.sendChatMessage('[room] You can unmute nobody,guest')
            return
        announce('%s unmuted %s' % (nick_repr(me), nick_repr(buddy)), True)
        set_role(buddy.address, 'user')
    def do_kick(me, nick):
        buddy = buddy_from_nick(nick, me)
        if not buddy:
            me.sendChatMessage('[room] Unknown nick')
            return
        if role_of(buddy.address) not in ('nobody', 'guest', 'user'):
            me.sendChatMessage('[room] You can kick nobody,guest,user')
            return
        buddy_list().removeBuddy(buddy, disconnect=False)
        announce('%s kicked %s' % (nick_repr(me), nick_repr(buddy)), True)
    def do_invite(me, address):
        if not torchat.tc_client.isValidAddress(address):
            me.sendChatMessage('[room] Bad address')
            return
        buddy = buddy_from_nick(address, me)
        if buddy:
            me.sendChatMessage('[room] Already in room')
            return
        buddy = torchat.tc_client.Buddy(address, buddy_list(), '')
        buddy_list().addBuddy(buddy)
        announce('%s invited %s' % (nick_repr(me), address), True)
    def do_ban(me, nick):
        buddy = buddy_from_nick(nick, me)
        if not buddy:
            me.sendChatMessage('[room] Unknown nick')
            return
        if role_of(buddy.address) not in ('nobody', 'guest', 'user'):
            me.sendChatMessage('[room] You can ban nobody,guest,user')
            return
        set_role(buddy.address, 'banned')
        buddy_list().removeBuddy(buddy, disconnect=False)
        announce('%s banned %s' % (nick_repr(me), nick_repr(buddy)), True)
    def do_unban(me, address):
        if role_of(address) != 'banned':
            me.sendChatMessage('[room] The address is not banned')
            return
        set_role(address, 'user')
        announce('%s unbanned %s' % (nick_repr(me), address), True)
    def do_topic(me, topic):
        torchat.config.set("profile", "name", topic)
        for buddy in buddy_list().list:
            buddy.sendProfile()
        announce('%s changed the topic to %s' % (nick_repr(me), topic), True)
    def do_description(me, topic):
        torchat.config.set("profile", "text", topic)
        for buddy in buddy_list().list:
            buddy.sendProfile()
        announce('%s changed the description to %s' % (nick_repr(me), topic), True)
    def do_set_avatar(me, nick):
        buddy = buddy_from_nick(nick, me)
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
    def do_role(me, argument):
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
    def yes_no_command(me, option, yes_no):
        if yes_no in ('yes', 'no'):
            # set
            set_option(option, int(yes_no == 'yes'))
            announce('%s set %s to %s' %
                    (nick_repr(me), option, yes_no), True)
        elif not yes_no:
            # get
            value = 'yes' if int(get(option)) == 1 else 'no'
            me.sendChatMessage('[room] %s=%s' % (option, value))
        else:
            me.sendChatMessage('[room] Bad argument')
    def do_prefer_nicks(me, yes_no):
        yes_no_command(me, 'prefer_nicks', yes_no)
    def do_allow_list(me, yes_no):
        yes_no_command(me, 'allow_list', yes_no)
    def do_allow_pm(me, yes_no):
        yes_no_command(me, 'allow_pm', yes_no)
    def do_list_status(me, yes_no):
        yes_no_command(me, 'list_status', yes_no)
    def do_list_role(me, yes_no):
        yes_no_command(me, 'list_role', yes_no)
    def do_show_admin_actions(me, yes_no):
        yes_no_command(me, 'show_admin_actions', yes_no)
    def do_show_enter_leave(me, yes_no):
        yes_no_command(me, 'show_enter_leave', yes_no)
    def do_welcome_help(me, yes_no):
        yes_no_command(me, 'welcome_help', yes_no)
    def do_default_role(me, role):
        if role in ('banned', 'nobody', 'guest', 'user'):
            # set
            set_option('default_role', role)
            announce('%s set default role to %s' %
                    (nick_repr(me), role), True)
        elif not role:
            # get
            me.sendChatMessage('[room] default_role=%s' % get('default_role'))
    def do_password(me, password):
        if password:
            # set
            set_option('password', password)
            announce('%s set password to %s' % (nick_repr(me), password), True)
        else:
            # get
            me.sendChatMessage('[room] password=%s' % get('password'))
    def do_add_admin(me, nick):
        buddy = buddy_from_nick(nick, me)
        if not buddy:
            me.sendChatMessage('[room] Unknown nick')
            return
        if role_of(buddy.address) in ('admin', 'owner'):
            me.sendChatMessage('[room] This user is already admin or owner')
            return
        set_role(buddy.address, 'admin')
        announce("%s set %s's role to admin" %
                (nick_repr(me), nick_repr(buddy)), True)
    def do_remove_admin(me, nick):
        buddy = buddy_from_nick(nick, me)
        if not buddy:
            me.sendChatMessage('[room] Unknown nick')
            return
        if role_of(buddy.address) != 'admin':
            me.sendChatMessage('[room] This user is not admin')
            return
        set_role(buddy.address, 'user')
        announce("%s set %s's role to user" %
                (nick_repr(me), nick_repr(buddy)), True)
    def resend(me, text):
        if not can(me.address, 'write'):
            me.sendChatMessage('[room] You can not send messages')
            return
        for buddy in buddy_list().list:
            if not can(buddy.address, 'read'):
                continue
            if buddy == me:
                continue
            if buddy == buddy_list().own_buddy:
                continue
            if me == buddy_list().own_buddy \
                    and text.startswith('['): # [room] [delayed] [private] etc
                continue
            if is_ignored(me.address, buddy.address):
                continue
            sender_nick = nick_repr(me, is_moder(buddy.address))
            resent_message = '%s: %s' % (sender_nick, text)
            buddy.sendChatMessage(resent_message)

    load_vars = vars()

    _message_execute = torchat.tc_client.ProtocolMsg_message.execute
    def message_execute(self):
        goood_message = self.buddy and self.buddy in self.bl.list
        if int(get("no_gui")) == 0 or not goood_message:
            _message_execute(self)
        if goood_message:
            me = self.buddy
            if get('password') and role_of(me.address) == 'nobody':
                if self.text == get('password'):
                    set_role(me.address, get('default_role'))
                    announce('%s entered password' % nick_repr(me), False)
                else:
                    me.sendChatMessage('[room] Wrong password')
                return
            if self.text.startswith('!'):
                command, argument = splitLine(self.text)
                command = command[1:] # pop "!"
                if command not in OWNER:
                    me.sendChatMessage('[room] Unknown command')
                    return
                if not can(me.address, command):
                    me.sendChatMessage('[room] Action not allowed')
                    return
                func_name = "do_%s" % command
                if func_name in load_vars:
                    func = load_vars[func_name]
                    func(me, argument)
                else:
                    me.sendChatMessage('[room] Not implemented')
                    return
            else:
                # resend message
                resend(me, self.text)
    torchat.tc_client.ProtocolMsg_message.execute = message_execute

    _add_me_execute = torchat.tc_client.ProtocolMsg_add_me.execute
    def add_me_execute(self):
        welcome = self.buddy not in buddy_list().list
        if role_of(self.buddy.address) == 'banned':
            self.buddy.sendChatMessage('[room] You are banned')
            return
        _add_me_execute(self)
        if int(get('welcome_help')) == 1 and welcome:
            do_help(self.buddy, None)
        if get('password') and welcome:
            set_role(self.buddy.address, 'nobody')
            self.buddy.sendChatMessage('[room] Enter password, please')
    torchat.tc_client.ProtocolMsg_add_me.execute = add_me_execute

    _remove_me_execute = torchat.tc_client.ProtocolMsg_remove_me.execute
    def remove_me_execute(self):
        user_left = self.buddy and self.buddy in buddy_list().list
        nick = nick_repr(self.buddy)
        _remove_me_execute(self)
        announce('%s left room' % nick, False)
    torchat.tc_client.ProtocolMsg_remove_me.execute = remove_me_execute

    def set_tr(lang, option, translation):
        setattr(torchat.TRANSLATIONS[lang],
                'DSET_CONFERENCE_' + option.upper(), translation)
    set_tr('en', 'no_gui', u'Do not reflect new messages in GUI')
    set_tr('ru', 'no_gui', u'Не отображать новые сообщения в графическом интерфейсе')
    set_tr('en', 'prefer_nicks', u'Show torchat nick if available instead of id to conference members')
    set_tr('ru', 'prefer_nicks', u'Показывать ник отправителя вместо id, если ник выставлен')
    set_tr('en', 'default_role', u'Default role for new members (banned|nobody|guest|user)')
    set_tr('ru', 'default_role', u'Роль по умолчанию для новых членов (banned|nobody|guest|user)')
    set_tr('en', 'allow_list', u'Regular users can get list of users')
    set_tr('ru', 'allow_list', u'Обычные пользователи могут смотреть список пользователей')
    set_tr('en', 'allow_pm', u'Allow private messages')
    set_tr('ru', 'allow_pm', u'Разрешить личные сообщения')
    set_tr('en', 'list_status', u'Regular users view status of users')
    set_tr('ru', 'list_status', u'Обычные пользователи видят статус пользователей')
    set_tr('en', 'list_role', u'Regular users view role of users')
    set_tr('ru', 'list_role', u'Обычные пользователи видят роль пользователей')
    set_tr('en', 'show_admin_actions', u"Regular users view administrators' actions")
    set_tr('ru', 'show_admin_actions', u'Обычные пользователи видят действия администраторов')
    set_tr('en', 'show_enter_leave', u'Regular users view somebody enter or leave room')
    set_tr('ru', 'show_enter_leave', u'Обычные пользователи видят, когда кто-то входит или выходит')
    set_tr('en', 'welcome_help', u'Welcome new users with help message')
    set_tr('ru', 'welcome_help', u'Приветствовать новых пользователей справкой')
    torchat.config.importLanguage()

    _constructor = torchat.dlg_settings.Dialog.__init__
    def constructor(self, main_window):
        _constructor(self, main_window)
        def tr(option):
            attr_name = 'DSET_CONFERENCE_' + option.upper()
            if hasattr(torchat.dlg_settings.lang, attr_name):
                return getattr(torchat.dlg_settings.lang, attr_name)
            else:
                return option
        def check(self, option):
            torchat.dlg.Check(self.p3, tr(option), ("conference", option))
        def text(self, option):
            torchat.dlg.Text(self.p3, tr(option), ("conference", option))
        check(self, 'no_gui')
        check(self, 'prefer_nicks')
        check(self, 'allow_list')
        check(self, 'allow_pm')
        check(self, 'list_status')
        check(self, 'list_role')
        check(self, 'show_admin_actions')
        check(self, 'show_enter_leave')
        check(self, 'welcome_help')
        text(self, 'default_role')
        self.p3.fit()
        self.outer_sizer.Fit(self)
    torchat.dlg_settings.Dialog.__init__ = constructor

