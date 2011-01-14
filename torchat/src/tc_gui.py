# -*- coding: UTF-8 -*-

##############################################################################
#                                                                            #
# Copyright (c) 2007-2010 Bernd Kreuss <prof7bit@gmail.com>                  #
#                                                                            #
# This program is licensed under the GNU General Public License V3,          #
# the full source code is included in the binary distribution.               #
#                                                                            #
# Included in the distribution are files from other open source projects:    #
# - TOR Onion Router (c) The Tor Project, 3-clause-BSD                       #
# - SocksiPy (c) Dan Haim, BSD Style License                                 #
# - Gajim buddy status icons (c) The Gajim Team, GNU GPL                     #
#                                                                            #
##############################################################################

# this is a graphical User interface for the TorChat client library.

import config
import wx
import tc_client
import sys
import os
import shutil
import time
import subprocess
import textwrap
import version
import dlg_settings
import translations
lang = translations.lang_en
tb = config.tb
tb1 = config.tb1

ICON_NAMES = {tc_client.STATUS_OFFLINE : "offline.png",
              tc_client.STATUS_ONLINE : "online.png",
              tc_client.STATUS_HANDSHAKE : "connecting.png",
              tc_client.STATUS_AWAY : "away.png",
              tc_client.STATUS_XA : "xa.png"}

_icon_images = {} #this is a cache for getStatusBitmap()

def getStatusBitmap(status):
    global _icon_images
    if not status in _icon_images:
        image = wx.Image(os.path.join(config.ICON_DIR, ICON_NAMES[status]), wx.BITMAP_TYPE_PNG)
        image.ConvertAlphaToMask()
        _icon_images[status] = image
    bitmap = _icon_images[status].ConvertToBitmap()
    return bitmap
class TaskbarIcon(wx.TaskBarIcon):
    def __init__(self, main_window):
        wx.TaskBarIcon.__init__(self)
        self.mw = main_window

        #load event icon
        img = wx.Image(os.path.join(config.ICON_DIR, "event.png"))
        img.ConvertAlphaToMask()
        self.event_icon = wx.IconFromBitmap(img.ConvertToBitmap())
        self.showStatus(self.mw.buddy_list.own_status)
        self.timer = wx.Timer(self, -1)
        self.blink_phase = False
        self.Bind(wx.EVT_TASKBAR_LEFT_DOWN, self.onLeftClick)
        self.Bind(wx.EVT_TIMER, self.onTimer)

    def showEvent(self):
        self.SetIcon(self.event_icon, self.getToolTipText())

    def showStatus(self, status):
        icon = wx.IconFromBitmap(getStatusBitmap(status))
        self.SetIcon(icon, self.getToolTipText())

    def onLeftClick(self, evt):
        self.mw.Show(not self.mw.IsShown())
        if self.mw.IsShown():
            self.mw.Iconize(False) # never show it minimized (can happen on KDE)

    def CreatePopupMenu(self):
        return TaskbarMenu(self.mw)

    def getToolTipText(self):
        text = "TorChat: %s" % config.getProfileLongName()
        for window in self.mw.chat_windows:
            if not window.IsShown():
                text += "\n" + window.getTitleShort()
        return text

    def blink(self, start=True):
        if start:
            self.timer.Start(500, False)
        else:
            self.timer.Stop()
            self.showStatus(self.mw.buddy_list.own_status)

    def onTimer(self, evt):
        self.blink_phase = not self.blink_phase
        if self.blink_phase:
            self.showStatus(self.mw.buddy_list.own_status)
        else:
            self.showEvent()

        #stop blinking, if there are no more hidden windows
        found = False
        for window in self.mw.chat_windows:
            if not window.IsShown():
                found = True
                break

        if not found:
            self.blink(False)


class TaskbarMenu(wx.Menu):
    def __init__(self, main_window):
        wx.Menu.__init__(self)
        self.mw = main_window
        self.mw.taskbar_icon.blink(False)

        # show/hide

        item = wx.MenuItem(self, wx.NewId(), lang.MTB_SHOW_HIDE_TORCHAT)
        self.AppendItem(item)
        self.Bind(wx.EVT_MENU, self.onShowHide, item)

        self.AppendSeparator()

        # (hidden) chat windows

        cnt = 0
        self.wnd = {}
        for window in self.mw.chat_windows:
            if not window.IsShown():
                id = wx.NewId()
                self.wnd[id] = window
                item = wx.MenuItem(self, id, window.getTitleShort())
                item.SetBitmap(getStatusBitmap(window.buddy.status))
                self.AppendItem(item)
                self.Bind(wx.EVT_MENU, self.onChatWindow, item)
                cnt += 1

        if cnt:
            self.AppendSeparator()

        # edit profile

        item = wx.MenuItem(self, wx.NewId(), lang.MPOP_EDIT_MY_PROFILE)
        self.AppendItem(item)
        self.Bind(wx.EVT_MENU, self.onProfile, item)

        # status

        item = wx.MenuItem(self, wx.NewId(), lang.ST_AVAILABLE)
        item.SetBitmap(getStatusBitmap(tc_client.STATUS_ONLINE))
        self.AppendItem(item)
        self.Bind(wx.EVT_MENU, self.onAvailable, item)

        item = wx.MenuItem(self, wx.NewId(), lang.ST_AWAY)
        item.SetBitmap(getStatusBitmap(tc_client.STATUS_AWAY))
        self.AppendItem(item)
        self.Bind(wx.EVT_MENU, self.onAway, item)

        item = wx.MenuItem(self, wx.NewId(), lang.ST_EXTENDED_AWAY)
        item.SetBitmap(getStatusBitmap(tc_client.STATUS_XA))
        self.AppendItem(item)
        self.Bind(wx.EVT_MENU, self.onXA, item)

        self.AppendSeparator()

        # quit

        item = wx.MenuItem(self, wx.NewId(), lang.MTB_QUIT)
        self.AppendItem(item)
        self.Bind(wx.EVT_MENU, self.onExit, item)

    def onShowHide(self, evt):
        self.mw.Show(not self.mw.IsShown())

    def onChatWindow(self, evt):
        self.wnd[evt.GetId()].Show()
        #force update of the tooltip text (window list)
        self.mw.taskbar_icon.showStatus(self.mw.buddy_list.own_status)

    def onExit(self, evt):
        self.mw.exitProgram()

    def onAvailable(self, evt):
        self.mw.status_switch.setStatus(tc_client.STATUS_ONLINE)

    def onAway(self, evt):
        self.mw.status_switch.setStatus(tc_client.STATUS_AWAY)

    def onXA(self, evt):
        self.mw.status_switch.setStatus(tc_client.STATUS_XA)

    def onProfile(self, evt):
        dialog = DlgEditProfile(self.mw, self.mw)
        dialog.ShowModal()

class NotificationWindow(wx.Frame):
    def __init__(self, mw, text, buddy):
        wx.Frame.__init__(self, mw, style=wx.FRAME_NO_TASKBAR | wx.NO_BORDER)
        self.panel = wx.Panel(self, style=wx.SIMPLE_BORDER)
        self.panel.SetBackgroundColour(wx.SystemSettings.GetColour(wx.SYS_COLOUR_INFOBK))
        sizer = wx.BoxSizer()
        self.panel.SetSizer(sizer)

        if buddy.profile_avatar_object <> None:
            bitmap = buddy.profile_avatar_object
        else:
            bitmap = wx.Bitmap(os.path.join(config.ICON_DIR, "torchat.png"), wx.BITMAP_TYPE_PNG)
        static_image = wx.StaticBitmap(self.panel, -1, bitmap)
        sizer.Add(static_image, 0, wx.ALL, 5 )

        self.label = wx.StaticText(self.panel)
        self.label.SetLabel(text)
        sizer.Add(self.label, 0, wx.ALL, 5 )

        wsizer = wx.BoxSizer()
        wsizer.Add(self.panel, 0, wx.ALL, 0)
        self.SetSizerAndFit(wsizer)
        self.Layout()

        # initialize animation
        cx, cy, maxx, maxy = wx.ClientDisplayRect()
        self.w, self.h = self.GetSize()
        self.x_end = maxx - self.w - 20
        self.y_end = maxy - self.h - 20

        self.x_pos = -self.w
        self.y_pos = self.y_end
        self.phase = 0

        self.SetPosition((self.x_pos, self.y_pos))
        
        # the following will prevent the focus 
        # stealing on windows
        self.Disable()
        self.Show()
        self.Enable()

        self.timer = wx.Timer(self, -1)
        self.Bind(wx.EVT_TIMER, self.onTimer)

        # start animation
        self.timer.Start(10, True)


    def onTimer(self, evt):
        if self.phase == 0:
            if self.x_pos < self.x_end:
                # move right and restart timer
                speed = ((self.x_end - self.x_pos) ^ 2) / 10
                self.x_pos += (1 + speed)
                self.SetPosition((self.x_pos, self.y_pos))
                self.timer.Start(10, True)
                return
            else:
                # we are at the right border.
                # now switch phase and wait a bit
                self.phase = 1
                self.timer.Start(3000, True)
                
                # and from now on we also close on mouse contact
                self.panel.Bind(wx.EVT_MOUSE_EVENTS, self.onMouse)
                return

        if self.phase == 1:
            if self.y_pos > -self.h:
                # move upwards and restart timer
                speed = ((self.y_end - self.y_pos) ^ 2) / 10
                self.y_pos -= (5 + speed)
                self.SetPosition((self.x_pos, self.y_pos))
                self.timer.Start(10, True)
                return
            else:
                # we reached the end of the animation
                self.Hide()
                self.Destroy()
        
    def onMouse(self, evt):
        # restart the timer to immediately end the waiting
        self.timer.Start(10, True)
        

class PopupMenu(wx.Menu):
    def __init__(self, main_window, type):
        wx.Menu.__init__(self)
        self.mw = main_window

        # options for buddy

        if type == "contact":
            self.buddy = self.mw.gui_bl.getSelectedBuddy()
            item = wx.MenuItem(self, wx.NewId(), lang.MPOP_CHAT)
            self.AppendItem(item)
            self.Bind(wx.EVT_MENU, self.mw.gui_bl.onDClick, item)

            item = wx.MenuItem(self, wx.NewId(), lang.MPOP_SEND_FILE)
            self.AppendItem(item)
            self.Bind(wx.EVT_MENU, self.onSendFile, item)

            if self.buddy.getOfflineMessages():
                item = wx.MenuItem(self, wx.NewId(), lang.MPOP_SHOW_OFFLINE_MESSAGES)
                self.AppendItem(item)
                self.Bind(wx.EVT_MENU, self.onShowOffline, item)

                item = wx.MenuItem(self, wx.NewId(), lang.MPOP_CLEAR_OFFLINE_MESSAGES)
                self.AppendItem(item)
                self.Bind(wx.EVT_MENU, self.onClearOffline, item)

            self.AppendSeparator()

            if not self.isCurrentBuddyLoggingActivated():
                item = wx.MenuItem(self, wx.NewId(), lang.MPOP_ACTIVATE_LOG)
                self.AppendItem(item)
                self.Bind(wx.EVT_MENU, self.onActivateLog, item)

                if self.hasOldLog():
                    item = wx.MenuItem(self, wx.NewId(), lang.MPOP_DELETE_EXISTING_LOG)
                    self.AppendItem(item)
                    self.Bind(wx.EVT_MENU, self.onDeleteLog, item)

            else:
                item = wx.MenuItem(self, wx.NewId(), lang.MPOP_STOP_LOG)
                self.AppendItem(item)
                self.Bind(wx.EVT_MENU, self.onStopLog, item)

                item = wx.MenuItem(self, wx.NewId(), lang.MPOP_DELETE_AND_STOP_LOG)
                self.AppendItem(item)
                self.Bind(wx.EVT_MENU, self.onDeleteLog, item)

            self.AppendSeparator()

            item = wx.MenuItem(self, wx.NewId(), lang.MPOP_EDIT_CONTACT)
            self.AppendItem(item)
            self.Bind(wx.EVT_MENU, self.onEdit, item)

            item = wx.MenuItem(self, wx.NewId(), lang.MPOP_DELETE_CONTACT)
            self.AppendItem(item)
            self.Bind(wx.EVT_MENU, self.onDelete, item)

            self.AppendSeparator()

        item = wx.MenuItem(self, wx.NewId(), lang.MPOP_ADD_CONTACT)
        self.AppendItem(item)
        self.Bind(wx.EVT_MENU, self.onAdd, item)

        #ask support

        item = wx.MenuItem(self, wx.NewId(), lang.MPOP_ASK_AUTHOR % config.get("branding", "support_name"))
        self.AppendItem(item)
        self.Bind(wx.EVT_MENU, self.onAskAuthor, item)

        item = wx.MenuItem(self, wx.NewId(), lang.MPOP_EDIT_MY_PROFILE)
        self.AppendItem(item)
        self.Bind(wx.EVT_MENU, self.onProfile, item)

        self.AppendSeparator()

        #settings

        item = wx.MenuItem(self, wx.NewId(), lang.MPOP_SETTINGS)
        self.AppendItem(item)
        self.Bind(wx.EVT_MENU, self.onSettings, item)

        #about

        item = wx.MenuItem(self, wx.NewId(), lang.MPOP_ABOUT)
        self.AppendItem(item)
        self.Bind(wx.EVT_MENU, self.onAbout, item)


        #exit program

        self.AppendSeparator()
        item = wx.MenuItem(self, wx.NewId(), lang.MTB_QUIT)
        self.AppendItem(item)
        self.Bind(wx.EVT_MENU, self.onQuit, item)

    def onSendFile(self, evt):
        title = lang.DFT_FILE_OPEN_TITLE % self.buddy.getAddressAndDisplayName()
        dialog = wx.FileDialog(self.mw, title, style=wx.OPEN)
        dialog.SetDirectory(config.getHomeDir())
        if dialog.ShowModal() == wx.ID_OK:
            file_name = dialog.GetPath()
            FileTransferWindow(self.mw, self.buddy, file_name)

    def onEdit(self, evt):
        dialog = DlgEditContact(self.mw, self.mw, self.buddy)
        dialog.ShowModal()

    def onDelete(self, evt):
        answer = wx.MessageBox(lang.D_CONFIRM_DELETE_MESSAGE % (self.buddy.address, self.buddy.name),
                               lang.D_CONFIRM_DELETE_TITLE,
                               wx.YES_NO|wx.NO_DEFAULT)
        if answer == wx.YES:
            #remove from list without disconnecting
            #this will send a remove_me message
            #the other buddy will then disconnect,
            #because there is not much it can do with the
            #connections anymore.
            self.mw.buddy_list.removeBuddy(self.buddy, disconnect=False)

    def onShowOffline(self, evt):
        om = self.buddy.getOfflineMessages()
        if om:
            om = lang.MSG_OFFLINE_QUEUED % (self.buddy.address, om)
        else:
            om = lang.MSG_OFFLINE_EMPTY % self.buddy.address
        wx.MessageBox(om, lang.MSG_OFFLINE_TITLE, wx.ICON_INFORMATION)

    def onClearOffline(self, evt):
        try:
            tc_client.wipeFile(self.buddy.getOfflineFileName())
        except:
            pass

    def getChatWindow(self):
        # this is called by the on*Log() functions,
        # because all logging is done by the chat windows,
        # logging does not belong to the functionality
        # of the Buddy in tc_client.py
        for window in self.mw.chat_windows:
            if window.buddy == self.buddy:
                print "found"
                return window

        #must open a hidden window
        return ChatWindow(self.mw, self.buddy, "", True)

    def isCurrentBuddyLoggingActivated(self):
        return os.path.exists(os.path.join(config.getDataDir(), '%s.log' % self.buddy.address))

    def hasOldLog(self):
        return os.path.exists(os.path.join(config.getDataDir(), 'disabled_%s.log' % self.buddy.address))

    def onActivateLog(self, evt):
        self.getChatWindow().onActivateLog(evt)

    def onStopLog(self, evt):
        self.getChatWindow().onStopLog(evt)

    def onDeleteLog(self, evt):
        self.getChatWindow().onDeleteLog(evt)

    def onAdd(self, evt):
        dialog = DlgEditContact(self.mw, self.mw)
        dialog.ShowModal()

    def onProfile(self, evt):
        dialog = DlgEditProfile(self.mw, self.mw)
        dialog.ShowModal()

    def onSettings(self, evt):
        dialog = dlg_settings.Dialog(self.mw)
        dialog.ShowModal()

    def onAbout(self, evt):
        wx.MessageBox(lang.ABOUT_TEXT % {"version":version.VERSION,
                                         "svn":version.VERSION_SVN,
                                         "copyright":config.COPYRIGHT,
                                         "python":".".join(str(x) for x in sys.version_info),
                                         "wx":wx.version(),
                                         "translators":config.getTranslators()},
                      lang.ABOUT_TITLE)

    def onAskAuthor(self, evt):
        if self.mw.buddy_list.getBuddyFromAddress(config.get("branding", "support_id")):
            wx.MessageBox(lang.DEC_MSG_ALREADY_ON_LIST % config.get("branding", "support_name"))
        else:
            dialog = DlgEditContact(self.mw, self.mw, add_author=True)
            dialog.ShowModal()

    def onQuit(self, evt):
        self.mw.exitProgram()


class DlgEditContact(wx.Dialog):
    def __init__(self, parent, main_window, buddy=None, add_author=False): #no buddy -> Add new
        wx.Dialog.__init__(self, parent, -1)
        self.mw = main_window
        self.bl = self.mw.buddy_list
        self.buddy = buddy
        if buddy is None:
            self.SetTitle(lang.DEC_TITLE_ADD)
            address = ""
            name = ""
        else:
            self.SetTitle(lang.DEC_TITLE_EDIT)
            address = buddy.address
            name = buddy.name

        self.panel = wx.Panel(self)

        #setup the sizers
        sizer = wx.GridBagSizer(vgap = 5, hgap = 5)
        box_sizer = wx.BoxSizer()
        box_sizer.Add(sizer, 1, wx.EXPAND | wx.ALL, 5)

        #address
        row = 0
        lbl = wx.StaticText(self.panel, -1, lang.DEC_TORCHAT_ID)
        sizer.Add(lbl, (row, 0))

        self.txt_address = wx.TextCtrl(self.panel, -1, address)
        self.txt_address.SetMinSize((250, -1))
        sizer.Add(self.txt_address, (row, 1), (1, 2))

        #name
        row += 1
        lbl = wx.StaticText(self.panel, -1, lang.DEC_DISPLAY_NAME)
        sizer.Add(lbl, (row, 0))

        self.txt_name = wx.TextCtrl(self.panel, -1, name)
        self.txt_name.SetMinSize((250, -1))
        sizer.Add(self.txt_name, (row, 1), (1, 2))

        #add-me message (new buddies)
        if not self.buddy:
            row += 1
            lbl = wx.StaticText(self.panel, -1, lang.DEC_INTRODUCTION)
            sizer.Add(lbl, (row, 0))

            self.txt_intro = wx.TextCtrl(self.panel, -1, "hello, my friend...")
            self.txt_intro.SetMinSize((250, -1))
            sizer.Add(self.txt_intro, (row, 1), (1, 2))

        if add_author:
            self.txt_address.SetValue(config.get("branding", "support_id"))
            self.txt_name.SetValue(config.get("branding", "support_name"))

        #buttons
        row += 1
        self.btn_cancel = wx.Button(self.panel, wx.ID_CANCEL, lang.BTN_CANCEL)
        sizer.Add(self.btn_cancel, (row, 1), flag=wx.EXPAND)

        self.btn_ok = wx.Button(self.panel, wx.ID_OK, lang.BTN_OK)
        self.btn_ok.SetDefault()
        sizer.Add(self.btn_ok, (row, 2), flag=wx.EXPAND)

        #fit the sizers
        self.panel.SetSizer(box_sizer)
        box_sizer.Fit(self)

        #bind the events
        self.btn_cancel.Bind(wx.EVT_BUTTON, self.onCancel)
        self.btn_ok.Bind(wx.EVT_BUTTON, self.onOk)

    def onOk(self, evt):
        address = self.txt_address.GetValue().rstrip().lstrip()
        if len(address) != 16:
            l = len(address)
            wx.MessageBox(lang.DEC_MSG_16_CHARACTERS % l)
            return

        for c in address:
            if c not in "0123456789abcdefghijklmnopqrstuvwxyz":
                wx.MessageBox(lang.DEC_MSG_ONLY_ALPANUM)
                return

        if self.buddy is None:
            buddy = tc_client.Buddy(address,
                          self.bl,
                          self.txt_name.GetValue())
            res = self.bl.addBuddy(buddy)
            if res == False:
                wx.MessageBox(lang.DEC_MSG_ALREADY_ON_LIST % address)
            else:
                if self.txt_intro.GetValue() <> "":
                    buddy.storeOfflineChatMessage(self.txt_intro.GetValue())
        else:
            address_old = self.buddy.address
            offline_file_name_old = self.buddy.getOfflineFileName()
            self.buddy.address = address
            offline_file_name_new = self.buddy.getOfflineFileName()
            self.buddy.name = self.txt_name.GetValue()
            self.bl.save()
            if address != address_old:
                self.buddy.disconnect()
                try:
                    os.rename(offline_file_name_old, offline_file_name_new)
                except:
                    pass

        self.Close()

    def onCancel(self,evt):
        self.Close()

class DlgEditProfile(wx.Dialog):
    def __init__(self, parent, main_window):
        wx.Dialog.__init__(self, parent, -1, title=lang.DEP_TITLE)
        self.mw = main_window
        self.panel = wx.Panel(self)

        #setup the sizers
        sizer = wx.GridBagSizer(vgap = 5, hgap = 5)
        box_sizer = wx.BoxSizer()
        box_sizer.Add(sizer, 1, wx.EXPAND | wx.ALL, 5)

        #avatar
        row = 0
        self.avatar = wx.StaticBitmap(self.panel, -1, self.getAvatarBitmap())
        sizer.Add(self.avatar, (row, 0), (2, 1))

        #name
        row = 0
        lbl = wx.StaticText(self.panel, -1, lang.DEP_NAME)
        sizer.Add(lbl, (row, 1))

        self.txt_name = wx.TextCtrl(self.panel, -1,
            config.get("profile", "name"))
        self.txt_name.SetMinSize((250, -1))
        sizer.Add(self.txt_name, (row, 2), (1, 2))

        #text
        row += 1
        lbl = wx.StaticText(self.panel, -1, lang.DEP_TEXT)
        sizer.Add(lbl, (row, 1))

        self.txt_text = wx.TextCtrl(self.panel, -1,
            config.get("profile", "text"),
            style=wx.TE_MULTILINE | wx.TE_PROCESS_ENTER)
        self.txt_text.SetMinSize((250, -1))
        sizer.Add(self.txt_text, (row, 2), (1, 2))

        #buttons
        row += 1
        self.btn_cancel = wx.Button(self.panel, wx.ID_CANCEL, lang.BTN_CANCEL)
        sizer.Add(self.btn_cancel, (row, 2), flag=wx.EXPAND)

        self.btn_ok = wx.Button(self.panel, wx.ID_OK, lang.BTN_OK)
        self.btn_ok.SetDefault()
        sizer.Add(self.btn_ok, (row, 3), flag=wx.EXPAND)

        #fit the sizers
        self.panel.SetSizer(box_sizer)
        box_sizer.Fit(self)

        #bind the events
        self.btn_cancel.Bind(wx.EVT_BUTTON, self.onCancel)
        self.btn_ok.Bind(wx.EVT_BUTTON, self.onOk)
        self.txt_text.Bind(wx.EVT_TEXT_ENTER, self.onEnter)
        self.avatar.Bind(wx.EVT_LEFT_UP, self.onAvatar)

        self.txt_text.SetFocus()

        self.avatar.SetDropTarget(AvatarDropTarget(self))
        self.txt_name.SetDropTarget(AvatarDropTarget(self))
        self.txt_text.SetDropTarget(AvatarDropTarget(self))

        # position the dialog near the mouse
        # (yes, I am paying attention to details)
        w,h = self.GetSize()
        sx, sy, sx1, sy1 = wx.ClientDisplayRect()
        (x,y) = wx.GetMousePosition()
        x = x - w/2
        y = y - h/2
        if x < sx:
            x = sx
        if y < sy:
            y = sy
        if x > sx1 - w:
            x = sx1 - w
        if y > sy1 - h:
            y = sy1 - h
        self.SetPosition((x,y))

    def getAvatarBitmap(self, file_name=None):
        if file_name:
            image = wx.Image(file_name, wx.BITMAP_TYPE_PNG)
            image.Rescale(64, 64, wx.IMAGE_QUALITY_HIGH)
            return wx.BitmapFromImage(image)
        else:
            if self.mw.buddy_list.own_avatar_data:
                image = wx.ImageFromData(64, 64, self.mw.buddy_list.own_avatar_data)
                if self.mw.buddy_list.own_avatar_data_alpha:
                    image.SetAlphaData(self.mw.buddy_list.own_avatar_data_alpha)
                return wx.BitmapFromImage(image)
            else:
                return wx.Bitmap(os.path.join(config.ICON_DIR, "torchat.png"), wx.BITMAP_TYPE_PNG)

    def onAvatarSelected(self, file_name):
        avatar_old = os.path.join(config.getDataDir(), "avatar.png")
        avatar_new = os.path.join(config.getDataDir(), "avatar_new.png")
        if file_name == avatar_old or file_name == avatar_new:
            wx.MessageBox(lang.DEP_WARN_IS_ALREADY, lang.DEP_WARN_TITLE)
        else:
            # onOk() will find the file avatar_new.png and know what to do
            shutil.copy(file_name, avatar_new)
            # set the new bitmap (in this dialog only)
            self.avatar.SetBitmap(self.getAvatarBitmap(avatar_new))

    def onAvatar(self, evt):
        title = lang.DEP_AVATAR_SELECT_PNG
        dialog = wx.FileDialog(self, title, style=wx.OPEN)
        dialog.SetWildcard("%s (*.png)|*.png|%s (*.*)|*.*" % (lang.DEP_PNG_FILES, lang.DEP_ALL_FILES))
        dialog.SetDirectory(config.getHomeDir())
        if dialog.ShowModal() == wx.ID_OK:
            self.onAvatarSelected(dialog.GetPath())
        pass

    def onEnter(self, evt):
        self.onOk(evt)

    def onCancel(self, evt):
        avatar_new = os.path.join(config.getDataDir(), "avatar_new.png")
        if os.path.exists(avatar_new):
            tc_client.wipeFile(avatar_new)

        self.Close()

    def onOk(self, evt):
        config.set("profile", "name", self.txt_name.GetValue())
        config.set("profile", "text", self.txt_text.GetValue())

        # replace the avatar if a new one has been selected
        avatar_old = os.path.join(config.getDataDir(), "avatar.png")
        avatar_new = os.path.join(config.getDataDir(), "avatar_new.png")
        if os.path.exists(avatar_new):
            shutil.copy(avatar_new, avatar_old)
            tc_client.wipeFile(avatar_new)
            self.mw.gui_bl.loadOwnAvatarData() # this will also send it

        for buddy in self.mw.buddy_list.list:
            buddy.sendProfile()
            buddy.sendStatus()

        self.Close()


class BuddyList(wx.ListCtrl):
    def __init__(self, parent, main_window):
        wx.ListCtrl.__init__(self, parent, -1, style=wx.LC_REPORT | wx.LC_NO_HEADER)
        self.mw = main_window
        self.bl = self.mw.buddy_list

        self.InsertColumn(0, "buddy")

        self.r_down = False
        self.last_mouse_time = time.time()
        self.tool_tip = None
        self.tool_tip_index = -1
        self.has_mouse = False

        self.il = wx.ImageList(16, 16)
        self.il_idx = {}
        for status in [tc_client.STATUS_OFFLINE,
                       tc_client.STATUS_HANDSHAKE,
                       tc_client.STATUS_ONLINE,
                       tc_client.STATUS_AWAY,
                       tc_client.STATUS_XA]:
            self.il_idx[status] = self.il.Add(getStatusBitmap(status))

        img_event = wx.Image(os.path.join(config.ICON_DIR, "event.png"))
        img_event.ConvertAlphaToMask()
        self.il_idx[100] = self.il.Add(img_event.ConvertToBitmap())

        self.SetImageList(self.il, wx.IMAGE_LIST_SMALL)
        self.blink_phase = False

        self.timer = wx.Timer(self, -1)
        self.Bind(wx.EVT_TIMER, self.onTimer, self.timer)
        self.old_sum = ""
        self.timer.Start(milliseconds=500, oneShot=False)

        self.Bind(wx.EVT_LEFT_DCLICK, self.onDClick)
        self.Bind(wx.EVT_LIST_ITEM_RIGHT_CLICK, self.onRClick)
        self.Bind(wx.EVT_RIGHT_DOWN, self.onRDown)
        self.Bind(wx.EVT_MOUSE_EVENTS, self.onMouse)
        self.Bind(wx.EVT_ENTER_WINDOW, self.onMouseEnter)
        self.Bind(wx.EVT_LEAVE_WINDOW, self.onMouseLeave)

        self.SetDropTarget(DropTarget(self.mw, None))

        if config.getint("gui", "color_text_use_system_colors") == 0:
            self.SetBackgroundColour(config.get("gui", "color_text_back"))
            self.SetForegroundColour(config.get("gui", "color_text_fore"))

        self.onListChanged()

        self.loadOwnAvatarData()

    def loadOwnAvatarData(self):
        file_name = os.path.join(config.getDataDir(), "avatar.png")
        if os.path.exists(file_name):
            print "(2) reading own avatar file %s" % file_name
            image = wx.Image(file_name, wx.BITMAP_TYPE_PNG)
            image.Rescale(64, 64, wx.IMAGE_QUALITY_HIGH)
            self.bl.own_avatar_data = image.GetData()
            print "(2) uncompressed image data: %i bytes" % len(self.bl.own_avatar_data)
            if image.HasAlpha():
                self.bl.own_avatar_data_alpha = image.GetAlphaData()
                print "(2) uncompressed aplha data: %i bytes" % len(self.bl.own_avatar_data_alpha)
            else:
                self.bl.own_avatar_data_alpha = ""
            for buddy in self.bl.list:
                buddy.sendAvatar()

    def setStatusIcon(self, index, image_idx):
        # we also store the image index in the ItemData because
        # we can then avoid setting it twice and avoid flickering
        old = self.GetItemData(index)
        if image_idx <> old:
            self.SetItemImage(index, image_idx)
            self.SetItemData(index, image_idx)

    def blinkBuddy(self, buddy, blink=True):
        name = buddy.getDisplayName()
        for index in xrange(0, self.GetItemCount()):
            if name == self.GetItemText(index):
                if blink:
                    if self.blink_phase:
                        self.setStatusIcon(index, self.il_idx[100])
                    else:
                        self.setStatusIcon(index, self.il_idx[buddy.status])
                else:
                    self.setStatusIcon(index, self.il_idx[buddy.status])

    def onTimer(self, evt):
        self.blink_phase = not self.blink_phase
        # blink all buddies with hidden chat windows
        for window in self.mw.chat_windows:
            if not window.IsShown():
                self.blinkBuddy(window.buddy, True)
            else:
                self.blinkBuddy(window.buddy, False)

        # tooltips:
        wp = wx.FindWindowAtPointer()
        if self.has_mouse and self.mw.IsActive():
            if time.time() - self.last_mouse_time > 0.5:
                index, flags = self.HitTest(self.ScreenToClient(wx.GetMousePosition()))
                if index == -1:
                    # not over any item (anymore)
                    self.closeToolTip()

                else:
                    # hovering over item
                    if self.tool_tip is None:
                        self.openToolTip(index)
        else:
            self.closeToolTip()

    def closeToolTip(self):
        if self.tool_tip <> None:
            self.tool_tip.Hide()
            self.tool_tip.Destroy()
            self.tool_tip = None
            self.tool_tip_index = -1

    def openToolTip(self, index):
        self.closeToolTip()
        self.tool_tip = BuddyToolTip(self, index)
        self.tool_tip_index = index

        #TODO: wx.PopupWindow stealing focus under wine
        #find a better way to prevent this
        wx.CallAfter(self.mw.SetFocus)

    def onDClick(self, evt):
        i = self.GetFirstSelected()
        if i <> -1:
            address = self.GetItemText(i)[0:16]
            for buddy in self.bl.list:
                if buddy.address == address:
                    found_window = False
                    for window in self.mw.chat_windows:
                        if window.buddy == buddy:
                            found_window = True
                            break

                    if not found_window:
                        window = ChatWindow(self.mw, buddy)

                    if not window.IsShown():
                        window.Show()

                    window.txt_out.SetFocus()
                    break

        evt.Skip()

    def onRClick(self, evt):
        index, flags = self.HitTest(evt.GetPosition())
        if index != -1:
            self.onMouseLeave(evt)
            self.mw.PopupMenu(PopupMenu(self.mw, "contact"))

    def onRDown(self, evt):
        index, flags = self.HitTest(evt.GetPosition())
        if index == -1:
            self.onMouseLeave(evt)
            self.mw.PopupMenu(PopupMenu(self.mw, "empty"))
        else:
            evt.Skip()

    def getSelectedBuddy(self):
        index = self.GetFirstSelected()
        addr = self.GetItemText(index)[0:16]
        return self.bl.getBuddyFromAddress(addr)

    def getBuddyFromXY(self, position):
        index, flags = self.HitTest(position)
        if index != -1:
            addr = self.GetItemText(index)[0:16]
            return self.bl.getBuddyFromAddress(addr)
        else:
            return None

    def onBuddyStatusChanged(self, buddy):
        assert isinstance(buddy, tc_client.Buddy)
        line = buddy.getDisplayName()
        index = self.FindItem(0, line)
        self.SetItemImage(index, self.il_idx[buddy.status])

        #notify the chat window
        for window in self.mw.chat_windows:
            if window.buddy == buddy:
                window.onBuddyStatusChanged()
                break

        # if a tooltip for this buddy is currently shown then refresh it
        if self.tool_tip <> None and index == self.tool_tip_index:
            self.openToolTip(index)

    def onBuddyProfileChanged(self, buddy):
        assert isinstance(buddy, tc_client.Buddy)

        #notify the chat window
        for window in self.mw.chat_windows:
            if window.buddy == buddy:
                window.onBuddyProfileChanged()
                break

        # if a tooltip for this buddy is currently shown then refresh it
        line = buddy.getDisplayName()
        index = self.FindItem(0, line)
        if self.tool_tip <> None and index == self.tool_tip_index:
            self.openToolTip(index)

    def onBuddyAvatarChanged(self, buddy):
        print "(2) converting %s avatar data into wx.Bitmap" % buddy.address
        try:
            image = wx.ImageFromData(64, 64, buddy.profile_avatar_data)
            if buddy.profile_avatar_data_alpha:
                print "(2) %s avatar has alpha channel" % buddy.address
                image.SetAlphaData(buddy.profile_avatar_data_alpha)
            buddy.profile_avatar_object = wx.BitmapFromImage(image)

        except:
            print "(2)  could not convert %s avatar data to wx.Bitmap" % buddy.address
            tb()

        # notify the chat window
        for window in self.mw.chat_windows:
            if window.buddy == buddy:
                window.onBuddyAvatarChanged()
                break

        # if a tooltip for this buddy is currently shown then refresh it
        line = buddy.getDisplayName()
        index = self.FindItem(0, line)
        if self.tool_tip <> None and index == self.tool_tip_index:
            self.openToolTip(index)


    def onListChanged(self):
        # usually called via callback from the client
        # whenever the client has saved the changed list

        # TODO: This whole method seems a bit ugly

        # remove items which are not in list anymore
        for index in xrange(0, self.GetItemCount()):
            found = False
            for buddy in self.bl.list:
                if buddy.getDisplayName() == self.GetItemText(index):
                    found = True
                    break
            if not found:
                self.DeleteItem(index)
                break

        #add new items to the list
        for buddy in self.bl.list:
            line = buddy.getDisplayName()
            index = self.FindItem(0, line)
            if index == -1:
                index = self.InsertImageStringItem(sys.maxint, line, self.il_idx[tc_client.STATUS_OFFLINE])
                self.SetColumnWidth(0, wx.LIST_AUTOSIZE)
                self.onBuddyStatusChanged(buddy)

    def onMouse(self, evt):
        self.has_mouse = True
        self.last_mouse_time = time.time()
        if self.tool_tip <> None:
            index, flags = self.HitTest(self.ScreenToClient(wx.GetMousePosition()))
            if index == -1:
                self.closeToolTip()
            elif index <> self.tool_tip_index:
                self.openToolTip(index)
            else:
                self.tool_tip.setPos(wx.GetMousePosition())
        evt.Skip()

    def onMouseEnter(self, evt):
        self.has_mouse = True

    def onMouseLeave(self, evt):
        self.has_mouse = False
        self.closeToolTip()

    def getBuddyFromIndex(self, index):
        name = self.GetItemText(index)
        for buddy in self.bl.list:
            if buddy.getDisplayName() == name:
                return buddy

class BuddyToolTip(wx.PopupWindow):
    def __init__(self, list, index):
        wx.PopupWindow.__init__(self, list)
        self.buddy = list.getBuddyFromIndex(index)
        self.mw = list.mw

        self.panel = wx.Panel(self, style=wx.SIMPLE_BORDER)
        self.panel.SetBackgroundColour(wx.SystemSettings.GetColour(wx.SYS_COLOUR_INFOBK))
        sizer = wx.BoxSizer()
        self.panel.SetSizer(sizer)

        if self.buddy.profile_avatar_object <> None:
            bitmap = self.buddy.profile_avatar_object
        else:
            bitmap = wx.Bitmap(os.path.join(config.ICON_DIR, "torchat.png"), wx.BITMAP_TYPE_PNG)
        self.avatar = wx.StaticBitmap(self.panel, -1, bitmap)
        sizer.Add(self.avatar, 0, wx.ALL, 5)

        name = self.buddy.name
        if self.buddy.profile_name <> u"":
            name = self.buddy.profile_name

        text =  "%s\n%s" % (self.buddy.address, name)

        if self.buddy.profile_text <> u"":
            text = "%s\n\n%s" % (text, textwrap.fill(self.buddy.profile_text, 30))

        if self.buddy.conn_in:
            text = "%s\n\n%s" % (text, lang.BPOP_CLIENT_SOFTWARE % (self.buddy.client, self.buddy.version))
        else:
            if self.buddy.status == tc_client.STATUS_HANDSHAKE:
                text = "%s\n\n%s" % (text, lang.BPOP_CONNECTED_AWAITING_RETURN_CONN)
            else:
                text = "%s\n\n%s" % (text, lang.BPOP_BUDDY_IS_OFFLINE)


        self.label = wx.StaticText(self.panel)
        self.label.SetLabel(text)
        sizer.Add(self.label, 0, wx.ALL, 5)

        # sizer for whole window, containing the panel
        wsizer = wx.BoxSizer()
        wsizer.Add(self.panel, 0, wx.ALL, 0)
        self.SetSizerAndFit(wsizer)
        self.Layout()

        self.setPos(wx.GetMousePosition())
        self.Show()

    def setPos(self, pos):
        self.SetPosition((pos.x +10, pos.y + 10))

class StatusSwitchList(wx.Menu):
    def __init__(self, status_switch):
        wx.Menu.__init__(self)
        self.status_switch = status_switch

        item = wx.MenuItem(self, wx.NewId(), lang.ST_AVAILABLE)
        item.SetBitmap(getStatusBitmap(tc_client.STATUS_ONLINE))
        self.AppendItem(item)
        self.Bind(wx.EVT_MENU, self.status_switch.onAvailable, item)

        item = wx.MenuItem(self, wx.NewId(), lang.ST_AWAY)
        item.SetBitmap(getStatusBitmap(tc_client.STATUS_AWAY))
        self.AppendItem(item)
        self.Bind(wx.EVT_MENU, self.status_switch.onAway, item)

        item = wx.MenuItem(self, wx.NewId(), lang.ST_EXTENDED_AWAY)
        item.SetBitmap(getStatusBitmap(tc_client.STATUS_XA))
        self.AppendItem(item)
        self.Bind(wx.EVT_MENU, self.status_switch.onXA, item)


class StatusSwitch(wx.Button):
    def __init__(self, parent, main_window):
        wx.Button.__init__(self, parent)
        self.parent = parent
        self.main_window = main_window
        self.status = self.main_window.buddy_list.own_status
        self.Bind(wx.EVT_BUTTON, self.onClick)
        self.setStatus(self.main_window.buddy_list.own_status)

    def onClick(self, evt):
        self.PopupMenu(StatusSwitchList(self))

    def onAvailable(self, evt):
        self.setStatus(tc_client.STATUS_ONLINE)

    def onAway(self, evt):
        self.setStatus(tc_client.STATUS_AWAY)

    def onXA(self, evt):
        self.setStatus(tc_client.STATUS_XA)

    def setStatus(self, status):
        self.status = status
        self.main_window.setStatus(status)
        if status == tc_client.STATUS_AWAY:
            status_text = lang.ST_AWAY
        if status == tc_client.STATUS_XA:
            status_text = lang.ST_EXTENDED_AWAY
        if status == tc_client.STATUS_ONLINE:
            status_text = lang.ST_AVAILABLE
        if status == tc_client.STATUS_OFFLINE:
            status_text = lang.ST_OFFLINE
        self.SetLabel(status_text)


class ChatWindow(wx.Frame):
    def __init__(self, main_window, buddy, message="",
                                    hidden=False,
                                    notify_offline_sent=False):
        wx.Frame.__init__(
            self,
            main_window,
            -1,
            size=(
                config.getint("gui", "chat_window_width"),
                config.getint("gui", "chat_window_height")
            )
        )

        self.mw = main_window
        self.buddy = buddy
        self.unread = 0
        self.updateTitle()

        self.splitter = wx.SplitterWindow(
            self, -1,
            style=wx.SP_3D |
                  wx.SP_BORDER |
                  wx.SP_LIVE_UPDATE
        )
        self.panel_lower = wx.Panel(self.splitter, -1)
        self.panel_upper = wx.Panel(self.splitter, -1)
        self.splitter.SetMinimumPaneSize(50)
        self.splitter.SetSashGravity(1)
        self.splitter.SetSashSize(3)

        # incoming text (upper area)
        self.txt_in = wx.TextCtrl(
            self.panel_upper,
            -1,
            style=wx.TE_READONLY |
                  wx.TE_MULTILINE |
                  wx.TE_AUTO_URL |
                  wx.TE_AUTO_SCROLL |
                  wx.TE_RICH2 |
                  wx.BORDER_SUNKEN
        )

        # outgoing text (lower area)
        self.txt_out = wx.TextCtrl(
            self.panel_lower,
            -1,
            style=wx.TE_MULTILINE |
                  wx.TE_RICH2 |
                  wx.BORDER_SUNKEN
        )

        self.doLayout() # set the sizers

        # restore peviously saved sash position
        lower = config.getint("gui", "chat_window_height_lower")
        w,h = self.GetSize()
        if lower > h - 50:
            lower = h - 50
        self.splitter.SetSashPosition(h - lower)

        self.setFontAndColor()
        self.insertBackLog()

        om = self.buddy.getOfflineMessages()
        if om:
            om = "\n*** %s\n" % lang.NOTICE_DELAYED_MSG_WAITING + om
            self.writeHintLine(om)

        self.txt_in.AppendText("\n") #scroll to end + 1 empty line

        if notify_offline_sent:
            self.notifyOfflineSent()

        if message != "":
            self.process(message)

        self.Bind(wx.EVT_CLOSE, self.onClose)
        self.Bind(wx.EVT_SHOW, self.onShow)
        self.txt_out.Bind(wx.EVT_KEY_DOWN, self.onKey)
        self.txt_in.Bind(wx.EVT_TEXT_URL, self.onURL)

        self.Bind(wx.EVT_ACTIVATE, self.onActivate)
        self.txt_in.Bind(wx.EVT_CONTEXT_MENU, self.onContextMenu)

        # file drop target
        self.txt_in.SetDropTarget(DropTarget(self.mw, self.buddy))

        # Only the upper part of the chat window will
        # accept files. The lower part only text and URLs
        self.txt_out.DragAcceptFiles(False)

        if not hidden:
            self.Show()

        self.mw.chat_windows.append(self)
        self.onBuddyStatusChanged()

    def doLayout(self):
        outer_sizer = wx.BoxSizer(wx.VERTICAL)
        sizer_lower = wx.BoxSizer(wx.HORIZONTAL)
        sizer_upper = wx.BoxSizer(wx.HORIZONTAL)
        sizer_upper.Add(self.txt_in, 1, wx.ALL|wx.EXPAND, 0)
        self.panel_upper.SetSizer(sizer_upper)
        sizer_lower.Add(self.txt_out, 1, wx.ALL|wx.EXPAND, 0)
        self.panel_lower.SetSizer(sizer_lower)
        self.splitter.SplitHorizontally(self.panel_upper, self.panel_lower)
        outer_sizer.Add(self.splitter, 1, wx.ALL|wx.EXPAND, 0)
        self.SetSizer(outer_sizer)
        self.Layout()

    def onShow(self, evt):
        # always make sure we are at the end when showing the window
        wx.CallAfter(self.txt_in.AppendText, "")
        if (config.isWindows()):
            # workaround for buggy richedit control on windows
            # scroll one line up and one line down to make it visible
            wx.CallAfter(self.txt_in.ScrollLines, -1)
            wx.CallAfter(self.txt_in.ScrollLines, 1)

    def insertBackLogContents(self, file_name):
        file = open(file_name)
        for line in file:
            self.writeHintLine(line.rstrip().decode("UTF-8"))
        file.close()

    def insertBackLog(self):
        old = os.path.join(config.getDataDir(), "disabled_%s.log" % self.buddy.address)
        cur = os.path.join(config.getDataDir(), "%s.log" % self.buddy.address)
        if os.path.exists(cur):
            self.insertBackLogContents(cur)
            self.writeHintLine("\n*** " + lang.LOG_IS_ACTIVATED % cur)
        else:
            if os.path.exists(old):
                self.insertBackLogContents(old)
                self.writeHintLine("\n*** " + lang.LOG_IS_STOPPED_OLD_LOG_FOUND % old)

    def setFontAndColor(self):
        font = wx.Font(
            config.getint("gui", "chat_font_size"),
            wx.SWISS,
            wx.NORMAL,
            wx.NORMAL,
            False,
            config.get("gui", "chat_font_name")
        )
        self.txt_out.SetFont(font)
        self.txt_in.SetFont(font)
        if config.getint("gui", "color_text_use_system_colors") == 0:
            self.txt_out.SetBackgroundColour(config.get("gui", "color_text_back"))
            self.txt_in.SetBackgroundColour(config.get("gui", "color_text_back"))
            self.txt_out.SetForegroundColour(config.get("gui", "color_text_fore"))
            self.txt_out.SetDefaultStyle(wx.TextAttr(config.get("gui", "color_text_fore")))

    def updateTitle(self):
        if self.unread == 1:
            title = "* "
        elif self.unread > 1:
            title = "*[%i] " % self.unread
        else:
            title = ""

        title += self.buddy.address
        if self.buddy.name != "":
            title += " (%s)" % self.buddy.name

        self.SetTitle(title + " %s" % config.getProfileLongName())

    def getTitleShort(self):
        t = self.GetTitle()
        return t[:-19]

    def writeColored(self, color, name, text):
        # this method will write to the chat window and
        # will also write to the log file if logging is enabled.
        self.txt_in.SetDefaultStyle(wx.TextAttr(config.get("gui", "color_time_stamp")))
        self.txt_in.write("%s " % time.strftime(config.get("gui", "time_stamp_format")))
        self.txt_in.SetDefaultStyle(wx.TextAttr(color))
        self.txt_in.write("%s: " % name)
        if config.getint("gui", "color_text_use_system_colors") == 0:
            self.txt_in.SetDefaultStyle(wx.TextAttr(config.get("gui", "color_text_fore")))
        else:
            self.txt_in.SetDefaultStyle(wx.TextAttr(wx.SystemSettings.GetColour(wx.SYS_COLOUR_WINDOWTEXT)))
        self.txt_in.write("%s\n" % text)

        # workaround scroll bug on windows
        # https://sourceforge.net/tracker/?func=detail&atid=109863&aid=665381&group_id=9863
        self.txt_in.ScrollLines(-1)
        self.txt_in.ShowPosition(self.txt_in.GetLastPosition())

        if os.path.exists(os.path.join(config.getDataDir(), "%s.log" % self.buddy.address)):
            logtext = "%s %s: %s" % (time.strftime("(%Y-%m-%d %H:%M)"), name, text)
            self.log(logtext)

    def writeHintLine(self, line):
        self.txt_in.SetDefaultStyle(wx.TextAttr(config.get("gui", "color_time_stamp")))
        self.txt_in.write("%s\n" % line)
        if config.getint("gui", "color_text_use_system_colors") == 0:
            self.txt_in.SetDefaultStyle(wx.TextAttr(config.get("gui", "color_text_fore")))
        else:
            self.txt_in.SetDefaultStyle(wx.TextAttr(wx.SystemSettings.GetColour(wx.SYS_COLOUR_WINDOWTEXT)))

        # workaround scroll bug on windows
        # https://sourceforge.net/tracker/?func=detail&atid=109863&aid=665381&group_id=9863
        self.txt_in.ScrollLines(-1)
        self.txt_in.ShowPosition(self.txt_in.GetLastPosition())

    def notify(self, name, message):
        #needs unicode
        if not self.IsActive():
            if config.getint("gui", "notification_flash_window"):
                self.RequestUserAttention(wx.USER_ATTENTION_INFO)
            self.unread += 1
            self.updateTitle()

            if config.getint("gui", "notification_popup"):
                nt = textwrap.fill("%s:\n%s" % (name, message), 40)
                NotificationWindow(self.mw, nt, self.buddy)

        if not self.IsShown():
            self.mw.taskbar_icon.blink()

    def notifyOfflineSent(self):
        #all should be unicode here
        message = "[%s]" % lang.NOTICE_DELAYED_MSG_SENT
        self.writeColored(config.get("gui", "color_nick_myself"), "myself", message)
        self.notify("to %s" % self.buddy.address, message)

    def process(self, message):
        #message must be unicode
        if self.buddy.name != "":
            name = self.buddy.name
        else:
            name = self.buddy.address
        self.writeColored(config.get("gui", "color_nick_buddy"), name, message)
        self.notify(name, message)

    def onActivate(self, evt):
        self.unread = 0
        self.updateTitle()
        evt.Skip()

    def onClose(self, evt):
        w,h = self.GetSize()
        config.set("gui", "chat_window_width", w)
        config.set("gui", "chat_window_height", h)
        config.set("gui", "chat_window_height_lower", h - self.splitter.GetSashPosition())
        self.mw.chat_windows.remove(self)
        self.Hide()
        self.Destroy()

    def onKey(self, evt):
        #TODO: in wine there is a problem with shift-enter
        if evt.GetKeyCode() == 13 and not evt.ShiftDown():
            self.onSend(evt)
        else:
            evt.Skip()

    def onSend(self, evt):
        evt.Skip()
        text = self.txt_out.GetValue().rstrip().lstrip()
        wx.CallAfter(self.txt_out.SetValue, "")
        if self.buddy.status not in  [tc_client.STATUS_OFFLINE, tc_client.STATUS_HANDSHAKE]:
            self.buddy.sendChatMessage(text)
            self.writeColored(config.get("gui", "color_nick_myself"),
                              "myself",
                              text)
        else:
            self.buddy.storeOfflineChatMessage(text)
            self.writeColored(config.get("gui", "color_nick_myself"),
                              "myself",
                              "[%s] %s" % (lang.NOTICE_DELAYED, text))

    def onURL(self, evt):
        #all URL mouse events trigger this
        if evt.GetMouseEvent().GetEventType() == wx.wxEVT_LEFT_DOWN:
            #left button down, now we need the URL
            start = evt.GetURLStart()
            end = evt.GetURLEnd()
            url = self.txt_in.GetRange(start, end)
            if config.isWindows():
                #this works very reliable
                subprocess.Popen(("cmd /c start %s" % url).split(), creationflags=0x08000000)
            else:
                #TODO: is this the way to go? better make it a config option.
                subprocess.Popen(("/etc/alternatives/x-www-browser %s" % url).split())
        else:
            evt.Skip()

    def onContextMenu(self, evt):
        menu = wx.Menu()

        id = wx.NewId()
        item = wx.MenuItem(menu, id, lang.CPOP_COPY)
        self.Bind(wx.EVT_MENU, self.onCopy, id=id)
        menu.AppendItem(item)
        sel_from, sel_to = self.txt_in.GetSelection()
        empty = (sel_from == sel_to)
        if empty:
            item.Enable(False)

        id = wx.NewId()
        item = wx.MenuItem(menu, id, lang.MPOP_SEND_FILE)
        self.Bind(wx.EVT_MENU, self.onSendFile, id=id)
        menu.AppendItem(item)

        id = wx.NewId()
        item = wx.MenuItem(menu, id, lang.MPOP_EDIT_CONTACT)
        self.Bind(wx.EVT_MENU, self.onEditBuddy, id=id)
        menu.AppendItem(item)

        menu.AppendSeparator()

        if not self.isLoggingActivated():
            item = wx.MenuItem(menu, wx.NewId(), lang.MPOP_ACTIVATE_LOG)
            menu.AppendItem(item)
            menu.Bind(wx.EVT_MENU, self.onActivateLog, item)

            if self.hasOldLog():
                item = wx.MenuItem(menu, wx.NewId(), lang.MPOP_DELETE_EXISTING_LOG)
                menu.AppendItem(item)
                menu.Bind(wx.EVT_MENU, self.onDeleteLog, item)

        else:
            item = wx.MenuItem(menu, wx.NewId(), lang.MPOP_STOP_LOG)
            menu.AppendItem(item)
            menu.Bind(wx.EVT_MENU, self.onStopLog, item)

            item = wx.MenuItem(menu, wx.NewId(), lang.MPOP_DELETE_AND_STOP_LOG)
            menu.AppendItem(item)
            menu.Bind(wx.EVT_MENU, self.onDeleteLog, item)

        self.PopupMenu(menu)
        menu.Destroy()

    def onCopy(self, evt):
        sel_from, sel_to = self.txt_in.GetSelection()
        if sel_from == sel_to:
            return
        text = self.txt_in.GetRange(sel_from, sel_to)
        clipdata = wx.TextDataObject()
        clipdata.SetText(text)
        wx.TheClipboard.Open()
        wx.TheClipboard.SetData(clipdata)
        wx.TheClipboard.Close()


    def onSendFile(self, evt):
        title = lang.DFT_FILE_OPEN_TITLE % self.buddy.getAddressAndDisplayName()
        dialog = wx.FileDialog(self, title, style=wx.OPEN)
        dialog.SetDirectory(config.getHomeDir())
        if dialog.ShowModal() == wx.ID_OK:
            file_name = dialog.GetPath()
            FileTransferWindow(self.mw, self.buddy, file_name)

    def onEditBuddy(self, evt):
        dialog = DlgEditContact(self, self.mw, self.buddy)
        dialog.ShowModal()

    def onBuddyStatusChanged(self):
        bmp = getStatusBitmap(self.buddy.status)
        icon = wx.IconFromBitmap(bmp)
        self.SetIcon(icon)

    def onBuddyAvatarChanged(self):
        # nothing to to yet
        pass

    def onBuddyProfileChanged(self):
        # nothing to to yet
        pass


    def isLoggingActivated(self):
        return os.path.exists(os.path.join(config.getDataDir(), '%s.log' % self.buddy.address))

    def hasOldLog(self):
        return os.path.exists(os.path.join(config.getDataDir(), 'disabled_%s.log' % self.buddy.address))

    def onActivateLog(self, evt):
        old = os.path.join(config.getDataDir(), "disabled_%s.log" % self.buddy.address)
        cur = os.path.join(config.getDataDir(), "%s.log" % self.buddy.address)
        if os.path.exists(old):
            shutil.move(old, cur)
        self.log("") # this will create it
        self.writeColored(config.get("gui", "color_nick_myself"), "***", "[%s]" % lang.LOG_STARTED)

    def onStopLog(self, evt):
        old = os.path.join(config.getDataDir(), "disabled_%s.log" % self.buddy.address)
        cur = os.path.join(config.getDataDir(), "%s.log" % self.buddy.address)
        if os.path.exists(cur):
            self.writeColored(config.get("gui", "color_nick_myself"), "***", "[%s]" % lang.LOG_STOPPED)
            shutil.move(cur, old)

    def onDeleteLog(self, evt):
        old = os.path.join(config.getDataDir(), "disabled_%s.log" % self.buddy.address)
        cur = os.path.join(config.getDataDir(), "%s.log" % self.buddy.address)
        tc_client.wipeFile(old)
        tc_client.wipeFile(cur)
        self.writeHintLine("*** %s" % lang.LOG_DELETED)

    def log(self, msg):
        file_name = os.path.join(config.getDataDir(), "%s.log" % self.buddy.address)
        if not os.path.exists(file_name):
            f = open(file_name, "w")
            f.write(("*** %s\r\n\r\n" % lang.LOG_HEADER).encode("UTF-8"))
        else:
            f = open(file_name, "a")

        if msg <> "":
            f.write(("%s\r\n" % msg).encode("UTF-8"))
        f.close()

class BetterFileDropTarget(wx.FileDropTarget):
    def getFileName(self, filenames):
        if len(filenames) == 0:
            return None

        file_name = filenames[0]

        # --- begin evil hack
        if not os.path.exists(file_name):
            #sometimes the file name is in utf8
            #but inside a unicode string!
            #TODO: must report this bug to wx
            print "(2) dropped file not found with dropped file_name, trying UTF-8 hack"
            try:
                file_name_utf8 = ""
                for c in file_name:
                    file_name_utf8 += chr(ord(c))
                file_name = file_name_utf8.decode("utf-8")
            except:
                tb()
                wx.MessageBox("there is a strange bug in wx for your platform with wx.FileDropTarget and non-ascii characters in file names")
                return None
        # --- end evil hack

        print "(2) file dropped: %s" % file_name
        return file_name

class DropTarget(BetterFileDropTarget):
    #TODO: file dopping does not work in wine at all
    def __init__(self, mw, buddy):
        wx.FileDropTarget.__init__(self)
        self.mw = mw
        self.buddy = buddy

    def OnDropFiles(self, x, y, filenames):
        if len(filenames) > 1:
            wx.MessageBox(lang.D_WARN_FILE_ONLY_ONE_MESSAGE,
                          lang.D_WARN_FILE_ONLY_ONE_TITLE)
            return

        file_name = self.getFileName(filenames)

        if file_name is None:
            return

        """
        if not self.window.buddy.isFullyConnected():
            wx.MessageBox(lang.D_WARN_BUDDY_OFFLINE_MESSAGE,
                          lang.D_WARN_BUDDY_OFFLINE_TITLE)
            return
        """

        if self.buddy:
            buddy = self.buddy
        else:
            # this is the drop target for the buddy list
            # find the buddy
            buddy = self.mw.gui_bl.getBuddyFromXY((x,y))
            if buddy:
                print "(2) file dropped at buddy %s" % buddy.address
            else:
                print "(2) file dropped on empty space, ignoring"
                return

        FileTransferWindow(self.mw, buddy, file_name)


class AvatarDropTarget(BetterFileDropTarget):
    def __init__(self, window):
        wx.FileDropTarget.__init__(self)
        self.window = window

    def OnDropFiles(self, x, y, filenames):
        file_name = self.getFileName(filenames)
        if file_name is None:
            return

        root, ext = os.path.splitext(file_name)
        if ext.lower() <> ".png":
            wx.MessageBox(lang.DEP_WARN_MUST_BE_PNG, lang.DEP_WARN_TITLE)
            return

        self.window.onAvatarSelected(file_name)

class FileTransferWindow(wx.Frame):
    def __init__(self, main_window, buddy, file_name, receiver=None):
        #if receiver is given (a FileReceiver instance) we initialize
        #a Receiver Window, else we initialize a sender window and
        #let the client library create us a FileSender instance
        wx.Frame.__init__(self, main_window, -1)
        self.mw = main_window
        self.buddy = buddy

        self.bytes_total = 1
        self.bytes_complete = 0
        self.file_name = file_name
        self.file_name_save = ""
        self.completed = False
        self.error = False
        self.error_msg = ""

        if not receiver:
            self.is_receiver = False
            self.transfer_object = self.buddy.sendFile(self.file_name,
                                                       self.onDataChange)
        else:
            self.is_receiver = True
            receiver.setCallbackFunction(self.onDataChange)
            self.transfer_object = receiver
            self.bytes_total = receiver.file_size
            self.file_name = file_name

        self.panel = wx.Panel(self)
        self.outer_sizer = wx.BoxSizer()
        grid_sizer = wx.GridBagSizer(vgap = 5, hgap = 5)
        grid_sizer.AddGrowableCol(0)
        self.outer_sizer.Add(grid_sizer, 1, wx.EXPAND | wx.ALL, 5)

        self.text = wx.StaticText(self.panel, -1, "")
        self.text.SetMinSize((300,-1))
        grid_sizer.Add(self.text, (0, 0), (1, 4), wx.EXPAND)

        self.progress_bar = wx.Gauge(self.panel)
        grid_sizer.Add(self.progress_bar, (1, 0), (1, 4), wx.EXPAND)

        self.btn_cancel = wx.Button(self.panel, wx.ID_CANCEL, lang.BTN_CANCEL)
        self.btn_cancel.Bind(wx.EVT_BUTTON, self.onCancel)

        if self.is_receiver:
            grid_sizer.Add(self.btn_cancel, (2, 2))

            self.btn_save = wx.Button(self.panel, wx.ID_SAVEAS, lang.BTN_SAVE_AS)
            grid_sizer.Add(self.btn_save, (2, 3))
            self.btn_save.Bind(wx.EVT_BUTTON, self.onSave)
            self.SetDefaultItem(self.btn_save)
            self.btn_save.SetFocus()
        else:
            grid_sizer.Add(self.btn_cancel, (2, 3))

        self.panel.SetSizer(self.outer_sizer)
        self.updateOutput()
        self.outer_sizer.Fit(self)

        self.Show()

    def updateOutput(self):
        if self.bytes_complete == -1:
            self.error = True
            self.completed = True
            self.bytes_complete = 0

        percent = 100.0 * self.bytes_complete / self.bytes_total
        peer_name = self.buddy.address
        if self.buddy.name != "":
            peer_name += " (%s)" % self.buddy.name
        title = "%04.1f%% - %s" % (percent, os.path.basename(self.file_name))
        self.SetTitle(title)
        self.progress_bar.SetValue(percent)

        if self.is_receiver:
            text = lang.DFT_RECEIVE \
                % (os.path.basename(self.file_name),
                   peer_name, percent,
                   self.bytes_complete,
                   self.bytes_total)
        else:
            text = lang.DFT_SEND \
                % (os.path.basename(self.file_name),
                   peer_name, percent,
                   self.bytes_complete,
                   self.bytes_total)

        try:
            # the client has no translation files,
            # it will send these english error messages
            error_msg_trans = {
                "waiting for connection" : lang.DFT_WAITING,
                "starting transfer" : lang.DFT_STARTING,
                "transfer complete" : lang.DFT_COMPLETE,
                "transfer aborted" : lang.DFT_ABORTED,
                "error" : lang.DFT_ERROR
            }[self.error_msg]
        except:
            error_msg_trans = self.error_msg

        # error_msg might also contain info messages like
        # "waiting", etc. which are not fatal errors
        text = "%s    %s" % (text, error_msg_trans)

        # a fatal error
        if self.error:
            self.btn_cancel.SetLabel(lang.BTN_CLOSE)
            if self.is_receiver:
                self.btn_save.Enable(False)

        self.text.SetLabel(text)

        if self.bytes_complete == self.bytes_total:
            self.completed = True
            self.progress_bar.SetValue(100)
            if self.is_receiver:
                if self.file_name_save != "":
                    self.btn_cancel.SetLabel(lang.BTN_CLOSE)
                    self.transfer_object.close() #this will actually save the file
            else:
                self.btn_cancel.SetLabel(lang.BTN_CLOSE)

    def onDataChange(self, total, complete, error_msg=""):
        #will be called from the FileSender/FileReceiver-object in the
        #protocol module to update gui (called from a non-GUI thread!)
        self.bytes_total = total
        self.bytes_complete = complete
        self.error_msg = error_msg

        #we must use wx.Callafter to make calls into wx
        #because we are *NOT* in the context of the GUI thread here
        wx.CallAfter(self.updateOutput)

    def onCancel(self, evt):
        try:
            # this is not always a real "cancel":
            # FileReceiver.close() *after* successful transmission
            # will save the file (if file name is known)
            self.transfer_object.close()
        except:
            pass
        self.Close()

    def onSave(self, evt):
        title = lang.DFT_FILE_SAVE_TITLE % self.buddy.getAddressAndDisplayName()
        dialog = wx.FileDialog(self, title, defaultFile=self.file_name, style=wx.SAVE)
        if config.isPortable():
            dialog.SetDirectory(config.getDataDir())
        else:
            dialog.SetDirectory(config.getHomeDir())
        if dialog.ShowModal() == wx.ID_OK:
            self.file_name_save = dialog.GetPath()

            if os.path.exists(self.file_name_save):
                overwrite = wx.MessageBox(lang.D_WARN_FILE_ALREADY_EXISTS_MESSAGE % self.file_name_save,
                                          lang.D_WARN_FILE_ALREADY_EXISTS_TITLE,
                                          wx.YES_NO)
                if overwrite != wx.YES:
                    self.file_name_save = ""
                    return

            self.transfer_object.setFileNameSave(self.file_name_save)
            if not self.transfer_object.file_handle_save:
                error = self.transfer_object.file_save_error
                wx.MessageBox(lang.D_WARN_FILE_SAVE_ERROR_MESSAGE % (self.file_name_save, error),
                              lang.D_WARN_FILE_SAVE_ERROR_TITLE)
                self.file_name_save = ""
                return

            self.btn_save.Enable(False)
            if self.completed:
                self.onCancel(evt)
        else:
            pass


class MainWindow(wx.Frame):
    def __init__(self, socket=None):
        wx.Frame.__init__(
            self,
            None,
            -1,
            "TorChat",
            size=(
                config.getint("gui", "main_window_width"),
                config.getint("gui", "main_window_height")
            )
        )
        self.conns = []
        self.chat_windows = []
        self.notification_window = None
        self.buddy_list = tc_client.BuddyList(self.callbackMessage, socket)

        self.SetTitle("TorChat: %s" % config.getProfileLongName())

        self.Bind(wx.EVT_CLOSE, self.onClose)

        # setup gui elements
        self.taskbar_icon = TaskbarIcon(self)
        self.main_panel = wx.Panel(self)
        sizer = wx.BoxSizer(wx.VERTICAL)
        self.gui_bl = BuddyList(self.main_panel, self)
        sizer.Add(self.gui_bl, 1, wx.EXPAND)

        self.status_switch = StatusSwitch(self.main_panel, self)
        sizer.Add(self.status_switch, 0, wx.EXPAND)

        self.main_panel.SetSizer(sizer)
        sizer.FitInside(self)

        icon = wx.Icon(name=os.path.join(config.ICON_DIR, "torchat.ico"),
                       type=wx.BITMAP_TYPE_ICO)
        self.SetIcon(icon)

        if not config.getint("gui", "open_main_window_hidden"):
            self.Show()
            
        if config.get("logging", "log_file") and config.getint("logging", "log_level"):
            print "(0) logging to file may leave sensitive information on disk"
            hidden = config.getint("gui", "open_chat_window_hidden")
            wx.CallAfter(
                ChatWindow,
                self,
                self.buddy_list.own_buddy,
                lang.D_LOG_WARNING_MESSAGE % config.log_writer.file_name, hidden
            )

    def setStatus(self, status):
        self.buddy_list.setStatus(status)
        self.taskbar_icon.showStatus(status)

    def callbackMessage(self, callback_type, callback_data):
        #we must always use wx.CallAfter() to interact with
        #the GUI-Thread because this method will be called
        #in the context of one of the connection threads

        if callback_type == tc_client.CB_TYPE_CHAT:
            buddy, message = callback_data
            for window in self.chat_windows:
                if window.buddy == buddy:
                    wx.CallAfter(window.process, message)
                    return
            #no window found, so we create a new one
            hidden = config.getint("gui", "open_chat_window_hidden")
            wx.CallAfter(ChatWindow, self, buddy, message, hidden)

            #we let this thread block until the window
            #shows up in our chat window list
            found = False
            while not found:
                time.sleep(1)
                for window in self.chat_windows:
                    if window.buddy == buddy:
                        found = True
                        break

        if callback_type == tc_client.CB_TYPE_OFFLINE_SENT:
            buddy = callback_data
            for window in self.chat_windows:
                if window.buddy == buddy:
                    wx.CallAfter(window.notifyOfflineSent)
                    return

            hidden = config.getint("gui", "open_chat_window_hidden")
            wx.CallAfter(ChatWindow, self, buddy, "", hidden, notify_offline_sent=True)

        if callback_type == tc_client.CB_TYPE_FILE:
            #this happens when an incoming file transfer was initialized
            #we must now create a FileTransferWindow and return its
            #event handler method to the caller
            receiver = callback_data
            buddy = receiver.buddy
            file_name = receiver.file_name

            #we cannot get return values from wx.CallAfter() calls
            #so we have to CallAfter() and then just wait for
            #the TransferWindow to appear.
            wx.CallAfter(FileTransferWindow, self,
                                             buddy,
                                             file_name,
                                             receiver)

        if callback_type == tc_client.CB_TYPE_STATUS:
            # this is called when the status of one of the
            # buddies has changed. callback_data is the Buddy instance
            wx.CallAfter(self.gui_bl.onBuddyStatusChanged, callback_data)

        if callback_type == tc_client.CB_TYPE_AVATAR:
            # this is called when the avatar of one of the
            # buddy has changed. callback_data is the Buddy instance
            wx.CallAfter(self.gui_bl.onBuddyAvatarChanged, callback_data)

        if callback_type == tc_client.CB_TYPE_PROFILE:
            # this is called when the profile of one of the
            # buddy has changed. callback_data is the Buddy instance
            wx.CallAfter(self.gui_bl.onBuddyProfileChanged, callback_data)

        if callback_type == tc_client.CB_TYPE_LIST_CHANGED:
            try:
                wx.CallAfter(self.gui_bl.onListChanged)
            except:
                # might be too early and there is no gui_bl object yet.
                # But this does not matter because gui_bl itself will
                # call this at least once after initialization.
                pass

        if callback_type == tc_client.CB_TYPE_REMOVE:
            # called when the client is removing the buddy from the list
            # callback_data is the buddy
            for window in self.chat_windows:
                if window.buddy == callback_data:
                    wx.CallAfter(window.Close)

    def onClose(self, evt):
        self.Show(False)

    def exitProgram(self):
        w,h = self.GetSize()
        config.set("gui", "main_window_width", w)
        config.set("gui", "main_window_height", h)
        found_unread = False
        for window in self.chat_windows:
            if not window.IsShown() or window.unread:
                found_unread = True
                break

        if found_unread:
            answer = wx.MessageBox(lang.D_WARN_UNREAD_MESSAGE,
                                   lang.D_WARN_UNREAD_TITLE,
                                   wx.YES_NO|wx.NO_DEFAULT)

            if answer == wx.NO:
                return

        self.taskbar_icon.RemoveIcon()
        self.buddy_list.stopClient() #this will also stop portable Tor

        # All my threads wouldn't join properly. Don't know why.
        # sys.exit() would spew lots of tracebacks *sometimes*,
        # so let's do it the easy way and just kill ourself:
        config.killProcess(os.getpid())
