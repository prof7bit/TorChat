# -*- coding: UTF-8 -*-

##############################################################################
#                                                                            #
# Copyright (c) 2007-2008 Bernd Kreuss <prof7bit@gmail.com>                  #
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

import wx
import tc_client
import sys
import os
import time
import subprocess
import textwrap
import threading
import version
import config


ICON_NAMES = {tc_client.STATUS_OFFLINE : "offline.png",
              tc_client.STATUS_ONLINE : "online.png",
              tc_client.STATUS_HANDSHAKE : "connecting.png",
              tc_client.STATUS_AWAY : "away.png",
              tc_client.STATUS_XA : "xa.png"}


def isWindows():
    return "win" in sys.platform

def getStatusBitmap(status):
    return wx.Bitmap(os.path.join(config.ICON_DIR, ICON_NAMES[status]), wx.BITMAP_TYPE_PNG)

class TaskbarIcon(wx.TaskBarIcon):
    def __init__(self, main_window):
        wx.TaskBarIcon.__init__(self)
        self.mw = main_window
        self.showStatus(self.mw.buddy_list.own_status)
        self.timer = wx.Timer(self, -1)
        self.blink_phase = False
        self.Bind(wx.EVT_TASKBAR_LEFT_DOWN, self.onLeftClick)
        self.Bind(wx.EVT_TIMER, self.onTimer)

    def showEvent(self):
        img = wx.Image(os.path.join(config.ICON_DIR, "event.png"))
        img.ConvertAlphaToMask()
        bmp = img.ConvertToBitmap()
        icon = wx.IconFromBitmap(bmp)
        self.SetIcon(icon, self.getToolTipText())
        
    def showStatus(self, status):
        icon_name = ICON_NAMES[status]
        img = wx.Image(os.path.join(config.ICON_DIR, icon_name))
        img.ConvertAlphaToMask()
        bmp = img.ConvertToBitmap()
        icon = wx.IconFromBitmap(bmp)
        self.SetIcon(icon, self.getToolTipText())

    def onLeftClick(self, evt):
        self.mw.Show(not self.mw.IsShown())

    def CreatePopupMenu(self):
        return TaskbarMenu(self.mw)

    def getToolTipText(self):
        text = "TorChat [%s]" % config.get("client", "own_hostname")
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


class TaskbarMenu(wx.Menu):
    def __init__(self, main_window):
        wx.Menu.__init__(self)
        self.mw = main_window
        self.mw.taskbar_icon.blink(False)

        # show/hide

        item = wx.MenuItem(self, wx.NewId(), "Show/Hide TorChat")
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

        # status

        item = wx.MenuItem(self, wx.NewId(), "Available")
        item.SetBitmap(getStatusBitmap(tc_client.STATUS_ONLINE))
        self.AppendItem(item)
        self.Bind(wx.EVT_MENU, self.onAvailable, item)

        item = wx.MenuItem(self, wx.NewId(), "Away")
        item.SetBitmap(getStatusBitmap(tc_client.STATUS_AWAY))
        self.AppendItem(item)
        self.Bind(wx.EVT_MENU, self.onAway, item)

        item = wx.MenuItem(self, wx.NewId(), "Extended Away")
        item.SetBitmap(getStatusBitmap(tc_client.STATUS_XA))
        self.AppendItem(item)
        self.Bind(wx.EVT_MENU, self.onXA, item)

        self.AppendSeparator()

        # quit
        
        item = wx.MenuItem(self, wx.NewId(), "Quit")
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


class NotificationWindowAnimation(threading.Thread):
    def __init__(self, win):
        threading.Thread.__init__(self)
        self.win = win
        self.start()
        
    def run(self):
        cx, cy, maxx, maxy = wx.ClientDisplayRect()
        w, h = self.win.GetSize()
        self.x_end = maxx - w - 20
        self.y_end = maxy - h - 20
        self.win.SetPosition((-w, self.y_end))
        wx.CallAfter(self.win.Show)
        for x in range(-w, self.x_end, 20):
            wx.CallAfter(self.win.SetPosition, (x, self.y_end))
            time.sleep(0.01)
        wx.CallAfter(self.win.SetPosition, (self.x_end, self.y_end))

        time.sleep(5)
                
        w, h = self.win.GetSize()
        for y in reversed(range(-h, self.y_end, 20)):
            wx.CallAfter(self.win.SetPosition, (self.x_end, y))
            time.sleep(0.01)
        wx.CallAfter(self.win.Hide)
        wx.CallAfter(self.win.Close)
                

class NotificationWindow(wx.PopupWindow):
    def __init__(self, mw, text):
        wx.PopupWindow.__init__(self, mw)
        self.panel = wx.Panel(self)
        sizer = wx.BoxSizer()
        self.panel.SetSizer(sizer)

        bitmap = wx.Bitmap(config.ICON_DIR + "/torchat.png", wx.BITMAP_TYPE_PNG)
        static_image = wx.StaticBitmap(self.panel, -1, bitmap)
        sizer.Add(static_image, 0, wx.ALL, 5 )
        
        self.label = wx.StaticText(self.panel)
        font = self.label.GetFont()
        font.SetPointSize(12)
        self.label.SetFont(font)
        self.label.SetLabel(text)
        sizer.Add(self.label, 0, wx.ALL, 5 )
        
        wsizer = wx.BoxSizer()
        wsizer.Add(self.panel, 0, wx.ALL, 0)
        self.SetSizerAndFit(wsizer)
        self.Layout()        
           
        self.a = NotificationWindowAnimation(self)
        

class PopupMenu(wx.Menu):
    def __init__(self, main_window, type):
        wx.Menu.__init__(self)
        self.mw = main_window

        # options for buddy

        if type == "contact": 
            item = wx.MenuItem(self, wx.NewId(), "Chat")
            self.AppendItem(item)
            self.Bind(wx.EVT_MENU, self.mw.gui_bl.onDClick, item)

            item = wx.MenuItem(self, wx.NewId(), "Send file...")
            self.AppendItem(item)
            self.Bind(wx.EVT_MENU, self.onSendFile, item)

            item = wx.MenuItem(self, wx.NewId(), "Edit contact...")
            self.AppendItem(item)
            self.Bind(wx.EVT_MENU, self.onEdit, item)

            item = wx.MenuItem(self, wx.NewId(), "Delete contact...")
            self.AppendItem(item)
            self.Bind(wx.EVT_MENU, self.onDelete, item)

            item = wx.MenuItem(self, wx.NewId(), "Show queued offline messages ")
            self.AppendItem(item)
            self.Bind(wx.EVT_MENU, self.onShowOffline, item)

            item = wx.MenuItem(self, wx.NewId(), "Clear queued offline messages ")
            self.AppendItem(item)
            self.Bind(wx.EVT_MENU, self.onClearOffline, item)

        if type == "empty": 
            item = wx.MenuItem(self, wx.NewId(), "Add contact...")
            self.AppendItem(item)
            self.Bind(wx.EVT_MENU, self.onAdd, item)

        self.AppendSeparator()

        #about
        
        item = wx.MenuItem(self, wx.NewId(), "About TorChat")
        self.AppendItem(item)
        self.Bind(wx.EVT_MENU, self.onAbout, item)

        #ask bernd
        
        if type == "empty": 
            self.AppendSeparator()
            item = wx.MenuItem(self, wx.NewId(), "Ask %s" % config.AUTHORS_NAME)
            self.AppendItem(item)
            self.Bind(wx.EVT_MENU, self.onAskAuthor, item)

    def onSendFile(self, evt):
        buddy = self.mw.gui_bl.getSelectedBuddy()
        dialog = wx.FileDialog ( None, style = wx.OPEN )
        if dialog.ShowModal() == wx.ID_OK:
            file_name = dialog.GetPath()
            transfer_window = FileTransferWindow(self.mw, buddy, file_name)

    def onEdit(self, evt):
        buddy = self.mw.gui_bl.getSelectedBuddy()
        dialog = DlgEditContact(self.mw, buddy)
        dialog.ShowModal()

    def onDelete(self, evt):
        buddy = self.mw.gui_bl.getSelectedBuddy()
        answer = wx.MessageBox("Really delete this contact?\n%s\n%s" 
                               % (buddy.address, buddy.name), 
                               "Confirm deletion", 
                               wx.YES_NO|wx.NO_DEFAULT)
        if answer == wx.YES:
            #remove from list without disconnecting
            #this will send a remove_me message
            #the other buddy will then disconnect,
            #because there is not much it can do with the
            #connections anymore.
            self.mw.buddy_list.removeBuddy(buddy, disconnect=False)

    def onShowOffline(self, event):
        buddy = self.mw.gui_bl.getSelectedBuddy()
        om = buddy.getOfflineMessages().decode("UTF-8")
        if om:
            om = ("queued offline messages for %s:\n\n" % buddy.address) + om
        else:
            om = "there are no (more) queued messages for %s" % buddy.address
        wx.MessageBox(om, "offline messages", wx.ICON_INFORMATION)

    def onClearOffline(self, evt):
        buddy = self.mw.gui_bl.getSelectedBuddy()
        try:
            os.unlink(buddy.getOfflineFileName())
        except:
            pass

    def onAdd(self, evt):
        dialog = DlgEditContact(self.mw)
        dialog.ShowModal()

    def onAbout(self, evt):
        wx.MessageBox(about_text, "About TorChat")

    def onAskAuthor(self, evt):
        if self.mw.buddy_list.getBuddyFromAddress(config.AUTHORS_ID):
            wx.MessageBox("%s is already on your list" % config.AUTHORS_NAME)
        else:
            dialog = DlgEditContact(self.mw, add_author=True)
            dialog.ShowModal()


class DlgEditContact(wx.Dialog):
    def __init__(self, main_window, buddy=None, add_author=False): #no buddy -> Add new
        wx.Dialog.__init__(self, main_window, -1)
        self.mw = main_window
        self.bl = self.mw.buddy_list
        self.buddy = buddy
        if buddy == None:
            self.SetTitle("Add new contact")
            address = ""
            name = ""
        else:
            self.SetTitle("Edit contact")
            address = buddy.address
            name = buddy.name

        self.panel = wx.Panel(self)
        
        #setup the sizers
        sizer = wx.GridBagSizer(vgap = 5, hgap = 5)
        box_sizer = wx.BoxSizer()
        box_sizer.Add(sizer, 1, wx.EXPAND | wx.ALL, 5)
        
        #address
        row = 0
        lbl = wx.StaticText(self.panel, -1, "TorChat ID")
        sizer.Add(lbl, (row, 0))
        
        self.txt_address = wx.TextCtrl(self.panel, -1, address)
        self.txt_address.SetMinSize((250, -1))
        sizer.Add(self.txt_address, (row, 1), (1, 2))
        
        #name
        row += 1
        lbl = wx.StaticText(self.panel, -1, "Display name")
        sizer.Add(lbl, (row, 0))
        
        self.txt_name = wx.TextCtrl(self.panel, -1, name)
        self.txt_name.SetMinSize((250, -1))
        sizer.Add(self.txt_name, (row, 1), (1, 2))
        
        #add-me message (new buddies)
        if not self.buddy:
            row += 1
            lbl = wx.StaticText(self.panel, -1, "Introduction")
            sizer.Add(lbl, (row, 0))

            self.txt_intro = wx.TextCtrl(self.panel, -1, "hello, my friend...")
            self.txt_intro.SetMinSize((250, -1))
            sizer.Add(self.txt_intro, (row, 1), (1, 2))
        
        if add_author:
            self.txt_address.SetValue(config.AUTHORS_ID)
            self.txt_name.SetValue(config.AUTHORS_NAME)
        
        #buttons
        row += 1
        self.btn_cancel = wx.Button(self.panel, -1, "Cancel")
        sizer.Add(self.btn_cancel, (row, 1), flag=wx.EXPAND)
        
        self.btn_ok = wx.Button(self.panel, -1, "Ok")
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
            wx.MessageBox("The address must be 16 characters long, not %i." % l)
            return
        
        for c in address:
            if c not in "0123456789abcdefghijklmnopqrstuvwxyz":
                wx.MessageBox("The address must only contain numbers and lowercase letters")
                return
            
        if self.buddy == None:
            buddy = tc_client.Buddy(address, 
                          self.bl, 
                          self.txt_name.GetValue())
            res = self.bl.addBuddy(buddy)
            if res == False:
                wx.MessageBox("This contact is already on your list")
            else:
                buddy.storeOfflineChatMessage(self.txt_intro.GetValue().encode("UTF-8"))
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


class BuddyList(wx.ListCtrl):
    def __init__(self, parent, main_window):
        wx.ListCtrl.__init__(self, parent, -1, style=wx.LC_REPORT | wx.LC_NO_HEADER)
        self.mw = main_window
        self.bl = self.mw.buddy_list
        
        self.InsertColumn(0, "buddy")
        
        self.r_down = False
        
        self.il = wx.ImageList(16, 16)
        self.il_idx = {}
        for status in [tc_client.STATUS_OFFLINE, 
                       tc_client.STATUS_HANDSHAKE, 
                       tc_client.STATUS_ONLINE,
                       tc_client.STATUS_AWAY,
                       tc_client.STATUS_XA]:
            self.il_idx[status] = self.il.Add(getStatusBitmap(status))
        self.SetImageList(self.il, wx.IMAGE_LIST_SMALL)
        
        self.timer = wx.Timer(self, -1)
        self.Bind(wx.EVT_TIMER, self.onTimer, self.timer)
        self.old_sum = ""
        self.timer.Start(milliseconds=1000, oneShot=False)
        
        self.Bind(wx.EVT_LEFT_DCLICK, self.onDClick)
        self.Bind(wx.EVT_LIST_ITEM_RIGHT_CLICK, self.onRClick)
        self.Bind(wx.EVT_RIGHT_DOWN, self.onRDown)
        
    def onTimer(self, evt):
        #first check if there have been any changes to the buddy list
        sum = ""
        for buddy in self.bl.list:
            sum += buddy.address + buddy.name + str(buddy.status)
        if sum == self.old_sum:
            return
        self.old_sum = sum
        
        #remove items which are not in list anymore
        for index in xrange(0, self.GetItemCount()):
            found = False
            for buddy in self.bl.list:
                if buddy.getDisplayName() == self.GetItemText(index):
                    found = True
                    break
            if not found:
                self.DeleteItem(index)
                break
        
        #add new items to the list or change status icons
        for buddy in self.bl.list:
            line = buddy.getDisplayName()
            index = self.FindItem(0, line)
            if index == -1:
                index = self.InsertImageStringItem(sys.maxint, line, self.il_idx[tc_client.STATUS_OFFLINE])
            self.SetItemImage(index, self.il_idx[buddy.status])  
        
        self.SetColumnWidth(0, wx.LIST_AUTOSIZE)
        self.Refresh()
    
    def onDClick(self, evt):
        i = self.GetFirstSelected()
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
            self.mw.PopupMenu(PopupMenu(self.mw, "contact"))
        
    def onRDown(self, evt):
        index, flags = self.HitTest(evt.GetPosition())
        if index == -1:
            self.mw.PopupMenu(PopupMenu(self.mw, "empty"))
        else:
            evt.Skip()

    def getSelectedBuddy(self):
        index = self.GetFirstSelected()
        addr = self.GetItemText(index)[0:16]
        return self.bl.getBuddyFromAddress(addr)


class StatusSwitchList(wx.Menu):
    def __init__(self, status_switch):
        wx.Menu.__init__(self)
        self.status_switch = status_switch

        item = wx.MenuItem(self, wx.NewId(), "Available")
        item.SetBitmap(getStatusBitmap(tc_client.STATUS_ONLINE))
        self.AppendItem(item)
        self.Bind(wx.EVT_MENU, self.status_switch.onAvailable, item)

        item = wx.MenuItem(self, wx.NewId(), "Away")
        item.SetBitmap(getStatusBitmap(tc_client.STATUS_AWAY))
        self.AppendItem(item)
        self.Bind(wx.EVT_MENU, self.status_switch.onAway, item)

        item = wx.MenuItem(self, wx.NewId(), "Extended Away")
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
            status_text = "Away"
        if status == tc_client.STATUS_XA:
            status_text = "Extended Away"
        if status == tc_client.STATUS_ONLINE:
            status_text = "Available"
        if status == tc_client.STATUS_OFFLINE:
            status_text = "Offline"
        self.SetLabel(status_text)


class ChatWindow(wx.Frame):
    def __init__(self, main_window, buddy, message="", 
                                    hidden=False,
                                    notify_offline_sent=False):
        wx.Frame.__init__(self, 
                          main_window, 
                          -1, 
                          size=(400,400))
        
        self.mw = main_window
        self.buddy = buddy
        self.unread = 0
        self.updateTitle()
        
        self.panel = wx.Panel(self)
        sizer = wx.BoxSizer(wx.VERTICAL)
        
        self.txt_in = wx.TextCtrl(self.panel,
                                   -1,
                                   style=wx.TE_READONLY |
                                   wx.TE_MULTILINE |
                                   wx.TE_AUTO_URL |
                                   wx.TE_AUTO_SCROLL |
                                   wx.TE_RICH2 |
                                   wx.BORDER_SUNKEN)
        
        sizer.Add(self.txt_in, 1, wx.EXPAND | wx.ALL, 0)
        
        self.txt_out = wx.TextCtrl(self.panel,
                                   -1,
                                   style=wx.TE_MULTILINE |
                                   wx.TE_PROCESS_ENTER |
                                   wx.TE_RICH2 |
                                   wx.BORDER_SUNKEN)
        
        sizer.Add(self.txt_out, 0, wx.EXPAND | wx.ALL, 0)
        
        sizer.SetItemMinSize(self.txt_out, (-1,50))
        
        self.panel.SetSizer(sizer)
        sizer.FitInside(self)
        
        if not hidden:
            self.Show()
        
        om = self.buddy.getOfflineMessages()
        if om:
            om = "[delayed messages waiting to be sent]\n" + om
            self.writeColored((0,0,192), "myself", om.decode("UTF-8"))
        
        if message != "":
            self.process(message)
        
        if notify_offline_sent:
            self.notifyOfflineSent()
            
        self.timer = wx.Timer(self, -1)
        self.timer.Start(1000, False)
        self.onTimer(None)
        
        self.Bind(wx.EVT_TIMER, self.onTimer)
        self.Bind(wx.EVT_CLOSE, self.onClose)
        self.txt_out.Bind(wx.EVT_TEXT_ENTER, self.onSend)
        self.txt_in.Bind(wx.EVT_TEXT_URL, self.onURL)
    
        self.Bind(wx.EVT_ACTIVATE, self.onActivate)
        self.mw.chat_windows.append(self)
        
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
        
        self.SetTitle(title + " [%s]" % config.get("client", "own_hostname"))
    
    def getTitleShort(self):
        t = self.GetTitle()
        return t[:-19]
    
    def writeColored(self, color, name, text):
        self.txt_in.SetDefaultStyle(wx.TextAttr(wx.Color(128, 128, 128)))    
        self.txt_in.write("%s " % time.strftime(config.get("gui", "time_stamp_format")))
        red, green, blue = color
        self.txt_in.SetDefaultStyle(wx.TextAttr(wx.Color(red, green, blue)))    
        self.txt_in.write("%s: " % name)
        self.txt_in.SetDefaultStyle(wx.TextAttr(wx.Color(0, 0, 0)))
        self.txt_in.write("%s\n" % text)
        
        # workaround scroll bug on windows 
        # https://sourceforge.net/tracker/?func=detail&atid=109863&aid=665381&group_id=9863
        self.txt_in.ScrollLines(-1)
        self.txt_in.ShowPosition(self.txt_in.GetLastPosition())
    
    def notify(self, name, message):
        if not self.IsActive():
            if config.getint("gui", "notification_flash_window"):
                self.RequestUserAttention(wx.USER_ATTENTION_INFO)
            self.unread += 1
            self.updateTitle()

            if config.getint("gui", "notification_popup"):
                nt = textwrap.fill("%s:\n%s" % (name, message.decode("utf-8")), 40)
                try:
                    NotificationWindow(self.mw, nt)
                except:
                    #Some platforms (Mac) dont have wx.PopupWindow
                    #FIXME: need alternative solution
                    pass

        if not self.IsShown():
            self.mw.taskbar_icon.blink()
    
    def notifyOfflineSent(self):
        message = "[delayed messages have been sent]"
        self.writeColored((0,0,192), "myself", message)
        self.notify("to %s" % self.buddy.address, message)
    
    def process(self, message):
        if self.buddy.name != "":
            name = self.buddy.name
        else:
            name = self.buddy.address
        self.writeColored((192,0,0), name, message.decode("utf-8"))
        self.notify(name, message)
        
    def onActivate(self, evt):
        self.unread = 0
        self.updateTitle()
        evt.Skip()
        
    def onClose(self, evt):
        self.mw.chat_windows.remove(self)
        evt.Skip()
        
    def onSend(self, evt):
        evt.Skip()
        text = self.txt_out.GetValue().rstrip().lstrip()
        wx.CallAfter(self.txt_out.SetValue, "")
        if self.buddy.status not in  [tc_client.STATUS_OFFLINE, tc_client.STATUS_HANDSHAKE]:
            self.buddy.sendChatMessage(text.encode("UTF-8"))
            self.writeColored((0,0,192), "myself", text)
        else:
            self.buddy.storeOfflineChatMessage(text.encode("UTF-8"))
            self.writeColored((0,0,192), "myself", "[delayed] " + text)

    def onTimer(self, evt):
        bmp = getStatusBitmap(self.buddy.status)
        icon = wx.IconFromBitmap(bmp)
        self.SetIcon(icon)
        
    def onURL(self, evt):
        #all URL mouse events trigger this
        if evt.GetMouseEvent().GetEventType() == wx.wxEVT_LEFT_DOWN:
            #left button down, now we need the URL
            start = evt.GetURLStart()
            end = evt.GetURLEnd()
            url = self.txt_in.GetRange(start, end)
            if isWindows():
                #this works very reliable 
                subprocess.Popen(("cmd /c start %s" % url).split(), creationflags=0x08000000)
            else:
                #FIXME: is this the way to go? better make it a config option.
                subprocess.Popen(("/etc/alternatives/x-www-browser %s" % url).split())


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
        
        if not receiver:
            self.is_receiver = False
            self.transfer_object = self.buddy.sendFile(self.file_name, 
                                                       self.onDataChange)
        else:
            self.is_receiver = True
            self.transfer_object = receiver
            self.bytes_total = receiver.file_size
            self.file_name = file_name
            
            #the other end of the dirty hack in MainWindow.callbackMessage
            self.mw.new_ft_window[receiver.id] = self
        
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
        
        self.btn_cancel = wx.Button(self.panel, wx.ID_CANCEL, "Cancel")
        self.btn_cancel.Bind(wx.EVT_BUTTON, self.onCancel)
        
        if self.is_receiver:
            grid_sizer.Add(self.btn_cancel, (2, 2))
            
            self.btn_save = wx.Button(self.panel, wx.ID_SAVEAS, "Save as...")
            grid_sizer.Add(self.btn_save, (2, 3))
            self.btn_save.Bind(wx.EVT_BUTTON, self.onSave)
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
        title = "%04.1f%% - %s" % (percent, self.file_name)
        self.SetTitle(title)
        self.progress_bar.SetValue(percent)
        
        if self.is_receiver:
            text = "receiving %s\nfrom %s\n%04.1f%% (%i of %i bytes)" \
                % (os.path.basename(self.file_name), 
                   peer_name, percent, 
                   self.bytes_complete, 
                   self.bytes_total)
        else:
            text = "sending %s\nto %s\n%04.1f%% (%i of %i bytes)" \
                % (os.path.basename(self.file_name), 
                   peer_name, percent, 
                   self.bytes_complete, 
                   self.bytes_total)
                
        if self.error:
            text = self.error_msg
            self.btn_cancel.SetLabel("Close")
            if self.is_receiver:
                self.btn_save.Enable(False)
            
        self.text.SetLabel(text)
        
        if self.bytes_complete == self.bytes_total:
            self.completed = True
            if self.is_receiver:
                if self.file_name_save != "":
                    self.btn_cancel.SetLabel("Close")
            else:
                self.btn_cancel.SetLabel("Close")
        
    def onDataChange(self, total, complete, error_msg=""):
        #will be called from the FileSender/FileReceiver-object in the
        #protocol module to update gui
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
        dialog = wx.FileDialog(None, defaultFile=self.file_name, style=wx.SAVE)
        if dialog.ShowModal() == wx.ID_OK:
            self.file_name_save = dialog.GetPath()
            self.transfer_object.setFileNameSave(self.file_name_save)
            self.btn_save.Enable(False)
            if self.completed:
                self.onCancel(evt)
        else:
            pass
    
    
class MainWindow(wx.Frame):
    def __init__(self):
        wx.Frame.__init__(self, None, -1, "TorChat", size=(250,350))
        self.conns = []
        self.chat_windows = []
        self.new_ft_window = {} # only used in self.callbackMessage
        self.notification_window = None
        self.buddy_list = tc_client.BuddyList(self.callbackMessage)

        self.SetTitle("TorChat: %s" % config.get("client", "own_hostname"))

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
            msg = "Logging to file is activated!" 
            msg += "\n\nLog File: %s" % config.log_writer.file_name
            msg += "\n\nRemember to delete the log file if you have finished debugging"
            msg += "\nbecause the log file may contain sensitive information."
            wx.MessageBox(msg, "TorChat: Logging is active", wx.ICON_WARNING)
            
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
            
            #FIXME: Is this the only way? Looks ugly to me.
            #the other part of this hack is in FileTransferWindow.__init__
            ftw = None
            while not ftw:
                try:
                    ftw = self.new_ft_window[receiver.id]
                except:
                    time.sleep(0.1)
            
            # remember the callback function
            handler = ftw.onDataChange
            # and clean up the array
            del self.new_ft_window[receiver.id]
            
            return handler
    
    def onClose(self, evt):
        self.Show(False)
        
    def exitProgram(self):
        found_unread = False
        for window in self.chat_windows:
            if not window.IsShown() or window.unread:
                found_unread = True
                break
            
        if found_unread:
            msg = "There are unread messages.\n"
            msg += "They will be lost forever!\n\n"
            msg += "Do you really want to exit TorChat now?"
            answer = wx.MessageBox(msg,
                                   "TorChat: Unread messages", 
                                   wx.YES_NO|wx.NO_DEFAULT)

            if answer == wx.NO:
                return
        
        for buddy in self.buddy_list.list:
            if buddy.conn_out != None:
                buddy.conn_out.close()
                
        self.taskbar_icon.RemoveIcon()
        
        # All my threads wouldn't join properly. Don't know why.
        # sys.exit() would spew lots of tracebacks *sometimes*,
        # so let's do it the easy way and just kill ourself:
        pid = os.getpid()
        if isWindows():
            os.popen2("taskkill /f /t /pid %i" % pid)
        else:
            try:
                os.kill(pid, 9)
            except:
                pass
        
        # still alive? obscure new windows version?
        sys.exit() # take that...

about_text = """TorChat %s
Copyright (c) 2007 Bernd Kreuss <prof7bit@gmail.com>
    
TorChat is free software: you can redistribute it and/or \
modify it under the terms of the GNU General Public \
License as published by the Free Software Foundation, \
either version 3 of the License, or (at your option) \
any later version.

TorChat is distributed in the hope that it will be useful, \
but WITHOUT ANY WARRANTY; without even the implied \
warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. \
See the GNU General Public License for more details.

*

And now for something completely different:

If you happen to run a software company in germany and are in \
need of a new coder, feel free to regard this little program \
as my application documents and drop me a mail with your answer.
""" % version.VERSION
