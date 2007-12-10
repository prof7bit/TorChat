#!/usr/bin/python
# -*- coding: UTF-8 -*-

##############################################################################
#                                                                            #
# Copyright (c) 2007 Bernd Kreuss <prof7bit@gmail.com>                       #
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
import TorIM
import sys
import os
import subprocess
import version
import config

ICON_NAMES = {TorIM.STATUS_OFFLINE : "offline.png",
              TorIM.STATUS_ONLINE : "online.png",
              TorIM.STATUS_HANDSHAKE : "connecting.png",
              TorIM.STATUS_AWAY : "away.png",
              TorIM.STATUS_XA : "xa.png"}

def isWindows():
    return "win" in sys.platform

def getStatusBitmap(status):
    return wx.Bitmap(os.path.join(config.ICON_DIR, ICON_NAMES[status]), wx.BITMAP_TYPE_PNG)


class TaskbarIcon(wx.TaskBarIcon):
    def __init__(self, main_window):
        wx.TaskBarIcon.__init__(self)
        self.mw = main_window
        self.showStatus(self.mw.buddy_list.own_status)
        self.Bind(wx.EVT_TASKBAR_LEFT_DOWN, self.onLeftClick)
        
    def showStatus(self, status):
        icon_name = ICON_NAMES[status]
        img = wx.Image(os.path.join(config.ICON_DIR, icon_name))
        img.ConvertAlphaToMask()
        bmp = img.ConvertToBitmap()
        icon = wx.IconFromBitmap(bmp)
        self.SetIcon(icon, 'TorChat')

    def onLeftClick(self, evt):
        self.mw.Show(not self.mw.IsShown())

    def CreatePopupMenu(self):
        return TaskbarMenu(self.mw)


class TaskbarMenu(wx.Menu):
    def __init__(self, main_window):
        wx.Menu.__init__(self)
        self.mw = main_window

        item = wx.MenuItem(self, wx.NewId(), "Show/Hide TorChat")
        self.AppendItem(item)
        self.Bind(wx.EVT_MENU, self.onShowHide, item)

        self.AppendSeparator()

        item = wx.MenuItem(self, wx.NewId(), "Available")
        item.SetBitmap(getStatusBitmap(TorIM.STATUS_ONLINE))
        self.AppendItem(item)
        self.Bind(wx.EVT_MENU, self.onAvailable, item)

        item = wx.MenuItem(self, wx.NewId(), "Away")
        item.SetBitmap(getStatusBitmap(TorIM.STATUS_AWAY))
        self.AppendItem(item)
        self.Bind(wx.EVT_MENU, self.onAway, item)

        item = wx.MenuItem(self, wx.NewId(), "Extended Away")
        item.SetBitmap(getStatusBitmap(TorIM.STATUS_XA))
        self.AppendItem(item)
        self.Bind(wx.EVT_MENU, self.onXA, item)
        
        self.AppendSeparator()
        
        item = wx.MenuItem(self, wx.NewId(), "Quit")
        self.AppendItem(item)
        self.Bind(wx.EVT_MENU, self.onExit, item)
        
    def onShowHide(self, evt):
        self.mw.Show(not self.mw.IsShown())

    def onExit(self, evt):
        self.mw.exitProgram()

    def onAvailable(self, evt):
        self.mw.status_switch.setStatus(TorIM.STATUS_ONLINE)

    def onAway(self, evt):
        self.mw.status_switch.setStatus(TorIM.STATUS_AWAY)

    def onXA(self, evt):
        self.mw.status_switch.setStatus(TorIM.STATUS_XA)


class PopupMenu(wx.Menu):
    def __init__(self, main_window, type):
        wx.Menu.__init__(self)
        self.mw = main_window
        
        if type == "contact": 
            item = wx.MenuItem(self, wx.NewId(), "Chat")
            self.AppendItem(item)
            self.Bind(wx.EVT_MENU, self.mw.gui_bl.onDClick, item)
        
            item = wx.MenuItem(self, wx.NewId(), "Edit contact")
            self.AppendItem(item)
            self.Bind(wx.EVT_MENU, self.onEdit, item)

            item = wx.MenuItem(self, wx.NewId(), "Delete contact")
            self.AppendItem(item)
            self.Bind(wx.EVT_MENU, self.onDelete, item)
    
        if type == "empty": 
            item = wx.MenuItem(self, wx.NewId(), "Add contact")
            self.AppendItem(item)
            self.Bind(wx.EVT_MENU, self.onAdd, item)
    
        self.AppendSeparator()
        
        item = wx.MenuItem(self, wx.NewId(), "About TorChat")
        self.AppendItem(item)
        self.Bind(wx.EVT_MENU, self.onAbout, item)

        if type == "empty": 
            self.AppendSeparator()
            item = wx.MenuItem(self, wx.NewId(), "Ask Bernd")
            self.AppendItem(item)
            self.Bind(wx.EVT_MENU, self.onAskBernd, item)

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
            self.mw.buddy_list.removeBuddy(buddy)

    def onAdd(self, evt):
        dialog = DlgEditContact(self.mw, None)
        dialog.ShowModal()

    def onAbout(self, evt):
        wx.MessageBox(about_text, "About TorChat")

    def onAskBernd(self, evt):
        res = self.mw.buddy_list.addBuddy(TorIM.Buddy("utvrla6mjdypbyw6", 
                                    self.mw.buddy_list,
                                    "Bernd"))
        if res == False:
            wx.MessageBox("Bernd is already on your list")


class DlgEditContact(wx.Dialog):
    def __init__(self, main_window, buddy=None): #no buddy -> Add new
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
        
        sizer = wx.GridBagSizer(vgap = 5, hgap = 5)
        box_sizer = wx.BoxSizer()
        box_sizer.Add(sizer, 1, wx.EXPAND | wx.ALL, 5)
        
        lbl = wx.StaticText(self.panel, -1, "Address")
        sizer.Add(lbl, (0, 0))
        
        lbl = wx.StaticText(self.panel, -1, "Name")
        sizer.Add(lbl, (1, 0))
        
        self.txt_address = wx.TextCtrl(self.panel, -1, address)
        self.txt_address.SetMinSize((250, -1))
        sizer.Add(self.txt_address, (0, 1), (1, 2))
        
        self.txt_name = wx.TextCtrl(self.panel, -1, name)
        self.txt_name.SetMinSize((250, -1))
        sizer.Add(self.txt_name, (1, 1), (1, 2))
        
        self.btn_cancel = wx.Button(self.panel, -1, "Cancel")
        sizer.Add(self.btn_cancel, (2, 1), flag=wx.EXPAND)
        
        self.btn_ok = wx.Button(self.panel, -1, "Ok")
        self.btn_ok.SetDefault()
        sizer.Add(self.btn_ok, (2, 2), flag=wx.EXPAND)
        
        self.panel.SetSizer(box_sizer)
        box_sizer.Fit(self)

        self.btn_cancel.Bind(wx.EVT_BUTTON, self.onCancel)
        self.btn_ok.Bind(wx.EVT_BUTTON, self.onOk)

    def onOk(self, evt):
        address = self.txt_address.GetValue()
        if len(address) != 16:
            l = len(address)
            wx.MessageBox("The address must be 16 characters long, not %i." % l)
            return
        
        for c in address:
            if c not in "0123456789abcdefghijklmnopqrstuvwxyz":
                wx.MessageBox("The address must only contain numbers and lowercase letters")
                return
            
        if self.buddy == None:
            buddy = TorIM.Buddy(address, 
                          self.bl, 
                          self.txt_name.GetValue())
            res = self.bl.addBuddy(buddy)
            if res == False:
                wx.MessageBox("This contact is already on your list")
        else:
            address_old = self.buddy.address
            self.buddy.address = address
            self.buddy.name = self.txt_name.GetValue()
            self.bl.save()
            if address != address_old:
                self.buddy.disconnect()
            
        self.Close()
        
    def onCancel(self,evt):
        self.Close()


class BuddyList(wx.ListCtrl):
    def __init__(self, parent, main_window):
        wx.ListCtrl.__init__(self, parent, -1, style=wx.LC_LIST)
        self.mw = main_window
        self.bl = self.mw.buddy_list
        
        self.r_down = False
        
        self.il = wx.ImageList(16, 16)
        self.il_idx = {}
        for status in [TorIM.STATUS_OFFLINE, 
                       TorIM.STATUS_HANDSHAKE, 
                       TorIM.STATUS_ONLINE,
                       TorIM.STATUS_AWAY,
                       TorIM.STATUS_XA]:
            self.il_idx[status] = self.il.Add(getStatusBitmap(status))
        self.SetImageList(self.il, wx.IMAGE_LIST_SMALL)
        
        self.InsertColumn(0, "Buddy")
        self.SetColumnWidth(0, 200)
        
        self.timer = wx.Timer(self, -1)
        self.Bind(wx.EVT_TIMER, self.onTimer, self.timer)
        self.timer.Start(milliseconds=1000, oneShot=False)
        
        self.Bind(wx.EVT_LEFT_DCLICK, self.onDClick)
        self.Bind(wx.EVT_LIST_ITEM_RIGHT_CLICK, self.onRClick)
        self.Bind(wx.EVT_RIGHT_DOWN, self.onRDown)
        
    def onTimer(self, evt):
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
                index = self.InsertImageStringItem(sys.maxint, line, self.il_idx[TorIM.STATUS_OFFLINE])
            self.SetItemImage(index, self.il_idx[buddy.status])  
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
        item.SetBitmap(getStatusBitmap(TorIM.STATUS_ONLINE))
        self.AppendItem(item)
        self.Bind(wx.EVT_MENU, self.status_switch.onAvailable, item)

        item = wx.MenuItem(self, wx.NewId(), "Away")
        item.SetBitmap(getStatusBitmap(TorIM.STATUS_AWAY))
        self.AppendItem(item)
        self.Bind(wx.EVT_MENU, self.status_switch.onAway, item)

        item = wx.MenuItem(self, wx.NewId(), "Extended Away")
        item.SetBitmap(getStatusBitmap(TorIM.STATUS_XA))
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
        self.setStatus(TorIM.STATUS_ONLINE)
    
    def onAway(self, evt):
        self.setStatus(TorIM.STATUS_AWAY)
    
    def onXA(self, evt):
        self.setStatus(TorIM.STATUS_XA)
    
    def setStatus(self, status):
        self.status = status
        self.main_window.setStatus(status)
        if status == TorIM.STATUS_AWAY:
            status_text = "Away"
        if status == TorIM.STATUS_XA:
            status_text = "Extended Away"
        if status == TorIM.STATUS_ONLINE:
            status_text = "Available"
        if status == TorIM.STATUS_OFFLINE:
            status_text = "Offline"
        self.SetLabel(status_text)


class ChatWindow(wx.Frame):
    def __init__(self, main_window, buddy, message=""):
        wx.Frame.__init__(self, 
                          main_window, 
                          -1, 
                          size=(400,400))
        
        self.mw = main_window
        self.mw.chat_windows.append(self)
        self.buddy = buddy
        title = self.buddy.address
        if self.buddy.name != "":
            title += " (%s)" % self.buddy.name
        self.SetTitle(title)
        
        self.panel = wx.Panel(self)
        sizer = wx.BoxSizer(wx.VERTICAL)
        
        self.txt_in = wx.TextCtrl(self.panel,
                                   -1,
                                   style=wx.TE_READONLY |
                                   wx.TE_MULTILINE |
                                   wx.TE_AUTO_URL |
                                   wx.TE_AUTO_SCROLL |
                                   wx.TE_RICH2)
        
        sizer.Add(self.txt_in, 1, wx.EXPAND)
        
        self.txt_out = wx.TextCtrl(self.panel,
                                   -1,
                                   style=wx.TE_MULTILINE |
                                   wx.TE_PROCESS_ENTER)
        sizer.Add(self.txt_out, 0, wx.EXPAND)
        
        sizer.SetItemMinSize(self.txt_out, (-1,50))
        
        self.panel.SetSizer(sizer)
        sizer.FitInside(self)
        self.Show()
        
        if message != "":
            self.process(message)
        
        self.Bind(wx.EVT_CLOSE, self.onClose)
        self.txt_out.Bind(wx.EVT_TEXT_ENTER, self.onSend)
        self.txt_in.Bind(wx.EVT_TEXT_URL, self.onURL)
    
    def writeColored(self, color, name, text):
        red, green, blue = color
        self.txt_in.SetDefaultStyle(wx.TextAttr(wx.Color(red, green, blue)))    
        self.txt_in.write("%s: " % name)
        self.txt_in.SetDefaultStyle(wx.TextAttr(wx.Color(0, 0, 0)))
        self.txt_in.write("%s\n" % text)
        
        # workaround scroll bug on windows 
        # https://sourceforge.net/tracker/?func=detail&atid=109863&aid=665381&group_id=9863
        self.txt_in.ScrollLines(-1)
        self.txt_in.ShowPosition(self.txt_in.GetLastPosition())
    
    def process(self, message):
        if self.buddy.name != "":
            name = self.buddy.name
        else:
            name = self.buddy.address
        self.writeColored((192,0,0), name, message.decode("utf-8"))
        
    def onClose(self, evt):
        self.mw.chat_windows.remove(self)
        evt.Skip()
        
    def onSend(self, evt):
        evt.Skip()
        if self.buddy.status not in  [TorIM.STATUS_OFFLINE, TorIM.STATUS_HANDSHAKE]:
            text = self.txt_out.GetValue().rstrip().lstrip()
            wx.CallAfter(self.txt_out.SetValue, "")
            self.buddy.send("message %s" % text.encode("UTF-8"))
            self.writeColored((0,0,192), "myself", text)
        else:
            wx.MessageBox("We have no connection to this contact. \nPlease wait.")

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


class MainWindow(wx.Frame):
    def __init__(self):
        wx.Frame.__init__(self, None, -1, "TorChat", size=(250,350))
        self.conns = []
        self.chat_windows = []
        self.buddy_list = TorIM.BuddyList(self.callbackMessage)

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
        
        self.Show()
    
    def setStatus(self, status):
        self.buddy_list.setStatus(status)
        self.taskbar_icon.showStatus(status)

 
    def callbackMessage(self, buddy, message):
        #we must always use wx.CallAfter() to interact with
        #the GUI-Thread because this method will be called
        #in the context of one of the connection threads 
        for window in self.chat_windows:
            if window.buddy == buddy:
                wx.CallAfter(window.process, message)
                return
        
        #no window found, so we create a new one
        wx.CallAfter(ChatWindow, self, buddy, message)
    
    def onClose(self, evt):
        self.Show(False)
        
    def exitProgram(self):
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
        
        
def main():
    os.chdir(config.getScriptDir())
    app = wx.App(redirect=False)
    app.mw = MainWindow()
    app.SetTopWindow(app.mw)
    app.MainLoop()

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

if __name__ == "__main__":
    main()
