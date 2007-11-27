#!/usr/bin/python
# -*- coding: UTF-8 -*-

##############################################################################
#                                                                            #
# Copyright (c) 2007 Bernd Kreuss <prof7bit@gmail.com>                       #
#                                                                            #
# This program is licensed under the GNU General Public License V3,             #
# the full source code is included in the binary distribution.               #
#                                                                            #
# Included in the distribution are files from other open source projects:    #
# - TOR Onion Router (c) The Tor Project, 3-clause-BSD                       #
# - SocksiPy (c) Dan Haim, BSD Style License                                 #
# - Gajim buddy status icons (c) The Gajim Team, GNU GPL                     #
#                                                                            #
##############################################################################


#change OWN_HOSTNAME to yours, or it will NOT work
#you find it in your hidden service dir in the file
#hostname. Leave the tld (.onion) away.
#(On windows in portable mode this is done automatically.)
OWN_HOSTNAME = "utvrla6mjdypbyw6" #.onion

#configure the following if your Tor is running on a separate machine
TOR_SERVER = "127.0.0.1"
TOR_SERVER_SOCKS_PORT = 9050
TOR_SERVER_CONTROL_PORT = 9051

#configure where to listen for connections *from* the Tor server
LISTEN_INTERFACE = "127.0.0.1"
LISTEN_PORT = 11009

LOG_TO_WINDOW = False
LOG_TO_FILE = False

import wx
import SocksiPy.socks as socks
import socket
import threading
import random
import time
import sys
import os

TORCHAT_PORT = 11009 #do NOT change this.
STATUS_OFFLINE = 0
STATUS_HANDSHAKE = 1
STATUS_ONLINE = 2
STATUS_AWAY = 3
STATUS_XA = 4
DIR = os.path.dirname(sys.argv[0])
os.chdir(DIR)
log_window = None

def isWindows():
    return "win" in sys.platform

def log(text):
    global log_window
    text += "\n"
    if LOG_TO_FILE:
        f = open(DIR + "/logfile.txt", "a")
        f.write(text)
        f.close()
    if LOG_TO_WINDOW:
        if log_window:
            wx.CallAfter(log_window.log, text)

def splitLine(text):
    sp = text.split(" ")
    try:
        a = sp[0]
        b = " ".join(sp[1:])
    except:
        a = text
        b = ""
    return a, b

def dummy_excepthook(cls, inst, tb):
    pass

class Buddy(object):
    def __init__(self, address, buddy_list, name=""):
        self.bl = buddy_list
        self.address = address
        self.name = name
        self.random1 = str(random.getrandbits(256))
        self.random2 = str(random.getrandbits(256))
        self.conn_out = None
        self.conn_in = None
        self.status = STATUS_OFFLINE
        self.chat_window = None
    
    def connect(self):
        self.conn_out = OutConnection(self.address + ".onion", self.bl)
        self.conn_out.buddy = self
        self.ping()
        
    def disconnect(self):
        if self.conn_out != None:
            self.conn_out.close()
            self.conn_out = None
        if self.conn_in != None:
            self.conn_in.close()
            self.conn_in = None
        self.status = STATUS_OFFLINE
        
    def send(self, text):
        if self.conn_out == None:
            self.connect()
        self.conn_out.send(text + "\n")
        
    def ping(self):
        if self.conn_out == None:
            self.connect()
        else:
            self.send("ping %s %s" % (OWN_HOSTNAME, self.random1))
            self.sendStatus()
                
    def sendStatus(self):
        if self.conn_out != None:
            status = ""
            if self.bl.own_status == STATUS_ONLINE:
                status = "available"
            if self.bl.own_status == STATUS_AWAY:
                status = "away"
            if self.bl.own_status == STATUS_XA:
                status = "xa"
            if status != "":
                self.send("status %s" % status)
        
    def getDisplayName(self):
        if self.name != "":
            line = "%s (%s)" % (self.address, self.name)
        else:
            line = self.address
        return line

class BuddyList(object):
    def __init__(self, main_window):
        self.mw = main_window
        
        self.own_status = STATUS_ONLINE
        
        #create it if it does not already exist
        f = open("buddy-list.txt", "a")
        f.close()
        
        f = open("buddy-list.txt", "r")
        l = f.read().split("\n")
        f.close
        self.list = []
        for line in l:
            line = line.rstrip()
            if len(line) > 15:
                address = line[0:16]
                if len(line) > 17:
                    name = line[17:]
                else:
                    name = ""
                buddy = Buddy(address, self, name)
                buddy.connect()
                self.list.append(buddy)
        
        found = False
        log("searching for own hostname in buddy list")
        for buddy in self.list:
            if buddy.address == OWN_HOSTNAME:
                found = True
                log("found own hostname %s in buddy list" % OWN_HOSTNAME)
        
        if not found:
            log("adding own hostname %s to buddy list" % OWN_HOSTNAME)
            self.addBuddy(Buddy(OWN_HOSTNAME, self, "myself"))
        
        self.test()

    def save(self):
        f = open("buddy-list.txt", "w")
        for buddy in self.list:
            line = "%s %s" % (buddy.address, buddy.name)
            f.write("%s\r\n" % line.rstrip())
        f.close()

    def test(self):
        for buddy in self.list:
            buddy.ping()
            
        self.timer = threading.Timer(15, self.test)
        self.timer.start()
        
    def addBuddy(self, buddy):
        if self.getBuddyFromAddress(buddy.address) == None:
            self.list.append(buddy)
            self.save()
            buddy.connect()
            return buddy
        else:
            return False
        
    def removeBuddy(self, buddy_to_remove):
        try:
            buddy_to_remove.disconnect()
            self.list.remove(buddy_to_remove)
            self.save()
            return True
        except:
            return False
        
    def removeBuddyWithAddress(self, address):
        buddy = self.getBuddyFromAddress(address)
        if buddy != None:
            self.removeBuddy(buddy)
        
    def getBuddyFromAddress(self, address):
        for buddy in self.list:
            if buddy.address == address:
                return buddy
        return None
        
    def setStatus(self, status):
        self.own_status = status
        for buddy in self.list:
            buddy.sendStatus()
    
    def process(self, connection, line):
        cmd, text = splitLine(line)
        if cmd == "ping":
            address, random = splitLine(text)
            buddy = self.getBuddyFromAddress(address)
            if buddy:
                buddy.send("pong " + random)
            else:
                buddy = self.addBuddy(Buddy(address, self))
                
        if cmd == "pong":
            for buddy in self.list:
                if buddy.random1 == text:
                    if buddy.conn_in == None:
                        log("identified incoming connection as %s" % buddy.address)
                        buddy.conn_in = connection
                        buddy.status = STATUS_ONLINE
                        connection.buddy = buddy
                    break
                
        if cmd == "error-in":
            for buddy in self.list:
                if buddy.conn_in == connection:
                    buddy.disconnect()
                    break
                
        if cmd == "error-out":
            buddy = connection.buddy
            buddy.disconnect()
        
        if cmd == "connected":
            connection.buddy.status = STATUS_HANDSHAKE
                    
        if cmd == "message":
            if connection.buddy != None:
                buddy = connection.buddy
                if buddy.chat_window == None:
                    wx.CallAfter(self.mw.newIncomingChatWindow, buddy, text)
                else:
                    wx.CallAfter(buddy.chat_window.process, text)
                    
        if cmd == "status":
            if connection.buddy != None:
                if text == "available":
                    connection.buddy.status = STATUS_ONLINE
                if text == "away":
                    connection.buddy.status = STATUS_AWAY
                if text == "xa":
                    connection.buddy.status = STATUS_XA


class InConnection(threading.Thread):
    def __init__(self, conn, buddy_list):
        threading.Thread.__init__(self)
        self.buddy = None
        self.bl = buddy_list
        self.conn = conn
        self.start()
        
    def run(self):
        self.running = True
        readbuffer = ""
        while self.running:
            recv = self.conn.recv(1024)
            if recv != "":
                readbuffer = readbuffer + recv
                temp = readbuffer.split("\n")
                readbuffer = temp.pop( )
            
                for line in temp:
                    line = line.rstrip()
                    if self.running:
                        self.bl.process(self, line)
            else:
                self.close()  
                self.bl.process(self, "error-in")

    def close(self):
        self.running = False
        try:
            self.conn.close()
        except:
            pass
        
        
class OutConnection(threading.Thread):
    def __init__(self, address, buddy_list):
        threading.Thread.__init__(self)
        self.bl = buddy_list
        self.address = address
        self.send_buffer = []
        self.start()
        
    def run(self):
        self.running = True
        try:
            self.conn = socks.socksocket()
            self.conn.setproxy(socks.PROXY_TYPE_SOCKS4, 
                               TOR_SERVER, 
                               TOR_SERVER_SOCKS_PORT)
            self.conn.connect((self.address, TORCHAT_PORT))
            self.bl.process(self, "connected")
            while self.running:
                if len(self.send_buffer) > 0:
                    text = self.send_buffer.pop(0)
                    self.conn.send(text)
                time.sleep(0.1)
                
        except:
            self.bl.process(self, "error-out")
            self.close()
            
    def send(self, text):
        self.send_buffer.append(text)
        
    def close(self):
        self.running = False
        try:
            self.conn.close()
        except:
            pass
        
        
class Listener(threading.Thread):
    def __init__(self, main_window):
        threading.Thread.__init__(self)
        self.mw = main_window
        self.start()
        
    def run(self):
        self.running = True
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        self.socket.bind((LISTEN_INTERFACE, LISTEN_PORT))
        self.socket.listen(1)
        log("listening on interface %s port %i" % (LISTEN_INTERFACE, LISTEN_PORT))
        try:
            while self.running:
                try:
                    conn, address = self.socket.accept()
                    log("incoming connection")
                    self.mw.conns.append(InConnection(conn, self.mw.buddy_list))
                except:
                    self.running = False
                    
        except TypeError:
            pass

    def close(self):
        self.running = False
        try:
            self.socket.close()
        except:
            pass
        

#--- ######## GUI #########

class GuiPopupMenu(wx.Menu):
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
        dialog = GuiEditContact(self.mw, buddy)
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
        dialog = GuiEditContact(self.mw, None)
        dialog.ShowModal()

    def onAbout(self, evt):
        wx.MessageBox(about_text, "About TorChat")

    def onAskBernd(self, evt):
        res = self.mw.buddy_list.addBuddy(Buddy("utvrla6mjdypbyw6", 
                                    self.mw.buddy_list,
                                    "Bernd"))
        if res == False:
            wx.MessageBox("Bernd is already on your list")

class GuiEditContact(wx.Dialog):
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
            buddy = Buddy(address, 
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

class GuiBuddyList(wx.ListCtrl):
    def __init__(self, parent, main_window):
        wx.ListCtrl.__init__(self, parent, -1, style=wx.LC_LIST)
        self.mw = main_window
        self.bl = self.mw.buddy_list
        
        self.r_down = False
        
        self.il = wx.ImageList(16, 16)
        self.icon_offline = self.il.Add(wx.Bitmap("icons/offline.png", wx.BITMAP_TYPE_PNG))
        self.icon_online = self.il.Add(wx.Bitmap("icons/online.png", wx.BITMAP_TYPE_PNG))
        self.icon_away = self.il.Add(wx.Bitmap("icons/away.png", wx.BITMAP_TYPE_PNG))
        self.icon_xa = self.il.Add(wx.Bitmap("icons/xa.png", wx.BITMAP_TYPE_PNG))
        self.icon_handshake = self.il.Add(wx.Bitmap("icons/connecting.png", wx.BITMAP_TYPE_PNG))
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
                index = self.InsertImageStringItem(sys.maxint, line, self.icon_offline)
            
            if buddy.status == STATUS_OFFLINE:
                self.SetItemImage(index, self.icon_offline)    
            if buddy.status == STATUS_ONLINE:
                self.SetItemImage(index, self.icon_online)    
            if buddy.status == STATUS_AWAY:
                self.SetItemImage(index, self.icon_away)    
            if buddy.status == STATUS_XA:
                self.SetItemImage(index, self.icon_xa)    
            if buddy.status == STATUS_HANDSHAKE:
                self.SetItemImage(index, self.icon_handshake)
        
        self.Refresh()    
    
    def onDClick(self, evt):
        i = self.GetFirstSelected()
        address = self.GetItemText(i)[0:16]
        for buddy in self.bl.list:
            if buddy.address == address:
                if buddy.chat_window == None:
                    buddy.chat_window = ChatWindow(self.mw, buddy)
                buddy.chat_window.txt_out.SetFocus()
                break
        evt.Skip()
        
    def onRClick(self, evt):
        index, flags = self.HitTest(evt.GetPosition())
        if index != -1:
            self.mw.PopupMenu(GuiPopupMenu(self.mw, "contact"))
        
    def onRDown(self, evt):
        index, flags = self.HitTest(evt.GetPosition())
        if index == -1:
            self.mw.PopupMenu(GuiPopupMenu(self.mw, "empty"))
        else:
            evt.Skip()

    def getSelectedBuddy(self):
        index = self.GetFirstSelected()
        addr = self.GetItemText(index)[0:16]
        return self.bl.getBuddyFromAddress(addr)
        

class GuiStatusSwitchList(wx.Menu):
    def __init__(self, status_switch):
        wx.Menu.__init__(self)
        self.status_switch = status_switch

        item = wx.MenuItem(self, wx.NewId(), "Available")
        item.SetBitmap(wx.Bitmap("icons/online.png"))
        self.AppendItem(item)
        self.Bind(wx.EVT_MENU, self.status_switch.onAvailable, item)

        item = wx.MenuItem(self, wx.NewId(), "Away")
        item.SetBitmap(wx.Bitmap("icons/away.png"))
        self.AppendItem(item)
        self.Bind(wx.EVT_MENU, self.status_switch.onAway, item)

        item = wx.MenuItem(self, wx.NewId(), "Extended Away")
        item.SetBitmap(wx.Bitmap("icons/xa.png"))
        self.AppendItem(item)
        self.Bind(wx.EVT_MENU, self.status_switch.onXA, item)

class GuiStatusSwitch(wx.Button):
    def __init__(self, parent, main_window):
        wx.Button.__init__(self, parent)
        self.parent = parent
        self.main_window = main_window
        self.status = self.main_window.buddy_list.own_status
        self.Bind(wx.EVT_BUTTON, self.onClick)
        self.setStatus(self.main_window.buddy_list.own_status)
        
    def onClick(self, evt):
        self.PopupMenu(GuiStatusSwitchList(self))

    def onAvailable(self, evt):
        self.setStatus(STATUS_ONLINE)
    
    def onAway(self, evt):
        self.setStatus(STATUS_AWAY)
    
    def onXA(self, evt):
        self.setStatus(STATUS_XA)
    
    def setStatus(self, status):
        self.status = status
        self.main_window.buddy_list.setStatus(status)
        if status == STATUS_AWAY:
            status_text = "Away"
        if status == STATUS_XA:
            status_text = "Extended Away"
        if status == STATUS_ONLINE:
            status_text = "Available"
        if status == STATUS_OFFLINE:
            status_text = "Offline"
        self.SetLabel(status_text)

    
class ChatWindow(wx.Frame):
    def __init__(self, main_window, buddy):
        wx.Frame.__init__(self, 
                          main_window, 
                          -1, 
                          size=(400,400))
        
        self.buddy = buddy
        self.buddy.chat_window = self
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
        
        self.Bind(wx.EVT_CLOSE, self.onClose)
        self.txt_out.Bind(wx.EVT_TEXT_ENTER, self.onSend)
    
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
    
    def process(self, text):
        if self.buddy.name != "":
            name = self.buddy.name
        else:
            name = self.buddy.address
        self.writeColored((192,0,0), name, text.decode("utf-8"))
        
    def onClose(self, evt):
        self.buddy.chat_window = None
        evt.Skip()
        
    def onSend(self, evt):
        evt.Skip()
        if self.buddy.status not in  [STATUS_OFFLINE, STATUS_HANDSHAKE]:
            text = self.txt_out.GetValue().rstrip().lstrip()
            wx.CallAfter(self.txt_out.SetValue, "")
            self.buddy.send("message %s" % text.encode("UTF-8"))
            self.writeColored((0,0,192), "myself", text)
        else:
            wx.MessageBox("We have no connection to this contact. \nPlease wait.")

class MainWindow(wx.Frame):
    def __init__(self):
        wx.Frame.__init__(self, None, -1, "TorChat", size=(250,350))
        self.conns = []
        self.buddy_list = BuddyList(self)
        self.listener = Listener(self)
        self.log_window = None

        self.Bind(wx.EVT_CLOSE, self.onClose)
        
        # setup gui elements
        self.main_panel = wx.Panel(self)
        sizer = wx.BoxSizer(wx.VERTICAL)
        self.gui_bl = GuiBuddyList(self.main_panel, self)
        sizer.Add(self.gui_bl, 1, wx.EXPAND)
        
        self.status_switch = GuiStatusSwitch(self.main_panel, self)
        sizer.Add(self.status_switch, 0, wx.EXPAND)
        
        self.main_panel.SetSizer(sizer)
        sizer.FitInside(self)
        
        icon = wx.Icon(name="icons/torchat.ico", type=wx.BITMAP_TYPE_ICO)
        self.SetIcon(icon)
        
        self.Show()
    
    def newIncomingChatWindow(self, buddy, text):
        #this will be called via wx.CallAfter() 
        #from the connection thread
        buddy.chat_window = ChatWindow(self, buddy)
        buddy.chat_window.process(text)
        
    def log(self, text):
        if self.log_window == None:
            self.log_window = LogWindow(self)
        self.log_window.log(text)    
    
    def onClose(self, evt):
        for buddy in self.buddy_list.list:
            if buddy.conn_out != None:
                buddy.conn_out.close()
                
        for conn in self.conns:
            conn.close()
        
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
        

class LogWindow(wx.Frame):
    def __init__(self):
        wx.Frame.__init__(self, None, -1, "Log", size=(500,300))
        self.txt = wx.TextCtrl(self, -1, style=wx.TE_MULTILINE)
        self.Show()
        self.Bind(wx.EVT_CLOSE, self.onClose)
        
    def log(self, text):
        self.txt.write(text)
    
    def onClose(self, evt):
        pass #will not close.    
        
def main():
    global OWN_HOSTNAME
    global log_window
    
    app = wx.App(redirect=False)
    if LOG_TO_WINDOW:
        log_window = LogWindow()

    #first we try to start tor (windows portable mode only)
    try:
        log("trying to start tor (portable mode)")
        os.chdir("tor")
        tor_in, tor_out = os.popen2("tor -f torrc.txt")
        log("tor started")
        # we now assume the existence of our hostname file
        # it WILL be created after the first start
        # if not, something must be totally wrong.
        cnt = 0
        while cnt < 20:
            try:
                f = open("hidden_service\\hostname", "r")
                OWN_HOSTNAME = f.read().rstrip()[:-6]
                f.close()
                log("own hostname: %s" % OWN_HOSTNAME)
                break
            except:
                log("cold not open hostname file, trying again.")
                # we wait 20 seconds for the file to appear
                time.sleep(1)
                cnt += 1

        os.chdir(DIR)
    except:
        log("not running in portable mode. assuming already configured tor.")
        os.chdir(DIR)
        
    app.mw = MainWindow()
    app.SetTopWindow(app.mw)
    app.MainLoop()

about_text = """Copyright (c) 2007 Bernd Kreuss <prof7bit@gmail.com>
    
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
"""

if __name__ == "__main__":
    main()
