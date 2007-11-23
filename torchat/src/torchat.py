#!/usr/bin/python

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
# you find it in your hidden service dir in the file
# hostname. Leave the tld (.onion) away.
# (On windows in portable mode this is done automatically.)
OWN_HOSTNAME = "utvrla6mjdypbyw6" #.onion

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

PORT = 11009
STATUS_OFFLINE = 0
STATUS_HANDSHAKE = 1
STATUS_ONLINE = 2
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
        
    def send(self, text):
        if self.conn_out == None:
            self.connect()
        self.conn_out.send(text + "\n")
        
    def ping(self):
        if self.conn_out == None:
            self.connect()
        self.send("ping %s %s" % (OWN_HOSTNAME, self.random1))

class BuddyList(object):
    def __init__(self, main_window):
        self.mw = main_window
        
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
            self.list.append(Buddy(OWN_HOSTNAME, self, "myself"))
            self.save() 
        
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
        
    def process(self, connection, line):
        cmd, text = splitLine(line)
        if cmd == "ping":
            address, random = splitLine(text)
            found = False
            for buddy in self.list:
                if buddy.address == address:
                    buddy.send("pong " + random)
                    found = True
                    break
            if not found:
                buddy = Buddy(address, self)
                self.list.append(buddy)
                buddy.connect()
                buddy.send("pong " + random)
                self.save()
                
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
                    buddy.conn_in.close()
                    buddy.conn_in = None
                    buddy.conn_out.close()
                    buddy.conn_out = None
                    buddy.status = STATUS_OFFLINE
                    break
                
        if cmd == "error-out":
            for buddy in self.list:
                if buddy.conn_out == connection:
                    buddy.conn_out.close()
                    buddy.conn_out = None
                    if buddy.conn_in != None:
                        buddy.conn_in.close()
                        buddy.conn_in = None
                    buddy.status = STATUS_OFFLINE
                    break
        
        if cmd == "connected":
            for buddy in self.list:
                if buddy.conn_out == connection:
                    buddy.status = STATUS_HANDSHAKE
                    
        if cmd == "message":
            if connection.buddy != None:
                buddy = connection.buddy
                if buddy.chat_window == None:
                    wx.CallAfter(self.mw.newIncomingChatWindow, buddy, text)
                else:
                    wx.CallAfter(buddy.chat_window.process, text)


class InConnection(threading.Thread):
    def __init__(self, conn, buddy_list):
        threading.Thread.__init__(self)
        self.buddy = None
        self.bl = buddy_list
        self.conn = conn
        self.start()
        
    def run(self):
        self.running = True
        try:
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
        
        except:
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
            self.conn.setproxy(socks.PROXY_TYPE_SOCKS4, "127.0.0.1", 9050)
            self.conn.connect((self.address, PORT))
            self.bl.process(self, "connected")
            while self.running:
                if len(self.send_buffer) > 0:
                    text = self.send_buffer.pop()
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
        HOST = '127.0.0.1'
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        self.socket.bind((HOST, PORT))
        self.socket.listen(1)
        log("listening on port %i" % PORT)
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

    def onChat(self, evt):
        pass

    def onEdit(self, evt):
        pass

    def onDelete(self, evt):
        pass

    def onAdd(self, evt):
        pass

    def onAbout(self, evt):
        pass

class GuiBuddyList(wx.ListCtrl):
    def __init__(self, parent, main_window):
        wx.ListCtrl.__init__(self, parent, -1, style=wx.LC_LIST)
        self.mw = main_window
        self.bl = self.mw.buddy_list
        
        self.r_down = False
        
        self.il = wx.ImageList(16, 16)
        self.icon_offline = self.il.Add(wx.Bitmap("icons/offline.png", wx.BITMAP_TYPE_PNG))
        self.icon_online = self.il.Add(wx.Bitmap("icons/online.png", wx.BITMAP_TYPE_PNG))
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
        for buddy in self.bl.list:
            if buddy.name != "":
                line = "%s (%s)" % (buddy.address, buddy.name)
            else:
                line = buddy.address
            index = self.FindItem(0, line)
            if index == -1:
                index = self.InsertImageStringItem(sys.maxint, line, self.icon_offline)
            
            if buddy.status == STATUS_OFFLINE:
                self.SetItemImage(index, self.icon_offline)    
            if buddy.status == STATUS_ONLINE:
                self.SetItemImage(index, self.icon_online)    
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
                               style=wx.TE_MULTILINE)
        sizer.Add(self.txt_in, 1, wx.EXPAND)
        
        self.txt_out = wx.TextCtrl(self.panel,
                                   -1,
                                   style=wx.TE_PROCESS_ENTER)
        sizer.Add(self.txt_out, 0, wx.EXPAND)
        sizer.SetItemMinSize(self.txt_out, (-1,20))
        
        self.panel.SetSizer(sizer)
        sizer.FitInside(self)
        self.Show()
        
        self.Bind(wx.EVT_CLOSE, self.onClose)
        self.txt_out.Bind(wx.EVT_TEXT_ENTER, self.onSend)
        
    def process(self, text):
        if self.buddy.name != "":
            name = self.buddy.name
        else:
            name = self.buddy.address
        self.txt_in.write("%s: %s\n" % (name, text.decode("UTF-8")))
        
    def onClose(self, evt):
        self.buddy.chat_window = None
        evt.Skip()
        
    def onSend(self, evt):
        if self.buddy.status == STATUS_ONLINE:
            text = self.txt_out.GetValue()
            self.txt_out.SetValue("")
            self.buddy.send("message %s" % text.encode("UTF-8"))
            self.txt_in.write("%s: %s\n" % ("myself", text))
        else:
            wx.MessageBox("We have no connection to this contact. \nPlease wait.")
        evt.Skip()

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

if __name__ == "__main__":
    main()
