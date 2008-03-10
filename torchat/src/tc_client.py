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

#change OWN_HOSTNAME to yours, or it will NOT work
#you find it in your hidden service dir in the file
#hostname. Leave the tld (.onion) away.
#(On windows in portable mode this is done automatically.)
OWN_HOSTNAME = "utvrla6mjdypbyw6" #.onion
TRY_PORTABLE_MODE = True

#configure the following if your Tor is running on a separate machine
TOR_SERVER = "127.0.0.1"
TOR_SERVER_SOCKS_PORT = 9050
TOR_SERVER_CONTROL_PORT = 9051

#configure where to listen for connections *from* the Tor server
LISTEN_INTERFACE = "127.0.0.1"
LISTEN_PORT = 11009

import SocksiPy.socks as socks
import socket
import threading
import random
import time
import sys
import os
import subprocess
import version
import config

TORCHAT_PORT = 11009 #do NOT change this.
STATUS_OFFLINE = 0
STATUS_HANDSHAKE = 1
STATUS_ONLINE = 2
STATUS_AWAY = 3
STATUS_XA = 4

def isWindows():
    return "win" in sys.platform

def splitLine(text):
    sp = text.split(" ")
    try:
        a = sp[0]
        b = " ".join(sp[1:])
    except:
        a = text
        b = ""
    return a, b

def escape(text):
    text = text.replace("\\", "\\/") #replace \ with \/
    text = text.replace("\n", "\\n")  #replace linebreak with \n
    return text

def unescape(text):
    text = text.replace("\\n", "\n") #replace \n with linebreak
    text = text.replace("\\/", "\\") #replace \/ with \
    return text

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
    
    def connect(self):
        self.conn_out = OutConnection(self.address + ".onion", self.bl, self)
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
        self.conn_out.send(escape(text) + "\n")
        
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
    def __init__(self, callbackMessage):
        self.callbackMessage = callbackMessage
        
        if TRY_PORTABLE_MODE:
            startPortableTor()
        
        self.listener = Listener(self)
        self.own_status = STATUS_ONLINE
        
        filename = os.path.join(config.getDataDir(), "buddy-list.txt")
        
        #create it if it does not already exist
        f = open(filename, "a")
        f.close()
        
        f = open(filename, "r")
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
        for buddy in self.list:
            if buddy.address == OWN_HOSTNAME:
                found = True
        
        if not found:
            self.addBuddy(Buddy(OWN_HOSTNAME, self, "myself"))
        
        self.test()

    def save(self):
        f = open(os.path.join(config.getDataDir(), "buddy-list.txt"), "w")
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
                self.callbackMessage(buddy, unescape(text))
                    
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
                readbuffer = temp.pop()
            
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
    def __init__(self, address, buddy_list, buddy):
        threading.Thread.__init__(self)
        self.bl = buddy_list
        self.buddy = buddy
        self.address = address
        self.send_buffer = []
        self.start()
        
    def run(self):
        self.running = True
        try:
            self.conn = socks.socksocket()
            self.conn.settimeout(25)
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
    def __init__(self, buddy_list):
        threading.Thread.__init__(self)
        self.buddy_list = buddy_list
        self.conns = []
        self.start()

    def run(self):
        self.running = True
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        self.socket.bind((LISTEN_INTERFACE, LISTEN_PORT))
        self.socket.listen(1)
        try:
            while self.running:
                try:
                    conn, address = self.socket.accept()
                    self.conns.append(InConnection(conn, self.buddy_list))
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

def startPortableTor():
    global OWN_HOSTNAME
    global TOR_SERVER_SOCKS_PORT
    global TOR_SERVER_CONTROL_PORT
    global tor_in, tor_out
    old_dir = os.getcwd()
    try:
        os.chdir("tor")
        # completely remove all cache files from the previous run
        for root, dirs, files in os.walk("tor_data", topdown=False):
            for name in files:
                os.remove(os.path.join(root, name))
            for name in dirs:
                os.rmdir(os.path.join(root, name))
        
        # now start tor with the supplied config file
        subprocess.Popen("tor -f torrc.txt".split(), creationflags=0x08000000)
        
        # we now assume the existence of our hostname file
        # it WILL be created after the first start
        # if not, something must be totally wrong.
        cnt = 0
        while cnt < 20:
            try:
                f = open("hidden_service\\hostname", "r")
                OWN_HOSTNAME = f.read().rstrip()[:-6]
                f.close()
                break
            except:
                # we wait 20 seconds for the file to appear
                time.sleep(1)
                cnt += 1

        #in portable mode we run Tor on some non-standard ports:
        TOR_SERVER_SOCKS_PORT = 11109
        TOR_SERVER_CONTROL_PORT = 11119
        os.chdir(old_dir)
    except:
        os.chdir(old_dir)
        