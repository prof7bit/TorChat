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
import tempfile
import md5
import traceback
import config

TORCHAT_PORT = 11009 #do NOT change this.
STATUS_OFFLINE = 0
STATUS_HANDSHAKE = 1
STATUS_ONLINE = 2
STATUS_AWAY = 3
STATUS_XA = 4

CB_TYPE_CHAT = 1
CB_TYPE_FILE = 2

def tb():
    traceback.print_exc()

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


class MProtocolMsg(type):
    #Meta-Class for ProtocolMsg. It automagically creates a hash for 
    #mapping protocol-commands and corresponding ProtocolMsg-subclasses
    subclasses = {}
    def __init__(cls, name, bases, dict):
        #this will be executed whenever a ProtocolMsg gets *defined*
        #(happens once at the time when this module is imported).
        #the commands and their classes will be stored in 
        #the static member MProtocolMsg.subclasses
        cls.subclasses[dict["command"]] = cls
        super(MProtocolMsg, cls).__init__(name, bases, dict)

        
class ProtocolMsg(object):
    __metaclass__ = MProtocolMsg
    command = ""
    #the base class for all ProtocolMsg-classes. All message classes
    #must inherit from this and declare the static member command
    #which is used (by the metaclass-magic-voodoo;-) to map between the
    #command-string and the corresponding message class
    
    #Besides being the base class for all ProtocolMsg_* classes
    #this class has two other use cases:
    # - it is used for outgoing messages (therefore the send method)
    # - it is instantiated for every unknown incoming message

    def __init__(self, bl, connection, command, data):
        #connection may be None for outgoing messages
        #data can be a number, a string, a tuple or a list
        self.connection = connection
        if connection:
            self.buddy = connection.buddy
        else:
            self.buddy = None
        self.bl = bl
        self.command = command
        
        #self.text is always a string containing all arguments and data
        if type(data) in (list, tuple):
            self.text = " ".join(str(x) for x in data)
        else:
            self.text = str(data)
        self.parse()

    def parse(self):
        pass
    
    def execute(self):
        #a generic message of this class will be automatically instantiated 
        #if an incoming message with an unknown command is received 
        #do nothing and just reply with "not_implemented"
        message = ProtocolMsg(self.bl, None, "not_implemented", self.command)
        message.send(self.buddy)

    def getLine(self):
        #bring the message into a form we can transmit over the socket
        #it will escape newline characters, as they are the only
        #characters with a special meaning.
        #the opposite of this operation takes place in the function
        #ProtocolMsgFromLine() where incoming messages are instantiated.
        return self.command + " " + escape(self.text)

    def send(self, buddy, conn=0):
        #conn=0 use outgoing connection
        #conn=1 use incoming connection
        #FIXME: what if buddy is None?
        buddy.sendLine(self.getLine(), conn)

    
def ProtocolMsgFromLine(bl, conn, line):
    #this is the factory for producing instances of ProtocolMsg classes.
    #it separates the first word from the line, looks up the corresponding
    #ProtocolMsg subclass which can handle this kind of protocol message
    #and returns an instance. If no class matches the command string it
    #returns a ProtocolMsg instance which is generic and just does nothing.
    command, text_escaped = splitLine(line)
    #the rest of the message can be arbitrary (but escaped) data.
    #unescape it, so it is in it's original (maybe even binary) form.
    data = unescape(text_escaped)
    try:
        return MProtocolMsg.subclasses[command](bl, conn, command, data)
    except:
        return ProtocolMsg(bl, conn, command, data)


class ProtocolMsg_not_implemented(ProtocolMsg):
    command = "not_implemented"
    #FIXME: Maybe it would be better to have a *separate* 
    #"not_implemented"-message for every protocol message.
    #I have to meditate over this for a while.
    def ececute(self):
        print "buddy says he can't handle '%s'" % self.text

 
class ProtocolMsg_ping(ProtocolMsg):
    command = "ping"
    #a ping message consists of sender address and a random string
    def parse(self):
        #the sender address is in the text. we take it for granted
        #and see if we can find a buddy in our list with that address.
        self.address, self.answer = splitLine(self.text)
        self.buddy = self.bl.getBuddyFromAddress(self.address)

    def execute(self):
        #ping messages must be answered with pong messages
        #the pong must contain the same random string as the ping.
        if not self.buddy:
            #we have received a ping, but there is no buddy with
            #that address in our list. The only reason for that
            #can be that someone new has added our address to his list
            #and his client now has connected us. We now just create
            #a new buddy with this address and add it to our list.
            self.buddy = Buddy(self.address, self.bl, "")
            self.bl.addBuddy(self.buddy)
            
        answer = ProtocolMsg(self.bl, None, "pong", self.answer)
        answer.send(self.buddy)


class ProtocolMsg_pong(ProtocolMsg):
    command = "pong"
    def parse(self):
        #incoming pong messages are used to identify and authenticate 
        #incoming connections. Basically we send out pings and see which
        #corresponding pongs come in on which connections.
        #we search all our known buddies for the corresponding random
        #string to identify which buddy is replying here.
        self.buddy = self.bl.getBuddyFromRandom(self.text)

    def execute(self):
        #if the pong is found to belong to a known buddy we can now
        #safely assign this incoming connection to this buddy and 
        #regard the handshake as completed.
        if self.buddy:
            if self.buddy.conn_in == None:
                self.buddy.conn_in = self.connection
                self.buddy.status = STATUS_ONLINE
                self.connection.buddy = self.buddy
        else:
            #if we receive an unknown pong, we just ignore it.
            #FIXME: are unknown pongs a sign for an attempted  MITM-Attack?
            #At least they should never happen.
            print "strange: incoming 'pong' without corresponding ping"

                
class ProtocolMsg_message(ProtocolMsg):
    command = "message"
    #this is a normal text chat message.
    def execute(self):
        #give buddy and text to bl. bl will then call into the gui
        #to open a chat window and/or display the text.
        if self.buddy:
            self.bl.onChatMessage(self.buddy, self.text)


class ProtocolMsg_status(ProtocolMsg):
    command = "status"
    #this is a status message.
    def execute(self):
        #set the status flag of the corresponding buddy
        if self.buddy:
            if self.text == "available":
                self.buddy.status = STATUS_ONLINE
            if self.text == "away":
                self.buddy.status = STATUS_AWAY
            if self.text == "xa":
                self.buddy.status = STATUS_XA


class ProtocolMsg_filename(ProtocolMsg):
    command = "filename"
    #the first message in a file transfer, initiating the transfer.
    def parse(self):
        self.id, text = splitLine(self.text)
        file_size, text = splitLine(text) 
        block_size, self.file_name = splitLine(text)
        self.file_size = int(file_size)
        self.block_size = int(block_size)

    def execute(self):
        #we create a file receiver instance which can deal with the
        #file data we expect to receive now
        FileReceiver(self.buddy, 
                     self.id, 
                     self.block_size, 
                     self.file_size,
                     self.file_name)
        
    
class ProtocolMsg_filedata(ProtocolMsg):
    command = "filedata"
    #after a filename message has initiated the transfer several
    #filedata messagess transport the actual data in blocks of fixed
    #size. The data blocks are transmitted as they are and only 
    #newline characters are escaped. (see escape() and unescape())
    def parse(self):
        self.id, text = splitLine(self.text)
        start, text = splitLine(text)
        self.hash, self.data = splitLine(text)
        self.start = int(start)

    def execute(self):
        #there should already be a receiver, because there should have been
        #a "filename"-message at the very beginning of the transfer.
        receiver = self.bl.getFileReceiver(self.buddy.address, self.id)
        if receiver:
            receiver.data(self.start, self.hash, self.data)
        else:
            #if there is no receiver for this data, we just reply
            #with a stop message and hope the sender gets it and
            #stops sending data. Not much else to do for us here.
            msg = ProtocolMsg(self.bl, None, "file_stop_sending", self.id)
            msg.send(self.buddy)


class ProtocolMsg_filedata_ok(ProtocolMsg):
    command = "filedata_ok"
    #every received "filedata" must be confirmed with a "filedata_ok"
    #(or a "filedata_error")
    def parse(self):
        self.id, start = splitLine(self.text)
        self.start = int(start)
        
    def execute(self):
        sender = self.bl.getFileSender(self.buddy.address, self.id)
        if sender:
            sender.receivedOK(self.start)
        else:
            #there is no sender (anymore) to handle confirmation messages
            #so we can send a stop message to tell the other side
            #to stop receiving  
            msg = ProtocolMsg(self.bl, None, "file_stop_receiving", self.id)
            msg.send(self.buddy)
      
      
class ProtocolMsg_filedata_error(ProtocolMsg):
    command = "filedata_error"
    def parse(self):
        self.id, start = splitLine(self.text)
        self.start = int(start)
        
    def execute(self):
        sender = self.bl.getFileSender(self.buddy.address, self.id)
        if sender:
            sender.restart(self.start)
        else:        
            msg = ProtocolMsg(self.bl, None, "file_stop_receiving", self.id)
            msg.send(self.buddy)


class ProtocolMsg_file_stop_sending(ProtocolMsg):
    command = "file_stop_sending"
    #if the file transfer is prematurely canceled by the receiver
    #then this message tells the sender to stop sending further data 
    def parse(self):
        self.id = self.text
    
    def execute(self):
        sender = self.bl.getFileSender(self.buddy.address, self.id)
        if sender:
            #close the sender (if not already closed)
            #otherwise just ignore it
            sender.close()
        

class ProtocolMsg_file_stop_receiving(ProtocolMsg):
    command = "file_stop_receiving"
    #if the file transfer is prematurely canceled by the sender
    #then this message tells the receiving buddy to close its receiver 
    def parse(self):
        self.id = self.text
    
    def execute(self):
        receiver = self.bl.getFileReceiver(self.buddy.address, self.id)
        if receiver:
            #close the receiver (if not already closed)
            #otherwise just ignore it
            receiver.closeForced()
        
    
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
        
    def sendLine(self, line, conn=0):
        #conn: use outgiong or incoming connection
        if self.conn_out == None:
            self.connect()
        if conn == 0:
            self.conn_out.send(line + "\n")
        else:
            if self.conn_in:
                self.conn_in.send(line + "\n")
            else:
                #FIXME: handle this condition
                pass

    def sendChatMessage(self, text):
        message = ProtocolMsg(self.bl, None, "message", text)
        message.send(self)

    def sendFile(self, filename, gui_callback):
        sender = FileSender(self, filename, gui_callback)
        return sender
    
    def ping(self):
        if self.conn_out == None:
            self.connect()
        else:
            ping = ProtocolMsg(self.bl, None, "ping", (OWN_HOSTNAME, self.random1))
            ping.send(self)
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
                msg = ProtocolMsg(self.bl, None, "status", status)
                msg.send(self)
        
    def getDisplayName(self):
        if self.name != "":
            line = "%s (%s)" % (self.address, self.name)
        else:
            line = self.address
        return line
    

class BuddyList(object):
    #the buddy list object is somewhat like a central API.
    #All functionality and access to all other objects should
    #be possible with it's methods. Most other objects carry
    #a reference to the one and only BuddyList object around 
    #to be able to find and interact with other objects.
    def __init__(self, guiCallback):
        self.guiCallback = guiCallback
        
        if TRY_PORTABLE_MODE:
            startPortableTor()
        
        self.file_sender = {}
        self.file_receiver = {}
        
        self.listener = Listener(self)
        self.own_status = STATUS_ONLINE
        
        filename = os.path.join(config.getDataDir(), "buddy-list.txt")
        
        #create empty buddy list file if it does not already exist
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
            
        self.timer = threading.Timer(30, self.test)
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
    
    def getBuddyFromRandom(self, random):
        for buddy in self.list:
            if buddy.random1 == random:
                return buddy
        return None
    
    def getFileReceiver(self, address, id):
        try:
            return self.file_receiver[address, id]
        except:
            return None
        
    def getFileSender(self, address, id):
        try:
            return self.file_sender[address, id]
        except:
            return None
    
    def setStatus(self, status):
        self.own_status = status
        for buddy in self.list:
            buddy.sendStatus()
    
    def onErrorIn(self, connection):
        for buddy in self.list:
            if buddy.conn_in == connection:
                buddy.disconnect()
                break
    
    def onErrorOut(self, connection):
        connection.buddy.disconnect()

    def onConnected(self, connection):
        connection.buddy.status = STATUS_HANDSHAKE

    def onChatMessage(self, buddy, message):
        self.guiCallback(CB_TYPE_CHAT, (buddy, message))
        
    def onFileReceive(self, file_receiver):
        return self.guiCallback(CB_TYPE_FILE, file_receiver)

class Receiver(threading.Thread):
    def __init__(self, conn):
        threading.Thread.__init__(self)
        self.conn = conn
        self.socket = conn.socket
        self.running = True
        self.start()

    def run(self):
        readbuffer = ""
        while self.running:
            try:
                recv = self.socket.recv(1024)
                if recv != "":
                    readbuffer = readbuffer + recv
                    temp = readbuffer.split("\n")
                    readbuffer = temp.pop()
                
                    for line in temp:
                        if self.running:
                            try:
                                message = ProtocolMsgFromLine(self.conn.bl, 
                                                              self.conn, 
                                                              line)
                                message.execute()
                            except:
                                tb()
                else:
                    self.running = False
                    self.conn.onReceiverError()     
            
            except socket.timeout:
                pass
            
            except socket.error:
                self.running = False
                self.conn.onReceiverError()
                               
        
class InConnection:
    def __init__(self, socket, buddy_list):
        self.buddy = None
        self.bl = buddy_list
        self.socket = socket
        self.receiver = Receiver(self)
    
    def send(self, text):
        try:
            self.socket.send(text)
        except:
            self.bl.onErrorIn(self)
        
    def onReceiverError(self):
        self.bl.onErrorIn(self)
    
    def close(self):
        try:
            self.socket.close()
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
            self.socket = socks.socksocket()
            self.socket.settimeout(300)
            self.socket.setproxy(socks.PROXY_TYPE_SOCKS5, 
                                 TOR_SERVER, 
                                 TOR_SERVER_SOCKS_PORT)
            self.socket.connect((self.address, TORCHAT_PORT))
            self.bl.onConnected(self)
            self.receiver = Receiver(self)
            while self.running:
                if len(self.send_buffer) > 0:
                    text = self.send_buffer.pop(0)
                    self.socket.send(text)
                time.sleep(0.05)
                
        except:
            self.bl.onErrorOut(self)
            self.close()
            
    def send(self, text):
        self.send_buffer.append(text)
        
    def onReceiverError(self):
        self.bl.onErrorOut(self)
        self.close()
    
    def close(self):
        self.running = False
        try:
            self.socket.close()
        except:
            pass
        

class FileSender(threading.Thread):
    def __init__(self, buddy, file_name, guiCallback):
        threading.Thread.__init__(self)
        self.buddy = buddy
        self.bl = buddy.bl
        self.file_name = file_name
        self.file_name_short = os.path.basename(self.file_name)
        self.guiCallback = guiCallback
        self.id = str(random.getrandbits(32))
        self.buddy.bl.file_sender[self.buddy.address, self.id] = self
        self.file_size = 0
        self.block_size = 8192
        self.blocks_wait = 16
        self.start_ok = -1
        self.restart_at = 0
        self.restart_flag = False
        self.completed = False
        self.timeout_count = 0
        self.start()
        
    
    def testTimeout(self):
        #this will be called every 0.1 seconds whenever the sender
        #is waiting for confirmation messages. Either in the 
        #sendBlocks() loop or when all blocks are sent in the
        #outer loop in run()
        #if a timeout is detected then the restart flag will be set
        if self.buddy.conn_in:
            #we only increase timeout if we are connected
            #otherwise other mechanisms are responsible and trying
            #to get us connected again and we just wait
            self.timeout_count += 1
            
        if self.timeout_count == 600: 
            #one minute without filedata_ok
            new_start = self.start_ok + self.block_size
            self.restart(new_start)
            #enforce a new connection
            try:
                self.buddy.conn_in.close()
            except:
                pass
            print "timeout file sender restart at %i" % new_start     

    def canGoOn(self, start):
        position_ok = self.start_ok + self.blocks_wait * self.block_size
        if not self.running or self.restart_flag:
            return True
        else:
            return  position_ok > start
        
    
    def sendBlocks(self, first):
        blocks = int(self.file_size / self.block_size) + 1
        #the inner loop (of the two loops)
        for i in range(blocks):
            start = i * self.block_size
            #jump over already sent blocks
            if start >= first:
                remaining = self.file_size - start
                if remaining > self.block_size:
                    size = self.block_size
                else:
                    size = remaining
                self.file_handle.seek(start)
                data = self.file_handle.read(size)
                hash = md5.md5(data).hexdigest()
                
                msg = ProtocolMsg(self.bl, None, "filedata", (self.id,
                                                              start,
                                                              hash,
                                                              data))
               
                #we can only send data if we are connected
                while not self.buddy.conn_in and not self.restart_flag:
                    time.sleep(0.1)
                    self.testTimeout()
                                    
                if self.buddy.conn_in:
                    msg.send(self.buddy, 1)
                
                #wait for confirmations more than blocks_wait behind
                while not self.canGoOn(start):                    
                    time.sleep(0.1)
                    self.testTimeout() #this can trigger the restart flag
                    
                if self.restart_flag:
                    #the outer loop in run() will start us again
                    break
                    
                if not self.running:
                    #the outer loop in run() will also end
                    break
                    
    def run(self):
        self.running = True
        try:
            self.file_handle = open(self.file_name, mode="rb")
            self.file_handle.seek(0, os.SEEK_END)
            self.file_size = self.file_handle.tell()
            self.guiCallback(self.file_size, 0)
            msg = ProtocolMsg(self.bl, None, "filename", (self.id, 
                                                          self.file_size, 
                                                          self.block_size,
                                                          self.file_name_short))
            msg.send(self.buddy, 1)
            
            #the outer loop (of the two sender loops)
            #runs forever until completed ore canceled
            while (not self.completed) and self.running:
                self.restart_flag = False
                
                #(re)start the inner loop
                self.sendBlocks(self.restart_at)
                
                #wait for *last* filedata_ok or restart flag
                while not self.restart_flag and not self.completed and self.running:
                    time.sleep(0.1)
                    self.testTimeout() #this can trigger the restart flag
            
            self.running = False
            self.file_handle.close()

        except:
            try:
                self.guiCallback(self.file_size, 
                                 -1, 
                                 "error while sending %s" % self.file_name)
            except:
                tb()
            self.close()
            tb()
        
    def receivedOK(self, start):
        self.timeout_count = 0 # we have received a sign of life
        end = start + self.block_size
        if end > self.file_size:
            end = self.file_size
        
        try:    
            self.guiCallback(self.file_size, end)
        except:
            #cannot update gui
            tb()
            self.close()
            
        self.start_ok = start
        
        if end == self.file_size:
            #the outer sender loop can now stop waiting for timeout
            self.completed = True
        
    def restart(self, start):
        #trigger the reatart flag
        self.timeout_count = 0
        self.restart_at = start
        self.restart_flag = True
        #the inner loop will now immediately break and
        #the outer loop will start it again at position restart_at
        
    def sendStopMessage(self):
        msg = ProtocolMsg(self.buddy.bl, None, "file_stop_receiving", self.id)
        msg.send(self.buddy)
    
    def close(self):
        if self.running:
            self.running = False
            self.sendStopMessage()
            try:
                self.guiCallback(self.file_size, -1, "transfer aborted")
            except:
                pass
        del self.buddy.bl.file_sender[self.buddy.address, self.id]


class FileReceiver:
    def __init__(self, buddy, id, block_size, file_size, file_name):
        self.buddy = buddy
        self.id = id
        self.block_size = block_size
        self.file_name = file_name
        self.file_name_save = ""
        tmp = tempfile.mkstemp("_" + self.file_name, "torchat_incoming_")
        fd, self.file_name_tmp = tmp
        self.file_handle_tmp = os.fdopen(fd, "w+b")
        self.file_size = file_size
        self.next_start = 0
        self.wrong_block_number_count = 0
        self.buddy.bl.file_receiver[self.buddy.address, self.id] = self
        self.guiCallback = self.buddy.bl.onFileReceive(self)
        
    def data(self, start, hash, data):
        if start > self.next_start:
            if self.wrong_block_number_count == 0:
                #not on every single out-of-order block in a row 
                #we must send an error message...
                msg = ProtocolMsg(self.buddy.bl, None, "filedata_error", (self.id, 
                                                                          self.next_start))
                msg.send(self.buddy)
                self.wrong_block_number_count += 1
                #...only every 16
                #FIXME: This must be solved more elegantly
                if self.wrong_block_number_count == 16:
                    self.wrong_block_number_count = 0
            return 

        self.wrong_block_number_count = 0
        hash2 = md5.md5(data).hexdigest()
        if hash == hash2:
            self.file_handle_tmp.seek(start)
            self.file_handle_tmp.write(data)
            self.next_start = start + len(data)
            msg = ProtocolMsg(self.buddy.bl, None, "filedata_ok", (self.id, 
                                                               start))
            msg.send(self.buddy)
            try:
                self.guiCallback(self.file_size, start + len(data))
            except:            
                #this condition should not be possible, but who knows?
                #if there is still a receiver but no gui we close the receiver
                #and send a stop message
                print "FileReceiver could not update the GUI"
                tb()
                
                self.sendStopMessage()
                self.close()
        else:
            print "receiver wrong hash %i len: %i" % (start, len(data))
            msg = ProtocolMsg(self.buddy.bl, None, "filedata_error", (self.id, 
                                                                      start))
            msg.send(self.buddy)
            #we try to avoid unnecessary wrong-block-number errors
            #the next block sure will be out of order, but we have sent
            #an error already because of the wrong hash
            self.wrong_block_number_count = 1
            
    
    def setFileNameSave(self, file_name_save):
        self.file_name_save = file_name_save
            
    def sendStopMessage(self):
        msg = ProtocolMsg(self.buddy.bl, None, "file_stop_sending", self.id)
        msg.send(self.buddy)
    
    def closeForced(self):
        self.guiCallback(self.file_size, -1, "transfer aborted")
        self.sendStopMessage()
        self.file_name_save = ""
        self.close()
    
    def close(self):
        try:
            self.file_handle_tmp.close()
            if self.file_name_save:
                #FIXME: this will always overwrite 
                #an existing file without any warning
                try:
                    os.unlink(self.file_name_save)
                except:
                    pass
                try:
                    os.rename(self.file_name_tmp, self.file_name_save)
                except:
                    pass
            else:
                try:
                    os.unlink(self.file_name_tmp)
                except:
                    pass
            del self.buddy.bl.file_receiver[self.buddy.address, self.id]
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
                    tb()
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
        #for root, dirs, files in os.walk("tor_data", topdown=False):
        #    for name in files:
        #        os.remove(os.path.join(root, name))
        #    for name in dirs:
        #        os.rmdir(os.path.join(root, name))
        
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
        
