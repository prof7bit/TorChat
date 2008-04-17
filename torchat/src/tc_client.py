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
import config

TORCHAT_PORT = 11009 #do NOT change this.
STATUS_OFFLINE = 0
STATUS_HANDSHAKE = 1
STATUS_ONLINE = 2
STATUS_AWAY = 3
STATUS_XA = 4

CB_TYPE_CHAT = 1
CB_TYPE_FILE = 2

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
        
        #if we receive an unknown pong, we just ignore it.
        
        #FIXME: are unknown pongs a sign for an attempted  MITM-Attack?
        #At least they should never happen.
        #Maybe we should throw out a warning to the user, but pongs
        #do not contain any further address info, so we cannot tell
        #*which* connection the attacker is trying to forge. 
        #maybe we must change the protocol so that valid pong messages
        #*must* contain the address too.
        #On the other hand: I have no idea why someone would even *try*
        #to do a MITM without controlling the *other* rendevouz-node
        #too, which would effectively mean that he must have control 
        #over the complete TOR-network.
        if self.buddy:
            if self.buddy.conn_in == None:
                self.buddy.conn_in = self.connection
                self.buddy.status = STATUS_ONLINE
                self.connection.buddy = self.buddy

                
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
        start, self.data = splitLine(text)
        self.start = int(start)

    def execute(self):
        #there should already be a receiver, because there should have been
        #a "filename"-message at the very beginning of the transfer.
        receiver = self.bl.getFileReceiver(self.buddy.address, self.id)
        if receiver:
            receiver.data(self.start, self.data)
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
            receiver.close()
        
    
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
                        line = line.rstrip()
                        if self.running:
                            message = ProtocolMsgFromLine(self.conn.bl, 
                                                          self.conn, 
                                                          line)
                            message.execute()
                else:
                    self.running = False
                    self.conn.onReceiverError()     
            
            except socket.timeout:
                pass
            
            except socket.error, exc:
                print exc.message
                self.conn.onReceiverError()
                           
            except:
                import traceback
                traceback.print_exc()
    
        
class InConnection:
    def __init__(self, socket, buddy_list):
        self.buddy = None
        self.bl = buddy_list
        self.socket = socket
        self.receiver = Receiver(self)
    
    def send(self, text):
        self.socket.send(text)
        
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
            self.socket.settimeout(25)
            self.socket.setproxy(socks.PROXY_TYPE_SOCKS4, 
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
        self.start()
        
    def run(self):
        self.running = True
        try:
            file_handle = open(self.file_name)
            file_handle.seek(0, os.SEEK_END)
            self.file_size = file_handle.tell()
            self.guiCallback(self.file_size, 0)
            msg = ProtocolMsg(self.bl, None, "filename", (self.id, 
                                                          self.file_size, 
                                                          self.block_size,
                                                          self.file_name_short))
            msg.send(self.buddy, 1)
            blocks = int(self.file_size / self.block_size) + 1
            for i in range(blocks):
                start = i * self.block_size
                remaining = self.file_size - start
                if remaining > self.block_size:
                    size = self.block_size
                else:
                    size = remaining
                file_handle.seek(start)
                data = file_handle.read(size)
                
                msg = ProtocolMsg(self.bl, None, "filedata", (self.id,
                                                              start,
                                                              data))
                msg.send(self.buddy, 1)
                while not self.start_ok + self.blocks_wait * self.block_size > start:
                    time.sleep(0.1)
                    
                if not self.running:
                    break
                
        except:
            #FIXME: call gui and tell it about error
            print "error sending file %s" % self.file_name
            import traceback
            traceback.print_exc()
        
    def receivedOK(self, start):
        end = start + self.block_size
        if end > self.file_size:
            end = self.file_size
            
        self.guiCallback(self.file_size, end)
        self.start_ok = start
        
    def close(self):
        self.running = False
        del self.buddy.bl.file_sender[self.buddy.address, self.id]

class FileReceiver:
    def __init__(self, buddy, id, block_size, file_size, file_name):
        self.buddy = buddy
        self.id = id
        self.block_size = block_size
        self.file_name = file_name
        self.file_size = file_size
        self.buddy.bl.file_receiver[self.buddy.address, self.id] = self
        self.guiCallback = self.buddy.bl.onFileReceive(self)
        
    def data(self, start, data):
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
            import traceback
            traceback.print_exc()

            self.sendStopMessage()
            self.close()
            
    def sendStopMessage(self):
        msg = ProtocolMsg(self.buddy.bl, None, "file_stop_sending", self.id)
        msg.send(self.buddy)
    
    def close(self):
        del self.buddy.bl.file_receiver[self.buddy.address, self.id]
        
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
                    import traceback
                    traceback.print_exc()
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
        