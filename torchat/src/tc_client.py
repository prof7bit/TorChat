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

# This is the TorChat client library. Import this module, make an instance
# of BuddyList, give it your call-back function and your client is running.

import SocksiPy.socks as socks
import socket
import threading
import random
import time
import sys
import os
import shutil
import subprocess
import tempfile
import hashlib
import config
import version

TORCHAT_PORT = 11009 #do NOT change this.
TOR_CONFIG = "tor" #the name of the active section in the .ini file
STATUS_OFFLINE = 0
STATUS_HANDSHAKE = 1
STATUS_ONLINE = 2
STATUS_AWAY = 3
STATUS_XA = 4

CB_TYPE_CHAT = 1
CB_TYPE_FILE = 2
CB_TYPE_OFFLINE_SENT = 3
CB_TYPE_STATUS = 4
CB_TYPE_LIST_CHANGED = 5
CB_TYPE_AVATAR = 6
CB_TYPE_PROFILE = 7
CB_TYPE_REMOVE = 8

tb = config.tb # the traceback function has moved to config
tb1 = config.tb1
tor_pid = None
tor_proc = None
tor_timer = None


def splitLine(text):
    """split a line of text on the first space character and return
    two strings, the first word and the remaining string. This is
    used for parsing the incoming messages from left to right since 
    the command and its arguments are all delimited by spaces"""
    sp = text.split(" ")
    try:
        a = sp[0]
        b = " ".join(sp[1:])
    except:
        a = text
        b = ""
    return a, b

def encodeLF(blob):
    """takes a string of 8 bit binary data and encodes 
    it so that there are no 0x0a (LF) bytes anymore"""
    # first we get all '\' out of the way by encoding each 
    # backslash '\' as  the character sequence '\/'
    # so there will not remain any '\n' sequence anymore,
    # and then we can safely encode every 0x0a as '\n'.
    # 
    # Please do not rant in the source code comments of your
    # own protocol implementations about how "suboptimal" my
    # decision was, actally I spent quite some time thinking
    # about it in the early design phase and this solution is 
    # pragmatic, easy to implement and rock solid.
    #
    # Also please do not suggest alternative encodings like
    # bandwidth wasting base64, there is NO NEED for it,
    # TorChat is NOT a text based protocol in the common
    # sense, we are transmitting binary data over 8-bit-clean
    # sockets. We don't have to fit them into RFC confoming 
    # message bodies of SMTP or NNTP messages, print them
    # line by line on a terminal or a printer or anything
    # else that would interpret control characters. The
    # only special character in this protocol is the message
    # delimiter which I chose to be 0x0a and because 0x0a is
    # often referred to as "newline" I call the chunks of
    # encoded data between them "lines" and each "line" is 
    # representing exactly one protocol message.
    return blob.replace("\\", "\\/").replace("\n", "\\n")

def decodeLF(line):
    """takes the line as it comes from the socket and decodes it to
    the original binary data contained in a string of bytes"""
    return line.replace("\\n", "\n").replace("\\/", "\\")
    

def createTemporaryFile(file_name):
    if config.getint("files", "temp_files_in_data_dir"):
        dir = config.getDataDir()
    else:
        dir = config.get("files", "temp_files_custom_dir")
    try:
        if dir == "":
            dir = None
        tmp = tempfile.mkstemp("_" + file_name, "torchat_incoming_", dir)
    except:
        print "(1) could not create temporary file in %s" % dir
        tb()
        print "(1) trying system temporary folder"
        tmp = tempfile.mkstemp("_" + file_name, "torchat_incoming_")
    fd, file_name_tmp = tmp
    file_handle_tmp = os.fdopen(fd, "w+b")
    print "(2) created temporary file  %s" % file_name_tmp
    return (file_name_tmp, file_handle_tmp)


class WipeFileThread(threading.Thread):
    """This wipes a file in a separate thread because
    wiping a file is a long running task and we don't
    want to freeze parts of the application. This is
    only called by the function wipeFile(). Call the
    function wipeFile() if you want to wipe a file."""
    def __init__(self, file_name):
        threading.Thread.__init__(self)
        self.file_name = file_name
        self.start()

    def run(self):
        BLOCK_SIZE = 8192
        print "(2) wiping %s" % self.file_name
        if os.path.exists(self.file_name):
            try:
                handle = open(self.file_name, mode="r+b")
                handle.seek(0, 2) #SEEK_END
                size = handle.tell()
                handle.seek(0)
                blocks = size / BLOCK_SIZE + 1
                for i in range(blocks):
                    handle.write(os.urandom(BLOCK_SIZE))
                print "(2) sync to disk"
                handle.flush()
                os.fsync(handle.fileno())
                handle.close()
                print "(2) unlinking wiped file"
                os.unlink(self.file_name)
            except:
                print "(0) could not wipe file %s (file is locked or wrong permissions)" % self.file_name
        else:
            print "(2) file %s does not exist" % self.file_name

def wipeFile(file_name):
    """Wipe a file by first overwriting it with random data,
    synching it to disk and finally unlinking it. For this
    purpose it will start a separat thread to do this in the
    background and return immediately."""
    WipeFileThread(file_name)

#--- ### Client API

class Buddy(object):
    """Represets a buddy. Every buddy on the buddy list will have sich 
    an instance created directly after program start and also every new 
    connection from unknown addresses will result in the instantiation 
    of a new buddy object when a valid ping message has been processed. 
    All Buddy objects are maintained by and contained in the BuddyList 
    object"""
    def __init__(self, address, buddy_list, name=u"", temporary=False):
        assert isinstance(buddy_list, BuddyList) #type hint for PyDev
        print "(2) initializing buddy %s, temporary=%s" % (address, temporary)
        self.bl = buddy_list
        self.address = address
        self.name = name
        self.profile_name = u""
        self.profile_text = u""
        self.profile_avatar_data = ""        # uncompressed 64*64*24 bit RGB.
        self.profile_avatar_data_alpha = ""  # uncompressed 64*64*8 bit alpha. (optional)
        self.profile_avatar_object = None    # tc_gui.py will cache a wx.Bitmap here, tc_client will not touch this
        self.random1 = str(random.getrandbits(256))
        self.random2 = str(random.getrandbits(256))
        self.conn_out = None
        self.conn_in = None
        self.status = STATUS_OFFLINE
        self.client = ""
        self.version = ""
        self.timer = False
        self.last_status_time = 0
        self.count_failed_connects = 0
        self.count_unanswered_pings = 0
        self.active = True
        self.temporary = temporary
        self.startTimer()

    def connect(self):
        print "(2) %s.connect()" % self.address
        if self.conn_out == None:
            self.conn_out = OutConnection(self.address + ".onion", self.bl, self)
            self.count_unanswered_pings = 0
            self.sendPing()

    def isFullyConnected(self):
        return self.conn_in and self.conn_out and self.conn_out.pong_sent

    def isAlreadyPonged(self):
        return self.conn_out and self.conn_out.pong_sent

    def disconnect(self):
        print "(2) %s.disconnect()" % self.address
        if self.conn_out != None:
            self.conn_out.close()
            self.conn_out = None
        if self.conn_in != None:
            self.conn_in.close()
            self.conn_in = None
        self.onStatus(STATUS_OFFLINE)

    def onOutConnectionFail(self):
        print "(2) %s.onOutConnectionFail()" % self.address
        self.count_failed_connects += 1
        self.startTimer()

    def onInConnectionFail(self):
        print "(2) %s.onInConnectionFail()" % self.address
        self.resetConnectionFailCounter()
        self.startTimer()

    def onOutConnectionSuccess(self):
        print "(2) %s.onOutConnectionSuccess()" % self.address
        self.resetConnectionFailCounter()
        self.count_unanswered_pings = 0
        self.startTimer()

    def onInConnectionFound(self, connection):
        print "(2) %s.onInConnectionFound()" % self.address
        self.count_unanswered_pings = 0
        conn_old = self.conn_in
        if conn_old == connection:
            print "(2) this connection is already the current conn_in. doing nothing."
            return

        self.conn_in = connection
        connection.buddy = self
        if conn_old:
            print "(2) closing old connection of %s, %s" % (self.address, conn_old)
            print "(2) new connection is %s" % connection
            conn_old.buddy = None
            conn_old.close()

    def resetConnectionFailCounter(self):
        self.count_failed_connects = 0

    def setActive(self, active):
        print "(2) %s.setActive(%s)" % (self.address, active)
        self.active = active

    def setTemporary(self, temporary):
        print "(2) %s.setTemporary(%s)" % (self.address, temporary)
        self.temporary = temporary

    def onStatus(self, status):
        print "(2) %s.onStatus(%s)" % (self.address, status)
        self.last_status_time = time.time()
        if status <> self.status:
            self.status = status
            self.bl.gui(CB_TYPE_STATUS, self)

    def onProfileName(self, name):
        print "(2) %s.onProfile" % self.address
        self.profile_name = name
        if self.name == "" and name <> "":
            self.name = name
            self.bl.save()
        self.bl.gui(CB_TYPE_PROFILE, self)

    def onProfileText(self, text):
        print "(2) %s.onProfile" % self.address
        self.profile_text = text
        self.bl.gui(CB_TYPE_PROFILE, self)

    def onAvatarDataAlpha(self, data):
        print "(2) %s.onAvatarDataAplha()" % self.address
        # just store it, no gui callback because this is always sent first.
        # The next message will be the acual image data which will finally notify the GUI
        self.profile_avatar_data_alpha = data

    def onAvatarData(self, data):
        print "(2) %s.onAvatarData()" % self.address
        if data <> self.profile_avatar_data:
            self.profile_avatar_data = data
            self.bl.gui(CB_TYPE_AVATAR, self)

    def onChatMessage(self, message):
        self.bl.gui(CB_TYPE_CHAT, (self, message))
    
    def sendChatMessage(self, text):
        #text must be unicode, will be encoded to UTF-8
        if self.isFullyConnected():
            message = ProtocolMsg_message(self, text.encode("UTF-8"))
            message.send()
        else:
            self.storeOfflineChatMessage(text)

    def getOfflineFileName(self):
        return os.path.join(config.getDataDir(),self.address + "_offline.txt")

    def storeOfflineChatMessage(self, text):
        #text must be unicode
        print "(2) storing offline message to %s" % self.address
        file = open(self.getOfflineFileName(), "a")
        file.write("[delayed] " + text.encode("UTF-8") + "\n")
        file.close()

    def getOfflineMessages(self):
        #will return the string as unicode
        try:
            file = open(self.getOfflineFileName(), "r")
            text = file.read().rstrip()
            file.close()
            return text.decode("UTF-8")
        except:
            return ""

    def sendOfflineMessages(self):
        #this will be called in the incoming status message
        #FIXME: call this from onStatus() instead, this would be the ntural place for it
        text = self.getOfflineMessages()
        if text:
            if self.isFullyConnected():
                wipeFile(self.getOfflineFileName())
                print "(2) sending offline messages to %s" % self.address
                #we send it without checking online status. because we have sent
                #a pong before, the receiver will have set the status to online.
                #text is unicode, so we must encode it to UTF-8 again.
                message = ProtocolMsg_message(self, text.encode("UTF-8"))
                message.send()
                self.bl.gui(CB_TYPE_OFFLINE_SENT, self)
            else:
                print "(2) could not send offline messages, not fully connected."
                pass

    def getDisplayNameOrAddress(self):
        if self.name == "":
            return self.address
        else:
            return self.name

    def getAddressAndDisplayName(self):
        if self.name == "":
            return self.address
        else:
            return self.address + " (" + self.name + ")"

    def sendFile(self, filename, gui_callback):
        sender = FileSender(self, filename, gui_callback)
        return sender

    def startTimer(self):
        if not self.active:
            print "(2) %s is not active. Will not start a new timer" % self.address
            return

        if self.status == STATUS_OFFLINE:
            if self.count_failed_connects < 10:
                t = random.randrange(50, 150) / 10.0
            else:
                if self.count_failed_connects < 20:
                    t = random.randrange(300, 400)
                else:
                    # more than an hour. The other one will ping us if it comes
                    # online which will immediately connect and reset the counting
                    t = random.randrange(5000, 6000)
            print "(2) %s had %i failed connections. Setting timer to %f seconds" \
                % (self.address, self.count_failed_connects, t)
        else:
            #whenever we are connected to someone we use a fixed timer.
            #otherwise we would create a unique pattern of activity
            #over time that could be identified at the other side
            if self.status == STATUS_HANDSHAKE:
                # ping more agressively during handshake
                # to trigger more connect back attempts there
                t = config.KEEPALIVE_INTERVAL / 4
            else:
                # when fully connected we can slow down to normal 
                t = config.KEEPALIVE_INTERVAL
                

        if self.timer:
            self.timer.cancel()
        self.timer = threading.Timer(t, self.onTimer)
        self.timer.start()

    def onTimer(self):
        print "(2) %s.onTimer()" % self.address
        if not self.active:
            print "(2) %s is not active, onTimer() won't do anything" % self.address
            return

        self.keepAlive()

        #only restart the timer automatically if we are connected (or handshaking).
        #else it will be restarted by outConnectionFail() / outConnectionSuccess()
        if self.status != STATUS_OFFLINE:
            self.startTimer()

    def keepAlive(self):
        print "(2) %s.keepAlive()" % self.address
        if self.conn_out == None:
            self.connect()
        else:
            if self.conn_in:
                self.sendStatus()
            else:
                # still waiting for return connection
                if self.count_unanswered_pings < config.MAX_UNANSWERED_PINGS:
                    self.sendPing()
                    print "(2) unanswered pings to %s so far: %i" % (self.address, self.count_unanswered_pings)
                else:
                    # maybe this will help
                    print "(2) too many unanswered pings to %s on same connection" % self.address 
                    self.disconnect()

    def sendPing(self):
        print "(2) PING >>> %s" % self.address
        #self.random1 = str(random.getrandbits(256))
        ping = ProtocolMsg_ping(self, (config.get("client","own_hostname"), self.random1))
        ping.send()
        self.count_unanswered_pings += 1

    def sendStatus(self):
        if self.isAlreadyPonged():
            status = ""
            if self.bl.own_status == STATUS_ONLINE:
                status = "available"
            if self.bl.own_status == STATUS_AWAY:
                status = "away"
            if self.bl.own_status == STATUS_XA:
                status = "xa"
            if status != "":
                print "(2) %s.sendStatus(): sending %s" % (self.address, status)
                msg = ProtocolMsg_status(self, status)
                msg.send()
        else:
            print "(2) %s.sendStatus(): not connected, not sending" % self.address

    def sendProfile(self):
        if self.isAlreadyPonged():
            print "(2) %s.sendProfile()" % self.address

            # this message is optional
            name = config.get("profile", "name")
            if name <> "":
                msg = ProtocolMsg_profile_name(self, name.encode("UTF-8"))
                msg.send()

            # this message is optional
            text = config.get("profile", "text")
            if text <> "":
                msg = ProtocolMsg_profile_text(self, text.encode("UTF-8"))
                msg.send()

    def sendAvatar(self, send_empty=False):
        if self.isAlreadyPonged():
            print "(2) %s.sendAvatar()" % self.address
            # the GUI has put our own avatar into the BuddyList object, ready for sending.
            # avatar is optional but if sent then both messages must be in the following order:
            if self.bl.own_avatar_data or send_empty:
                # alpha might be empty (0 bytes) but we must always send it.
                data = self.bl.own_avatar_data_alpha
                msg = ProtocolMsg_profile_avatar_alpha(self, data) #send raw binary data
                msg.send()

                data = self.bl.own_avatar_data
                msg = ProtocolMsg_profile_avatar(self, data) #send raw binary data
                msg.send()
            else:
                print "(2) we have no avatar, sending nothing"

        else:
            print "(2) %s.sendAvatar(): not connected, not sending avatar" % self.address



    def sendAddMe(self):
        if self.isAlreadyPonged():
            msg = ProtocolMsg_add_me(self)
            msg.send()
        else:
            print "(2) not connected, not sending add_me to %s" % self.address

    def sendRemoveMe(self):
        if self.isFullyConnected():
            msg = ProtocolMsg_remove_me(self)
            msg.send()
        else:
            print "(2) not connected, not sending remove_me to %s" % self.address

    def sendVersion(self):
        if self.isAlreadyPonged():
            msg = ProtocolMsg_client(self, version.NAME)
            msg.send()
            msg = ProtocolMsg_version(self, version.VERSION)
            msg.send()
        else:
            print "(2) not connected, not sending version to %s" % self.address

    def getDisplayName(self):
        if self.name != "":
            line = "%s (%s)" % (self.address, self.name)
        else:
            line = self.address
        return line


class BuddyList(object):
    """the BuddyList object is the central API of the client.
    Initializing it will start the client, load and initialize
    all Buddy objects on the buddy list, etc. It does much more
    than only maintaining the buddies, it also maintains a bunch 
    of other objects like for example the FileSender and 
    FileReceiver objects for currrently running file transfers etc.
    BuddyList actually represents the whole client functionality
    and controls everything else. 
    The GUI will instantiate a BuddyList object and this is all
    it needs to do in order to start the client and access 
    all functionality"""
    def __init__(self, callback, socket=None):
        print "(1) initializing buddy list"
        self.gui = callback

        startPortableTor()

        self.file_sender = {}
        self.file_receiver = {}

        #temporary buddies, created from incoming pings with new hostnames
        #these buddies are not yet in the list and if they do not
        #answer and authenticate on the first try they will be deleted
        self.incoming_buddies = []

        self.listener = Listener(self, socket)
        self.own_status = STATUS_ONLINE

        filename = os.path.join(config.getDataDir(), "buddy-list.txt")

        #create empty buddy list file if it does not already exist
        f = open(filename, "a")
        f.close()

        f = open(filename, "r")
        l = f.read().replace("\r", "\n").replace("\n\n", "\n").split("\n")
        f.close
        self.list = []
        for line in l:
            line = line.rstrip().decode("UTF-8")
            if len(line) > 15:
                address = line[0:16]
                if len(line) > 17:
                    name = line[17:]
                else:
                    name = u""
                buddy = Buddy(address, self, name)
                self.list.append(buddy)

        found = False
        for buddy in self.list:
            if buddy.address == config.get("client", "own_hostname"):
                found = True
                self.own_buddy = buddy
                break

        if not found:
            print "(1) adding own hostname %s to list" % config.get("client", "own_hostname")
            if config.get("client", "own_hostname") != "0000000000000000":
                self.own_buddy = Buddy(config.get("client", "own_hostname"),
                                 self,
                                 "myself")
                self.addBuddy(self.own_buddy)

        # the own avatar is set by the GUI.
        # Only the GUI knows how to deal with graphics, so we just
        # provide these variables, if the GUI puts the 64*64*24 RGB bitmaps
        # here and an optional alpha channel 64*64*8 and then it will be
        # transmitted as it is. tc_client.py will not interpret or convert it,
        # only transmit and receive this raw data and notify the GUI
        self.own_avatar_data = ""
        self.own_avatar_data_alpha = ""

        print "(1) BuddList initialized"

    def save(self):
        f = open(os.path.join(config.getDataDir(), "buddy-list.txt"), "w")
        for buddy in self.list:
            line = ("%s %s\r\n" % (buddy.address, buddy.name.rstrip())).encode("UTF-8")
            f.write(line)
        f.close()
        print "(2) buddy list saved"

        # this is the optimal spot to notify the GUI to redraw the list
        self.gui(CB_TYPE_LIST_CHANGED, None)

    def logMyselfMessage(self, msg):
        self.own_buddy.onChatMessage("*** %s" % msg)

    def addBuddy(self, buddy):
        if self.getBuddyFromAddress(buddy.address) == None:
            self.list.append(buddy)
            buddy.setTemporary(False)
            buddy.setActive(True)
            if buddy in self.incoming_buddies:
                self.incoming_buddies.remove(buddy)
            self.save()
            buddy.keepAlive()
            return buddy
        else:
            return False

    def removeBuddy(self, buddy_to_remove, disconnect=True):
        print "(2) removeBuddy(%s, %s)" % (buddy_to_remove.address, disconnect)
        self.gui(CB_TYPE_REMOVE, buddy_to_remove)
        buddy_to_remove.setActive(False)

        if not disconnect:
            # send remove_me and leave the connections open
            # but remove them from this buddy.
            # the connections will be closed by the other buddy
            # or if the timeout for unused connections occurs
            buddy_to_remove.sendRemoveMe()
            if buddy_to_remove.conn_out:
                buddy_to_remove.conn_out.buddy = None
                buddy_to_remove.conn_out = None
            if buddy_to_remove.conn_in:
                buddy_to_remove.conn_in.buddy = None
                buddy_to_remove.conn_in = None
        else:
            buddy_to_remove.disconnect()
        self.list.remove(buddy_to_remove)
        file_name = buddy_to_remove.getOfflineFileName()
        try:
            wipeFile(file_name)
        except:
            pass
        self.save()

    def removeBuddyWithAddress(self, address):
        buddy = self.getBuddyFromAddress(address)
        if buddy != None:
            self.removeBuddy(buddy)

    def getBuddyFromAddress(self, address):
        for buddy in self.list:
            if buddy.address == address:
                return buddy
        return None

    def getIncomingBuddyFromAddress(self, address):
        for buddy in self.incoming_buddies:
            if buddy.address == address:
                return buddy
        return None

    def getBuddyFromRandom(self, random):
        for buddy in self.list:
            if buddy.random1 == random:
                return buddy
        return None

    def getIncomingBuddyFromRandom(self, random):
        for buddy in self.incoming_buddies:
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
        for buddy in self.incoming_buddies:
            if buddy.conn_in == connection:
                print "(2) in-connection %s of temporary buddy %s failed" % (connection, buddy.address)
                print "(2) removing buddy instance %s" % buddy.address
                buddy.setActive(False)
                buddy.disconnect()
                if buddy in self.incoming_buddies:
                    self.incoming_buddies.remove(buddy)
                break

        for buddy in self.list:
            if buddy.conn_in == connection:
                buddy.disconnect()
                buddy.onInConnectionFail()
                break

    def onErrorOut(self, connection):
        buddy = connection.buddy
        if buddy:
            if buddy.temporary:
                print "(2) out-connection of temporary buddy %s failed" % buddy.address
                print "(2) removing buddy instance %s" % buddy.address
                buddy.setActive(False)
                if buddy in self.incoming_buddies:
                    self.incoming_buddies.remove(buddy)

            buddy.disconnect()
            buddy.onOutConnectionFail()
        else:
            print "(2) out-connection without buddy failed"

    def onConnected(self, connection):
        connection.buddy.onStatus(STATUS_HANDSHAKE)
        connection.buddy.onOutConnectionSuccess()

    def stopClient(self):
        stopPortableTor()
        for buddy in self.list + self.incoming_buddies:
            buddy.disconnect()
        self.listener.close() #FIXME: does this really work?


class FileSender(threading.Thread):
    def __init__(self, buddy, file_name, callback):
        threading.Thread.__init__(self)
        self.buddy = buddy
        self.bl = buddy.bl
        self.file_name = file_name
        self.file_name_short = os.path.basename(self.file_name)
        self.gui = callback
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
        if self.buddy.isFullyConnected():
            #we only increase timeout if we are connected
            #otherwise other mechanisms are responsible and trying
            #to get us connected again and we just wait
            self.timeout_count += 1
        else:
            self.timeout_count = 0

        if self.timeout_count == 6000:
            #ten minutes without filedata_ok
            new_start = self.start_ok + self.block_size
            self.restart(new_start)
            #enforce a new connection
            try:
                self.buddy.disconnect()
            except:
                pass
            print "(2) timeout file sender restart at %i" % new_start

    def canGoOn(self, start):
        position_ok = self.start_ok + self.blocks_wait * self.block_size
        if not self.running or self.restart_flag:
            return True
        else:
            return  position_ok > start


    def sendBlocks(self, first):
        blocks = int(self.file_size / self.block_size) + 1

        print "(2) FileSender now entering inner loop, starting at block #%i, last block in file #%i" \
              % (first, blocks - 1)
        #the inner loop (of the two loops
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
                hash = hashlib.md5(data).hexdigest()

                #we can only send data if we are connected
                while not self.buddy.isFullyConnected() and not self.restart_flag:
                    time.sleep(0.1)
                    self.testTimeout()
                
                # the message is sent over conn_in
                msg = ProtocolMsg_filedata(self.buddy.conn_in, (self.id, start, hash, data))
                msg.send()

                #wait for confirmations more than blocks_wait behind
                while not self.canGoOn(start):
                    time.sleep(0.1)
                    self.testTimeout() #this can trigger the restart flag

                if self.restart_flag:
                    #the outer loop in run() will start us again
                    print "(2) FileSender restart_flag, breaking innner loop"
                    break

                if not self.running:
                    #the outer loop in run() will also end
                    print "(2) FileSender not running, breaking innner loop"
                    break

        print "(2) FileSender inner loop ended, last sent block: #%i, last block in file #%i" % (i, blocks-1)

    def run(self):
        self.running = True
        try:
            self.file_handle = open(self.file_name, mode="rb")
            self.file_handle.seek(0, 2) #SEEK_END
            self.file_size = self.file_handle.tell()
            self.gui(self.file_size, 0)
            filename_utf8 = self.file_name_short.encode("utf-8")

            if not self.buddy.isFullyConnected():
                print "(2) file transfer waiting for connection"
                self.gui(self.file_size, 0, "waiting for connection")

            # self.running will be set to false when the user hits "cancel"
            # wait for connection to start file transfer
            while self.running and not self.buddy.isFullyConnected():
                time.sleep(1)

            # user could have aborted while waiting in the loop above
            if self.running:
                print "(2) sending 'filename' message"
                self.gui(self.file_size, 0, "starting transfer")
                msg = ProtocolMsg_filename(self.buddy.conn_in, (self.id, self.file_size, self.block_size, filename_utf8))
                msg.send()

            #the outer loop (of the two sender loops)
            #runs forever until completed ore canceled
            while self.running and not self.completed:
                print "(2) FileSender now at start of retry loop"
                self.restart_flag = False

                #(re)start the inner loop
                self.sendBlocks(self.restart_at)

                #wait for *last* filedata_ok or restart flag
                while self.running and not self.completed and not self.restart_flag:
                    time.sleep(0.1)
                    self.testTimeout() #this can trigger the restart flag

            if self.running:
                print "(2) FileSender, retry loop ended because of success"
            else:
                print "(2) FileSender, retry loop ended because of cancel"

            self.running = False
            self.file_handle.close()

        except:
            # haven't seen this happening yet
            self.gui(self.file_size,
                                 -1,
                                 "error")
            self.close()
            tb()

    def receivedOK(self, start):
        self.timeout_count = 0 # we have received a sign of life
        end = start + self.block_size
        if end > self.file_size:
            end = self.file_size

        try:
            self.gui(self.file_size, end)
        except:
            #cannot update gui
            tb()
            self.close()

        self.start_ok = start

        if end == self.file_size:
            #the outer sender loop can now stop waiting for timeout
            self.gui(self.file_size, end, "transfer complete")
            self.completed = True

    def restart(self, start):
        #trigger the reatart flag
        self.timeout_count = 0
        self.restart_at = start
        self.restart_flag = True
        #the inner loop will now immediately break and
        #the outer loop will start it again at position restart_at

    def sendStopMessage(self):
        msg = ProtocolMsg_file_stop_receiving(self.buddy, self.id)
        msg.send()

    def close(self):
        if self.running:
            self.running = False
            self.sendStopMessage()
            try:
                self.gui(self.file_size, -1, "transfer aborted")
            except:
                pass
        del self.buddy.bl.file_sender[self.buddy.address, self.id]


class FileReceiver(object):
    # ths will be instantiated automatically on an incoming file transfer.
    # it will then notify the GUI which will open a window and give us a callback to interact
    def __init__(self, buddy, id, block_size, file_size, file_name):
        self.buddy = buddy
        self.id = id
        self.closed = False
        self.block_size = block_size
        self.file_name = file_name
        self.file_name_save = ""
        tmp = createTemporaryFile(self.file_name)
        self.file_name_tmp, self.file_handle_tmp = tmp
        print "(2) FileReceiver: created temp file: %s" % self.file_name_tmp
        self.file_size = file_size
        self.next_start = 0
        self.wrong_block_number_count = 0
        self.buddy.bl.file_receiver[self.buddy.address, self.id] = self

        #this will (MUST) point to the file transfer GUI callback
        self.gui = None

        #the following will result in a call into the GUI
        #the GUI will then give us a callback function
        print "(2) FileReceiver: notifying GUI about new file transfer"
        self.buddy.bl.gui(CB_TYPE_FILE, self)

        #we cannot receive without a GUI (or other piece of code
        #that provides the callback) because this other code
        #(usually the GUI) will decide what to do with the file
        #and will be responsible to close this FileReceiver object
        #again after it is done.

        #therefore now we wait for the callback
        #function to be provided before we continue.
        #It CANNOT get stuck in this loop unless the GUI
        #code is broken. The GUI WILL provide this callback!
        while self.gui == None:
            time.sleep(0.1)

        print "(2) FileReceiver: attached GUI seems ready, initialization done"

    def setCallbackFunction(self, callback):
        # this must be called from the GUI
        # to set the callback function so we can notify
        # the GUI about the progress (or errors)
        self.gui = callback

    def data(self, start, hash, data):
        if self.closed:
            # ignore still incoming data blocks
            # for already aborted transfers
            print "(2) ignoring incoming file data block for canceled receiver"
            return

        if start > self.next_start:
            if self.wrong_block_number_count == 0:
                #not on every single out-of-order block in a row
                #we must send an error message...
                msg = ProtocolMsg_filedata_error(self.buddy, (self.id, self.next_start))
                msg.send()
                self.wrong_block_number_count += 1
                #...only every 16
                #FIXME: This must be solved more elegantly
                if self.wrong_block_number_count == 16:
                    self.wrong_block_number_count = 0
            return

        self.wrong_block_number_count = 0
        hash2 = hashlib.md5(data).hexdigest()
        if hash == hash2:
            self.file_handle_tmp.seek(start)
            self.file_handle_tmp.write(data)
            self.next_start = start + len(data)
            msg = ProtocolMsg_filedata_ok(self.buddy, (self.id, start))
            msg.send()
            self.gui(self.file_size, start + len(data))

        else:
            print "(3) receiver wrong hash %i len: %i" % (start, len(data))
            msg = ProtocolMsg_filedata_error(self, (self.id, start))
            msg.send()
            #we try to avoid unnecessary wrong-block-number errors
            #the next block sure will be out of order, but we have sent
            #an error already because of the wrong hash
            self.wrong_block_number_count = 1

    def setFileNameSave(self, file_name_save):
        self.file_name_save = file_name_save
        try:
            self.file_handle_save = open(file_name_save, "w")
            print "(2) created and opened placeholder file %s" % self.file_name_save
        except:
            self.file_handle_save = None
            self.file_name_save = None
            self.file_save_error = str(sys.exc_info()[1])
            print "(2) %s could not be created: %s" % (self.file_name_save, self.file_save_error)

    def sendStopMessage(self):
        msg = ProtocolMsg_file_stop_receiving(self.buddy, self.id)
        msg.send()

    def closeForced(self):
        try:
            self.gui(self.file_size, -1, "transfer aborted")
        except:
            pass
        self.sendStopMessage()
        if self.file_name_save:
            self.file_handle_save.close()
            print "(2) unlinking empty placeholder file %s" % self.file_name_save
            os.unlink(self.file_name_save) #its still empty, no wiping needed
            self.file_name_save = ""
        self.close()

    def close(self):
        # this is called from the GUI (or its replacement)
        # therefore this FileReceiver object cannot work without
        # a GUI attached to it (or some other piece of code) that
        # properly provides and reacts to the callback function
        # and closes this obect after it is done
        # (user clicked save or whatever this GUI or GUI-replacement does)

        if self.closed:
            return
        try:
            self.closed = True
            self.file_handle_tmp.close()
            if self.file_name_save:
                self.file_handle_save.close()
                shutil.copy(self.file_name_tmp, self.file_name_save)
                print "(2) copied file to %s" % self.file_name_save

            print "(2) wiping received temporary file data"
            wipeFile(self.file_name_tmp)
            del self.buddy.bl.file_receiver[self.buddy.address, self.id]
        except:
            tb() #TODO: what could go wrong here? Why did I use try/except?


#--- ### Protocol messages

def ProtocolMsgFromLine(bl, conn, line):
    """this is the factory for producing instances of ProtocolMsg classes
    for incoming messages. The receiver will call this for every line it
    receives and then call the message's execute() method."""
    
    # each protocol message as it is transmitted and received from the socket 
    # is in the following form (which I call the "line")
    # <command>0x20<encoded>
    # we split it at the first space character (0x20)
    command, encoded = splitLine(line)
    
    # encoded is a string of encoded binary data.
    # The constructor will decode and parse it and we can return 
    # a readily initialized message object. 
    try:
        Msg = globals()["ProtocolMsg_%s" % command]
    except:
        Msg = ProtocolMsg
    
    return Msg(bl, conn, command, encoded)


class ProtocolMsg(object):
    """the base class for all ProtocolMsg_* classes. All message classes
    must inherit from this

    Besides being the base class for all ProtocolMsg_* classes
    this class is also instantiated for every unknown incoming message.
    in this case execute() will simply reply with not_implemented"""
    
    def __init__(self, *args):
        """ this is actually a few overloaded constructors, 
        depending on the types of argumments
        
        when receiving a message we instantiate it like this:
        __init__(self, bl, connection, command, encoded)
        
        when preparing a message for sending we do it like this:
        __init__(self, connection, blob)
        __init__(self, buddy, blob)

        blob is a string of raw binary 8 bit data, the contents 
        of chat messages, names, texts must be UTF8 encoded"""
        
        self.bl = None
        self.buddy = None
        self.connection = None
        
        #
        # incoming
        #
        #__init__(self, bl, connection, command, encoded)
        if type(args[0]) == BuddyList:
            self.bl = args[0]
            self.connection = args[1]
            if self.connection:
                self.buddy = self.connection.buddy
            self.command = args[2]
            
            # decode from line format to raw binary
            # and then let the message parse it 
            self.blob = decodeLF(args[3])
            self.parse()
            
            # the incoming message is now properly initialized and somebody
            # could now call its execute() method to trigger its action
            return
            
        
        #
        # outgoing
        #
        #__init__(self, connection, blob)
        #__init__(self, buddy, blob)
        if type(args[0]) in [InConnection, OutConnection, Buddy]:
            if type(args[0]) in [InConnection, OutConnection]:
                self.connection = args[0]
                if self.connection.buddy:
                    self.buddy = self.connection.buddy
                
            elif type(args[0]) == Buddy:
                self.buddy = args[0]
                self.connection = self.buddy.conn_out
                
            if len(args) > 1:
                blob = args[1]
                if type(blob) in [list, tuple]:
                    self.blob = " ".join(str(part) for part in blob)
                else:
                    self.blob = str(blob)
            else:
                self.blob = ""
            
            self.command = type(self).__name__[12:]


    def parse(self):
        pass

    def execute(self):
        # a generic message of this class will be automatically instantiated
        # if an incoming message with an unknown command is received
        # do nothing and just reply with "not_implemented"
        if self.buddy:
            print "(2) received unimplemented msg (%s) from %s" % (self.command, self.buddy.address)
            message = ProtocolMsg_not_implemented(self.buddy)
            message.send()
        else:
            print "(2) received unknown command on unknown connection. closing."
            print "(2) unknown connection had '%s' in last ping. closing" % self.connection.last_ping_address
            self.connection.close()

    def getLine(self):
        """return the entire message readily encoded as a string of charactrs 
        that we can transmit over the socket, terminated by a 0x0a character"""
        # This is important: 
        # The data that is transmitted over the socket (the entire contents 
        # of one protocol message will be put into one string of bytes that
        # is terminated by exactly one newline character 0x0a at the end.
        # 
        # This string of bytes is what I refer to as the "line"
        #
        # Therefore the entire message data (the contents of ProtocolMsg.blob)
        # which can contain any arbitrary byte sequence (even chat messages are 
        # considered a blob since they are UTF-8 text with arbitrary formatting 
        # chars) will be properly encoded for transmission in such a way that 
        # it will not contain any 0x0a bytes anymore.
        #
        # This is implemented in the functions encodeLF() and decodeLF()
        #
        # getLine() is called right before transmitting it over the socket
        # to produce the "line" and the exact inverse operation on the 
        # receiving side will happen in __init__() when a new message object 
        # is constructed from the incoming encoded line string. 
        return "%s %s\n" % (self.command, encodeLF(self.blob))

    def send(self):
        """send the outgoing message"""
        if self.connection:
            self.connection.send(self.getLine())
        else:
            print "(0) message without connection could not be sent"
            

class ProtocolMsg_not_implemented(ProtocolMsg):
    """This message is sent whenever we cannot understand the command. When
    receiving this we currently do nothing, except logging it to the debug log"""
    def parse(self):
        self.offending_command = self.blob
    
    def execute(self):
        if self.buddy:
            print "(2) %s says it can't handle '%s'" % (self.buddy.address, self.offending_command)


class ProtocolMsg_ping(ProtocolMsg):
    """a ping message consists of sender address and a random string (cookie). 
    It must be answered with a pong message containing the same cookie to so that 
    the other side can undoubtedly identify the connection"""
    def parse(self):
        self.address, self.answer = splitLine(self.blob)

    def isValidAddress(self):
        if len(self.address) <> 16:
            return False
        for c in self.address:
            if not c in "234567abcdefghijklmnopqrstuvwxyz":  # base32
                return False
        return True

    def execute(self):
        print "(2) <<< PING %s" % self.address

        #is sender a valid onion address?
        if not self.isValidAddress():
            print "(1) ping sender '%s' not a valid onion ID. closing connection." % self.address
            self.connection.close()
            return

        #first a little security check to detect mass pings
        #with faked host names over the same connection
        if self.connection.last_ping_address != "":
            #this is not the first ping over this connection
            #lets see if it has the correct address:
            if self.address != self.connection.last_ping_address:
                print "(1) Possible Attack: in-connection sent fake address %s" % self.address
                print "(1) Will disconnect incoming connection from fake %s" % self.address
                self.connection.close()
                return
        else:
            self.connection.last_ping_address = self.address
            self.connection.last_ping_cookie = self.answer

        #another check for faked pings: we search all our already
        #*connected* buddies and if there is one with the same address
        #but another incoming connection then this one must be a fake.
        #
        # FIXME: Might this check disrupt legitimate conditions?
        #
        found = False
        for buddy in self.bl.list + self.bl.incoming_buddies:
            if buddy.conn_in:
                if buddy.address == self.address:
                    if buddy.conn_in != self.connection:
                        found = True
                        break
        if found:
            print "(1) detected ping from %s on other connection." % self.address
            print "(1) last cookie %s" %buddy.conn_in.last_ping_cookie
            print "(1) this cookie %s" %self.answer
            print "(1) simply ignoring it"

            ## #warn the victim
            ## print "(1) ***** sending double connection warning to %s" %self.address
            ## buddy.sendAddMe() #might only be on his temporary list
            ## #buddy.sendChatMessage("Received more than one connection with your ID!")

            self.connection.send("not_implemented double connection\n")
            return

        #if someone is pinging us with our own address and the
        #random value is not from us, then someone is definitely
        #trying to fake and we can close.
        if self.address == config.get("client", "own_hostname"):
            own_buddy = self.bl.getBuddyFromAddress(self.address)
            if own_buddy.random1 != self.answer:
                print "(1) faked ping with our own address. closing"
                self.connection.close() #close this possibly faked incoming connection
                return

        #ping messages must be answered with pong messages
        #the pong must contain the same random string as the ping.
        #note that we will NOT yet assign buddy.conn_in
        #this can only be done in reaction to a pong message
        self.buddy = self.bl.getBuddyFromAddress(self.address)
        if not self.buddy:
            print "(2) %s is not on the buddy-list" % self.address
            #we have received a ping, but there is no buddy with
            #that address in our buddy-list. The only reason for that
            #can be that someone new has added our address to his list
            #and his client now has connected us. First we search if
            #we already have a connection to this buddy, if not we
            #create one.
            self.buddy = self.bl.getIncomingBuddyFromAddress(self.address)
            if not self.buddy:
                #create it and put it in the temporary list
                print "(2) %s is new. creating a temporary buddy instance" % self.address
                self.buddy = Buddy(self.address, self.bl, temporary=True)
                self.bl.incoming_buddies.append(self.buddy)
            else:
                print "(2) %s is already in the incoming list" % self.address
                print "(2) %s status is %i" % (self.address, self.buddy.status)


        #this ping is valid. We can now reset in-connection timeout
        #and also reset the out-connection fail counter because now we
        #assume the buddy is alive and we have a motivation to try harder again.
        self.connection.last_active = time.time()
        self.buddy.resetConnectionFailCounter()

        #connect if needed
        if not self.buddy.conn_out:
            self.buddy.connect()
        else:
            if not self.buddy.conn_in:
                #the buddie's last pong might have been lost when his first conn-out failed
                #so we send another ping, just to be on the safe side.
                self.buddy.count_unanswered_pings = 0
                self.buddy.sendPing()

        if self.buddy.isAlreadyPonged():
            #but we don't need to send more than one pong on the same conn_out
            #only if this is also a new conn_out because the last one failed
            print "(2) not sending another pong over same connection"
            return

        #now we can finally put our answer into the send queue
        print "(2) PONG >>> %s" % self.address
        answer = ProtocolMsg_pong(self.buddy, self.answer)
        answer.send()
        self.buddy.conn_out.pong_sent = True

        self.buddy.sendVersion()
        self.buddy.sendProfile()
        self.buddy.sendAvatar()
        if self.buddy in self.bl.list:
            self.buddy.sendAddMe()

        #send status as the last message because the other
        #client will update the GUI only after status messages
        self.buddy.sendStatus()




class ProtocolMsg_pong(ProtocolMsg):
    """incoming pong messages are used to identify and authenticate
    incoming connections. Basically we send out pings and see which
    corresponding pongs come in on which connections.
    we search all our known buddies for the corresponding random
    cookie to identify which buddy is replying here."""
    def parse(self):
        self.cookie = self.blob
        
    def execute(self):
        #first we search the buddy-list
        buddy = self.bl.getBuddyFromRandom(self.cookie)
        if not buddy:
            #we also try to find it in the temporary buddies list
            buddy = self.bl.getIncomingBuddyFromRandom(self.cookie)

        if buddy:
            if self.connection.last_ping_address == buddy.address:
                self.buddy = buddy
            else:
                #MITM protection:
                #this pong is an answer to a ping we have sent to a different address.
                #we will simply ignore this pong to make any mitm attacks that
                #simply try to forward original pings to other clients impossilbe
                print "(2) ignoring pong from %s which should have come from %s" % (self.connection.last_ping_address, buddy.address)
                return
                
        #if the pong is found to belong to a known buddy we can now
        #safely assign this incoming connection to this buddy and
        #regard the handshake as completed.
        if self.buddy:
            print "(2) <<< PONG %s" % self.buddy.address
            #assign the in-connection to this buddy
            self.buddy.onInConnectionFound(self.connection)
        else:
            #there is no buddy for this pong. nothing to do.
            print "(1) ignoring pong with unknown cookie. Sender: %s" % self.connection.last_ping_address
            print "(1) !!! There might be another client sending pings with OUR address!"
            print "(1) !!! Are we trying to run a second instance with the same ID?"


class ProtocolMsg_client(ProtocolMsg):
    """transmits the name of the client software. Usually sent after the pong"""
    def parse(self):
        self.client = self.blob

    def execute(self):
        if self.buddy:
            print "(2) %s is using %s" % (self.buddy.address, self.client)
            self.buddy.client = self.client


class ProtocolMsg_version(ProtocolMsg):
    """transmits the version number of the client software. Usually sent after the 'client' message"""
    def parse(self):
        self.version = self.blob

    def execute(self):
        if self.buddy:
            print "(2) %s has version %s" % (self.buddy.address, self.version)
            self.buddy.version = self.version


class ProtocolMsg_status(ProtocolMsg):
    """transmit the status, this MUST be sent every 120 seconds 
    or the client may trigger a timeout and close the conection.
    When receiving this message the client will update the status
    icon of the buddy, it will be transmitted after the pong upon
    connection, immediately on every status change or at least 
    once every 120 seconds. Allowed values for the data are
    "avalable", "away", "xa", other values are not defined yet"""
    def parse(self):
        self.status = self.blob
        
    def execute(self):
        #set the status flag of the corresponding buddy
        if self.buddy:
            print "(3) received status %s from %s" % (self.status, self.buddy.address)

            #send offline messages if buddy was previously offline
            if self.buddy.status == STATUS_HANDSHAKE:
                print "(2) %s came online, sending delayed messages" % self.buddy.address
                self.buddy.sendOfflineMessages()

            #set buddy status
            if self.status == "available":
                self.buddy.onStatus(STATUS_ONLINE)
            if self.status == "away":
                self.buddy.onStatus(STATUS_AWAY)
            if self.status == "xa":
                self.buddy.onStatus(STATUS_XA)

            #avoid timeout of in-connection
            self.connection.last_active = time.time()


class ProtocolMsg_profile_name(ProtocolMsg):
    """transmit the name that is set in the pofile (this message is optional)"""
    def parse(self):
        self.name = self.blob.decode("UTF-8")
        
    def execute(self):
        if self.buddy:
            print "(2) received name from %s" % self.buddy.address
            self.buddy.onProfileName(self.name)


class ProtocolMsg_profile_text(ProtocolMsg):
    """transmit the text that is set in the pofile (this message is optional)"""
    def parse(self):
        self.text = self.blob.decode("UTF-8")
        
    def execute(self):
        if self.buddy:
            print "(2) received profile text from %s" % self.buddy.address
            self.buddy.onProfileText(self.text)


class ProtocolMsg_profile_avatar_alpha(ProtocolMsg):
    """This message has to be sent BEFORE profile_avatar because
    only the latter one will trigger the GUI notification.
    It contains the uncompressed 64*64*8bit alpha channel.
    this message must be sent with empty data (0 bytes) if there
    is no alpha, it may not be omitted if you have an avatar.
    It CAN be omitted only if you also omit profile_avatar"""
    def parse(self):
        if len(self.blob) == 4096 or len(self.blob) == 0:
            self.bitmap = self.blob
        else:
            self.bitmap = None
            
    def execute(self):
        if self.buddy:
            print "(2) received avatar alpha channel from %s (%i bytes)" % (self.buddy.address, len(self.bitmap))
            if self.bitmap:
                # the buddy obect stores the raw binary data
                self.buddy.onAvatarDataAlpha(self.bitmap)
            else:
                print("(1) %s sent invalid avatar alpha data (wrong size)" % self.buddy.address)
                self.buddy.onAvatarDataAlpha("")

class ProtocolMsg_profile_avatar(ProtocolMsg):
    """the uncompesseed 64*64*24bit image. Avatar messages can 
    be completely omitted but IF they are sent then the correct 
    order is first the alpha and then this one"""
    def parse(self):
        if len(self.blob) == 12288 or len(self.blob) == 0:
            self.bitmap = self.blob
        else:
            self.bitmap = None
    
    def execute(self):
        if self.buddy:
            print "(2) received avatar from %s (%i bytes)" % (self.buddy.address, len(self.bitmap))
            if self.bitmap:
                # the buddy obect stores the raw binary data
                self.buddy.onAvatarData(self.bitmap)
            else:
                print("(1) %s sent invalid avatar image data (wrong size)" % self.buddy.address)
                self.buddy.onAvatarData("")


class ProtocolMsg_add_me(ProtocolMsg):
    """This must be sent after connection if you are (or want to be) 
    on the other's buddy list. Since a client can also connect for 
    the purpose of joining a chat room without automatically appearing 
    on the buddy list this message is needed."""
    def execute(self):
        if self.buddy:
            print "(2) add me from %s" % self.buddy.address
            if not self.buddy in self.bl.list:
                print "(2) received add_me from new buddy %s" % self.buddy.address
                self.buddy.name = self.buddy.profile_name
                self.bl.addBuddy(self.buddy)
                msg = "<- has added you"
                self.buddy.onChatMessage(msg)


class ProtocolMsg_remove_me(ProtocolMsg):
    """when receiving this message the buddy MUST be removed from
    the buddy list (or somehow marked as removed) so that it will not
    automatically add itself again and cause annoyance. When removing
    a buddy first send this message before disconnecting or the other
    client will never know about it and add itself again next time"""
    def execute(self):
        if self.buddy:
            print "(2) received remove_me from buddy %s" % self.buddy.address
            if self.buddy in self.bl.list:
                print "(2) removing %s from list" % self.buddy.address
                self.bl.removeBuddy(self.buddy)
        else:
            print "(2) received 'remove_me' on unknown connection"
            print "(2) unknown connection had '%s' in last ping. closing" % self.connection.last_ping_address
            self.connection.close()


class ProtocolMsg_message(ProtocolMsg):
    """this is a normal text message. Text is encoded UTF-8"""
    def parse(self):
        self.text = self.blob.decode("UTF-8")

    def execute(self):
        #give buddy and text to bl. bl will then call into the gui
        #to open a chat window and/or display the text.
        if self.buddy:
            if self.buddy in self.bl.list:
                self.buddy.onChatMessage(self.text)
            else:
                print "(1) ***** protocol violation reply to %s" % self.buddy.address
                msg = "This is an automatic reply."
                msg += "Your version might be out of date or some"
                msg += "other reason caused this unexpected protocol violation."
                msg += "Make sure you have the latest version of TorChat"
                msg += "and everything is configured correctly."
                self.buddy.sendChatMessage(msg)
                time.sleep(5)
                self.buddy.disconnect()

        else:
            print "(2) received 'message' on unknown connection"
            print "(2) unknown connection had '%s' in last ping. closing" % self.connection.last_ping_address
            self.connection.close()


class ProtocolMsg_filename(ProtocolMsg):
    """The first message in a file transfer, initiating the transfer.
    Note that File transfer messages are the only messages that are allowed
    to be sent out on the incoming connection to avoid delaying of chat messages"""
    def parse(self):
        self.id, text = splitLine(self.blob) # each transfer has a unique ID, made up by the sender
        file_size, text = splitLine(text) # bytes
        block_size, self.file_name = splitLine(text) 
        self.file_size = int(file_size)
        self.block_size = int(block_size)

        # the filename is tansmitted in UTF-8 encoding
        # so we decode the name to unicode (widestring)
        name = self.file_name.decode("utf-8")
        # remove all occurences 0x0000
        name = u"".join([c for c in name if c <> "\u0000"])
        # remove all path manipulations in front of the name
        name = os.path.basename(name)
        # the filename may not start with '.'
        # or be completely empty
        root, ext = os.path.splitext(name)
        if root == u"":
            root = u"unnamed"

        self.file_name = root + ext


    def execute(self):
        if not self.buddy:
            print "(2) received 'filename' on unknown connection"
            print "(2) unknown connection had '%s' in last ping. closing" % self.connection.last_ping_address
            self.connection.close()
            return

        #we create a file receiver instance which can deal with the
        #file data we expect to receive now. This obect will then
        #also notify the GUI and interact with it
        FileReceiver(self.buddy,
                     self.id,
                     self.block_size,
                     self.file_size,
                     self.file_name)


class ProtocolMsg_filedata(ProtocolMsg):
    """After a filename message has initiated the transfer several
    filedata messagess transport the actual data in blocks of fixed
    size. start is the byte offset of the block in the file and hash
    is an md5 hash of the block used as a checksum. Each message must
    be answered with filedata_ok after sucessfully verifying the hash.
    The sender should send only a limited number of blocks ahead of
    incoming ok messages (for example send the 5th block only after
    the 1st is confirmed, the 6th only after the 2nd confirmed, etc.,
    this number is only a wild guess and might need some tuning)"""
    def parse(self):
        self.id, text = splitLine(self.blob)
        start, text = splitLine(text) # block start position in bytes
        self.hash, self.data = splitLine(text) # md5
        self.start = int(start)

    def execute(self):
        if not self.buddy:
            print "(2) received 'filedata' on unknown connection"
            print "(2) unknown connection had '%s' in last ping. closing" % self.connection.last_ping_address
            self.connection.close()
            return

        #there should already be a receiver, because there should have been
        #a "filename"-message at the very beginning of the transfer.
        receiver = self.bl.getFileReceiver(self.buddy.address, self.id)
        if receiver:
            receiver.data(self.start, self.hash, self.data)
        else:
            #if there is no receiver for this data, we just reply
            #with a stop message and hope the sender gets it and
            #stops sending data. Not much else to do for us here.
            msg = ProtocolMsg_file_stop_sending(self.buddy, self.id)
            msg.send()


class ProtocolMsg_filedata_ok(ProtocolMsg):
    """Every received "filedata" must be confirmed with a "filedata_ok"
    (or a "filedata_error") message. A File sender will use these messages
    to update the sending progress bar and to know that it can send more
    blocks"""
    def parse(self):
        self.id, start = splitLine(self.blob)
        self.start = int(start) # block start position in bytes

    def execute(self):
        if self.buddy:
            sender = self.bl.getFileSender(self.buddy.address, self.id)
            if sender:
                sender.receivedOK(self.start)
            else:
                #there is no sender (anymore) to handle confirmation messages
                #so we can send a stop message to tell the other side
                #to stop receiving
                msg = ProtocolMsg_file_stop_receiving(self.buddy, self.id)
                msg.send()
        else:
            print "(2) received 'filedata_ok' on unknown connection"
            print "(2) unknown connection had '%s' in last ping. closing" % self.connection.last_ping_address
            self.connection.close()


class ProtocolMsg_filedata_error(ProtocolMsg):
    """This is sent instead of filedata_ok when the hash was wrong or the block start
    was later than what we would have expected (entire blocks have been skipped/lost 
    due to temporary disconnect). A file sender must react to this message by 
    restarting the file transmission at the offset given in start. A file receiver will
    send this message whenever it wants the the transfer restart at a certain position."""
    def parse(self):
        self.id, start = splitLine(self.blob)
        self.start = int(start) # block start position in bytes

    def execute(self):
        if self.buddy:
            sender = self.bl.getFileSender(self.buddy.address, self.id)
            if sender:
                sender.restart(self.start)
            else:
                msg = ProtocolMsg_file_stop_receiving(self.buddy, self.id)
                msg.send()
        else:
            print "(2) received 'filedata_error' on unknown connection"
            print "(2) unknown connection had '%s' in last ping. closing" % self.connection.last_ping_address
            self.connection.close()


class ProtocolMsg_file_stop_sending(ProtocolMsg):
    """A file receiver sends this to make the file sender stop sending,
    a file sender must react to this message by stopping the file sending,
    the GUI should notify the user that the receiver has canceled. This
    message usually occurs when a file receiver clicks the cancel button"""
    def parse(self):
        self.id = self.blob

    def execute(self):
        if self.buddy:
            sender = self.bl.getFileSender(self.buddy.address, self.id)
            if sender:
                #close the sender (if not already closed)
                #otherwise just ignore it
                sender.close()
        else:
            print "(2) received 'file_stop_sending' on unknown connection"
            print "(2) unknown connection had '%s' in last ping. closing" % self.connection.last_ping_address
            self.connection.close()


class ProtocolMsg_file_stop_receiving(ProtocolMsg):
    """A file sender sends this message to tell the file receiver that
    the transfer has been canceled. A file receiver when receiving this
    message must stop waiting for further blocks, no further messages 
    regarding the same transfer can be expected after this, all allocated
    resources regarding this transfer can be freed, incomplete temp files 
    should be wiped and the user notified about the cancel."""
    def parse(self):
        self.id = self.blob

    def execute(self):
        if self.buddy:
            receiver = self.bl.getFileReceiver(self.buddy.address, self.id)
            if receiver:
                #close the receiver (if not already closed)
                #otherwise just ignore it
                receiver.closeForced()
        else:
            print "(2) received 'file_stop_receiving' on unknown connection"
            print "(2) unknown connection had '%s' in last ping. closing" % self.connection.last_ping_address
            self.connection.close()


#--- ### Low level network stuff

class Receiver(threading.Thread):
    def __init__(self, conn, is_incoming=False):
        threading.Thread.__init__(self)
        self.conn = conn
        self.is_incoming = is_incoming
        self.socket = conn.socket
        self.running = True
        self.start()

    def run(self):
        readbuffer = ""
        #self.socket.settimeout(5)
        while self.running:
            try:
                recv = self.socket.recv(4096)
                if recv != "":
                    readbuffer = readbuffer + recv
                    temp = readbuffer.split("\n")
                    readbuffer = temp.pop()

                    for line in temp:
                        if self.running:
                            try:
                                # on outgoing connections we do not allow any
                                # incoming messages other than file*
                                # this prevents an attacker from messaging
                                # or sending commands before the handshake is
                                # completed or pong on the wrong connection
                                if (self.is_incoming or line[:4] == "file"):
                                    message = ProtocolMsgFromLine(self.conn.bl,
                                                              self.conn,
                                                              line)
                                    message.execute()
                                else:
                                    # this is an outgoing connection. Incoming protocol messages are ignored
                                    print "(1) received unexpected '%s' on outgoing connection to %s" % (line, self.conn.buddy.address)
                            except:
                                tb()
                else:
                    self.running = False
                    self.conn.onReceiverError()

            except socket.timeout:
                self.running = False
                self.conn.onReceiverError()

            except socket.error:
                self.running = False
                self.conn.onReceiverError()


class InConnection(object):
    def __init__(self, socket, buddy_list):
        self.buddy = None
        self.bl = buddy_list
        self.socket = socket
        self.last_ping_address = "" #used to detect mass pings with fake adresses
        self.last_ping_cookie = "" #used to detect pings with fake cookies
        self.last_active = time.time()
        self.started = True
        self.receiver = Receiver(self, True)

    def send(self, text):
        try:
            self.socket.send(text)
        except:
            tb()
            print "(2) in-connection send error."
            self.bl.onErrorIn(self)
            self.close()

    def onReceiverError(self):
        if self.buddy:
            addr = self.buddy.address
        else:
            addr = self.last_ping_address + " (unverified)"
        print "(2) in-connection receive error: %s" % addr
        self.bl.onErrorIn(self)
        self.close()

    def close(self):
        print "(2) in-connection closing %s" % self.last_ping_address

        try:
            self.socket.shutdown(socket.SHUT_RDWR)
        except:
            print "(3) socket.shutdown() %s" % sys.exc_info()[1]
        try:
            self.socket.close()
        except:
            print "(3) socket.close() %s" % sys.exc_info()[1]

        self.started = False
        if self in self.bl.listener.conns:
            self.bl.listener.conns.remove(self)
        if self.buddy:
            self.buddy.conn_in = None


class OutConnection(threading.Thread):
    def __init__(self, address, buddy_list, buddy):
        threading.Thread.__init__(self)
        self.bl = buddy_list
        self.buddy = buddy
        self.address = address
        self.pong_sent = False
        self.send_buffer = []
        self.start()

    def run(self):
        self.running = True
        try:
            self.socket = socks.socksocket()
            self.socket.setproxy(socks.PROXY_TYPE_SOCKS4,
                                 config.get(TOR_CONFIG, "tor_server"),
                                 config.getint(TOR_CONFIG, "tor_server_socks_port"))
            print "(2) trying to connect '%s'" % self.address
            self.socket.connect((str(self.address), TORCHAT_PORT))
            print "(2) connected to %s" % self.address
            self.bl.onConnected(self)
            self.receiver = Receiver(self, False) # this Receiver will only accept file* messages
            while self.running:
                while len(self.send_buffer) > 0:
                    text = self.send_buffer.pop(0)
                    try:
                        print "(2) %s out-connection sending buffer" % self.address
                        self.socket.send(text)
                    except:
                        print "(2) out-connection send error"
                        self.bl.onErrorOut(self)
                        self.close()

                time.sleep(0.2)

        except:
            print "(2) out-connection to %s failed: %s" % (self.address, sys.exc_info()[1])
            self.bl.onErrorOut(self)
            self.close()

    def send(self, text):
        self.send_buffer.append(text)

    def onReceiverError(self):
        print "(2) out-connection receiver error"
        self.bl.onErrorOut(self)
        self.close()

    def close(self):
        self.running = False
        self.send_buffer = []
        try:
            self.socket.shutdown(socket.SHUT_RDWR)
        except:
            print "(3) socket.shutdown() %s" % sys.exc_info()[1]
        try:
            self.socket.close()
        except:
            print "(3) socket.close() %s" % sys.exc_info()[1]

        if self.buddy:
            self.buddy.conn_out = None
            print "(2) out-connection closed (%s)" % self.buddy.address
        else:
            print "(2) out connection without buddy closed" # happens after removeBudddy()


class Listener(threading.Thread):
    def __init__(self, buddy_list, socket=None):
        threading.Thread.__init__(self)
        self.buddy_list = buddy_list
        self.conns = []
        self.socket = socket
        self.start()
        self.startTimer()

    def run(self):
        self.running = True
        if not self.socket:
            interface = config.get("client", "listen_interface")
            port = config.getint("client", "listen_port")
            self.socket = tryBindPort(interface, port)
        self.socket.listen(5)
        while self.running:
            try:
                conn, address = self.socket.accept()
                self.conns.append(InConnection(conn, self.buddy_list))
                print "(2) new incoming connection"
                print "(2) have now %i incoming connections" % len(self.conns)
            except:
                print "socket listener error!"
                tb()
                self.running = False


    def close(self):
        self.running = False
        try:
            print "(2) closing listening socket %s:%s" \
              % (config.get("client", "listen_interface"),
                 config.get("client", "listen_port"))
            self.socket.close()
            print "(2) success"
        except:
            print "(2) closing socket failed, traceback follows:"
            tb()

    def startTimer(self):
        self.timer = threading.Timer(30, self.onTimer)
        self.timer.start()

    def onTimer(self):
        for conn in self.conns:
            if time.time() - conn.last_active > config.DEAD_CONNECTION_TIMEOUT:
                if conn.buddy:
                    print "(2) conn_in timeout: disconnecting %s" % conn.buddy.address
                    conn.buddy.disconnect()
                else:
                    print "(2) closing unused in-connection from %s" % conn.last_ping_address
                    conn.close()
                if conn in self.conns:
                    self.conns.remove(conn)
                print "(2) have now %i incoming connections" % len(self.conns)
        self.startTimer()


def tryBindPort(interface, port):
    try:
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        s.bind((interface, port))
        s.listen(5)
        return s
    except:
        tb()
        return False

def startPortableTor():
    print "(1) entering function startPortableTor()"
    global tor_in, tor_out
    global TOR_CONFIG
    global tor_pid
    global tor_proc
    old_dir = os.getcwd()
    print "(1) current working directory is %s" % os.getcwd()
    try:
        print "(1) changing working directory"
        os.chdir(config.getDataDir())
        os.chdir("Tor")
        print "(1) current working directory is %s" % os.getcwd()
        # completely remove all cache files from the previous run
        #for root, dirs, files in os.walk("tor_data", topdown=False):
        #    for name in files:
        #        os.remove(os.path.join(root, name))
        #    for name in dirs:
        #        os.rmdir(os.path.join(root, name))

        # now start tor with the supplied config file
        print "(1) trying to start Tor"

        if config.isWindows():
            if os.path.exists("tor.exe"):
                #start the process without opening a console window
                startupinfo = subprocess.STARTUPINFO()
                startupinfo.dwFlags |= 1 #STARTF_USESHOWWINDOW
                tor_proc = subprocess.Popen("tor.exe -f torrc.txt".split(), startupinfo=startupinfo)
                tor_pid = tor_proc.pid
            else:
                print "(1) there is no portable tor.exe"
                tor_pid = False
        else:
            if os.path.exists("tor.sh"):
                #let our shell script start a tor instance
                os.system("chmod +x tor.sh")
                tor_proc = subprocess.Popen("./tor.sh".split())
                tor_pid = tor_proc.pid
                print "(1) tor pid is %i" % tor_pid
            else:
                print "(1) there is no Tor starter script (tor.sh)"
                tor_pid = False

        if tor_pid:
            #tor = subprocess.Popen("tor.exe -f torrc.txt".split(), creationflags=0x08000000)
            print "(1) successfully started Tor (pid=%i)" % tor_pid

            # we now assume the existence of our hostname file
            # it WILL be created after the first start
            # if not, something must be totally wrong.
            cnt = 0
            found = False
            while cnt <= 20:
                try:
                    print "(1) trying to read hostname file (try %i of 20)" % (cnt + 1)
                    f = open(os.path.join("hidden_service", "hostname"), "r")
                    hostname = f.read().rstrip()[:-6]
                    print "(1) found hostname: %s" % hostname
                    print "(1) writing own_hostname to torchat.ini"
                    config.set("client", "own_hostname", hostname)
                    found = True
                    f.close()
                    break
                except:
                    # we wait 20 seconds for the file to appear
                    time.sleep(1)
                    cnt += 1

            if not found:
                print "(0) very strange: portable tor started but hostname could not be read"
                print "(0) will use section [tor] and not [tor_portable]"
            else:
                #in portable mode we run Tor on some non-standard ports:
                #so we switch to the other set of config-options
                print "(1) switching active config section from [tor] to [tor_portable]"
                TOR_CONFIG = "tor_portable"
                #start the timer that will periodically check that tor is still running
                startPortableTorTimer()
        else:
            print "(1) no own Tor instance. Settings in [tor] will be used"

    except:
        print "(1) an error occured while starting tor, see traceback:"
        tb(1)

    print "(1) changing working directory back to %s" % old_dir
    os.chdir(old_dir)
    print "(1) current working directory is %s" % os.getcwd()

def stopPortableTor():
    if not tor_pid:
        return
    else:
        print "(1) tor has pid %i, terminating." % tor_pid
        config.killProcess(tor_pid)

def startPortableTorTimer():
    global tor_timer
    tor_timer = threading.Timer(10, onPortableTorTimer)
    tor_timer.start()

def onPortableTorTimer():
    if tor_proc.poll() != None:
        print "(1) Tor stopped running. Will restart it now"
        startPortableTor()
    else:
        startPortableTorTimer()
