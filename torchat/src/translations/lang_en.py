# -*- coding: UTF-8 -*-

##############################################################################
#                                                                            #
# Copyright (c) 2007-2010 Bernd Kreuss <prof7bit@gmail.com>                  #
#                                                                            #
# Translation file for TorChat                                               #
#                                                                            #
##############################################################################

LANGUAGE_CODE = u"en"
LANGUAGE_NAME = u"English"
LANGUAGE_NAME_ENGLISH = u"English"
TRANSLATOR_NAMES = []

#buttons
BTN_CANCEL = u"Cancel"
BTN_OK = u"Ok"
BTN_SAVE_AS = u"Save as..."
BTN_CLOSE = u"Close"

#status
ST_AVAILABLE = u"Avaliable"
ST_AWAY = u"Away"
ST_EXTENDED_AWAY = u"Extended away"
ST_OFFLINE = u"Offline"

#TaskbarMenu
MTB_SHOW_HIDE_TORCHAT = u"Show/Hide TorChat"
MTB_QUIT = u"Quit"

#popup menu
MPOP_CHAT = u"Chat..."
MPOP_SEND_FILE = u"Send file..."
MPOP_EDIT_CONTACT = u"Edit contact..."
MPOP_DELETE_CONTACT = u"Delete contact..."
MPOP_SHOW_OFFLINE_MESSAGES = u"Show queued offline messages"
MPOP_CLEAR_OFFLINE_MESSAGES = u"Clear queued offline messages"
MPOP_ADD_CONTACT = u"Add contact..."
MPOP_ABOUT = u"About TorChat"
MPOP_ASK_AUTHOR = u"Ask %s..."
MPOP_SETTINGS = u"Settings..."
MPOP_EDIT_MY_PROFILE = u"Edit my profile..."

#chat window popup menu
CPOP_COPY = u"Copy"

#confirm delete message box
D_CONFIRM_DELETE_TITLE = u"Confirm deletion"
D_CONFIRM_DELETE_MESSAGE = u"Really delete this contact?\n(%s %s)"

#warning about log
D_LOG_WARNING_TITLE = u"TorChat: Logging is active"
D_LOG_WARNING_MESSAGE = u"Logging to file is activated!\n\nLog File: %s\n\nRemember to delete the log file if you have finished debugging because the log file may contain sensitive information."

#warning about used port
D_WARN_USED_PORT_TITLE = u"TorChat: Port already in use"
D_WARN_USED_PORT_MESSAGE = u"Something, probably another TorChat instance, is already listening at %s:%s. You must create another profile using different ports to be able to start TorChat a second time."

#warnig about unread messages
D_WARN_UNREAD_TITLE = u"TorChat: Unread messages"
D_WARN_UNREAD_MESSAGE = u"There are unread messages.\nThey will be lost forever!\n\nDo you really want to exit TorChat now?"

#warning about offline buddy
D_WARN_BUDDY_OFFLINE_TITLE = u"TorChat: Buddy is offline"
D_WARN_BUDDY_OFFLINE_MESSAGE = u"This operation is not possible with offline buddies"

#warning about multiple files
D_WARN_FILE_ONLY_ONE_TITLE = u"TorChat: Multiple files"
D_WARN_FILE_ONLY_ONE_MESSAGE = u"You may not start multiple file transfers with one operation. Start the transfers individually or send a zip-file instead"

#warning about file save error
D_WARN_FILE_SAVE_ERROR_TITLE = u"TorChat: Error saving file"
D_WARN_FILE_SAVE_ERROR_MESSAGE = u"The file '%s' could not be created.\n\n%s"

#warning about file already exists
D_WARN_FILE_ALREADY_EXISTS_TITLE = u"TorChat: File exists"
D_WARN_FILE_ALREADY_EXISTS_MESSAGE = u"The file '%s' already exists.\nOverwrite it?"

#dialog: add/edit contact
DEC_TITLE_ADD = u"Add new contact"
DEC_TITLE_EDIT = u"Edit contact"
DEC_TORCHAT_ID = u"TorChat ID"
DEC_DISPLAY_NAME = u"Display name"
DEC_INTRODUCTION = u"Introduction"
DEC_MSG_16_CHARACTERS = u"The address must be 16 characters long, not %i."
DEC_MSG_ONLY_ALPANUM = u"The address must only contain numbers and lowercase letters"
DEC_MSG_ALREADY_ON_LIST = u"%s is already on your list"

#file transfer window
DFT_FILE_OPEN_TITLE = u"Send file to %s"
DFT_FILE_SAVE_TITLE = u"Save file from %s"
DFT_SEND = u"Sending %s\nto %s\n%04.1f%% (%i of %i bytes)"
DFT_RECEIVE = u"Receiving %s\nfrom %s\n%04.1f%% (%i of %i bytes)"

#settings dialaog
DSET_TITLE = u"TorChat configuration"
DSET_NET_TITLE = u"Network"
DSET_NET_ACTIVE = u"active"
DSET_NET_INACTIVE = u"inactive"
DSET_NET_TOR_ADDRESS = u"Tor proxy address"
DSET_NET_TOR_SOCKS = u"Socks port"
DSET_NET_TOR_CONTROL = u"Control port"
DSET_NET_OWN_HOSTNAME = u"Own TorChat-ID"
DSET_NET_LISTEN_INTERFACE = u"Listen interface"
DSET_NET_LISTEN_PORT = u"Listen port"
DSET_GUI_TITLE = u"User interface"
DSET_GUI_LANGUAGE = u"Language"
DSET_GUI_OPEN_MAIN_HIDDEN = u"Start with minimized main window"
DSET_GUI_OPEN_CHAT_HIDDEN = u"Don't automatically open new windows"
DSET_GUI_NOTIFICATION_POPUP = u"Notification pop-up"
DSET_GUI_FLASH_WINDOW = u"Flash window title on new message"
DSET_MISC_TITLE = u"Misc"
DSET_MISC_TEMP_IN_DATA = u"Store temporary files inside data directory"
DSET_MISC_TEMP_CUSTOM_DIR = u"Temporary directory (leave empty for OS-default)"

#notices in the chat window (those in square brackets)
NOTICE_DELAYED_MSG_WAITING = u"delayed messages waiting to be sent"
NOTICE_DELAYED_MSG_SENT = u"delayed messages have been sent"
NOTICE_DELAYED = u"delayed"

#messagebox for offline messages
MSG_OFFLINE_TITLE = u"TorChat: queued messages"
MSG_OFFLINE_EMPTY = u"there are no (more) queued messages for %s"
MSG_OFFLINE_QUEUED = u"queued offline messages for %s:\n\n%s"

#buddy list mouse hover popup
BPOP_BUDDY_IS_OFFLINE = u"Buddy is offline"
BPOP_CONNECTED_AWAITING_RETURN_CONN = u"Connected, awaiting return connection..."
BPOP_CLIENT_SOFTWARE = u"Client: %s %s"

#about box
ABOUT_TITLE = u"About TorChat"
ABOUT_TEXT = u"""TorChat %(version)s (svn: r%(svn)s)
  %(copyright)s

Translations: 
  %(translators)s

Runtime environment:
  Python: %(python)s
  wx: %(wx)s
    
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

If you happen to run a software company near Hannover, Germany and \
are in need of a new coder, feel free to regard this little program \
as my application documents and drop me a mail with your answer.
"""
