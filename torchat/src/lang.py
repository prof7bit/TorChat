# -*- coding: UTF-8 -*-

##############################################################################
#                                                                            #
# Copyright (c) 2007-2008 Bernd Kreuss <prof7bit@gmail.com>                  #
#                                                                            #
# Translation file for TorChat                                               #
#                                                                            #
# Language: English [en]                                                     #
#                                                                            #
##############################################################################

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
MPOP_ADD_CONTACT = u"Add contact"
MPOP_ABOUT = u"About TorChat"
MPOP_ASK_AUTHOR = u"Ask %s"

#confirm delete message box
D_CONFIRM_DELETE_TITLE = u"Confirm deletion"
D_CONFIRM_DELETE_MESSAGE = u"Really delete this contact?\n(%s %s)"

#warning about log
D_LOG_WARNING_TITLE = u"TorChat: Logging is active"
D_LOG_WARNING_MESSAGE = u"Logging to file is activated!\n\nLog File: %s\n\nRemember to delete the log file if you have finished debugging because the log file may contain sensitive information."

#warnig about unread messages
D_WARN_UNREAD_TITLE = u"TorChat: Unread messages"
D_WARN_UNREAD_MESSAGE = u"There are unread messages.\nThey will be lost forever!\n\nDo you really want to exit TorChat now?"

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
DFT_SEND = u"Sending %s\nto %s\n%04.1f%% (%i of %i bytes)"
DFT_RECEIVE = u"Receiving %s\nfrom %s\n%04.1f%% (%i of %i bytes)"

#notices in the chat window (those in square brackets)
NOTICE_DELAYED_MSG_WAITING = u"delayed messages waiting to be sent"
NOTICE_DELAYED_MSG_SENT = u"delayed messages have been sent"
NOTICE_DELAYED = u"delayed"

#about box
ABOUT_TITLE = u"About TorChat"
ABOUT_TEXT = u"""TorChat %(version)s
  %(copyright)s

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
