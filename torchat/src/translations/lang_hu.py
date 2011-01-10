# -*- coding: UTF-8 -*-

##############################################################################
#                                                                            #
# Copyright (c) 2007-2010 Bernd Kreuss <prof7bit@gmail.com>                  #
#                                                                            #
# Translation file for TorChat                                               #
#                                                                            #
##############################################################################

LANGUAGE_CODE = u"hu"
LANGUAGE_NAME = u"Magyar"
LANGUAGE_NAME_ENGLISH = u"Hungarian"
TRANSLATOR_NAMES = [u"d4n3sz"]

#buttons
BTN_CANCEL = u"Mégse"
BTN_OK = u"Ok"
BTN_SAVE_AS = u"Mentés másként..."
BTN_CLOSE = u"Bezár"

#status
ST_AVAILABLE = u"Elérhető"
ST_AWAY = u"Rögtön jövök"
ST_EXTENDED_AWAY = u"Nincs a gépnél"
ST_OFFLINE = u"Kapcsolat nélkül"

#TaskbarMenu
MTB_SHOW_HIDE_TORCHAT = u"Mutat/Rejt TorChat"
MTB_QUIT = u"Kilépés"

#popup menu
MPOP_CHAT = u"Chat..."
MPOP_SEND_FILE = u"Fájl küldés..."
MPOP_EDIT_CONTACT = u"Partner szerkesztése..."
MPOP_DELETE_CONTACT = u"Partner törlése..."
MPOP_SHOW_OFFLINE_MESSAGES = u"Offline ki küldött üzenetek mutatása"
MPOP_CLEAR_OFFLINE_MESSAGES = u"Offline kiküldött üzenetek törlése"
# MPOP_ACTIVATE_LOG = u"Activate logging to file"
# MPOP_STOP_LOG = u"Stop logging"
# MPOP_DELETE_EXISTING_LOG = u"Delete existing log file"
# MPOP_DELETE_AND_STOP_LOG = u"Delete log and stop logging"
MPOP_ADD_CONTACT = u"Partner hozzáadása..."
MPOP_ABOUT = u"A TorChat-ról"
MPOP_ASK_AUTHOR = u"Kérdezd %s..."
MPOP_SETTINGS = u"Beállítások..."
# MPOP_EDIT_MY_PROFILE = u"Edit my profile..."

#chat window popup menu
CPOP_COPY = u"Másolás"

#confirm delete message box
D_CONFIRM_DELETE_TITLE = u"Törlés megerősítése"
D_CONFIRM_DELETE_MESSAGE = u"Valóban törölni akarod?\n(%s %s)"

#warning about log
D_LOG_WARNING_TITLE = u"TorChat: Logolás aktív"
D_LOG_WARNING_MESSAGE = u"Logolás mentése fájlba bekapcsolva!\n\nLog Fájl: %s\n\nNe felejtsd el törölni a log fájlt, ha befejezted a hibakeresést,mert személyes információkat is tartalmazhat."

#warning about used port
D_WARN_USED_PORT_TITLE = u"TorChat: A port már használatban van"
D_WARN_USED_PORT_MESSAGE = u"Valószínűleg már egy másik TorChat fut itt:  %s:%s. Csinálj egy másik profilt más porttal, hogy el tudj indítani egy másik TorChat-et!"

#warnig about unread messages
D_WARN_UNREAD_TITLE = u"TorChat: Olvasatlan üzenet"
D_WARN_UNREAD_MESSAGE = u"Olvasatlan üzeneted van.\nEl fog veszni véglegesen!\n\nBiztosan ki akarsz lépni a TorChat-ból?"

#warning about offline buddy
D_WARN_BUDDY_OFFLINE_TITLE = u"TorChat: Partner nincs kapcsolódva"
D_WARN_BUDDY_OFFLINE_MESSAGE = u"Ezt a műveleted nem lehet végrehajtani kapcsolat nélküli partnerrel"

#warning about multiple files
D_WARN_FILE_ONLY_ONE_TITLE = u"TorChat: Sok fájl"
D_WARN_FILE_ONLY_ONE_MESSAGE = u"Nem tudsz egyszerre több fájlt küldeni. Küldd el a fájlokat egyenként, vagy használj tömörítést (pl. zip)"

#warning about file save error
D_WARN_FILE_SAVE_ERROR_TITLE = u"TorChat: Hiba a fájl mentése közben"
D_WARN_FILE_SAVE_ERROR_MESSAGE = u"A '%s' fájl létrehozása sikertelen.\n\n%s"

#warning about file already exists
D_WARN_FILE_ALREADY_EXISTS_TITLE = u"TorChat: A fájl már létezik"
D_WARN_FILE_ALREADY_EXISTS_MESSAGE = u"A '%s' már létezik.\nFelülírod?"

#dialog: add/edit contact
DEC_TITLE_ADD = u"Új partner hozzáadása"
DEC_TITLE_EDIT = u"Partner szerkesztése"
DEC_TORCHAT_ID = u"TorChat azonosító"
DEC_DISPLAY_NAME = u"Megjelenített név"
DEC_INTRODUCTION = u"Bemutatkozás"
DEC_MSG_16_CHARACTERS = u"Az azonosító 16 karakter hosszú lehet, nem %i."
DEC_MSG_ONLY_ALPANUM = u"Az azonosító csak számokat és az angol abc betüit tartalmazhatja"
DEC_MSG_ALREADY_ON_LIST = u"%s már a listádban van"

# #dialog: edit my profile
# DEP_TITLE = u"Edit my profile"
# DEP_NAME = u"Name"
# DEP_TEXT = u"Text"
# DEP_AVATAR_SELECT_PNG = u"Select .PNG file to use as your avatar (will be scaled to 64*64, may contain transparency)"
# DEP_PNG_FILES = u"PNG files"
# DEP_ALL_FILES = u"All files"
# DEP_WARN_TITLE = u"Avatar selection not possible"
# DEP_WARN_IS_ALREADY = u"This is already the current avatar"
# DEP_WARN_MUST_BE_PNG = u"The avatar must be a .png file"

#file transfer window
DFT_FILE_OPEN_TITLE = u"Fájl küldése %s számára"
DFT_FILE_SAVE_TITLE = u"Fájl mentése %s-től"
DFT_SEND = u"Külédes %s\n %s-nak\n%04.1f%% (%i kész %i bájtból)"
DFT_RECEIVE = u"Fogadás  %s\n %s-től\n%04.1f%% (%i kész %i bájtból)"
# DFT_WAITING = u"waiting for connection"
# DFT_STARTING = u"starting transfer"
# DFT_ABORTED = u"transfer aborted"
# DFT_COMPLETE = u"transfer complete"
# DFT_ERROR = u"error"

#settings dialaog
DSET_TITLE = u"TorChat beállítások"
DSET_NET_TITLE = u"Hálózat"
DSET_NET_ACTIVE = u"aktív"
DSET_NET_INACTIVE = u"inaktív"
DSET_NET_TOR_ADDRESS = u"Tor proxy cím"
DSET_NET_TOR_SOCKS = u"Socks port"
DSET_NET_TOR_CONTROL = u"Control port"
DSET_NET_OWN_HOSTNAME = u"Saját TorChat-azonosító"
DSET_NET_LISTEN_INTERFACE = u"Figyelő cím"
DSET_NET_LISTEN_PORT = u"Figyelő port"
DSET_GUI_TITLE = u"Felhasználói felület"
DSET_GUI_LANGUAGE = u"Nyelv"
DSET_GUI_OPEN_MAIN_HIDDEN = u"Indítás minimalizált ablakkal"
DSET_GUI_OPEN_CHAT_HIDDEN = u"Ne nyisson automatikusan új ablakokat"
DSET_GUI_NOTIFICATION_POPUP = u"Felugró figyelmeztetések"
DSET_GUI_FLASH_WINDOW = u"Ablak címének villogtatása új üzenet érkezésekor"
DSET_MISC_TITLE = u"Egyebek"
DSET_MISC_TEMP_IN_DATA = u"Ideiglenes fájlok tárolása a saját könyvtárban"
DSET_MISC_TEMP_CUSTOM_DIR = u"Ideiglenes könyvtár (ha üres, akkor az op.rendszer alapértelmezett könyvtára)"

#notices in the chat window (those in square brackets)
NOTICE_DELAYED_MSG_WAITING = u"várakozó üzenetek küldése"
NOTICE_DELAYED_MSG_SENT = u"várakozó üzenetek elküldve"
NOTICE_DELAYED = u"várakozik/késleltetett"

#messagebox for offline messages
MSG_OFFLINE_TITLE = u"TorChat: várakozó üzenetek"
MSG_OFFLINE_EMPTY = u"nincs (több) várakozó üzenet %s felöl"
MSG_OFFLINE_QUEUED = u"várakozó offline üzenet %s felöl:\n\n%s"

# #buddy list mouse hover popup
# BPOP_BUDDY_IS_OFFLINE = u"Buddy is offline"
# BPOP_CONNECTED_AWAITING_RETURN_CONN = u"Connected, awaiting return connection..."
# BPOP_CLIENT_SOFTWARE = u"Client: %s %s"

# #logging of conversations to file
# LOG_HEADER = u"This log file is not signed and has no cogency of proof"
# LOG_STARTED = u"Logging started"
# LOG_STOPPED = u"Logging stopped"
# LOG_DELETED = u"Log files have been deleted"
# LOG_IS_ACTIVATED = u"Logging to file is activated:\n%s"
# LOG_IS_STOPPED_OLD_LOG_FOUND = u"Logging is stopped but old log file still exists:\n%s"

#about box
ABOUT_TITLE = u"A TorChat-ról"
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