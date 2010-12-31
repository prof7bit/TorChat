# -*- coding: UTF-8 -*-

##############################################################################
#                                                                            #
# Copyright (c) 2007-2010 Bernd Kreuss <prof7bit@gmail.com>                  #
#                                                                            #
# Translation file for TorChat                                               #
#                                                                            #
##############################################################################

LANGUAGE_CODE = u"pl"
LANGUAGE_NAME = u"Polski"
LANGUAGE_NAME_ENGLISH = u"Polish"
TRANSLATOR_NAMES = [u"yr3gwa7yonxrl26q"]

#buttons
BTN_CANCEL = u"Anuluj"
BTN_OK = u"Ok"
BTN_SAVE_AS = u"Zapisz jako..."
BTN_CLOSE = u"Zamknij"

#status
ST_AVAILABLE = u"Dostępny"
ST_AWAY = u"Zaraz wracam"
ST_EXTENDED_AWAY = u"Z dala od komputera"
ST_OFFLINE = u"Niedostępny"

#TaskbarMenu
MTB_SHOW_HIDE_TORCHAT = u"Pokaż/Ukryj TorChat"
MTB_QUIT = u"Zamknij"

#popup menu
MPOP_CHAT = u"Rozmowa..."
MPOP_SEND_FILE = u"Wyślij plik..."
MPOP_EDIT_CONTACT = u"Edytuj kontakt..."
MPOP_DELETE_CONTACT = u"Usuń kontakt..."
MPOP_SHOW_OFFLINE_MESSAGES = u"Pokaż wiadomości oczekujące"
MPOP_CLEAR_OFFLINE_MESSAGES = u"Wyczyść wiadomości oczekujące"
# MPOP_ACTIVATE_LOG = u"Activate logging to file"
# MPOP_STOP_LOG = u"Stop logging"
# MPOP_DELETE_EXISTING_LOG = u"Delete existing log file"
# MPOP_DELETE_AND_STOP_LOG = u"Delete log and stop logging"
MPOP_ADD_CONTACT = u"Dodaj kontakt..."
MPOP_ABOUT = u"O programie TorChat"
MPOP_ASK_AUTHOR = u"Zapytaj %s..."
MPOP_SETTINGS = u"Ustawienia..."
# MPOP_EDIT_MY_PROFILE = u"Edit my profile..."

#chat window popup menu
CPOP_COPY = u"Kopiuj"

#confirm delete message box
D_CONFIRM_DELETE_TITLE = u"Potwierdź usunięcie"
D_CONFIRM_DELETE_MESSAGE = u"Czy na pewno chcesz usunać ?\n(%s %s)"

#warning about log
D_LOG_WARNING_TITLE = u"TorChat: Rejestracja jest aktywna"
D_LOG_WARNING_MESSAGE = u"Rejestracja została aktywowana!\n\nLog Plik: %s\n\nPamietaj usuń plik po zakończeniu pracy."

# #warning about used port
# D_WARN_USED_PORT_TITLE = u"TorChat: Port already in use"
# D_WARN_USED_PORT_MESSAGE = u"Something, probably another TorChat instance, is already listening at %s:%s. You must create another profile using different ports to be able to start TorChat a second time."

#warnig about unread messages
D_WARN_UNREAD_TITLE = u"TorChat: Nieprzeczytane wiadomosci"
D_WARN_UNREAD_MESSAGE = u"Częśc wiadomości została nieprzeczytana.\nZostana one usunięte!\n\nNaprawdę chcesz wyjść ?"

# #warning about offline buddy
D_WARN_BUDDY_OFFLINE_TITLE = u"TorChat: Kontakt znajduje się poza siecią"
D_WARN_BUDDY_OFFLINE_MESSAGE = u"Nie można wykonac tej operacji z kontaktami pozostającymi poza siecią"

#warning about multiple files
D_WARN_FILE_ONLY_ONE_TITLE = u"TorChat: Kilka plików"
D_WARN_FILE_ONLY_ONE_MESSAGE = u"Nie można wysłac kilku plików podczas jednej operacji. Wyślij każdy osobno lub użyj archiwum ZIP/RAR/7Z."

# #warning about file save error
# D_WARN_FILE_SAVE_ERROR_TITLE = u"TorChat: Error saving file"
# D_WARN_FILE_SAVE_ERROR_MESSAGE = u"The file '%s' could not be created.\n\n%s"

# #warning about file already exists
# D_WARN_FILE_ALREADY_EXISTS_TITLE = u"TorChat: File exists"
# D_WARN_FILE_ALREADY_EXISTS_MESSAGE = u"The file '%s' already exists.\nOverwrite it?"

#dialog: add/edit contact
DEC_TITLE_ADD = u"Dodaj nowy kontakt"
DEC_TITLE_EDIT = u"Edytuj kontakt"
DEC_TORCHAT_ID = u"TorChat ID"
DEC_DISPLAY_NAME = u"Pokaż nazwę"
DEC_INTRODUCTION = u"Zaproszenie"
DEC_MSG_16_CHARACTERS = u"Adres musi mieć 16 znaków, nie %i."
DEC_MSG_ONLY_ALPANUM = u"Adres musi zawierać tylko małe litery i cyfry"
DEC_MSG_ALREADY_ON_LIST = u"%s już jest na liście"

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
DFT_FILE_OPEN_TITLE = u"Wyślij plik do %s"
DFT_FILE_SAVE_TITLE = u"Zapisz plik od %s"
DFT_SEND = u"Wysłano %s\ndo %s\n%04.1f%% (%i z %i bajtów)"
DFT_RECEIVE = u"Otrzymano %s\nod %s\n%04.1f%% (%i z %i bajtów)"

#settings dialaog
DSET_TITLE = u"Konfiguracja TorChat"
DSET_NET_TITLE = u"Sieć"
DSET_NET_ACTIVE = u"aktywne"
DSET_NET_INACTIVE = u"nieaktywne"
DSET_NET_TOR_ADDRESS = u"Adres Tor proxy"
DSET_NET_TOR_SOCKS = u"Socks port"
DSET_NET_TOR_CONTROL = u"Control port"
DSET_NET_OWN_HOSTNAME = u"Własny TorChat-ID"
DSET_NET_LISTEN_INTERFACE = u"Listen interface"
DSET_NET_LISTEN_PORT = u"Listen port"
DSET_GUI_TITLE = u"Interfejs użytkownika"
DSET_GUI_LANGUAGE = u"Język"
DSET_GUI_OPEN_MAIN_HIDDEN = u"Uruchom w tle."
DSET_GUI_OPEN_CHAT_HIDDEN = u"Nie otwieraj automatycznie nowych okien."
DSET_GUI_NOTIFICATION_POPUP = u"Powiadamiający pop-up"
DSET_GUI_FLASH_WINDOW = u"Sygnalizuj nadejście nowej wiadomości"
DSET_MISC_TITLE = u"Inne"
DSET_MISC_TEMP_IN_DATA = u"Przechowuj pliki tymczasowe w katalogu danych"
DSET_MISC_TEMP_CUSTOM_DIR = u"Tymczasowy katalog danych (zostaw puste dla domyślnego systemu operacyjnego)"

#notices in the chat window (those in square brackets)
NOTICE_DELAYED_MSG_WAITING = u"opóźnione wiadomości czekają w kolejce"
NOTICE_DELAYED_MSG_SENT = u"opóźnione wiadomości zostały wysłane"
NOTICE_DELAYED = u"opóźnione"

# #messagebox for offline messages
# MSG_OFFLINE_TITLE = u"TorChat: queued messages"
# MSG_OFFLINE_EMPTY = u"there are no (more) queued messages for %s"
# MSG_OFFLINE_QUEUED = u"queued offline messages for %s:\n\n%s"

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
ABOUT_TITLE = u"O programie TorChat"
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