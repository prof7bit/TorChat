# -*- coding: UTF-8 -*-

##############################################################################
#                                                                            #
# Copyright (c) 2007-2008 Bernd Kreuss <prof7bit@gmail.com>                  #
#                                                                            #
# Translation file for TorChat                                               #
#                                                                            #
##############################################################################

LANGUAGE_CODE = u"de"
LANGUAGE_NAME = u"Deutsch"
LANGUAGE_NAME_ENGLISH = u"German"
TRANSLATOR_NAMES = [u"Bernd Kreuß"]

#buttons
BTN_CANCEL = u"Abbrechen"
BTN_OK = u"Ok"
BTN_SAVE_AS = u"Speichern unter..."
BTN_CLOSE = u"Schließen"

#status
ST_AVAILABLE = u"Verfügbar"
ST_AWAY = u"Abwesend"
ST_EXTENDED_AWAY = u"Nicht verfügbar"
ST_OFFLINE = u"Offine"

#TaskbarMenu
MTB_SHOW_HIDE_TORCHAT = u"TorChat anzeigen/verstecken"
MTB_QUIT = u"Beenden"

#popup menu
MPOP_CHAT = u"Nachricht schreiben..."
MPOP_SEND_FILE = u"Datei senden..."
MPOP_EDIT_CONTACT = u"Kontakt bearbeiten..."
MPOP_DELETE_CONTACT = u"Kontakt löschen..."
MPOP_SHOW_OFFLINE_MESSAGES = u"Nachrichten in Warteschlange anzeigen"
MPOP_CLEAR_OFFLINE_MESSAGES = u"Nachrichten in Warteschlange löschen"
MPOP_ADD_CONTACT = u"Kontakt hinzufügen..."
MPOP_ABOUT = u"Über TorChat"
MPOP_ASK_AUTHOR = u"%s fragen"
MPOP_SETTINGS = u"Einstellungen..."

#confirm delete message box
D_CONFIRM_DELETE_TITLE = u"Löschen bestätigen"
D_CONFIRM_DELETE_MESSAGE = u"Soll dieser Kontakt wirklich gelöscht werden?\n(%s %s)"

#warning about log
D_LOG_WARNING_TITLE = u"TorChat: Logging ist aktiviert"
D_LOG_WARNING_MESSAGE = u"Logging in Datei ist aktiviert!\n\nLogdatei: %s\n\nVergessen Sie nicht, die Logdatei nach Beendingung der Fehlersuche wieder zu löschen, da diese Datei vertrauliche Informationen enthalten könnte."

#warnig about unread messages
D_WARN_UNREAD_TITLE = u"TorChat: Ungelesene Nachrichten"
D_WARN_UNREAD_MESSAGE = u"Es liegen noch ungelesene Nachrichten vor. Diese würden unwiderruflich verloren gehen!\n\nMöchten sie TorChat dennoch jetzt beenden?"

#dialog: add/edit contact
DEC_TITLE_ADD = u"Neuen Kontakt anlegen"
DEC_TITLE_EDIT = u"Kontakt bearbeiteh"
DEC_TORCHAT_ID = u"TorChat ID"
DEC_DISPLAY_NAME = u"Angezeigter Name"
DEC_INTRODUCTION = u"Kurze Vorstellung"
DEC_MSG_16_CHARACTERS = u"Die Adresse muss genau 16 Zeichen lang sein, nicht %i."
DEC_MSG_ONLY_ALPANUM = u"Die Adresse kann nur as Ziffern und Kleinbuchstaben ohne Umlaute bestehen"
DEC_MSG_ALREADY_ON_LIST = u"%s ist bereits auf Ihrer Liste"

#file transfer window
DFT_SEND = u"Sende %s\nan %s\n%04.1f%% (%i von %i Bytes)"
DFT_RECEIVE = u"Empfange %s\nvon %s\n%04.1f%% (%i von %i Bytes)"

#settings dialaog
DSET_TITLE = u"TorChat Konfiguration"
DSET_NET_TITLE = u"Netzwerk"
DSET_NET_ACTIVE = u"aktiv"
DSET_NET_INACTIVE = u"inaktiv"
DSET_NET_TOR_ADDRESS = u"Tor-Proxy Addresse"
DSET_NET_TOR_SOCKS = u"Socks Port"
DSET_NET_TOR_CONTROL = u"Control Port"
DSET_NET_OWN_HOSTNAME = u"Eigene TorChat-ID"
DSET_NET_LISTEN_INTERFACE = u"Binden an Adapter"
DSET_NET_LISTEN_PORT = u"Binden an Port"
DSET_GUI_TITLE = u"Benutzeroberfläche"
DSET_GUI_LANGUAGE = u"Sprache"
DSET_GUI_OPEN_MAIN_HIDDEN = u"Starte mit minimiertem Hauptfenster"
DSET_GUI_OPEN_CHAT_HIDDEN = u"Öffne neue Fenster nicht automatisch"
DSET_GUI_NOTIFICATION_POPUP = u"Benachrichtigungs-PopUp"
DSET_GUI_FLASH_WINDOW = u"Blinkender Fenstertitel bei neuer Nachricht"

#notices in the chat window (those in square brackets)
NOTICE_DELAYED_MSG_WAITING = u"Verzögerte Nachrichten in der Sendewarteschlange"
NOTICE_DELAYED_MSG_SENT = u"Verzögerte Nachrichten wurden gesendet"
NOTICE_DELAYED = u"Verzögert"

#about box
ABOUT_TITLE = u"Über TorChat"
ABOUT_TEXT = u"""TorChat %(version)s
  %(copyright)s

Übersetzungen: 
  %(translators)s

Laufzeitumgebung:
  Python: %(python)s
  wx: %(wx)s
    
Dieses Programm ist freie Software. Sie können es unter den \
Bedingungen der GNU General Public License, wie von der \
Free Software Foundation veröffentlicht, weitergeben und/oder \
modifizieren, entweder gemäß Version 3 der Lizenz oder \
(nach Ihrer Option) jeder späteren Version.

Die Veröffentlichung dieses Programms erfolgt in der Hoffnung, \
daß es Ihnen von Nutzen sein wird, aber OHNE IRGENDEINE GARANTIE, \
sogar ohne die implizite Garantie der MARKTREIFE oder der \
VERWENDBARKEIT FÜR EINEN BESTIMMTEN ZWECK. Details finden Sie \
in der GNU General Public License.

Sie sollten ein Exemplar der GNU General Public License zusammen \
mit diesem Programm erhalten haben. Falls nicht, siehe \
<http://www.gnu.org/licenses/>.     

*

Und nun zu etwas vollkommen Anderem:

Sollten Sie zufällig eine Software-Firma irgendwo in der Nähe \
von Hannover betreiben und gerade Bedarf an einem neuen Programmierer \
haben, dann betrachten Sie dieses kleine Programm einfach als meine \
Bewerbung und schicken Sie mir eine E-Mail mit Ihrer Zusage ;-)
"""
