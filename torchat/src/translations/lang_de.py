# -*- coding: UTF-8 -*-

##############################################################################
#                                                                            #
# Copyright (c) 2007-2010 Bernd Kreuss <prof7bit@gmail.com>                  #
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
MPOP_ACTIVATE_LOG = u"Aktiviere Mitschnitt in Datei"
MPOP_STOP_LOG = u"Stoppe Mitschnitt"
MPOP_DELETE_EXISTING_LOG = u"Lösche existierenden Mitschnitt"
MPOP_DELETE_AND_STOP_LOG = u"Lösche Mitschnitt und stoppe Mitschneiden"
MPOP_ADD_CONTACT = u"Kontakt hinzufügen..."
MPOP_ABOUT = u"Über TorChat"
MPOP_ASK_AUTHOR = u"%s fragen..."
MPOP_SETTINGS = u"Einstellungen..."
MPOP_EDIT_MY_PROFILE = u"Eigenes Profil bearbeiten..."

# #chat window popup menu
CPOP_COPY = u"Kopieren"

#confirm delete message box
D_CONFIRM_DELETE_TITLE = u"Löschen bestätigen"
D_CONFIRM_DELETE_MESSAGE = u"Soll dieser Kontakt wirklich gelöscht werden?\n(%s %s)"

#warning about log
D_LOG_WARNING_TITLE = u"TorChat: Logging ist aktiviert"
D_LOG_WARNING_MESSAGE = u"Logging in Datei ist aktiviert!\n\nLogdatei: %s\n\nVergessen Sie nicht, die Logdatei nach Beendingung der Fehlersuche wieder zu löschen, da diese Datei vertrauliche Informationen enthalten könnte."

# #warning about used port
D_WARN_USED_PORT_TITLE = u"TorChat: Port ist bereits belegt"
D_WARN_USED_PORT_MESSAGE = u"Eine Anwendung, wahrscheinlich eine andere TorChat-Instanz, verwendet bereits den Port %s:%s. Sie müssen andere Profile mit anderen Ports verwenden, um TorChat mehrmals starten zu können."

#warnig about unread messages
D_WARN_UNREAD_TITLE = u"TorChat: Ungelesene Nachrichten"
D_WARN_UNREAD_MESSAGE = u"Es liegen noch ungelesene Nachrichten vor. Diese würden unwiderruflich verloren gehen!\n\nMöchten sie TorChat dennoch jetzt beenden?"

#warning about offline buddy
D_WARN_BUDDY_OFFLINE_TITLE = u"TorChat: Buddy ist offline"
D_WARN_BUDDY_OFFLINE_MESSAGE = u"Diese Operation ist nicht möglich mit Offline-Buddies"

#warning about multiple files
D_WARN_FILE_ONLY_ONE_TITLE = u"TorChat: Mehrere Dateien"
D_WARN_FILE_ONLY_ONE_MESSAGE = u"Sie können nicht mit einer Operation mehrere Dateitransfers gleichzeitig auslösen. Starten Sie die Transfers einzeln, oder senden Sie eine Zip-Datei."

#warning about file save error
D_WARN_FILE_SAVE_ERROR_TITLE = u"TorChat: Fehler beim Anlegen der Datei"
D_WARN_FILE_SAVE_ERROR_MESSAGE = u"Die Datei '%s' konnte nicht erzeugt werden.\n\n%s"

#warning about file already exists
D_WARN_FILE_ALREADY_EXISTS_TITLE = u"TorChat: Datei existiert bereits"
D_WARN_FILE_ALREADY_EXISTS_MESSAGE = u"Die Datei '%s' existiert bereits.\nÜberschreiben?"

#dialog: add/edit contact
DEC_TITLE_ADD = u"Neuen Kontakt anlegen"
DEC_TITLE_EDIT = u"Kontakt bearbeiten"
DEC_TORCHAT_ID = u"TorChat ID"
DEC_DISPLAY_NAME = u"Angezeigter Name"
DEC_INTRODUCTION = u"Kurze Vorstellung"
DEC_MSG_16_CHARACTERS = u"Die Adresse muss genau 16 Zeichen lang sein, nicht %i."
DEC_MSG_ONLY_ALPANUM = u"Die Adresse kann nur as Ziffern und Kleinbuchstaben ohne Umlaute bestehen"
DEC_MSG_ALREADY_ON_LIST = u"%s ist bereits auf Ihrer Liste"

# #dialog: edit my profile
DEP_TITLE = u"Mein Profil bearbeiten"
DEP_NAME = u"Name"
DEP_TEXT = u"Text"
DEP_AVATAR_SELECT_PNG = u"Auswählen einer .PNG Datei als Profilbild (wird auf 64*64 skaliert, darf Transparenz enthalten)"
DEP_PNG_FILES = u"PNG Dateien"
DEP_ALL_FILES = u"Alle Dateien"
DEP_WARN_TITLE = u"Auswahl nicht möglich"
DEP_WARN_IS_ALREADY = u"Dies ist bereits ihr aktuelles Profilbild"
DEP_WARN_MUST_BE_PNG = u"Bild muss eine .png Datei sein"

#file transfer window
DFT_FILE_OPEN_TITLE = u"Sende Datei an %s"
DFT_FILE_SAVE_TITLE = u"Speichere Datei von %s"
DFT_SEND = u"Sende %s\nan %s\n%04.1f%% (%i von %i Bytes)"
DFT_RECEIVE = u"Empfange %s\nvon %s\n%04.1f%% (%i von %i Bytes)"
DFT_WAITING = u"warte auf Verbindung"
DFT_STARTING = u"starte Transfer"
DFT_ABORTED = u"Transfer abgebrochen"
DFT_COMPLETE = u"Transfer vollständig"
DFT_ERROR = u"Fehler"

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
DSET_GUI_NOTIFICATION_METHOD = u"Benachrichtigungsmethode"
DSET_GUI_FLASH_WINDOW = u"Blinkender Fenstertitel bei neuer Nachricht"
DSET_MISC_TITLE = u"Verschiedenes"
DSET_MISC_TEMP_IN_DATA = u"Temporäre Dateien im Datenverzeichnis"
DSET_MISC_TEMP_CUSTOM_DIR = u"Verzeichnis für temporäre Dateien (leer lassen für OS-Default)"

#notices in the chat window (those in square brackets)
NOTICE_DELAYED_MSG_WAITING = u"Verzögerte Nachrichten in der Sendewarteschlange"
NOTICE_DELAYED_MSG_SENT = u"Verzögerte Nachrichten wurden gesendet"
NOTICE_DELAYED = u"Verzögert"

#messagebox for offline messages
MSG_OFFLINE_TITLE = u"TorChat: Ungesendete Nachrichten"
MSG_OFFLINE_EMPTY = u"Es liegen keine ungesendeten Nachrichten (mehr) für %s vor"
MSG_OFFLINE_QUEUED = u"Ungesendete Nachrichten für %s:\n\n%s"

#buddy list mouse hover popup
BPOP_BUDDY_IS_OFFLINE = u"Buddy ist offline"
BPOP_CONNECTED_AWAITING_RETURN_CONN = u"Verbunden, erwarte Rückverbindung..."
BPOP_CLIENT_SOFTWARE = u"Client: %s %s"

#logging of conversations to file
LOG_HEADER = u"Dieser Mitschnitt ist nicht signiert und beinhaltet keine Beweiskraft"
LOG_STARTED = u"Mitschneiden gestartet"
LOG_STOPPED = u"Mitschneiden gestoppt"
LOG_DELETED = u"Mitschnitt gelöscht"
LOG_IS_ACTIVATED = u"Mitschneiden ist aktiviert:\n%s"
LOG_IS_STOPPED_OLD_LOG_FOUND = u"Mitschneiden ist gestoppt aber ein alter Mitschnitt existiert noch:\n%s"

#about box
ABOUT_TITLE = u"Über TorChat"
ABOUT_TEXT = u"""TorChat %(version)s (svn: r%(svn)s)
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