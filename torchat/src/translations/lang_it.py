# # -*- coding: UTF-8 -*-

##############################################################################
#                                                                            #
# Copyright (c) 2007-2010 Bernd Kreuss <prof7bit@gmail.com>                  #
#                                                                            #
# Translation file for TorChat                                               #
#                                                                            #
##############################################################################

LANGUAGE_CODE = u"it"
LANGUAGE_NAME = u"Italiano"
LANGUAGE_NAME_ENGLISH = u"Italian"
TRANSLATOR_NAMES = [u"HostFat"]

#buttons
BTN_CANCEL = u"Annulla"
BTN_OK = u"Ok"
BTN_SAVE_AS = u"Salva come..."
BTN_CLOSE = u"Chiudi"

#status
ST_AVAILABLE = u"Disponibile"
ST_AWAY = u"Indisposto / lontano"
ST_EXTENDED_AWAY = u"Molto indisposto / lontano"
ST_OFFLINE = u"Non connesso"

#TaskbarMenu
MTB_SHOW_HIDE_TORCHAT = u"Mostra/Nascondi TorChat"
MTB_QUIT = u"Esci"

#popup menu
MPOP_CHAT = u"Chat..."
MPOP_SEND_FILE = u"Invia file..."
MPOP_EDIT_CONTACT = u"Modifica contatto..."
MPOP_DELETE_CONTACT = u"Cancella contatto..."
MPOP_SHOW_OFFLINE_MESSAGES = u"Mostra i messaggi offline in attesa"
MPOP_CLEAR_OFFLINE_MESSAGES = u"Cancella i messaggi offline in attesa"
MPOP_ACTIVATE_LOG = u"Attiva il salvataggio log su file"
MPOP_STOP_LOG = u"Ferma salvataggio log"
MPOP_DELETE_EXISTING_LOG = u"Elimina file log esistente"
MPOP_DELETE_AND_STOP_LOG = u"Cancella log e smetti di salvare"
MPOP_ADD_CONTACT = u"Aggiungi contatto..."
MPOP_ABOUT = u"About TorChat"
MPOP_ASK_AUTHOR = u"Chiedi %s..."
MPOP_SETTINGS = u"Impostazioni..."
MPOP_EDIT_MY_PROFILE = u"Modifica il mio profilo..."

#chat window popup menu
CPOP_COPY = u"Copia"

#confirm delete message box
D_CONFIRM_DELETE_TITLE = u"Conferma cancellazione"
D_CONFIRM_DELETE_MESSAGE = u"Vuoi veramente cancellare questo contatto?\n(%s %s)"

#warning about log
D_LOG_WARNING_TITLE = u"TorChat: Salvataggio log attivo"
D_LOG_WARNING_MESSAGE = u"Salvataggio log su file attivato!\n\nLog File: %s\n\nRicordati di cancellare il file log quando hai finito il debugging il file log potrebbe contenere informazioni sensibili."

#warning about used port
D_WARN_USED_PORT_TITLE = u"TorChat: Porta già in uso"
D_WARN_USED_PORT_MESSAGE = u"Qualcosa, probabilmente c'è un altro TorChat in esecuzione, è già in ascolto su %s:%s. Devi creare un altro profilo che usi porte differenti per poter avviare TorChat una seconda volta."

#warnig about unread messages
D_WARN_UNREAD_TITLE = u"TorChat: Messaggi non letti"
D_WARN_UNREAD_MESSAGE = u"Sono presenti messaggi non letti.\nVerranno persi per sempre!\n\nVuoi veramente uscire ora da TorChat?"

#warning about offline buddy
D_WARN_BUDDY_OFFLINE_TITLE = u"TorChat: L'utente è scollegato"
D_WARN_BUDDY_OFFLINE_MESSAGE = u"Questa operazione non è possibile con utenti scollegati"

#warning about multiple files
D_WARN_FILE_ONLY_ONE_TITLE = u"TorChat: File multipli"
D_WARN_FILE_ONLY_ONE_MESSAGE = u"Non dovresti avviare trasferimenti multipli di file in un'unica operazione. Invia trasferimenti singoli individualmente oppure inviali zippati"

#warning about file save error
D_WARN_FILE_SAVE_ERROR_TITLE = u"TorChat: Errore salvataggio file"
D_WARN_FILE_SAVE_ERROR_MESSAGE = u"Non è stato possibile creare il file '%s'.\n\n%s"

#warning about file already exists
D_WARN_FILE_ALREADY_EXISTS_TITLE = u"TorChat: File esistente"
D_WARN_FILE_ALREADY_EXISTS_MESSAGE = u"Il file '%s' esiste già.\nSovrascriverlo?"

#dialog: add/edit contact
DEC_TITLE_ADD = u"Aggiungi nuovo contatto"
DEC_TITLE_EDIT = u"Modifica contatto"
DEC_TORCHAT_ID = u"TorChat ID"
DEC_DISPLAY_NAME = u"Mostra nome"
DEC_INTRODUCTION = u"Introduzione"
DEC_MSG_16_CHARACTERS = u"L'indirizzo deve essere lungo 16 caratteri, non %i."
DEC_MSG_ONLY_ALPANUM = u"L'indirizzo può contenere solo numeri e caratteri in minuscolo."
DEC_MSG_ALREADY_ON_LIST = u"%s è già nella tua lista."

#dialog: edit my profile
DEP_TITLE = u"Modifca il mio profilo"
DEP_NAME = u"Nome"
DEP_TEXT = u"Testo"
DEP_SET_AVATAR = u"Imposta Avatar"
DEP_REMOVE_AVATAR = u"Rimuovi Avatar"
DEP_AVATAR_SELECT_PNG = u"Seleziona file .PNG da usare come avatar (verà scalato a 64*64, può contenere trasparenze)"
DEP_PNG_FILES = u"File PNG"
DEP_ALL_FILES = u"Tutti i file"
DEP_WARN_TITLE = u"Impossibile selezionare Avatar"
DEP_WARN_IS_ALREADY = u"Questo è già l'avatar corrente"
DEP_WARN_MUST_BE_PNG = u"L'avatar deve essere un file .png"

#file transfer window
DFT_FILE_OPEN_TITLE = u"Invia file a %s"
DFT_FILE_SAVE_TITLE = u"Salva file da %s"
DFT_SEND = u"Inviando %s\na %s\n%04.1f%% (%i di %i bytes)"
DFT_RECEIVE = u"Ricevendo %s\nda %s\n%04.1f%% (%i di %i bytes)"
DFT_WAITING = u"in attesa di connessione"
DFT_STARTING = u"avvio trasferimento"
DFT_ABORTED = u"trasferimento inconcluso"
DFT_COMPLETE = u"trasferimento completato"
DFT_ERROR = u"errore"

#settings dialaog
DSET_TITLE = u"Configurazione TorChat"
DSET_NET_TITLE = u"Network"
DSET_NET_ACTIVE = u"attivo"
DSET_NET_INACTIVE = u"inattivo"
DSET_NET_TOR_ADDRESS = u"Indirizzo proxy Tor"
DSET_NET_TOR_SOCKS = u"Porta Socks"
DSET_NET_TOR_CONTROL = u"Porta di controllo"
DSET_NET_OWN_HOSTNAME = u"Il proprio TorChat-ID"
DSET_NET_LISTEN_INTERFACE = u"Interfaccia in ascolto"
DSET_NET_LISTEN_PORT = u"Porta in ascolto"
DSET_GUI_TITLE = u"Interfaccia utente"
DSET_GUI_LANGUAGE = u"Lingua"
DSET_GUI_OPEN_MAIN_HIDDEN = u"Avvia con finestra minimizzata"
DSET_GUI_OPEN_CHAT_HIDDEN = u"Non aprire automaticamente nuove finestre"
DSET_GUI_NOTIFICATION_POPUP = u"Notifica pop-up"
DSET_GUI_NOTIFICATION_METHOD = u"Metodo di notifica"
DSET_GUI_FLASH_WINDOW = u"Titolo finestra lampeggiante con nuovi messaggi"
DSET_MISC_TITLE = u"Varie"
DSET_MISC_TEMP_IN_DATA = u"Salva file temporanei nella cartella data"
DSET_MISC_TEMP_CUSTOM_DIR = u"Cartella temporanea (lascia vuoto per la default dell'OS)"

#notices in the chat window (those in square brackets)
NOTICE_DELAYED_MSG_WAITING = u"messaggi in ritardo in attesa di essere inviati"
NOTICE_DELAYED_MSG_SENT = u"messaggi in ritardo sono stati inviati"
NOTICE_DELAYED = u"ritardato"

#messagebox for offline messages
MSG_OFFLINE_TITLE = u"TorChat: messaggi in attesa"
MSG_OFFLINE_EMPTY = u"non ci sono (ulteriori) messaggi in attesa per %s"
MSG_OFFLINE_QUEUED = u"messaggi offline in attesa per %s:\n\n%s"

#buddy list mouse hover popup
BPOP_BUDDY_IS_OFFLINE = u"Contatto non connesso"
BPOP_CONNECTED_AWAITING_RETURN_CONN = u"Collegato, in attesa di connessione di ritorno..."
BPOP_CLIENT_SOFTWARE = u"Client: %s %s"

#logging of conversations to file
LOG_HEADER = u"Questo file log non è firmato e non è valido come prova"
LOG_STARTED = u"Salvataggio log avviato"
LOG_STOPPED = u"Salvataggio log fermato"
LOG_DELETED = u"I file log sono stati cancellati"
LOG_IS_ACTIVATED = u"Salvataggio log su file è stato attivato:\n%s"
LOG_IS_STOPPED_OLD_LOG_FOUND = u"Il salvataggio è stato fermato ma il vecchio file log è ancora presente:\n%s"

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