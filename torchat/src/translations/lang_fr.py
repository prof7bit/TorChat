# -*- coding: UTF-8 -*-

##############################################################################
#                                                                            #
# Copyright (c) 2007-2008 Bernd Kreuss <prof7bit@gmail.com>                  #
#                                                                            #
# Translation file for TorChat                                               #
#                                                                            #
##############################################################################

LANGUAGE_CODE = u"fr"
LANGUAGE_NAME = u"Français"
LANGUAGE_NAME_ENGLISH = u"French"
TRANSLATOR_NAMES = [u"vitisch"]

#buttons
BTN_CANCEL = u"Annuler"
BTN_OK = u"Ok"
BTN_SAVE_AS = u"Save as..."
BTN_CLOSE = u"Fermer"

#status
ST_AVAILABLE = u"Disponible"
ST_AWAY = u"Absent"
ST_EXTENDED_AWAY = u"Absent pour longtemps"
ST_OFFLINE = u"Déconnecté"

#TaskbarMenu
MTB_SHOW_HIDE_TORCHAT = u"Montrer/Cacher TorChat"
MTB_QUIT = u"Arrêtez"

#popup menu
MPOP_CHAT = u"Chat..."
MPOP_SEND_FILE = u"Envoyer un fichier..."
MPOP_EDIT_CONTACT = u"Rediger contact..."
MPOP_DELETE_CONTACT = u"Supprimer contact..."
MPOP_SHOW_OFFLINE_MESSAGES = u"Montrer les messages hors-ligne"
MPOP_CLEAR_OFFLINE_MESSAGES = u"Effacer les messages hors-ligne"
MPOP_ADD_CONTACT = u"Ajouter un contact..."
MPOP_ABOUT = u"À propos..."
MPOP_ASK_AUTHOR = u"Demandez %s"
MPOP_SETTINGS = u"Paramètres..."

# #chat window popup menu
# CPOP_COPY = u"Copy"

#confirm delete message box
D_CONFIRM_DELETE_TITLE = u"Confirmez la supression"
D_CONFIRM_DELETE_MESSAGE = u"Êtes-vous sûr de vouloir supprimer le contact?\n(%s %s)"

#warning about log
D_LOG_WARNING_TITLE = u"TorChat: Archivage est actif"
D_LOG_WARNING_MESSAGE = u"L'archivage au fichier est activé!!\n\nFicher d'archivage: %s\n\nRappelez-vous de supprimer le ficher d'archivage si vous avez fini la correction parce que le ficher d'archivage peut contenir l'information sensible."

#warnig about unread messages
D_WARN_UNREAD_TITLE = u"TorChat: Messages non lus"
D_WARN_UNREAD_MESSAGE = u"Il y a des messages non lus.\nIls seront perdus pour toujours!\n\nVoulez-vous vraiment sortir maintenant?"

#dialog: add/edit contact
DEC_TITLE_ADD = u"Ajouter un nouveau contact"
DEC_TITLE_EDIT = u"Modifier le contact"
DEC_TORCHAT_ID = u"TorChat ID"
DEC_DISPLAY_NAME = u"Nom d'utilisateur"
DEC_INTRODUCTION = u"Introduction"
DEC_MSG_16_CHARACTERS = u"L'adresse doit avoir 16 caractères, pas %i."
DEC_MSG_ONLY_ALPANUM = u"L'adresse doit seulement contenir des nombres et des lettres minuscule."
DEC_MSG_ALREADY_ON_LIST = u"%s est déjà sur votre liste."

#file transfer window
# DFT_FILE_OPEN_TITLE = u"Send file to %s"
# DFT_FILE_SAVE_TITLE = u"Save file from %s"
DFT_SEND = u"Envoyer %s\nà %s\n%04.1f%% (%i de %i bytes)"
DFT_RECEIVE = u"Recevoir %s\nde %s\n%04.1f%% (%i de %i bytes)"

#settings dialaog
DSET_TITLE = u"Configuration de TorChat"
DSET_NET_TITLE = u"Réseau"
DSET_NET_ACTIVE = u"actif"
DSET_NET_INACTIVE = u"inactif"
DSET_NET_TOR_ADDRESS = u"Adresse de procuration pour Tor"
DSET_NET_TOR_SOCKS = u"Port de SOCKS"
DSET_NET_TOR_CONTROL = u"Port de commande"
DSET_NET_OWN_HOSTNAME = u"Mon TorChat ID"
DSET_NET_LISTEN_INTERFACE = u"Interface d'écouter"
DSET_NET_LISTEN_PORT = u"Port d'écouter"
DSET_GUI_TITLE = u"Interface d'utilisateur"
# DSET_GUI_LANGUAGE = u"Language"
# DSET_GUI_OPEN_MAIN_HIDDEN = u"Start with minimized main window"
# DSET_GUI_OPEN_CHAT_HIDDEN = u"Don't automatically open new windows"
# DSET_GUI_NOTIFICATION_POPUP = u"Notification pop-up"
# DSET_GUI_FLASH_WINDOW = u"Flash window title on new message"
# DSET_MISC_TITLE = u"Misc"
# DSET_MISC_TEMP_IN_DATA = u"Store temporary files inside data directory"
# DSET_MISC_TEMP_CUSTOM_DIR = u"Temporary directory (leave empty for OS-default)"

#notices in the chat window (those in square brackets)
NOTICE_DELAYED_MSG_WAITING = u"messages retardés attendant pour être envoyé"
NOTICE_DELAYED_MSG_SENT = u"messages retardés ont été envoyés"
NOTICE_DELAYED = u"retardé"

#about box
ABOUT_TITLE = u"À propos de TorChat"
ABOUT_TEXT = u"""TorChat %(version)s
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