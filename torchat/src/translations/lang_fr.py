# -*- coding: UTF-8 -*-

##############################################################################
#                                                                            #
# Copyright (c) 2007-2010 Bernd Kreuss <prof7bit@gmail.com>                  #
#                                                                            #
# Translation file for TorChat                                               #
#                                                                            #
##############################################################################

LANGUAGE_CODE = u"fr"
LANGUAGE_NAME = u"Français"
LANGUAGE_NAME_ENGLISH = u"French"
TRANSLATOR_NAMES = [u"vitisch", u"Pierre Abbat"]

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
MPOP_ACTIVATE_LOG = u"Activer le fichier d'archivage"
MPOP_STOP_LOG = u"Désactiver l'archivage"
MPOP_DELETE_EXISTING_LOG = u"Supprimer le fichier d'archivage"
MPOP_DELETE_AND_STOP_LOG = u"Cesser d'archiver et supprimer le fichier"
MPOP_ADD_CONTACT = u"Ajouter un contact..."
MPOP_ABOUT = u"À propos..."
MPOP_ASK_AUTHOR = u"Demandez %s..."
MPOP_SETTINGS = u"Paramètres..."
MPOP_EDIT_MY_PROFILE = u"Modifier mon profil..."

# #chat window popup menu
CPOP_COPY = u"Copier"

#confirm delete message box
D_CONFIRM_DELETE_TITLE = u"Confirmez la supression"
D_CONFIRM_DELETE_MESSAGE = u"Êtes-vous sûr de vouloir supprimer le contact?\n(%s %s)"

#warning about log
D_LOG_WARNING_TITLE = u"TorChat: Archivage est actif"
D_LOG_WARNING_MESSAGE = u"L'archivage au fichier est activé!!\n\nFicher d'archivage: %s\n\nRappelez-vous de supprimer le ficher d'archivage si vous avez fini la correction parce que le ficher d'archivage peut contenir l'information sensible."

# #warning about used port
D_WARN_USED_PORT_TITLE = u"TorChat: Port déjà occupé"
D_WARN_USED_PORT_MESSAGE = u"Quelque chose, probablement une autre instance de TorChat, écoute déjà à %s:%s. Vous devez créer un autre profil qui utilise des autres ports pour pouvoir commencer TorChat une autre fois."

#warnig about unread messages
D_WARN_UNREAD_TITLE = u"TorChat: Messages non lus"
D_WARN_UNREAD_MESSAGE = u"Il y a des messages non lus.\nIls seront perdus pour toujours!\n\nVoulez-vous vraiment sortir maintenant?"

#warning about offline buddy
D_WARN_BUDDY_OFFLINE_TITLE = u"TorChat: Copain hors ligne"
D_WARN_BUDDY_OFFLINE_MESSAGE = u"Cette opération n'est pas possible quand le copain est hors ligne"

#warning about multiple files
D_WARN_FILE_ONLY_ONE_TITLE = u"TorChat: Plusieurs fichiers"
D_WARN_FILE_ONLY_ONE_MESSAGE = u"On ne peut pas transférer plusieurs fichiers en une seule opération. Commencez les transferts individualement ou envoyez un fichier zip ou tar"

# #warning about file save error
D_WARN_FILE_SAVE_ERROR_TITLE = u"TorChat: Erreur sauvant fichier"
D_WARN_FILE_SAVE_ERROR_MESSAGE = u"Le fichier '%s' ne peut pas être créé.\n\n%s"

# #warning about file already exists
D_WARN_FILE_ALREADY_EXISTS_TITLE = u"TorChat: Fichier existe"
D_WARN_FILE_ALREADY_EXISTS_MESSAGE = u"Le fichier '%s' existe déjà.\nSurécrire?"

#dialog: add/edit contact
DEC_TITLE_ADD = u"Ajouter un nouveau contact"
DEC_TITLE_EDIT = u"Modifier le contact"
DEC_TORCHAT_ID = u"TorChat ID"
DEC_DISPLAY_NAME = u"Nom d'utilisateur"
DEC_INTRODUCTION = u"Introduction"
DEC_MSG_16_CHARACTERS = u"L'adresse doit avoir 16 caractères, pas %i."
DEC_MSG_ONLY_ALPANUM = u"L'adresse doit seulement contenir des nombres et des lettres minuscule."
DEC_MSG_ALREADY_ON_LIST = u"%s est déjà sur votre liste."

# #dialog: edit my profile
DEP_TITLE = u"Modifier mon profil"
DEP_NAME = u"Nom"
DEP_TEXT = u"Texte"
# DEP_SET_AVATAR = u"Set Avatar"
# DEP_REMOVE_AVATAR = u"Remove Avatar"
DEP_AVATAR_SELECT_PNG = u"Sélectionner fichier .PNG pour votre avatar (agrandi ou réduit à 64*64, peut contenir transparence)"
DEP_PNG_FILES = u"Fichiers PNG"
DEP_ALL_FILES = u"Tous fichiers"
DEP_WARN_TITLE = u"Sélection d'avatar impossible"
DEP_WARN_IS_ALREADY = u"C'est déjá l'avatar actuel"
DEP_WARN_MUST_BE_PNG = u"L'avatar doit être un fichier .png"

#file transfer window
DFT_FILE_OPEN_TITLE = u"Envoyer fichier à %s"
DFT_FILE_SAVE_TITLE = u"Sauver fichier de %s"
DFT_SEND = u"Envoyer %s\nà %s\n%04.1f%% (%i de %i octets)"
DFT_RECEIVE = u"Recevoir %s\nde %s\n%04.1f%% (%i de %i octets)"
DFT_WAITING = u"attendant connexion"
DFT_STARTING = u"commençant transfert"
DFT_ABORTED = u"transfert avorté"
DFT_COMPLETE = u"transfert complet"
DFT_ERROR = u"erreur"

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
DSET_GUI_LANGUAGE = u"Langue"
DSET_GUI_OPEN_MAIN_HIDDEN = u"Commencer avec fenêtre principale minimalisée"
DSET_GUI_OPEN_CHAT_HIDDEN = u"Ne pas ouvrir automatiquement des nouvelles fenêtres"
DSET_GUI_NOTIFICATION_POPUP = u"Notification surgissante"
# DSET_GUI_NOTIFICATION_METHOD = u"Notification method"
DSET_GUI_FLASH_WINDOW = u"Clignoter titre de fenêtre à un nouveau message"
DSET_MISC_TITLE = u"Misc"
DSET_MISC_TEMP_IN_DATA = u"Cacher fichiers temporaires dans le directoir de données"
DSET_MISC_TEMP_CUSTOM_DIR = u"Directoire temporaire (laissez vide pour défaut de SE)"

#notices in the chat window (those in square brackets)
NOTICE_DELAYED_MSG_WAITING = u"messages retardés attendant pour être envoyé"
NOTICE_DELAYED_MSG_SENT = u"messages retardés ont été envoyés"
NOTICE_DELAYED = u"retardé"

# #messagebox for offline messages
MSG_OFFLINE_TITLE = u"TorChat: messages en queue"
MSG_OFFLINE_EMPTY = u"pas de messages en queue pour %s"
MSG_OFFLINE_QUEUED = u"messages en queue pour %s hors ligne:\n\n%s"

# #buddy list mouse hover popup
BPOP_BUDDY_IS_OFFLINE = u"Copain est hors ligne"
BPOP_CONNECTED_AWAITING_RETURN_CONN = u"Connexion aller, attendant connexion retour..."
BPOP_CLIENT_SOFTWARE = u"Client: %s %s"

# #logging of conversations to file
LOG_HEADER = u"Ce fichier d'archive n'es pas signé et n'a pas de cogence de preuve."
LOG_STARTED = u"Commence à archiver"
LOG_STOPPED = u"Cesse d'archiver"
LOG_DELETED = u"Supprime les fichiers d'archive"
LOG_IS_ACTIVATED = u"Active l'archive à fichier:\n%s"
LOG_IS_STOPPED_OLD_LOG_FOUND = u"Désactive l'archive mais le fichier existe encore:\n%s"

#about box
ABOUT_TITLE = u"À propos de TorChat"
ABOUT_TEXT = u"""TorChat %(version)s (svn: r%(svn)s)
  %(copyright)s

Traductions:
  %(translators)s

Environnement de marche:
  Python: %(python)s
  wx: %(wx)s

TorChat est un logiciel libre: vous pouvez le redistribuer et/ou \
modifier sous les termes de la GNU General Public \
License publié par la Free Software Foundation, \
soit version 3 de la License, ou (à votre option) \
une version postérieure.

TorChat est distribué en espérant qu'il soit utile, \
mais SANS AUCUNE GARANTIE; ni même la garantie \
implicite de MARCHANTABILITÉ or APTITUDE À PROPOS PARTICULIER. \
Voir la GNU General Public License pour plus de détails.

*

Et maintenant, une chose complètement différente:

Si par chance vous gérez une compagnie de logiciel près de Hannover, Allemagne et \
avez besoin d'un codeur, sentez-vous libre de considérer ce petit programme \
comme mes documents d'application et m'envoyer un courriel avec votre réponse.
"""