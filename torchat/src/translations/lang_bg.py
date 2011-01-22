# -*- coding: UTF-8 -*-

##############################################################################
#                                                                            #
# # Copyright (c) 2007-2010 Bernd Kreuss <prof7bit@gmail.com>                  #
#                                                                            #
# Translation file for TorChat                                               #
#                                                                            #
##############################################################################

LANGUAGE_CODE = u"bg"
LANGUAGE_NAME = u"Български"
LANGUAGE_NAME_ENGLISH = u"Bulgarian"
TRANSLATOR_NAMES = [u"Asen Anastassov smaragdus@gmail.com"]

#buttons
BTN_CANCEL = u"Отмяна"
BTN_OK = u"Добре"
BTN_SAVE_AS = u"Запазване като..."
BTN_CLOSE = u"Затваряне"

#status
ST_AVAILABLE = u"на линия"
ST_AWAY = u"отсъстващ"
ST_EXTENDED_AWAY = u"недостъпен"
ST_OFFLINE = u"извън линия"

#TaskbarMenu
MTB_SHOW_HIDE_TORCHAT = u"Показване/Скриване на TorChat"
MTB_QUIT = u"Напускане"

#popup menu
MPOP_CHAT = u"Чат..."
MPOP_SEND_FILE = u"Изпращане на файл..."
MPOP_EDIT_CONTACT = u"Редактиране на контакт..."
MPOP_DELETE_CONTACT = u"Изтриване на контакт..."
MPOP_SHOW_OFFLINE_MESSAGES = u"Показване на неизпратени офлайн съобщения"
MPOP_CLEAR_OFFLINE_MESSAGES = u"Премахване на неизпратени офлайн съобщения"
# MPOP_ACTIVATE_LOG = u"Activate logging to file"
# MPOP_STOP_LOG = u"Stop logging"
# MPOP_DELETE_EXISTING_LOG = u"Delete existing log file"
# MPOP_DELETE_AND_STOP_LOG = u"Delete log and stop logging"
MPOP_ADD_CONTACT = u"Прибавяне на контакт..."
MPOP_ABOUT = u"Относно TorChat"
MPOP_ASK_AUTHOR = u"Попитайте %s..."
MPOP_SETTINGS = u"Настройки..."
# MPOP_EDIT_MY_PROFILE = u"Edit my profile..."

#chat window popup menu
CPOP_COPY = u"Копиране"

#confirm delete message box
D_CONFIRM_DELETE_TITLE = u"Потвърждаване на изтриване"
D_CONFIRM_DELETE_MESSAGE = u"Наистина ли желаете да изтриете този контакт?\n(%s %s)"

#warning about log
D_LOG_WARNING_TITLE = u"TorChat: Дневник активен"
D_LOG_WARNING_MESSAGE = u"Запазване на дневник във файл активирано!\n\nLog File: %s\n\nНе забравяйте да изтриете файла с дневника след като приключите анализа на грешките, защото той може да съдържа поверителна информация."

#warning about used port
D_WARN_USED_PORT_TITLE = u"TorChat: Портът е вече в употреба"
D_WARN_USED_PORT_MESSAGE = u"Нещо, вероятно второ копие на TorChat вече ползва %s:%s. Трябва да създадете нов профил ползващ други портове, за да  можете да стартирате второ копие на TorChat."

#warnig about unread messages
D_WARN_UNREAD_TITLE = u"TorChat: Непрочетени съобщения"
D_WARN_UNREAD_MESSAGE = u"Има непрочетени съобщения.\nТе ще бъдат загубени безвъзвратно!\n\nНаистина ли желаете да напуснете TorChat сега?"

#warning about offline buddy
D_WARN_BUDDY_OFFLINE_TITLE = u"TorChat: Контактът не е на линия"
D_WARN_BUDDY_OFFLINE_MESSAGE = u"Тази операция не е възможна с контакти извън линия"

#warning about multiple files
D_WARN_FILE_ONLY_ONE_TITLE = u"TorChat: Няколко файла"
D_WARN_FILE_ONLY_ONE_MESSAGE = u"Не можете да изпратите няколко файла едновременно. Изпратете ги поотделно или като zip-файл."

#warning about file save error
D_WARN_FILE_SAVE_ERROR_TITLE = u"TorChat: Грешка при запазване на файла"
D_WARN_FILE_SAVE_ERROR_MESSAGE = u"Файлът '%s' не може да бъде създаден.\n\n%s"

#warning about file already exists
D_WARN_FILE_ALREADY_EXISTS_TITLE = u"TorChat: Файлът съществува"
D_WARN_FILE_ALREADY_EXISTS_MESSAGE = u"Файлът '%s' вече съществува.\nЖелаете ли да го замените?"

#dialog: add/edit contact
DEC_TITLE_ADD = u"Добавяне на нов контакт"
DEC_TITLE_EDIT = u"Редактиране на контакт"
DEC_TORCHAT_ID = u"TorChat ID"
DEC_DISPLAY_NAME = u"Име"
DEC_INTRODUCTION = u"Въведение"
DEC_MSG_16_CHARACTERS = u"Адресът трябва да се състои от 16 символа, не %i."
DEC_MSG_ONLY_ALPANUM = u"Адресът трябва да се състои единствено от цифри и малки букви."
DEC_MSG_ALREADY_ON_LIST = u"%s е вече във вашия списък"

# #dialog: edit my profile
# DEP_TITLE = u"Edit my profile"
# DEP_NAME = u"Name"
# DEP_TEXT = u"Text"
# DEP_SET_AVATAR = u"Set Avatar"
# DEP_REMOVE_AVATAR = u"Remove Avatar"
# DEP_AVATAR_SELECT_PNG = u"Select .PNG file to use as your avatar (will be scaled to 64*64, may contain transparency)"
# DEP_PNG_FILES = u"PNG files"
# DEP_ALL_FILES = u"All files"
# DEP_WARN_TITLE = u"Avatar selection not possible"
# DEP_WARN_IS_ALREADY = u"This is already the current avatar"
# DEP_WARN_MUST_BE_PNG = u"The avatar must be a .png file"

#file transfer window
DFT_FILE_OPEN_TITLE = u"Изпращане на файл до %s"
DFT_FILE_SAVE_TITLE = u"Запазване на файл от %s"
DFT_SEND = u"Изпращане на %s\nto %s\n%04.1f%% (%i of %i байта)"
DFT_RECEIVE = u"Получаване на %s\nfrom %s\n%04.1f%% (%i of %i байта)"
# DFT_WAITING = u"waiting for connection"
# DFT_STARTING = u"starting transfer"
# DFT_ABORTED = u"transfer aborted"
# DFT_COMPLETE = u"transfer complete"
# DFT_ERROR = u"error"

#settings dialaog
DSET_TITLE = u"Конфигуриране на TorChat"
DSET_NET_TITLE = u"Мрежа"
DSET_NET_ACTIVE = u"Активна"
DSET_NET_INACTIVE = u"Неактивна"
DSET_NET_TOR_ADDRESS = u"Адрес на Tor прокси"
DSET_NET_TOR_SOCKS = u"Socks порт"
DSET_NET_TOR_CONTROL = u"Контролен порт"
DSET_NET_OWN_HOSTNAME = u"Собствено TorChat-ID"
DSET_NET_LISTEN_INTERFACE = u"Интерфейс за прослушване"
DSET_NET_LISTEN_PORT = u"Порт за прослушване"
DSET_GUI_TITLE = u"Потребителски интерфейс"
DSET_GUI_LANGUAGE = u"Език"
DSET_GUI_OPEN_MAIN_HIDDEN = u"Стартиране с минимизиран главен прозорец"
DSET_GUI_OPEN_CHAT_HIDDEN = u"Нови прозорци не се отварят автоматично"
DSET_GUI_NOTIFICATION_POPUP = u"Изскачащо известяване"
# DSET_GUI_NOTIFICATION_METHOD = u"Notification method"
DSET_GUI_FLASH_WINDOW = u"Пробягващ прозорец при получаване на ново съобщение"
DSET_MISC_TITLE = u"Разни"
DSET_MISC_TEMP_IN_DATA = u"Съхраняване на временни файлове в директория с данни"
DSET_MISC_TEMP_CUSTOM_DIR = u"Временна директория (празна по подразбиране за ОС)"

#notices in the chat window (those in square brackets)
NOTICE_DELAYED_MSG_WAITING = u"забавени съобщения, чакащи да бъдат изпратени"
NOTICE_DELAYED_MSG_SENT = u"забавени съобщения изпратени"
NOTICE_DELAYED = u"забавени"

#messagebox for offline messages
MSG_OFFLINE_TITLE = u"TorChat: съобщения на опашка"
MSG_OFFLINE_EMPTY = u"няма (повече) съобщения на опашка за %s"
MSG_OFFLINE_QUEUED = u"офлайн съобщения на опашка за %s:\n\n%s"

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
ABOUT_TITLE = u"Относно TorChat"
ABOUT_TEXT = u"""TorChat %(version)s (svn: r%(svn)s)
  %(copyright)s

Превод:
  %(translators)s

Runtime environment:
  Python: %(python)s
  wx: %(wx)s

TorChat е свободен софтуер: можете да го разпространявате и / или \
модифицирате при условията на GNU General Public License както е публикуван от Free Software Foundation, \
или версия 3, или (по ваш избор) \
която и да е следваща версия.

TorChat се разпространява с надеждата, че ще бъде от полза, \
но БЕЗ НИКАКВИ ГАРАНЦИИ; без дори косвена \
гаранция за ПРИГОДНОСТ ЗА ОПРЕДЕЛЕНА ЦЕЛ. \
Вижте GNU General Public License за повече подробности.

*

И сега за нещо съвсем различно:

Ако случайно управлявате софтуерна компания в района на Хановер, Германия и \
се нуждаете от нов програмист, приемете тази малка програма \
като моите документи за кандидатстване и ми пратете е-мейл с вашия отговор.
"""