# -*- coding: UTF-8 -*-

##############################################################################
#                                                                            #
# Copyright (c) 2007-2010 Bernd Kreuss <prof7bit@gmail.com>                  #
#                                                                            #
# Translation file for TorChat                                               #
#                                                                            #
##############################################################################

LANGUAGE_CODE = u"pt"
LANGUAGE_NAME = u"Português"
LANGUAGE_NAME_ENGLISH = u"Portuguese"
TRANSLATOR_NAMES = [u"Marc Young mycbx@lavabit.com", u"Vitor Rozsas vcatao@bol.com.br"]

#buttons
BTN_CANCEL = u"Cancelar"
BTN_OK = u"OK"
BTN_SAVE_AS = u"Salvar como..."
BTN_CLOSE = u"Fechar"

#status
ST_AVAILABLE = u"Disponível"
ST_AWAY = u"Ocupado"
ST_EXTENDED_AWAY = u"Muito ocupado"
ST_OFFLINE = u"Desligado"

#TaskbarMenu
MTB_SHOW_HIDE_TORCHAT = u"Mostrar/Esconder o TorChat"
MTB_QUIT = u"Sair"

#popup menu
MPOP_CHAT = u"Bate-papo..."
MPOP_SEND_FILE = u"Enviar arquivo..."
MPOP_EDIT_CONTACT = u"Editar contato..."
MPOP_DELETE_CONTACT = u"Deletar contato..."
MPOP_SHOW_OFFLINE_MESSAGES = u"Mostrar mensagens offline enfileiradas"
MPOP_CLEAR_OFFLINE_MESSAGES = u"Limpar mensagens offline enfileiradas"
MPOP_ACTIVATE_LOG = u"Registrar (log) para arquivo"
MPOP_STOP_LOG = u"Parar os registros"
MPOP_DELETE_EXISTING_LOG = u"Deletar registros existentes"
MPOP_DELETE_AND_STOP_LOG = u"Deletar registros e parar de registrar"
MPOP_ADD_CONTACT = u"Adicionar contato..."
MPOP_ABOUT = u"Sobre o TorChat"
MPOP_ASK_AUTHOR = u"Perguntar ao %s..."
MPOP_SETTINGS = u"Configurações..."
MPOP_EDIT_MY_PROFILE = u"Editar meu perfil..."

#chat window popup menu
CPOP_COPY = u"Copiar"

#confirm delete message box
D_CONFIRM_DELETE_TITLE = u"Confirmar exclusão"
D_CONFIRM_DELETE_MESSAGE = u"Realmente deletar este contato?\n(%s %s)"

#warning about log
D_LOG_WARNING_TITLE = u"TorChat: O Arquivo de registros(Log) está ativo"
D_LOG_WARNING_MESSAGE = u"Arquivo de registros(Log) ativado!\n\nArquivo de registros: %s\n\nLembrar de deletar o arquivo de registros se você terminou de eliminar os erros(debugging) porque o arquivo de registros pode conter informações sensíveis."

#warning about used port
D_WARN_USED_PORT_TITLE = u"TorChat: Porta em uso "
D_WARN_USED_PORT_MESSAGE = u"Algo, provavelmente outra intância do TorChat já está escutando pela porta %s:%s. Você deve criar outro perfil usando diferentes portas para ser possível iniciar o TorChat uma segunda vez."

#warnig about unread messages
D_WARN_UNREAD_TITLE = u"TorChat: Mensagens não lidas"
D_WARN_UNREAD_MESSAGE = u"Essas mensagens não foram lidas.\nElas serão perdidas para sempre!\n\nVocê realmente quer sair do TorChat agora?"

#warning about offline buddy
D_WARN_BUDDY_OFFLINE_TITLE = u"TorChat: Esse amigo está desligado"
D_WARN_BUDDY_OFFLINE_MESSAGE = u"Essa operação não é possível com amigos desligados"

#warning about multiple files
D_WARN_FILE_ONLY_ONE_TITLE = u"TorChat: Múltiplos arquivos"
D_WARN_FILE_ONLY_ONE_MESSAGE = u"Você não pode iniciar a tranferência de arquivos múltiplos usando uma única operação. Inicie transferências uma de cada vez ou ao invés disso envie um arquivo zip."

#warning about file save error
D_WARN_FILE_SAVE_ERROR_TITLE = u"TorChat: Erro ao salvar arquivo"
D_WARN_FILE_SAVE_ERROR_MESSAGE = u"O arquivo '%s' não pôde ser criado.\n\n%s"

#warning about file already exists
D_WARN_FILE_ALREADY_EXISTS_TITLE = u"TorChat: O arquivo existe"
D_WARN_FILE_ALREADY_EXISTS_MESSAGE = u"O arquivo '%s' já existe.\nSobrescrevê-lo?"

#dialog: add/edit contact
DEC_TITLE_ADD = u"Adicionar novo contato"
DEC_TITLE_EDIT = u"Editar contato"
DEC_TORCHAT_ID = u"TorChat ID"
DEC_DISPLAY_NAME = u"Nome aparente(display name)"
DEC_INTRODUCTION = u"Introdução"
DEC_MSG_16_CHARACTERS = u"O endereço deve ter 16 caracteres, não %i."
DEC_MSG_ONLY_ALPANUM = u"O endereço deve conter apenas números e letras minúsculas"
DEC_MSG_ALREADY_ON_LIST = u"%s já está na sua lista"

#dialog: edit my profile
DEP_TITLE = u"Editar meu perfil"
DEP_NAME = u"Nome"
DEP_TEXT = u"Texto"
DEP_SET_AVATAR = u"Trocar Avatar"
DEP_REMOVE_AVATAR = u"Remover Avatar"
DEP_AVATAR_SELECT_PNG = u"Selecione um arquivo .PNG para usar como avatar (será redimensionado para 64x64 píxeis, pode ficar um pouco transparente)"
DEP_PNG_FILES = u"Arquivos PNG"
DEP_ALL_FILES = u"Todos os arquivos"
DEP_WARN_TITLE = u"Seleção de avatar não foi possível"
DEP_WARN_IS_ALREADY = u"Este já é o seu avatar"
DEP_WARN_MUST_BE_PNG = u"O avatar deve ser um arquivo .png"

#file transfer window
DFT_FILE_OPEN_TITLE = u"Enviar arquivo para %s"
DFT_FILE_SAVE_TITLE = u"Salvar arquivo de %s"
DFT_SEND = u"Enviando %s\npara %s\n%04.1f%% (%i de %i bytes)"
DFT_RECEIVE = u"Recebendo %s\nde %s\n%04.1f%% (%i de %i bytes)"
DFT_WAITING = u"aguardando conexão"
DFT_STARTING = u"iniciando transferência"
DFT_ABORTED = u"transferência interrompida"
DFT_COMPLETE = u"transferência completa"
DFT_ERROR = u"erro"

#settings dialaog
DSET_TITLE = u"Configuração do TorChat"
DSET_NET_TITLE = u"Rede"
DSET_NET_ACTIVE = u"ativo"
DSET_NET_INACTIVE = u"inativo"
DSET_NET_TOR_ADDRESS = u"Endereço de proxy do Tor"
DSET_NET_TOR_SOCKS = u"Porta Socks"
DSET_NET_TOR_CONTROL = u"Porta de Controle"
DSET_NET_OWN_HOSTNAME = u"Meu próprio TorChat-ID"
DSET_NET_LISTEN_INTERFACE = u"Interface de escuta"
DSET_NET_LISTEN_PORT = u"Porta de Escuta"
DSET_GUI_TITLE = u"Interface do usuário"
DSET_GUI_LANGUAGE = u"Idioma"
DSET_GUI_OPEN_MAIN_HIDDEN = u"Iniciar com a janela principal minimizada"
DSET_GUI_OPEN_CHAT_HIDDEN = u"Não abrir novas janelas automáticamente"
DSET_GUI_NOTIFICATION_POPUP = u"Pop-up de notificação"
DSET_GUI_NOTIFICATION_METHOD = u"Método de notificação"
DSET_GUI_FLASH_WINDOW = u"Janela rápida quando chegar uma nova mensagem"
DSET_MISC_TITLE = u"Variado"
DSET_MISC_TEMP_IN_DATA = u"Armazenar arquivos temporários no diretório de dados"
DSET_MISC_TEMP_CUSTOM_DIR = u"Diretório temporário (deixar vazio para OS-padrão)"

#notices in the chat window (those in square brackets)
NOTICE_DELAYED_MSG_WAITING = u"mensagens atrasadas estão esperando para serem enviadas"
NOTICE_DELAYED_MSG_SENT = u"as mensagens atrasadas foram enviadas"
NOTICE_DELAYED = u"atrasada"

#messagebox for offline messages
MSG_OFFLINE_TITLE = u"TorChat: mensagens não lidas"
MSG_OFFLINE_EMPTY = u"essas não são (mais) mensagens enfileiradas para %s"
MSG_OFFLINE_QUEUED = u"mensagens enfileiradas offline para %s:\n\n%s"

# #buddy list mouse hover popup
BPOP_BUDDY_IS_OFFLINE = u"Contato está desligado"
BPOP_CONNECTED_AWAITING_RETURN_CONN = u"Conectado, aguardando conexão de retorno..."
BPOP_CLIENT_SOFTWARE = u"Cliente: %s %s"

#logging of conversations to file
LOG_HEADER = u"Este arquivo de registro não está assinado e não possui confirmação alguma de legitimidade"
LOG_STARTED = u"O registro começou"
LOG_STOPPED = u"O registro foi interrompido"
LOG_DELETED = u"Os arquivos de registro foram apagados"
LOG_IS_ACTIVATED = u"O registro para arquivo foi ativado:\n%s"
LOG_IS_STOPPED_OLD_LOG_FOUND = u"O registro foi interrompido, mas os arquivos ainda existem:\n%s"

#about box
ABOUT_TITLE = u"Sobre o TorChat"
ABOUT_TEXT = u"""TorChat %(version)s (svn: r%(svn)s)
  %(copyright)s

Traduções:
  %(translators)s

Ambiente Runtime:
  Python: %(python)s
  wx: %(wx)s

O TorChat é um software livre: você pode redistribuí-lo e/ou \
modificá-lo sob os termos da GNU General Public \
License publicada pela Free Software Foundation, \
usando qualquer versão 3 dessa licença, ou (conforme sua opção) \
qualquer versão anterior.

O TorChat é distribuído na esperança de que ele seja útil, \
mas SEM QUALQUER GARANTIA; sem que isso implique \
em garantia de MERCANTIBILIDADE ou APTIDÃO PARA PROPÓSITOS PARTICULARES. \
Veja a GNU General Public License para mais detalhes.

*

E agora para algo completamente diferente:

Se acontecer de você rodar um software proprietário perto de Hannover, Alemanha ou \
está precisando de um novo programador, sinta-se livre para considerar esse pequeno programa \
bem como meus documentos da aplicação e enviar-me um email com suas perguntas.
"""
