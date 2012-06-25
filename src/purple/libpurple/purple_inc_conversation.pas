{$ifdef _const}
  PURPLE_MESSAGE_SEND         = $0001;
  PURPLE_MESSAGE_RECV         = $0002;
  PURPLE_MESSAGE_SYSTEM       = $0004;
  PURPLE_MESSAGE_AUTO_RESP    = $0008;
  PURPLE_MESSAGE_ACTIVE_ONLY  = $0010;
  PURPLE_MESSAGE_NICK         = $0020;
  PURPLE_MESSAGE_NO_LOG       = $0040;
  PURPLE_MESSAGE_WHISPER      = $0080;
  PURPLE_MESSAGE_ERROR        = $0200;
  PURPLE_MESSAGE_DELAYED      = $0400;
  PURPLE_MESSAGE_RAW          = $0800;
  PURPLE_MESSAGE_IMAGES       = $1000;
  PURPLE_MESSAGE_NOTIFY       = $2000;
  PURPLE_MESSAGE_NO_LINKIFY   = $4000;
  PURPLE_MESSAGE_INVISIBLE    = $8000;
{$endif}

{$ifdef _type}
  TPurpleMessageFlags = cint; // PURPLE_MESSAGE_XXX flags

  TPurpleTypingState = cint;

  TPurpleConversationType = (
    PURPLE_CONV_TYPE_UNKNOWN = 0,
    PURPLE_CONV_TYPE_IM,
    PURPLE_CONV_TYPE_CHAT,
    PURPLE_CONV_TYPE_MISC,
    PURPLE_CONV_TYPE_ANY
  );

  PPurpleConversation = ^TPurpleConversation;
  TPurpleConversation = Object
  end;
{$endif}

