{$ifdef WINDOWS}
  Winsock2,
{$endif}

{$ifdef UNIX}
  BaseUnix, NetDB,
{$endif}

{$ifdef NETWARE}
  WinSock,
{$endif}

{$ifdef OS2}
  WinSock,
{$endif}

  SysUtils, Sockets;

