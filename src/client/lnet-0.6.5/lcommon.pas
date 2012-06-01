{ lCommon

  CopyRight (C) 2004-2008 Ales Katona

  This library is Free software; you can rediStribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is diStributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; withOut even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a Copy of the GNU Library General Public License
  along with This library; if not, Write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
  
  This license has been modified. See File LICENSE.ADDON for more inFormation.
  Should you find these sources without a LICENSE File, please contact
  me at ales@chello.sk
}

unit lCommon;

{$mode objfpc}{$H+}
{$inline on}

interface

uses
  {$i sys/osunits.inc}

const
  {$IFDEF WINDOWS}
  SOL_SOCKET = $ffff;
  LMSG = 0;
  SOCKET_ERROR = WinSock2.SOCKET_ERROR;
  SHUT_RDWR = SD_BOTH;
  SHUT_WR = SD_SEND;
  {$ENDIF}

  {$IFDEF OS2}
  SOL_SOCKET = WinSock.SOL_SOCKET;
  LMSG = 0;
  SOCKET_ERROR = WinSock.SOCKET_ERROR;
  {$ENDIF}

  {$IFDEF NETWARE}
  SOL_SOCKET = WinSock.SOL_SOCKET;
  LMSG = 0;
  SOCKET_ERROR = WinSock.SOCKET_ERROR;
  {$ENDIF}

  {$IFDEF UNIX}
  INVALID_SOCKET = -1;
  SOCKET_ERROR = -1;
    {$IFDEF LINUX} // TODO: fix this crap, some don't even have MSG_NOSIGNAL
    LMSG = MSG_NOSIGNAL;
    {$ELSE}
      {$IFDEF FREEBSD}
        LMSG = $20000; // FPC BUG in 2.0.4-, freeBSD value
      {$ELSE}
        LMSG = 0;
      {$ENDIF}
    {$ENDIF}
    
    {$IFDEF DARWIN}
    SO_NOSIGPIPE = $1022; // for fpc 2.0.4
    {$ENDIF}
  {$ENDIF}
  { Default Values }
  LDEFAULT_BACKLOG = 5;
  BUFFER_SIZE = 262144;
  { Net types }
  LAF_INET      =  AF_INET;
  LAF_INET6     = AF_INET6;
  { Address constants }
  LADDR_ANY = '0.0.0.0';
  LADDR_BR  = '255.255.255.255';
  LADDR_LO  = '127.0.0.1';
  LADDR6_ANY = '::0';
  LADDR6_LO  = '::1';
  { ICMP }
  LICMP_ECHOREPLY     = 0;
  LICMP_UNREACH       = 3;
  LICMP_ECHO          = 8;
  LICMP_TIME_EXCEEDED = 11;
  { Protocols }
  LPROTO_IP     =     0;
  LPROTO_ICMP   =     1;
  LPROTO_IGMP   =     2;
  LPROTO_TCP    =     6;
  LPROTO_UDP    =    17;
  LPROTO_IPV6   =    41;
  LPROTO_ICMPV6 =    58;
  LPROTO_RAW    =   255;
  LPROTO_MAX    =   256;

type

  { TLSocketAddress }

  TLSocketAddress = record
    case Integer of
      LAF_INET  : (IPv4: TInetSockAddr);
      LAF_INET6 : (IPv6: TInetSockAddr6);
  end;
  
  { Base functions }
  {$IFNDEF UNIX}
  function fpSelect(const nfds: Integer; const readfds, writefds, exceptfds: PFDSet;
                    const timeout: PTimeVal): Integer; inline;
  function fpFD_ISSET(const Socket: Integer; var FDSet: TFDSet): Integer; inline;
  procedure fpFD_SET(const Socket: Integer; var FDSet: TFDSet); inline;
  procedure fpFD_ZERO(var FDSet: TFDSet); inline;
  {$ENDIF}
  { DNS }
  function GetHostName(const Address: string): string;
  function GetHostIP(const Name: string): string;
  function GetHostName6(const Address: string): string;
  function GetHostIP6(const Name: string): string;

  function LStrError(const Ernum: Longint; const UseUTF8: Boolean = False): string;
  function LSocketError: Longint;
  
  function SetBlocking(const aHandle: Integer; const aValue: Boolean): Boolean;
  function SetNoDelay(const aHandle: Integer; const aValue: Boolean): Boolean;

  function IsBlockError(const anError: Integer): Boolean; inline;
  function IsNonFatalError(const anError: Integer): Boolean; inline;
  function IsPipeError(const anError: Integer): Boolean; inline;

  function TZSeconds: Integer; inline;

  function StrToHostAddr(const IP: string): Cardinal; inline;
  function HostAddrToStr(const Entry: Cardinal): string; inline;
  function StrToNetAddr(const IP: string): Sockets.in_addr; inline;
  function NetAddrToStr(const Entry: Cardinal): string; inline;
  
  procedure FillAddressInfo(var aAddrInfo: TLSocketAddress; const aFamily: sa_family_t;
                            const Address: string; const aPort: Word);
                            
implementation

uses
  StrUtils
  
{$IFNDEF UNIX}

{$IFDEF WINDOWS}
  , Windows, lws2tcpip;
  
{$IFDEF WINCE}

function LStrError(const Ernum: Longint; const UseUTF8: Boolean = False): string;
const
  MAX_ERROR = 1024;
var
  Tmp: string;
  TmpW: widestring;
begin
  Result := '[' + IntToStr(Ernum) + '] ';
    SetLength(TmpW, MAX_ERROR);
    SetLength(TmpW, FormatMessageW(FORMAT_MESSAGE_FROM_SYSTEM or
                                   FORMAT_MESSAGE_IGNORE_INSERTS or
                                   FORMAT_MESSAGE_ARGUMENT_ARRAY,
                                   nil, Ernum, 0, @TmpW[1], MAX_ERROR, nil));
    Tmp := UTF8Encode(TmpW);
  if Length(Tmp) > 2 then
    Delete(Tmp, Length(Tmp)-1, 2);
  Result := Tmp;
end;

{$ELSE} // any other windows

function LStrError(const Ernum: Longint; const UseUTF8: Boolean = False): string;
const
  MAX_ERROR = 1024;
var
  Tmp: string;
  TmpW: widestring;
begin
  Result := ' [' + IntToStr(Ernum) + ']: ';
  if USEUtf8 then begin
    SetLength(TmpW, MAX_ERROR);
    SetLength(TmpW, FormatMessageW(FORMAT_MESSAGE_FROM_SYSTEM or
                                   FORMAT_MESSAGE_IGNORE_INSERTS or
                                   FORMAT_MESSAGE_ARGUMENT_ARRAY,
                                   nil, Ernum, 0, @TmpW[1], MAX_ERROR, nil));
    Tmp := UTF8Encode(TmpW);
  end else begin
    SetLength(Tmp, MAX_ERROR);
    SetLength(Tmp, FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM or
                                 FORMAT_MESSAGE_IGNORE_INSERTS or
                                 FORMAT_MESSAGE_ARGUMENT_ARRAY,
                                 nil, Ernum, 0, @Tmp[1], MAX_ERROR, nil));
  end;
  if Length(Tmp) > 2 then
    Delete(Tmp, Length(Tmp)-1, 2);
  Result := Result + Tmp;
end;

{$ENDIF}

function TZSeconds: integer; inline;
var
  lInfo: Windows.TIME_ZONE_INFORMATION;
begin
  { lInfo.Bias is in minutes }
  if Windows.GetTimeZoneInformation(@lInfo) <> $FFFFFFFF then
    Result := lInfo.Bias * 60
  else
    Result := 0;
end;

{$ELSE}
  ; // uses
  
function LStrError(const Ernum: Longint; const UseUTF8: Boolean = False): string;
begin
  Result := IntToStr(Ernum); // TODO: fix for non-windows winsock users
end;

function TZSeconds: integer; inline;
begin
  Result := 0; // todo: fix for non-windows non unix
end;

{$ENDIF}

function LSocketError: Longint;
begin
  Result := WSAGetLastError;
end;

function CleanError(const Ernum: Longint): Byte;
begin
  Result := Byte(Ernum - 10000);
end;

function fpSelect(const nfds: Integer; const readfds, writefds, exceptfds: PFDSet;
                  const timeout: PTimeVal): Longint; inline;
begin
  Result := Select(nfds, readfds, writefds, exceptfds, timeout);
end;

function fpFD_ISSET(const Socket: Longint; var FDSet: TFDSet): Integer; inline;
begin
  Result := 0;
  if FD_ISSET(Socket, FDSet) then
    Result := 1;
end;

procedure fpFD_SET(const Socket: Longint; var FDSet: TFDSet); inline;
begin
  FD_SET(Socket, FDSet);
end;

procedure fpFD_ZERO(var FDSet: TFDSet); inline;
begin
  FD_ZERO(FDSet);
end;

function GetHostName(const Address: string): string;
var
  HE: PHostEnt;
  Addr: DWord;
begin
  Result := '';
  HE := nil;
  Addr := inet_addr(PChar(Address));
  HE := gethostbyaddr(@Addr, SizeOf(Addr), AF_INET);
  if Assigned(HE) then
    Result := HE^.h_name;
end;

function GetHostIP(const Name: string): string;
var
  HE: PHostEnt;
  P: PDWord;
begin
  Result := '';
  HE := nil;
  HE := gethostbyname(PChar(Name));
  if Assigned(HE) then begin
    P := Pointer(HE^.h_addr_list[0]);
    Result := NetAddrToStr(P^);
  end;
end;

function GetHostName6(const Address: string): string;
var
  H: TAddrInfo;
  R: PAddrInfo;
  n: Integer;
begin
  Result := '';
  ZeroMemory(@H, SizeOf(H));
  H.ai_flags := AI_NUMERICHOST;
  H.ai_family := AF_INET6;
  H.ai_protocol := PF_INET6;
  H.ai_socktype := SOCK_STREAM;

  n := getaddrinfo(pChar(Address), nil, @H, R);
  if n <> 0 then
    Exit;
  Result := R^.ai_canonname;
  freeaddrinfo(R);
end;

function GetHostIP6(const Name: string): string;
var
  H: TAddrInfo;
  R: PAddrInfo;
  n: Integer;
begin
  Result := '';
  ZeroMemory(@H, SizeOf(H));
  H.ai_family := AF_INET6;
  H.ai_protocol := PF_INET6;
  H.ai_socktype := SOCK_STREAM;

  n := getaddrinfo(pChar(Name), nil, @H, R);
  if n <> 0 then
    Exit;
  Result := NetAddrToStr6(sockets.in6_addr(R^.ai_addr^));
  freeaddrinfo(R);
end;

function SetBlocking(const aHandle: Integer; const aValue: Boolean): Boolean;
const
  BlockAr: array[Boolean] of DWord = (1, 0);
var
  opt: DWord;
begin
  opt := BlockAr[aValue];
  if ioctlsocket(aHandle, Longint(FIONBIO), opt) = SOCKET_ERROR then
    Exit(False);
  Result := True;
end;

function IsBlockError(const anError: Integer): Boolean; inline;
begin
  Result := anError = WSAEWOULDBLOCK;
end;

function IsNonFatalError(const anError: Integer): Boolean; inline;
begin
  Result := (anError = WSAEINVAL) or (anError = WSAEFAULT)
         or (anError = WSAEOPNOTSUPP) or (anError = WSAEMSGSIZE)
         or (anError = WSAEADDRNOTAVAIL) or (anError = WSAEAFNOSUPPORT)
         or (anError = WSAEDESTADDRREQ);
end;

function IsPipeError(const anError: Integer): Boolean; inline;
begin
  {$WARNING check these ambiguous errors}
  Result := anError = WSAECONNRESET;
end;

{$ELSE}

// unix

  ,Errors, UnixUtil;

function LStrError(const Ernum: Longint; const UseUTF8: Boolean = False): string;
begin
  Result := ' [' + IntToStr(Ernum) + ']: ' + Errors.StrError(Ernum);
end;

function LSocketError: Longint;
begin
  Result := fpgeterrno;
end;

function CleanError(const Ernum: Longint): Longint; inline;
begin
  Result := Byte(Ernum);
end;

function GetHostName(const Address: string): string;
var
  HE: THostEntry;
begin
  Result := '';
  if GetHostbyAddr(in_addr(StrToHostAddr(Address)), HE) then
    Result := HE.Name
  else if ResolveHostbyAddr(in_addr(StrToHostAddr(Address)), HE) then
    Result := HE.Name;
end;

function GetHostIP(const Name: string): string;
var
  HE: THostEntry;
begin
  Result := '';
  if GetHostByName(Name, HE) then
    Result := HostAddrToStr(Cardinal(HE.Addr)) // for localhost
  else if ResolveHostByName(Name, HE) then
    Result := NetAddrToStr(Cardinal(HE.Addr));
end;

function GetHostName6(const Address: string): string;
var
  HE: THostEntry6;
begin
  Result := '';
{  if GetHostByAddr(StrToHostAddr6(Address), HE) then
    Result := HE.Name
  else} if ResolveHostbyAddr6(StrToHostAddr6(Address), HE) then
    Result := HE.Name;
end;

function GetHostIP6(const Name: string): string;
var
  HE: THostEntry6;
begin
  Result := '';
{  if GetHostByName(Name, HE) then
    Result := HostAddrToStr6(HE.Addr) // for localhost
  else} if ResolveHostByName6(Name, HE) then
    Result := NetAddrToStr6(HE.Addr);
end;

function SetBlocking(const aHandle: Integer; const aValue: Boolean): Boolean;
var
  opt: cInt;
begin
  opt := fpfcntl(aHandle, F_GETFL);
  if opt = SOCKET_ERROR then
    Exit(False);
    
  if aValue then
    opt := opt and not O_NONBLOCK
  else
    opt := opt or O_NONBLOCK;

  if fpfcntl(aHandle, F_SETFL, opt) = SOCKET_ERROR then
    Exit(False);
  Result := True;
end;

function IsBlockError(const anError: Integer): Boolean; inline;
begin
  Result := (anError = ESysEWOULDBLOCK) or (anError = ESysENOBUFS);
end;

function IsNonFatalError(const anError: Integer): Boolean; inline;
begin
  Result := (anError = ESysEINTR) or (anError = ESysEMSGSIZE)
         or (anError = ESysEFAULT) or (anError = ESysEINVAL)
         or (anError = ESysEOPNOTSUPP);
end;

function IsPipeError(const anError: Integer): Boolean; inline;
begin
  Result := anError = ESysEPIPE;
end;

function TZSeconds: Integer; inline;
begin
  Result := unixutil.TZSeconds;
end;

{$ENDIF}

function SetNoDelay(const aHandle: Integer; const aValue: Boolean): Boolean;
var
  opt: Integer = 0;
begin
  if aValue then
    opt := 1;

  if fpsetsockopt(aHandle, IPPROTO_TCP, TCP_NODELAY, @opt, SizeOf(opt)) < 0 then
    Exit(False);

  Result := True;
end;

function StrToHostAddr(const IP: string): Cardinal; inline;
begin
  Result := Cardinal(Sockets.StrToHostAddr(IP));
end;

function HostAddrToStr(const Entry: Cardinal): string; inline;
begin
  Result := Sockets.HostAddrToStr(in_addr(Entry));
end;

function StrToNetAddr(const IP: string): Sockets.in_addr; inline;
begin
  Result := Sockets.StrToNetAddr(IP);
end;

function NetAddrToStr(const Entry: Cardinal): string; inline;
begin
  Result := Sockets.NetAddrToStr(in_addr(Entry));
end;

function IsIP6Empty(const aIP6: TInetSockAddr6): Boolean; inline;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to High(aIP6.sin6_addr.u6_addr32) do
    if aIP6.sin6_addr.u6_addr32[i] <> 0 then
      Exit(False);
end;

procedure FillAddressInfo(var aAddrInfo: TLSocketAddress; const aFamily: sa_family_t;
  const Address: string; const aPort: Word);
begin
  aAddrInfo.IPv4.sin_family := aFamily;
  aAddrInfo.IPv4.sin_Port := htons(aPort);

  case aFamily of
    LAF_INET  :
      begin
        aAddrInfo.IPv4.sin_addr := StrToNetAddr(Address);
        if (Address <> LADDR_ANY) and (Cardinal(aAddrInfo.IPv4.sin_addr) = 0) then
          aAddrInfo.IPv4.sin_Addr := StrToNetAddr(GetHostIP(Address));
      end;
    LAF_INET6 :
      begin
        aAddrInfo.IPv6.sin6_addr := StrToNetAddr6(Address);
        if (Address <> LADDR6_ANY) and (IsIP6Empty(aAddrInfo.IPv6)) then
          aAddrInfo.IPv6.sin6_addr := StrToNetAddr6(GetHostIP6(Address));
      end;
  end;
end;


end.

