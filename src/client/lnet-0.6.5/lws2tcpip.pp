unit lws2tcpip;

{$mode delphi}

interface

uses
  WinSock2;

const
  ws2tcpip = 'ws2_32.dll';

  AI_PASSIVE     = $1;
  AI_CANONNAME   = $2;
  AI_NUMERICHOST = $4;

type
  LPADDRINFO = ^addrinfo;
  addrinfo = record
    ai_flags: Integer;
    ai_family: Integer;
    ai_socktype: Integer;
    ai_protocol: Integer;
    ai_addrlen: size_t;
    ai_canonname: PChar;
    ai_addr: PSockAddr;
    ai_next: LPADDRINFO;
  end;
  TAddrInfo = addrinfo;
  PAddrInfo = LPADDRINFO;

function getaddrinfo(nodename, servname: PChar; hints: PAddrInfo; var res: PAddrInfo): Integer; stdcall;
procedure freeaddrinfo(ai: PAddrInfo); stdcall;

implementation

uses
  dynlibs;

type
  TGetAddrInfoFunc = function (nodename, servname: PChar; hints: PAddrInfo; var res: PAddrInfo): Integer; stdcall;
  TFreeAddrInfoProc = procedure (ai: PAddrInfo); stdcall;

var
  _lib: TLibHandle;
  _getaddrinfo: TGetAddrInfoFunc;
  _freeaddrinfo: TFreeAddrInfoProc;

function getaddrinfo(nodename, servname: PChar; hints: PAddrInfo;
  var res: PAddrInfo): Integer; stdcall;
begin
  _getaddrinfo(nodename, servname, hints, res);
end;

procedure freeaddrinfo(ai: PAddrInfo); stdcall;
begin

end;

initialization
  _lib := LoadLibrary(ws2tcpip);
  _getaddrinfo := GetProcedureAddress(_lib, 'getaddrinfo');
  _freeaddrinfo := GetProcedureAddress(_lib, 'freeaddrinfo');

finalization
  UnloadLibrary(_lib);


end.

