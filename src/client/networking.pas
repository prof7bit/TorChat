unit networking;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Sockets, resolve, errors;

const
  Sys_EINPROGRESS = 115;

type
  ENetworkError = class(Exception)
  end;

  { TConnection wraps a socket}
  TConnection = class(THandleStream)
  end;

function ConnectTCP(AServer: String; APort: DWord): TConnection;
function ConnectSocks4a(AProxy: String; AProxyPort: DWord; AServer: String; APort: DWord): TConnection;
function NameResolve(AName: String): THostAddr;

implementation

function ConnectTCP(AServer: String; APort: DWord): TConnection;
var
  HostAddr: THostAddr;     // host byte order
  SockAddr: TInetSockAddr; // network byte order
  HSocket: THandle;
begin
  HostAddr := NameResolve(AServer);
  SockAddr.sin_family := AF_INET;
  SockAddr.sin_port := ShortHostToNet(APort);
  SockAddr.sin_addr := HostToNet(HostAddr);
  HSocket := Sockets.FPSocket(AF_INET, SOCK_STREAM, 0);
  if HSocket = -1 then
    raise ENetworkError.CreateFmt('could not create socket (%s)',
      [StrError(SocketError)]);
  if Sockets.FpConnect(HSocket, @SockAddr, SizeOf(SockAddr))<>0 Then
    if (SocketError <> Sys_EINPROGRESS) and (SocketError <> 0) then
      raise ENetworkError.CreateFmt('connect failed: %s:%d (%s)',
        [AServer, APort, StrError(SocketError)]);
  Result := TConnection.Create(HSocket);
end;

function ConnectSocks4a(AProxy: String; AProxyPort: DWord; AServer: String; APort: DWord): TConnection;
var
  C : TConnection;
begin
  C := ConnectTCP(AProxy, AProxyPort);
  Result := C;
end;

function NameResolve(AName: String): THostAddr;
var
  Resolver: THostResolver;
begin
  Result := StrToHostAddr(AName);
  if Result.s_addr = 0 then begin
    try
      Resolver := THostResolver.Create(nil);
      if not Resolver.NameLookup(AName) then
        raise ENetworkError.CreateFmt('could not resolve address: %s', [AName]);
      Result := Resolver.HostAddress;
    finally
      Resolver.Free;
    end;
  end;
end;


end.

