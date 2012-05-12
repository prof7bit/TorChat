{ TorChat - TSocketWrapper, thin wrapper around network sockets

  Copyright (C) 2012 Bernd Kreuss <prof7bit@gmail.com>

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 3 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}
unit networking;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef unix}errors,{$endif}
  Classes,
  SysUtils,
  Sockets,
  resolve;

const
  Sys_EINPROGRESS = 115;
  Sys_EAGAIN = 11;
{$ifdef windows}
  SND_FLAGS = 0;
  RCV_FLAGS = 0;
{$else}
  SOCKET_ERROR = -1;
  SND_FLAGS = MSG_NOSIGNAL;
  RCV_FLAGS = MSG_NOSIGNAL;
{$endif}

type
  ENetworkError = class(Exception)
  end;

  TAsyncConnectThread = class;

  { TTCPStream wraps a TCP connection}
  TTCPStream = class(THandleStream)
    constructor Create(AHandle: THandle);
    destructor Destroy; override;
    function Write(const Buffer; Count: LongInt): LongInt; override;
    function Read(var Buffer; Count: LongInt): LongInt; override;
    procedure DoClose; virtual;
  end;

  PConnectionCallback = procedure(AStream: TTCPStream; E: Exception) of object;

  { TListenerThread }
  TListenerThread = class(TThread)
    constructor Create(APort: DWord; ACallback: PConnectionCallback); reintroduce;
    procedure Execute; override;
    procedure Terminate;
  strict protected
    FPort             : DWord;
    FSocket           : THandle;
    FCallback         : PConnectionCallback;
  end;

  { TSocketWrapper }
  TSocketWrapper = Class(TComponent)
  strict protected
    FSocksProxyAddress  : String;
    FSocksProxyPort     : DWord;
    FSocksUser          : String;
    FIncomingCallback   : PConnectionCallback;
    FListeners          : array of TListenerThread;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Bind(APort: DWord);
    { this method will block, close HSocket to interrupt it! }
    function Connect(AServer: String; APort: DWord;
      out HSocket: THandle): TTCPStream;
    function ConnectAsync(AServer: String; APort: DWord;
      ACallback: PConnectionCallback): TAsyncConnectThread;
    property SocksProxyAddress: String write FSocksProxyAddress;
    property SocksProxyPort: DWord write FSocksProxyPort;
    property IncomingCallback: PConnectionCallback write FIncomingCallback;
  end;

  { TAsyncConnectThread }

  TAsyncConnectThread = class(TThread)
    constructor Create(ASocketWrapper: TSocketWrapper; AServer: String;
      APort: DWord; ACallback: PConnectionCallback);
    procedure Execute; override;
    { terminate the connect attempt }
    procedure Terminate;
  strict protected
    FSocket: THandle;
    FSocketWrapper: TSocketWrapper;
    FCallback: PConnectionCallback;
    FServer: String;
    FPort: DWord;
  end;

implementation

function LastErrorString: String;
begin
  {$ifdef unix}
  Result := StrError(SocketError);
  {$else}
  {$note find the windows version of the above}
  Result := IntToStr(SocketError);
  {$endif}
end;

function CreateHandle: THandle;
begin
  Result := Sockets.FPSocket(AF_INET, SOCK_STREAM, 0);
  if Result <= 0 then
    raise ENetworkError.CreateFmt('could not create socket (%s)',
      [LastErrorString]);
end;

procedure CloseHandle(ASocket: THandle);
begin
  fpshutdown(ASocket, SHUT_RDWR);
  Sockets.CloseSocket(ASocket);
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

procedure ConnectTCP(ASocket: THandle; AServer: String; APort: DWord);
var
  HostAddr: THostAddr;     // host byte order
  SockAddr: TInetSockAddr; // network byte order
  N: Integer;
begin
  HostAddr := NameResolve(AServer);
  SockAddr.sin_family := AF_INET;
  SockAddr.sin_port := ShortHostToNet(APort);
  SockAddr.sin_addr := HostToNet(HostAddr);
  N := Sockets.FpConnect(ASocket, @SockAddr, SizeOf(SockAddr));
  if N <> 0 Then
    if (SocketError <> Sys_EINPROGRESS) and (SocketError <> 0) then begin
      CloseHandle(ASocket);
      raise ENetworkError.CreateFmt('connect failed: %s:%d (%s)',
        [AServer, APort, LastErrorString]);
    end;
end;

{ TAsyncConnectThread }

constructor TAsyncConnectThread.Create(ASocketWrapper: TSocketWrapper; AServer: String;
  APort: DWord; ACallback: PConnectionCallback);
begin
  FSocketWrapper := ASocketWrapper;
  FCallback := ACallback;
  FServer := AServer;
  FPort := APort;
  FreeOnTerminate := True;
  Inherited Create(False);
end;

procedure TAsyncConnectThread.Execute;
var
  C : TTCPStream;
begin
  try
    C := FSocketWrapper.Connect(FServer, FPort, FSocket);
    FCallback(C, nil);
  except
    on E: Exception do begin
      FCallback(nil, E);
    end;
  end;
end;

procedure TAsyncConnectThread.Terminate;
begin
  CloseHandle(FSocket);
  inherited Terminate;
end;


{ TSocketWrapper }

constructor TSocketWrapper.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIncomingCallback := nil;
  FSocksUser := '';
  FSocksProxyAddress := '';
  FSocksProxyPort := 0;
  SetLength(FListeners, 0);
end;

destructor TSocketWrapper.Destroy;
var
  Listener: TListenerThread;
begin
  for Listener in FListeners do begin
    Listener.Terminate;
    Listener.Free;
  end;
  SetLength(FListeners, 0);
  inherited Destroy;
end;

procedure TSocketWrapper.Bind(APort: DWord);
var
  Listener: TListenerThread;
begin
  if FIncomingCallback = nil then
    raise ENetworkError.Create('No callback for incoming connections');
  Listener := TListenerThread.Create(APort, FIncomingCallback);
  SetLength(FListeners, Length(FListeners) + 1);
  FListeners[Length(FListeners)-1] := Listener;
end;

function TSocketWrapper.Connect(AServer: String; APort: DWord; out HSocket: THandle): TTCPStream;
var
  REQ : String;
  ANS : array[1..8] of Byte;
  N   : Integer;
begin
  HSocket := CreateHandle;
  if (FSocksProxyAddress = '') or (FSocksProxyPort = 0) then begin
    ConnectTCP(HSocket, AServer, APort);
  end
  else begin
    ConnectTCP(HSocket, FSocksProxyAddress, FSocksProxyPort);
    SetLength(REQ, 8);
    REQ[1] := #4; // Socks 4
    REQ[2] := #1; // CONNECT command
    PWord(@REQ[3])^ := ShortHostToNet(APort);
    PDWord(@REQ[5])^ := HostToNet(1); // address '0.0.0.1' means: Socks 4a
    REQ := REQ + FSocksUser + #0;
    REQ := REQ + AServer + #0;
    fpSend(HSocket, @REQ[1], Length(REQ), SND_FLAGS);
    ANS[1] := $ff;
    N := fpRecv(HSocket, @ANS, 8, RCV_FLAGS);
    if (N <> 8) or (ANS[1] <> 0) then begin
      CloseHandle(HSocket);
      Raise ENetworkError.CreateFmt(
        'socks connect %s:%d via %s:%d handshake invalid response',
        [AServer, APort, FSocksProxyAddress, FSocksProxyPort]
      );
    end;
    if ANS[2] <> 90 then begin
      CloseHandle(HSocket);
      Raise ENetworkError.CreateFmt(
        'socks connect %s:%d via %s:%d failed (error %d)',
        [AServer, APort, FSocksProxyAddress, FSocksProxyPort, ANS[2]]
      );
    end;
  end;
  Result := TTCPStream.Create(HSocket);
end;

function TSocketWrapper.ConnectAsync(AServer: String; APort: DWord;
  ACallback: PConnectionCallback): TAsyncConnectThread;
begin
  Result := TAsyncConnectThread.Create(Self, AServer, APort, ACallback);
end;

{ TListenerThread }

constructor TListenerThread.Create(APort: DWord; ACallback: PConnectionCallback);
begin
  FPort := APort;
  FCallback := ACallback;
  Inherited Create(false);
end;

procedure TListenerThread.Execute;
var
  TrueValue : Integer;
  SockAddr  : TInetSockAddr;
  SockAddrx : TInetSockAddr;
  AddrLen   : PtrInt;
  Incoming  : THandle;
begin
  TrueValue := 1;
  AddrLen := SizeOf(SockAddr);

  FSocket := CreateHandle;
  fpSetSockOpt(FSocket, SOL_SOCKET, SO_REUSEADDR, @TrueValue, SizeOf(TrueValue));
  SockAddr.sin_family := AF_INET;
  SockAddr.sin_port := ShortHostToNet(FPort);
  SockAddr.sin_addr.s_addr := 0;

  if fpBind(FSocket, @SockAddr, SizeOf(SockAddr))<>0 then
    raise ENetworkError.CreateFmt('could not bind port %d (%s)',
      [FPort, LastErrorString]);

  fpListen(FSocket, 1);
  repeat
    Incoming := fpaccept(FSocket, @SockAddrx, @AddrLen);
    if Incoming > 0 then
      FCallback(TTCPStream.Create(Incoming), nil)
    else
      break;
  until Terminated;
end;

procedure TListenerThread.Terminate;
begin
  CloseHandle(FSocket);
  inherited Terminate;
end;

{ TTCPStream }

constructor TTCPStream.Create(AHandle: THandle);
begin
  inherited Create(AHandle);
end;

destructor TTCPStream.Destroy;
begin
  DoClose;
  inherited Destroy;
end;

function TTCPStream.Write(const Buffer; Count: LongInt): LongInt;
begin
  Result := fpSend(Handle, @Buffer, Count, SND_FLAGS);
end;

function TTCPStream.Read(var Buffer; Count: LongInt): LongInt;
begin
  Result := fpRecv(Handle, @Buffer, Count, RCV_FLAGS);
  if Result = SOCKET_ERROR then
    DoClose;
end;

procedure TTCPStream.DoClose;
begin
  CloseHandle(Handle);
end;

end.

