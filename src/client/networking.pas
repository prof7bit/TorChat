unit networking;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef unix}errors,{$endif}
  Classes, SysUtils, Sockets, resolve;

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

  { TConnection wraps a socket}
  TConnection = class(THandleStream)
    constructor Create(AHandle: THandle); virtual;
    destructor Destroy; override;
    function Write(const Buffer; Count: LongInt): LongInt; override;
    function Read(var Buffer; Count: LongInt): LongInt; override;
    procedure DoClose; virtual;
  strict protected
    FClosed: Boolean;
    procedure SetClosed; virtual;
  public
    property Closed: Boolean read FClosed;
  end;

  TConnectionClass = class of TConnection;
  TListenerCallback = procedure(AConnection: TConnection) of object;

  { TListenerThread }
  TListenerThread = class(TThread)
    constructor Create(APort: DWord; ACallback: TListenerCallback; AConnectionClass: TConnectionClass); reintroduce;
    procedure Execute; override;
    procedure Terminate;
  strict protected
    FPort             : DWord;
    FSocket           : THandle;
    FCallback         : TListenerCallback;
    FConnectionClass  : TConnectionClass
  end;

  { TSocketWrapper }
  TSocketWrapper = Class(TComponent)
  strict protected
    FSocksProxyAddress  : String;
    FSocksProxyPort     : DWord;
    FSocksUser          : String;
    FOutgoingClass      : TConnectionClass;
    FIncomingClass      : TConnectionClass;
    FIncomingCallback   : TListenerCallback;
    FListeners          : array of TListenerThread;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Bind(APort: DWord);
    function Connect(AServer: String; APort: DWord): TConnection;
    property SocksProxyAddress: String write FSocksProxyAddress;
    property SocksProxyPort: DWord write FSocksProxyPort;
    property OutgoingClass: TConnectionClass write FOutgoingClass;
    property IncomingClass: TConnectionClass write FIncomingClass;
    property IncomingCallback: TListenerCallback write FIncomingCallback;
  end;



implementation

function LastErrorString: String;
begin
  {$ifdef unix}
  Result := StrError(SocketError);
  {$else}
  {$note find the winndows version of the above}
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
begin
  HostAddr := NameResolve(AServer);
  SockAddr.sin_family := AF_INET;
  SockAddr.sin_port := ShortHostToNet(APort);
  SockAddr.sin_addr := HostToNet(HostAddr);
  if Sockets.FpConnect(ASocket, @SockAddr, SizeOf(SockAddr))<>0 Then
    if (SocketError <> Sys_EINPROGRESS) and (SocketError <> 0) then
      raise ENetworkError.CreateFmt('connect failed: %s:%d (%s)',
        [AServer, APort, LastErrorString]);
end;

{ TSocketWrapper }

constructor TSocketWrapper.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOutgoingClass := TConnection;
  FIncomingClass := TConnection;
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
  Listener := TListenerThread.Create(APort, FIncomingCallback, FIncomingClass);
  SetLength(FListeners, Length(FListeners) + 1);
  FListeners[Length(FListeners)-1] := Listener;
end;

function TSocketWrapper.Connect(AServer: String; APort: DWord): TConnection;
var
  HSocket: THandle;
  REQ : String;
  ANS : array[1..8] of Byte;
begin
  HSocket := CreateHandle;
  if (FSocksProxyAddress = '') or (FSocksProxyPort = 0) then
    ConnectTCP(HSocket, AServer, APort)
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
    if (fpRecv(HSocket, @ANS, 8, RCV_FLAGS) <> 8) or (ANS[1] <> 0) then
      Raise ENetworkError.CreateFmt(
        'socks connect %s:%d via %s:%d handshake invalid response',
        [AServer, APort, FSocksProxyAddress, FSocksProxyPort]
      );
    if ANS[2] <> 90 then
      Raise ENetworkError.CreateFmt(
        'socks connect %s:%d via %s:%d failed (error %d)',
        [AServer, APort, FSocksProxyAddress, FSocksProxyPort, ANS[2]]
      );
  end;
  Result := FOutgoingClass.Create(HSocket);
end;

{ TListenerThread }

constructor TListenerThread.Create(APort: DWord; ACallback: TListenerCallback; AConnectionClass: TConnectionClass);
begin
  FPort := APort;
  FCallback := ACallback;
  FConnectionClass := AConnectionClass;
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
      FCallback(FConnectionClass.Create(Incoming))
    else
      break;
  until Terminated;
end;

procedure TListenerThread.Terminate;
begin
  CloseHandle(FSocket);
  inherited Terminate;
end;

{ TConnection }

constructor TConnection.Create(AHandle: THandle);
begin
  inherited Create(AHandle);
end;

destructor TConnection.Destroy;
begin
  DoClose;
  inherited Destroy;
end;

function TConnection.Write(const Buffer; Count: LongInt): LongInt;
begin
  Result := fpSend(Handle, @Buffer, Count, SND_FLAGS);
end;

function TConnection.Read(var Buffer; Count: LongInt): LongInt;
begin
  Result := fpRecv(Handle, @Buffer, Count, RCV_FLAGS);
  if Result = SOCKET_ERROR then
    DoClose;
end;

procedure TConnection.DoClose;
begin
  if not Closed then begin
    CloseHandle(Handle);
    SetClosed;
  end;
end;

procedure TConnection.SetClosed;
begin
  FClosed := True;
end;


end.

