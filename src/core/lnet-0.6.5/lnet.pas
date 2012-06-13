{ lNet v0.6.5

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

unit lNet;

{$mode objfpc}{$H+}{$T-}
{$interfaces corba}

interface

uses
  Classes, lEvents, lCommon,
  {$i sys/osunits.inc}

const
  { API compatibility, these had to be moved to prevent circular unit usage and a
    fpc bug with inline }
  LADDR_ANY   = lCommon.LADDR_ANY;
  LADDR_BR    = lCommon.LADDR_BR;
  LADDR_LO    = lCommon.LADDR_LO;
  LADDR6_ANY  = lCommon.LADDR6_ANY;
  LADDR6_LO   = lCommon.LADDR6_LO;

type
  TLSocket = class;
  TLComponent = class;
  TLConnection = class;
  TLSession = class;

  { Callback Event procedure for errors }
  TLSocketErrorEvent = procedure(const msg: string; aSocket: TLSocket) of object;

  { Callback Event procedure for others }
  TLSocketEvent = procedure(aSocket: TLSocket) of object;

  { Callback Event procedure for progress reports}
  TLSocketProgressEvent = procedure (aSocket: TLSocket; const Bytes: Integer) of object;

  { TLSocketState }
  TLSocketState = (ssServerSocket, ssBlocking, ssReuseAddress, ssCanSend,
                   ssCanReceive, ssSSLActive, ssNoDelay);

  { TLSocketStates }
  TLSocketStates = set of TLSocketState;

  { TLSocketConnection }
  TLSocketConnectionStatus = (scNone, scConnecting, scConnected, scDisconnecting);

  { TLSocketOperation }
  TLSocketOperation = (soSend, soReceive);

  { TLSocket }

  TLSocket = class(TLHandle)
   protected
    FAddress: TLSocketAddress;
    FPeerAddress: TLSocketAddress;
    FReuseAddress: Boolean;
    FConnectionStatus: TLSocketConnectionStatus;
    FNextSock: TLSocket;
    FPrevSock: TLSocket;
    FSocketState: TLSocketStates;
    FOnFree: TLSocketEvent;
    FBlocking: Boolean;
    FListenBacklog: Integer;
    FProtocol: Integer;
    FSocketType: Integer;
    FSocketNet: Integer;
    FCreator: TLComponent;
    FSession: TLSession;
    FConnection: TLConnection;
   protected
    function GetConnected: Boolean; virtual; deprecated;
    function GetConnecting: Boolean; virtual; deprecated;
    function GetConnectionStatus: TLSocketConnectionStatus; virtual;
    function GetIPAddressPointer: psockaddr;
    function GetIPAddressLength: TSocklen;

    function SetupSocket(const APort: Word; const Address: string): Boolean; virtual;
    
    function DoSend(const aData; const aSize: Integer): Integer; virtual;
    function DoGet(out aData; const aSize: Integer): Integer; virtual;

    function HandleResult(const aResult: Integer; aOp: TLSocketOperation): Integer; virtual;
    
    function GetLocalPort: Word;
    function GetPeerPort: Word;
    function GetPeerAddress: string;
    function GetLocalAddress: string;
    function SendPossible: Boolean; inline;
    function ReceivePossible: Boolean; inline;

    procedure SetOptions; virtual;
    procedure SetBlocking(const aValue: Boolean);
    procedure SetReuseAddress(const aValue: Boolean);
    procedure SetNoDelay(const aValue: Boolean);

    procedure HardDisconnect(const NoShutdown: Boolean = False);
    procedure SoftDisconnect;

    function Bail(const msg: string; const ernum: Integer): Boolean;
    
    function LogError(const msg: string; const ernum: Integer): Boolean; virtual;
    
    property SocketType: Integer read FSocketType write FSocketType; // inherit and publicize if you need to set this outside
   public
    constructor Create; override;
    destructor Destroy; override;
    
    function SetState(const aState: TLSocketState; const TurnOn: Boolean = True): Boolean; virtual;
    
    function Listen(const APort: Word; const AIntf: string = LADDR_ANY): Boolean;
    function Accept(const SerSock: TSocket): Boolean;
    function Connect(const Address: string; const APort: Word): Boolean;
    
    function Send(const aData; const aSize: Integer): Integer; virtual;
    function SendMessage(const msg: string): Integer;
    
    function Get(out aData; const aSize: Integer): Integer; virtual;
    function GetMessage(out msg: string): Integer;
    
    procedure Disconnect(const Forced: Boolean = False); virtual;
   public
    property Connected: Boolean read GetConnected; deprecated;
    property Connecting: Boolean read GetConnecting; deprecated;
    property ConnectionStatus: TLSocketConnectionStatus read GetConnectionStatus;
    property ListenBacklog: Integer read FListenBacklog write FListenBacklog;
    property Protocol: Integer read FProtocol write FProtocol;
    property SocketNet: Integer read FSocketNet write FSocketNet;
    property PeerAddress: string read GetPeerAddress;
    property PeerPort: Word read GetPeerPort;
    property LocalAddress: string read GetLocalAddress;
    property LocalPort: Word read GetLocalPort;
    property NextSock: TLSocket read FNextSock write FNextSock;
    property PrevSock: TLSocket read FPrevSock write FPrevSock;
    property SocketState: TLSocketStates read FSocketState;
    property Creator: TLComponent read FCreator;
    property Session: TLSession read FSession;
  end;
  TLSocketClass = class of TLSocket;
  
  { this is the socket used by TLConnection }
  
  TLActionEnum = (acConnect, acAccept, acSend, acReceive, acError);

  { Base interface common to ALL connections }
  
  ILComponent = interface
    procedure Disconnect(const Forced: Boolean = False);
    procedure CallAction;
    
    property SocketClass: TLSocketClass;
    property Host: string;
    property Port: Word;
  end;
  
  { Interface for protools with direct send/get capabilities }

  ILDirect = interface
    function Get(out aData; const aSize: Integer; aSocket: TLSocket = nil): Integer;
    function GetMessage(out msg: string; aSocket: TLSocket = nil): Integer;

    function Send(const aData; const aSize: Integer; aSocket: TLSocket = nil): Integer;
    function SendMessage(const msg: string; aSocket: TLSocket = nil): Integer;
  end;
  
  { Interface for all servers }
  
  ILServer = interface
    function Listen(const APort: Word; const AIntf: string = LADDR_ANY): Boolean;
  end;

  { Interface for all clients }
  
  ILClient = interface
    function Connect(const Address: string; const APort: Word): Boolean; overload;
    function Connect: Boolean; overload;
  end;
  
  { TLComponent }

  TLComponent = class(TComponent, ILComponent)
   protected
    FHost: string;
    FPort: Word;
    FCreator: TLComponent;
    FActive: Boolean;
    procedure SetCreator(AValue: TLComponent); virtual;
   public
    constructor Create(aOwner: TComponent); override;
    procedure Disconnect(const Forced: Boolean = False); virtual; abstract;
    procedure CallAction; virtual; abstract;
   public
    SocketClass: TLSocketClass;
    property Host: string read FHost write FHost;
    property Port: Word read FPort write FPort;
    property Creator: TLComponent read FCreator write SetCreator;
    property Active: Boolean read FActive;
  end;
  
  { TLConnection
    Common ancestor for TLTcp and TLUdp classes. Holds Event properties
    and common variables. }

  TLConnection = class(TLComponent, ILDirect, ILServer, ILClient)
   protected
    FTimeVal: TTimeVal;
    FOnReceive: TLSocketEvent;
    FOnAccept: TLSocketEvent;
    FOnConnect: TLSocketEvent;
    FOnDisconnect: TLSocketEvent;
    FOnCanSend: TLSocketEvent;
    FOnError: TLSocketErrorEvent;
    FRootSock: TLSocket;
    FIterator: TLSocket;
    FID: Integer; // internal number for server
    FEventer: TLEventer;
    FEventerClass: TLEventerClass;
    FTimeout: Integer;
    FListenBacklog: Integer;
    FSession: TLSession;
   protected
    function InitSocket(aSocket: TLSocket): TLSocket; virtual;
    
    function GetConnected: Boolean; virtual; abstract;
    function GetCount: Integer; virtual;
    function GetItem(const i: Integer): TLSocket;
    
    function GetTimeout: Integer;
    procedure SetTimeout(const AValue: Integer);
    
    procedure SetEventer(Value: TLEventer);
    procedure SetSession(aSession: TLSession);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure ConnectAction(aSocket: TLHandle); virtual;
    procedure AcceptAction(aSocket: TLHandle); virtual;
    procedure ReceiveAction(aSocket: TLHandle); virtual;
    procedure SendAction(aSocket: TLHandle); virtual;
    procedure ErrorAction(aSocket: TLHandle; const msg: string); virtual;
    
    procedure ConnectEvent(aSocket: TLHandle); virtual;
    procedure DisconnectEvent(aSocket: TLHandle); virtual;
    procedure AcceptEvent(aSocket: TLHandle); virtual;
    procedure ReceiveEvent(aSocket: TLHandle); virtual;
    procedure CanSendEvent(aSocket: TLHandle); virtual;
    procedure ErrorEvent(aSocket: TLHandle; const msg: string); virtual;
    procedure EventerError(const msg: string; Sender: TLEventer);
    
    procedure RegisterWithEventer; virtual;
    
    procedure FreeSocks(const Forced: Boolean); virtual;
   public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    
    function Connect(const Address: string; const APort: Word): Boolean; virtual; overload;
    function Connect: Boolean; virtual; overload;
    
    function Listen(const APort: Word; const AIntf: string = LADDR_ANY): Boolean; virtual; abstract; overload;
    function Listen: Boolean; virtual; overload;
    
    function Get(out aData; const aSize: Integer; aSocket: TLSocket = nil): Integer; virtual; abstract;
    function GetMessage(out msg: string; aSocket: TLSocket = nil): Integer; virtual; abstract;
    
    function Send(const aData; const aSize: Integer; aSocket: TLSocket = nil): Integer; virtual; abstract;
    function SendMessage(const msg: string; aSocket: TLSocket = nil): Integer; virtual; abstract;
    
    function IterNext: Boolean; virtual; abstract;
    procedure IterReset; virtual; abstract;
   public
    property OnError: TLSocketErrorEvent read FOnError write FOnError;
    property OnReceive: TLSocketEvent read FOnReceive write FOnReceive;
    property OnDisconnect: TLSocketEvent read FOnDisconnect write FOnDisconnect;
    property OnCanSend: TLSocketEvent read FOnCanSend write FOnCanSend;
    property Socks[index: Integer]: TLSocket read GetItem; default;
    property Count: Integer read GetCount;
    property Connected: Boolean read GetConnected;
    property ListenBacklog: Integer read FListenBacklog write FListenBacklog;
    property Iterator: TLSocket read FIterator;
    property Timeout: Integer read GetTimeout write SetTimeout;
    property Eventer: TLEventer read FEventer write SetEventer;
    property EventerClass: TLEventerClass read FEventerClass write FEventerClass;
    property Session: TLSession read FSession write SetSession;
  end;
  
  { TLUdp }

  TLUdp = class(TLConnection)
   protected
    function InitSocket(aSocket: TLSocket): TLSocket; override;
    
    function GetConnected: Boolean; override;
    
    procedure ReceiveAction(aSocket: TLHandle); override;
    procedure ErrorAction(aSocket: TLHandle; const msg: string); override;
    
    function Bail(const msg: string): Boolean;
    
    procedure SetAddress(const Address: string);
   public
    constructor Create(aOwner: TComponent); override;
    
    function Connect(const Address: string; const APort: Word): Boolean; override;
    function Listen(const APort: Word; const AIntf: string = LADDR_ANY): Boolean; override;
    
    function Get(out aData; const aSize: Integer; aSocket: TLSocket = nil): Integer; override;
    function GetMessage(out msg: string; aSocket: TLSocket = nil): Integer; override;
    
    function SendMessage(const msg: string; aSocket: TLSocket = nil): Integer; override;
    function SendMessage(const msg: string; const Address: string): Integer; overload;
    
    function Send(const aData; const aSize: Integer; aSocket: TLSocket = nil): Integer; override;
    function Send(const aData; const aSize: Integer; const Address: string): Integer; overload;
    
    function IterNext: Boolean; override;
    procedure IterReset; override;

    procedure Disconnect(const Forced: Boolean = False); override;

    procedure CallAction; override;
  end;
  
  { TLTcp }

  TLTcp = class(TLConnection)
   protected
    FSocketNet: Integer;
    FCount: Integer;
    FReuseAddress: Boolean;
    function InitSocket(aSocket: TLSocket): TLSocket; override;

    function GetConnected: Boolean; override;
    function GetConnecting: Boolean;
    function GetCount: Integer; override;
    function GetValidSocket: TLSocket;

    procedure SetReuseAddress(const aValue: Boolean);
    procedure SetSocketNet(const aValue: Integer);

    procedure ConnectAction(aSocket: TLHandle); override;
    procedure AcceptAction(aSocket: TLHandle); override;
    procedure ReceiveAction(aSocket: TLHandle); override;
    procedure SendAction(aSocket: TLHandle); override;
    procedure ErrorAction(aSocket: TLHandle; const msg: string); override;

    function Bail(const msg: string; aSocket: TLSocket): Boolean;

    procedure SocketDisconnect(aSocket: TLSocket);
   public
    constructor Create(aOwner: TComponent); override;

    function Connect(const Address: string; const APort: Word): Boolean; override;
    function Listen(const APort: Word; const AIntf: string = LADDR_ANY): Boolean; override;

    function Get(out aData; const aSize: Integer; aSocket: TLSocket = nil): Integer; override;
    function GetMessage(out msg: string; aSocket: TLSocket = nil): Integer; override;

    function Send(const aData; const aSize: Integer; aSocket: TLSocket = nil): Integer; override;
    function SendMessage(const msg: string; aSocket: TLSocket = nil): Integer; override;

    function IterNext: Boolean; override;
    procedure IterReset; override;

    procedure CallAction; override;

    procedure Disconnect(const Forced: Boolean = False); override;
   public
    property Connecting: Boolean read GetConnecting;
    property OnAccept: TLSocketEvent read FOnAccept write FOnAccept;
    property OnConnect: TLSocketEvent read FOnConnect write FOnConnect;
    property ReuseAddress: Boolean read FReuseAddress write SetReuseAddress;
    property SocketNet: Integer read FSocketNet write SetSocketNet;
  end;

  { TLSession }

  TLSession = class(TComponent)
   protected
    FActive: Boolean;
   public
    procedure RegisterWithComponent(aConnection: TLConnection); virtual;

    procedure InitHandle(aHandle: TLHandle); virtual;

    procedure ReceiveEvent(aHandle: TLHandle); virtual;
    procedure SendEvent(aHandle: TLHandle); virtual;
    procedure ErrorEvent(aHandle: TLHandle; const msg: string); virtual;
    procedure ConnectEvent(aHandle: TLHandle); virtual;
    procedure AcceptEvent(aHandle: TLHandle); virtual;
    procedure DisconnectEvent(aHandle: TLHandle); virtual;

    procedure CallReceiveEvent(aHandle: TLHandle); inline;
    procedure CallSendEvent(aHandle: TLHandle); inline;
    procedure CallErrorEvent(aHandle: TLHandle; const msg: string); inline;
    procedure CallConnectEvent(aHandle: TLHandle); inline;
    procedure CallAcceptEvent(aHandle: TLHandle); inline;
    procedure CallDisconnectEvent(aHandle: TLHandle); inline;
   public
    property Active: Boolean read FActive;
  end;

implementation

//********************************TLSocket*************************************

constructor TLSocket.Create;
begin
  inherited Create;
  FHandle := INVALID_SOCKET;
  FListenBacklog := LDEFAULT_BACKLOG;
  FPrevSock := nil;
  FNextSock := nil;
  FSocketState := [ssCanSend];
  FConnectionStatus := scNone;
  FSocketType := SOCK_STREAM;
  FSocketNet := LAF_INET;
  FProtocol := LPROTO_TCP;
end;

destructor TLSocket.Destroy;
begin
  if Assigned(FOnFree) then
    FOnFree(Self);

  inherited Destroy; // important! must be called before disconnect
  Disconnect(True);
end;

function TLSocket.SetState(const aState: TLSocketState; const TurnOn: Boolean = True): Boolean;
begin
  Result := False;

  case aState of
    ssServerSocket      : if TurnOn then
                            FSocketState := FSocketState + [aState]
                          else
                            raise Exception.Create('Can not turn off server socket feature');
                            
    ssBlocking          : SetBlocking(TurnOn);
    ssReuseAddress      : SetReuseAddress(TurnOn);

    ssCanSend,
    ssCanReceive        : if TurnOn then
                            FSocketState := FSocketState + [aState]
                          else
                            FSocketState := FSocketState - [aState];
    
    ssSSLActive         : raise Exception.Create('Can not turn SSL/TLS on in TLSocket instance');
    ssNoDelay           : SetNoDelay(TurnOn);
  end;
  
  Result := True;
end;

procedure TLSocket.Disconnect(const Forced: Boolean = False);
begin
  if FDispose // don't do anything when already invalid
  and (FHandle = INVALID_SOCKET)
  and (FConnectionStatus = scNone) then
    Exit;

  if Forced then
    HardDisconnect
  else
    SoftDisconnect;
end;

function TLSocket.LogError(const msg: string; const ernum: Integer): Boolean;
begin
  Result := False;
  if Assigned(FOnError) then
    if ernum > 0 then
      FOnError(Self, msg + LStrError(ernum))
    else
      FOnError(Self, msg);
end;

function TLSocket.Bail(const msg: string; const ernum: Integer): Boolean;
begin
  Result := False; // return the result for the caller
  if FDispose then // why?
    Exit;
  Disconnect(True);
  LogError(msg, ernum);
end;

function TLSocket.GetPeerAddress: string;
begin
  Result := '';
  if FSocketType = SOCK_STREAM then
    Result := NetAddrtoStr(FAddress.IPv4.sin_addr)
  else
    Result := NetAddrtoStr(FPeerAddress.IPv4.sin_addr);
end;

function TLSocket.GetLocalAddress: string;
var
  a: TSockAddr;
  l: Integer;
begin
  Result := '';
  l := SizeOf(a);
  if fpGetSockName(FHandle, @a, @l) = 0 then
    Result := NetAddrToStr(a.sin_addr);
end;

function TLSocket.SendPossible: Boolean; inline;
begin
  Result := True;
  if FConnectionStatus <> scConnected then
    Exit(LogError('Can''t send when not connected', -1));

  if not (ssCanSend in FSocketState) then begin
    if not Assigned(FConnection)
    or not Assigned(FConnection.FOnCanSend) then
      LogError('Send buffer full, try again later', -1);
    Exit(False);
  end;

  if ssServerSocket in FSocketState then
    Exit(LogError('Can''t send on server socket', -1));
end;

function TLSocket.ReceivePossible: Boolean; inline;
begin
  Result := (FConnectionStatus in [scConnected, scDisconnecting])
    and (ssCanReceive in FSocketState) and not (ssServerSocket in FSocketState);
end;

procedure TLSocket.SetOptions;
begin
  SetBlocking(FBlocking);
end;

procedure TLSocket.SetBlocking(const aValue: Boolean);
begin
  if FHandle >= 0 then // we already set our socket
    if not lCommon.SetBlocking(FHandle, aValue) then
      Bail('Error on SetBlocking', LSocketError)
    else begin
      FBlocking := aValue;
      if aValue then
        FSocketState := FSocketState + [ssBlocking]
      else
        FSocketState := FSocketState - [ssBlocking];
    end;
end;

procedure TLSocket.SetReuseAddress(const aValue: Boolean);
begin
  if FConnectionStatus = scNone then begin
    FReuseAddress := aValue;
    if aValue then
      FSocketState := FSocketState + [ssReuseAddress]
    else
      FSocketState := FSocketState - [ssReuseAddress];
  end;
end;

procedure TLSocket.HardDisconnect(const NoShutdown: Boolean = False);
var
  NeedsShutdown: Boolean;
begin
  NeedsShutdown := (FConnectionStatus = scConnected) and (FSocketType = SOCK_STREAM)
               and (not (ssServerSocket in FSocketState));
  if NoShutdown then
    NeedsShutdown := False;

  FDispose := True;
  FSocketState := FSocketState + [ssCanSend, ssCanReceive];
  FIgnoreWrite := True;
  if FConnectionStatus in [scConnected, scConnecting, scDisconnecting] then begin
    FConnectionStatus := scNone;
    if NeedsShutdown then
      if fpShutDown(FHandle, SHUT_RDWR) <> 0 then
        LogError('Shutdown error', LSocketError);

    if Assigned(FEventer) then
      FEventer.UnregisterHandle(Self);

    if CloseSocket(FHandle) <> 0 then
      LogError('Closesocket error', LSocketError);
    FHandle := INVALID_SOCKET;
  end;
end;

procedure TLSocket.SoftDisconnect;
begin
  if FConnectionStatus in [scConnected, scConnecting] then begin
    if  (FConnectionStatus = scConnected) and (not (ssServerSocket in FSocketState))
    and (FSocketType = SOCK_STREAM) then begin
      FConnectionStatus := scDisconnecting;
      if fpShutDown(FHandle, SHUT_WR) <> 0 then
        LogError('Shutdown error', LSocketError);
    end else
      HardDisconnect; // UDP or ServerSocket
  end;
end;

procedure TLSocket.SetNoDelay(const aValue: Boolean);
begin
  if FHandle >= 0 then begin // we already set our socket
    if not lCommon.SetNoDelay(FHandle, aValue) then
      Bail('Error on SetNoDelay', LSocketError)
    else begin
      if aValue then
        FSocketState := FSocketState + [ssNoDelay]
      else
        FSocketState := FSocketState - [ssNoDelay];
    end;
  end;
end;

function TLSocket.GetMessage(out msg: string): Integer;
begin
  Result := 0;
  SetLength(msg, BUFFER_SIZE);
  SetLength(msg, Get(PChar(msg)^, Length(msg)));
  Result := Length(msg);
end;

function TLSocket.Get(out aData; const aSize: Integer): Integer;
begin
  Result := 0;
  
  if aSize = 0 then
    raise Exception.Create('Invalid buffer size 0 in Get');

  if ReceivePossible then begin
    Result := DoGet(aData, aSize);

    if Result = 0 then
      if FSocketType = SOCK_STREAM then
        Disconnect(True)
      else begin
        Bail('Receive Error [0 on recvfrom with UDP]', 0);
        Exit(0);
      end;
      
    Result := HandleResult(Result, soReceive);
  end;
end;

function TLSocket.GetConnected: Boolean;
begin
  Result := (FConnectionStatus = scConnected);
end;

function TLSocket.GetConnecting: Boolean;
begin
  Result := FConnectionStatus = scConnecting;
end;

function TLSocket.GetConnectionStatus: TLSocketConnectionStatus;
begin
  Result := FConnectionStatus;
end;

function TLSocket.GetIPAddressPointer: psockaddr;
begin
  case FSocketNet of
    LAF_INET  : Result := psockaddr(@FAddress.IPv4);
    LAF_INET6 : Result := psockaddr(@FAddress.IPv6);
  else
    raise Exception.Create('Unknown socket network type (not IPv4 or IPv6)');
  end;
end;

function TLSocket.GetIPAddressLength: TSocklen;
begin
  case FSocketNet of
    LAF_INET  : Result := SizeOf(FAddress.IPv4);
    LAF_INET6 : Result := SizeOf(FAddress.IPv6);
  else
    raise Exception.Create('Unknown socket network type (not IPv4 or IPv6)');
  end;
end;

function TLSocket.SetupSocket(const APort: Word; const Address: string): Boolean;
var
  Done: Boolean;
  Arg, Opt: Integer;
begin
  Result := false;
  if FConnectionStatus = scNone then begin
    Done := true;
    FHandle := fpSocket(FSocketNet, FSocketType, FProtocol);
    if FHandle = INVALID_SOCKET then
      Exit(Bail('Socket error', LSocketError));
    SetOptions;

    Arg := 1;
    if FSocketType = SOCK_DGRAM then begin
      if fpsetsockopt(FHandle, SOL_SOCKET, SO_BROADCAST, @Arg, Sizeof(Arg)) = SOCKET_ERROR then
        Exit(Bail('SetSockOpt error', LSocketError));
    end else if FReuseAddress then begin
      Opt := SO_REUSEADDR;
      {$ifdef WIN32} // I expect 64 has it oddly, so screw them for now
      if (Win32Platform = 2) and (Win32MajorVersion >= 5) then
        Opt := Integer(not Opt);
      {$endif}
      if fpsetsockopt(FHandle, SOL_SOCKET, Opt, @Arg, Sizeof(Arg)) = SOCKET_ERROR then
        Exit(Bail('SetSockOpt error', LSocketError));
    end;
    
    {$ifdef darwin}
    Arg := 1;
    if fpsetsockopt(FHandle, SOL_SOCKET, SO_NOSIGPIPE, @Arg, Sizeof(Arg)) = SOCKET_ERROR then
      Exit(Bail('SetSockOpt error', LSocketError));
    {$endif}
    
    FillAddressInfo(FAddress, FSocketNet, Address, aPort);
    FillAddressInfo(FPeerAddress, FSocketNet, LADDR_BR, aPort);

    Result  :=  Done;
  end;
end;

function TLSocket.DoSend(const aData; const aSize: Integer): Integer;
var
  AddressLength: Longint = SizeOf(FPeerAddress);
begin
  if FSocketType = SOCK_STREAM then
    Result := Sockets.fpSend(FHandle, @aData, aSize, LMSG)
  else
    Result := sockets.fpsendto(FHandle, @aData, aSize, LMSG, @FPeerAddress, AddressLength);
end;

function TLSocket.DoGet(out aData; const aSize: Integer): Integer;
var
  AddressLength: Longint = SizeOf(FPeerAddress);
begin
  if FSocketType = SOCK_STREAM then
    Result := sockets.fpRecv(FHandle, @aData, aSize, LMSG)
  else
    Result := sockets.fpRecvfrom(FHandle, @aData, aSize, LMSG, @FPeerAddress, @AddressLength);
end;

function TLSocket.HandleResult(const aResult: Integer; aOp: TLSocketOperation): Integer;
const
  GSStr: array[TLSocketOperation] of string = ('Send', 'Get');
var
  LastError: Longint;
begin
  Result := aResult;
  if Result = SOCKET_ERROR then begin
    LastError := LSocketError;
    if IsBlockError(LastError) then case aOp of
      soSend:
         begin
           FSocketState := FSocketState - [ssCanSend];
           IgnoreWrite := False;
         end;
      soReceive:
         begin
           FSocketState := FSocketState - [ssCanReceive];
           IgnoreRead := False;
         end;
    end else if IsNonFatalError(LastError) then
      LogError(GSStr[aOp] + ' error', LastError) // non fatals don't cause disconnect
    else if (aOp = soSend) and IsPipeError(LastError) then begin
      LogError(GSStr[aOp] + ' error', LastError);
      HardDisconnect(True); {$warning check if we need aOp = soSend in the IF, perhaps bad recv is possible?}
    end else
      Bail(GSStr[aOp] + ' error', LastError);
      
    Result := 0;
  end;
end;

function TLSocket.GetLocalPort: Word;
begin
  Result := ntohs(FAddress.IPv4.sin_port);
end;

function TLSocket.GetPeerPort: Word;
begin
  Result := ntohs(FPeerAddress.IPv4.sin_port);
end;

function TLSocket.Listen(const APort: Word; const AIntf: string = LADDR_ANY): Boolean;
begin
  Result := False;

  if FConnectionStatus <> scNone then
    Disconnect(True);

  SetupSocket(APort, AIntf);
  if fpBind(FHandle, GetIPAddressPointer, GetIPAddressLength) = SOCKET_ERROR then
    Bail('Error on bind', LSocketError)
  else
    Result := true;
  if (FSocketType = SOCK_STREAM) and Result then
    if fpListen(FHandle, FListenBacklog) = SOCKET_ERROR then
      Result := Bail('Error on Listen', LSocketError)
    else
      Result := true;
end;

function TLSocket.Accept(const sersock: TSocket): Boolean;
var
  AddressLength: tsocklen;
begin
  Result := false;
  AddressLength := GetIPAddressLength;

  if FConnectionStatus <> scNone then
    Disconnect(True);

  FHandle := fpAccept(sersock, GetIPAddressPointer, @AddressLength);
  if FHandle <> INVALID_SOCKET then begin
    SetOptions;
    FIsAcceptor := True;
    Result := true;
  end else
    Bail('Error on accept', LSocketError);
end;

function TLSocket.Connect(const Address: string; const aPort: Word): Boolean;
begin
  Result := False;
  
  if FConnectionStatus <> scNone then
    Disconnect(True);
    
  if SetupSocket(APort, Address) then begin
    fpConnect(FHandle, GetIPAddressPointer, GetIPAddressLength);
    FConnectionStatus := scConnecting;
    Result := True;
  end;
end;

function TLSocket.SendMessage(const msg: string): Integer;
begin
  Result := Send(PChar(msg)^, Length(msg));
end;

function TLSocket.Send(const aData; const aSize: Integer): Integer;
begin
  Result := 0;
  
  if aSize = 0 then
    raise Exception.Create('Invalid buffersize 0 in Send');

  if SendPossible then begin
    if aSize <= 0 then begin
      LogError('Send error: Size <= 0', -1);
      Exit(0);
    end;

    Result := HandleResult(DoSend(aData, aSize), soSend);
  end;
end;

//*******************************TLComponent*********************************

procedure TLComponent.SetCreator(AValue: TLComponent);
begin
  FCreator := aValue;
end;

constructor TLComponent.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FCreator := Self;
end;

//*******************************TLConnection*********************************

constructor TLConnection.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  FHost := '';
  FPort := 0;
  FListenBacklog := LDEFAULT_BACKLOG;
  FTimeout := 0;
  SocketClass := TLSocket;
  FOnReceive := nil;
  FOnError := nil;
  FOnDisconnect := nil;
  FOnCanSend := nil;
  FOnConnect := nil;
  FOnAccept := nil;
  FTimeVal.tv_sec := 0;
  FTimeVal.tv_usec := 0;
  FIterator := nil;
  FEventer := nil;
  FEventerClass := BestEventerClass;
end;

destructor TLConnection.Destroy;
begin
  FreeSocks(True);
  if Assigned(FEventer) then
    FEventer.DeleteRef;
  inherited Destroy;
end;

function TLConnection.Connect(const Address: string; const APort: Word
  ): Boolean;
begin
  FHost := Address;
  FPort := aPort;
  Result := False;
end;

function TLConnection.Connect: Boolean;
begin
  Result := Connect(FHost, FPort);
end;

function TLConnection.Listen: Boolean;
begin
  Result := Listen(FPort, FHost);
end;

procedure TLConnection.SetSession(aSession: TLSession);
begin
  if FSession = aSession then Exit;

  if FActive then
    raise Exception.Create('Cannot change session on active component');

  FSession := aSession;
  if Assigned(FSession) then begin
    FSession.FreeNotification(Self);
    FSession.RegisterWithComponent(Self);
  end;
end;

procedure TLConnection.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  
  if (Operation = opRemove) and (AComponent = FSession) then
    FSession := nil;
end;

function TLConnection.InitSocket(aSocket: TLSocket): TLSocket;
begin
  FActive := True; // once we got a socket, we're considered active
  aSocket.OnRead := @ReceiveAction;
  aSocket.OnWrite := @SendAction;
  aSocket.OnError := @ErrorAction;
  aSocket.ListenBacklog := FListenBacklog;
  aSocket.FCreator := FCreator;
  aSocket.FConnection := Self;
  aSocket.FSession := FSession;
  if Assigned(FSession) then
    FSession.InitHandle(aSocket);
  Result := aSocket;
end;

function TLConnection.GetCount: Integer;
begin
  Result := 1;
end;

function TLConnection.GetItem(const i: Integer): TLSocket;
var
  Tmp: TLSocket;
  Jumps: Integer;
begin
  Result := nil;
  Tmp := FRootSock;
  Jumps := 0;
  while Assigned(Tmp.NextSock) and (Jumps < i) do begin
    Tmp := Tmp.NextSock;
    Inc(Jumps);
  end;
  if Jumps = i then
    Result := Tmp;
end;

function TLConnection.GetTimeout: Integer;
begin
  if Assigned(FEventer) then
    Result := FEventer.Timeout
  else
    Result := FTimeout;
end;

procedure TLConnection.ConnectAction(aSocket: TLHandle);
begin
end;

procedure TLConnection.AcceptAction(aSocket: TLHandle);
begin
end;

procedure TLConnection.ReceiveAction(aSocket: TLHandle);
begin
end;

procedure TLConnection.SendAction(aSocket: TLHandle);
begin
  with TLSocket(aSocket) do begin
    SetState(ssCanSend);
    IgnoreWrite := True;

    if Assigned(FSession) then
      FSession.SendEvent(aSocket)
    else
      CanSendEvent(aSocket);
  end;
end;

procedure TLConnection.ErrorAction(aSocket: TLHandle; const msg: string);
begin
end;

procedure TLConnection.ConnectEvent(aSocket: TLHandle);
begin
  if Assigned(FOnConnect) then
    FOnConnect(TLSocket(aSocket));
end;

procedure TLConnection.DisconnectEvent(aSocket: TLHandle);
begin
  if Assigned(FOnDisconnect) then
    FOnDisconnect(TLSocket(aSocket));
end;

procedure TLConnection.AcceptEvent(aSocket: TLHandle);
begin
  if Assigned(FOnAccept) then
    FOnAccept(TLSocket(aSocket));
end;

procedure TLConnection.ReceiveEvent(aSocket: TLHandle);
begin
  if Assigned(FOnReceive) then
    FOnReceive(TLSocket(aSocket));
end;

procedure TLConnection.CanSendEvent(aSocket: TLHandle);
begin
  if Assigned(FOnCanSend) then
    FOnCanSend(TLSocket(aSocket));
end;

procedure TLConnection.ErrorEvent(aSocket: TLHandle; const msg: string);
begin
  if Assigned(FOnError) then
    FOnError(msg, TLSocket(aSocket));
end;

procedure TLConnection.SetTimeout(const AValue: Integer);
begin
  if Assigned(FEventer) then
    FEventer.Timeout := aValue;
  FTimeout := aValue;
end;

procedure TLConnection.SetEventer(Value: TLEventer);
begin
  if Assigned(FEventer) then
    FEventer.DeleteRef;
  FEventer := Value;
  FEventer.AddRef;
end;

procedure TLConnection.EventerError(const msg: string; Sender: TLEventer);
begin
  ErrorEvent(nil, msg);
end;

procedure TLConnection.RegisterWithEventer;
begin
  if not Assigned(FEventer) then begin
    FEventer := FEventerClass.Create;
    FEventer.OnError := @EventerError;
  end;

  if Assigned(FRootSock) then
    FEventer.AddHandle(FRootSock);

  if (FEventer.Timeout = 0) and (FTimeout <> 0) then
    FEventer.Timeout := FTimeout
  else
    FTimeout := FEventer.Timeout;
end;

procedure TLConnection.FreeSocks(const Forced: Boolean);
var
  Tmp, Tmp2: TLSocket;
begin
  Tmp := FRootSock;
  while Assigned(Tmp) do begin
    Tmp2 := Tmp;
    Tmp := Tmp.NextSock;
    if Forced // forced, already closed or server socket
    or not (Tmp2.ConnectionStatus in [scConnected, scConnecting, scDisconnecting])
    or (ssServerSocket in Tmp2.SocketState) then
      Tmp2.Free
    else
      Tmp2.Disconnect(Forced);
  end;
end;

//*******************************TLUdp*********************************

constructor TLUdp.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FTimeVal.tv_usec := 0;
  FTimeVal.tv_sec := 0;
end;

procedure TLUdp.Disconnect(const Forced: Boolean = False);
begin
  if Assigned(FRootSock) then begin
    FRootSock.Disconnect(True); // true on UDP it always goes there anyways
    FRootSock := nil; // even if the old one exists, eventer takes care of it
  end;
end;

function TLUdp.Connect(const Address: string; const APort: Word): Boolean;
begin
  Result := inherited Connect(Address, aPort);

  if Assigned(FRootSock) and (FRootSock.FConnectionStatus <> scNone) then
    Disconnect(True);

  FRootSock := InitSocket(SocketClass.Create);
  FIterator := FRootSock;

  Result := FRootSock.SetupSocket(APort, LADDR_ANY);
  
  if Result then begin
    FillAddressInfo(FRootSock.FPeerAddress, FRootSock.FSocketNet, Address, aPort);
    FRootSock.FConnectionStatus := scConnected;
    RegisterWithEventer;
  end;
end;

function TLUdp.Listen(const APort: Word; const AIntf: string = LADDR_ANY): Boolean;
begin
  Result := False;

  if Assigned(FRootSock) and (FRootSock.FConnectionStatus <> scNone) then
    Disconnect(True);

  FRootSock := InitSocket(SocketClass.Create);
  FIterator := FRootSock;
  
  if FRootSock.Listen(APort, AIntf) then begin
    FillAddressInfo(FRootSock.FPeerAddress, FRootSock.FSocketNet, LADDR_BR, aPort);
  
    FRootSock.FConnectionStatus := scConnected;
    RegisterWithEventer;
    Result := True;
  end;
end;

function TLUdp.Bail(const msg: string): Boolean;
begin
  Result := False;

  Disconnect(True);

  if Assigned(FSession) then
    FSession.ErrorEvent(nil, msg)
  else
    ErrorEvent(FRootSock, msg);
end;

procedure TLUdp.SetAddress(const Address: string);
var
  n: Integer;
  s: string;
  p: Word;
begin
  n := Pos(':', Address);
  if n > 0 then begin
    s := Copy(Address, 1, n-1);
    p := Word(StrToInt(Copy(Address, n+1, Length(Address))));

    FillAddressInfo(FRootSock.FPeerAddress, FRootSock.FSocketNet, s, p);
  end else
    FillAddressInfo(FRootSock.FPeerAddress, FRootSock.FSocketNet, Address,
                                            FRootSock.PeerPort);
end;

function TLUdp.InitSocket(aSocket: TLSocket): TLSocket;
begin
  Result := FRootSock;
  if not Assigned(FRootSock) then begin
    aSocket.SocketType := SOCK_DGRAM;
    aSocket.Protocol := LPROTO_UDP;
    Result := inherited InitSocket(aSocket); // call last, to make sure sessions get their turn in overriding
  end;
end;

procedure TLUdp.ReceiveAction(aSocket: TLHandle);
begin
  with TLSocket(aSocket) do begin
    SetState(ssCanReceive);
    if Assigned(FSession) then
      FSession.ReceiveEvent(aSocket)
    else
      ReceiveEvent(aSocket);
  end;
end;

procedure TLUdp.ErrorAction(aSocket: TLHandle; const msg: string);
begin
  if Assigned(FSession) then
    FSession.ErrorEvent(aSocket, msg)
  else
    ErrorEvent(aSocket, msg);
end;

function TLUdp.IterNext: Boolean;
begin
  Result := False;
end;

procedure TLUdp.IterReset;
begin
end;

procedure TLUdp.CallAction;
begin
  if Assigned(FEventer) then
    FEventer.CallAction;
end;

function TLUdp.GetConnected: Boolean;
begin
  Result := False;
  if Assigned(FRootSock) then
  Result := FRootSock.ConnectionStatus = scConnected;
end;

function TLUdp.Get(out aData; const aSize: Integer; aSocket: TLSocket): Integer;
begin
  Result := 0;
  if Assigned(FRootSock) then
    Result := FRootSock.Get(aData, aSize);
end;

function TLUdp.GetMessage(out msg: string; aSocket: TLSocket): Integer;
begin
  Result := 0;
  if Assigned(FRootSock) then
    Result := FRootSock.GetMessage(msg);
end;

function TLUdp.SendMessage(const msg: string; aSocket: TLSocket = nil): Integer;
begin
  Result := 0;
  if Assigned(FRootSock) then
    Result := FRootSock.SendMessage(msg)
end;

function TLUdp.SendMessage(const msg: string; const Address: string): Integer;
begin
  Result := 0;
  if Assigned(FRootSock) then begin
    SetAddress(Address);
    Result := FRootSock.SendMessage(msg)
  end;
end;

function TLUdp.Send(const aData; const aSize: Integer; aSocket: TLSocket): Integer;
begin
  Result := 0;
  if Assigned(FRootSock) then
    Result := FRootSock.Send(aData, aSize)
end;

function TLUdp.Send(const aData; const aSize: Integer; const Address: string
  ): Integer;
begin
  Result := 0;
  if Assigned(FRootSock) then begin
    SetAddress(Address);
    Result := FRootSock.Send(aData, aSize);
  end;
end;

//******************************TLTcp**********************************

constructor TLTcp.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FSocketNet := LAF_INET; // default to IPv4
  FIterator  := nil;
  FCount     := 0;
  FRootSock  := nil;
end;

function TLTcp.Connect(const Address: string; const APort: Word): Boolean;
begin
  Result := inherited Connect(Address, aPort);
  
  if Assigned(FRootSock) then
    Disconnect(True);
    
  FRootSock := InitSocket(SocketClass.Create);
  Result := FRootSock.Connect(Address, aPort);
  
  if Result then begin
    Inc(FCount);
    FIterator := FRootSock;
    RegisterWithEventer;
  end else begin
    FreeAndNil(FRootSock); // one possible use, since we're not in eventer yet
    FIterator := nil;
  end;
end;

function TLTcp.Listen(const APort: Word; const AIntf: string = LADDR_ANY): Boolean;
begin
  Result := false;
  
  if Assigned(FRootSock) then
    Disconnect(True);
  
  FRootSock := InitSocket(SocketClass.Create);
  FRootSock.SetReuseAddress(FReuseAddress);
  if FRootSock.Listen(APort, AIntf) then begin
    FRootSock.SetState(ssServerSocket);
    FRootSock.FConnectionStatus := scConnected;
    FIterator := FRootSock;
    Inc(FCount);
    RegisterWithEventer;
    Result := true;
  end;
end;

function TLTcp.Bail(const msg: string; aSocket: TLSocket): Boolean;
begin
  Result  :=  False;

  if Assigned(FSession) then
    FSession.ErrorEvent(aSocket, msg)
  else
    ErrorEvent(aSocket, msg);

  if Assigned(aSocket) then
    aSocket.Disconnect(True)
  else
    Disconnect(True);
end;

procedure TLTcp.SocketDisconnect(aSocket: TLSocket);
begin
  if aSocket = FIterator then begin
    if Assigned(FIterator.NextSock) then
      FIterator := FIterator.NextSock
    else if Assigned(FIterator.PrevSock) then
      FIterator := FIterator.PrevSock
    else FIterator := nil; // NOT iterreset, not reorganized yet
    if Assigned(FIterator) and (ssServerSocket in FIterator.SocketState) then
      FIterator := nil;
  end;

  if aSocket = FRootSock then
    FRootSock := aSocket.NextSock;
  if Assigned(aSocket.PrevSock) then
    aSocket.PrevSock.NextSock := aSocket.NextSock;
  if Assigned(aSocket.NextSock) then
    aSocket.NextSock.PrevSock := aSocket.PrevSock;
    
  Dec(FCount);
end;

function TLTcp.InitSocket(aSocket: TLSocket): TLSocket;
begin
  aSocket.SocketType := SOCK_STREAM;
  aSocket.Protocol := LPROTO_TCP;
  aSocket.SocketNet := FSocketNet;
  aSocket.FOnFree := @SocketDisconnect;

  Result := inherited InitSocket(aSocket); // call last to make sure session can override options
end;

function TLTcp.IterNext: Boolean;
begin
  Result := False;
  if not Assigned(FIterator) then
    Exit;

  if Assigned(FIterator.NextSock) then begin
    FIterator := FIterator.NextSock;
    Result := True;
  end else IterReset;
end;

procedure TLTcp.IterReset;
begin
  FIterator := FRootSock;
end;

procedure TLTcp.Disconnect(const Forced: Boolean = False);
begin
  FreeSocks(Forced);
  if Forced then begin // only unlick for forced, we still need to wait otherwise!
    FRootSock := nil;
    FIterator := nil;
    FCount := 0;
  end;
end;

procedure TLTcp.CallAction;
begin
  if Assigned(FEventer) then
    FEventer.CallAction;
end;

procedure TLTcp.ConnectAction(aSocket: TLHandle);
var
  addr4: TInetSockAddr;
  addr6: TInetSockAddr6;
  n, l: Longint;
begin
  with TLSocket(aSocket) do begin
    case SocketNet of
      LAF_INET  : begin
                    l := SizeOf(addr4);
                    n := Sockets.fpGetPeerName(FHandle, @addr4, @l);
                  end;
      LAF_INET6 : begin
                    l := SizeOf(addr6);
                    n := Sockets.fpGetPeerName(FHandle, @addr6, @l);
                  end;
    else
      raise Exception.Create('Unknown SocketNet in ConnectAction');
    end;
    if n  <> 0 then
      Self.Bail('Error on connect: connection refused', TLSocket(aSocket))
    else begin
      FConnectionStatus := scConnected;
      IgnoreWrite := True;
      if Assigned(FSession) then
        FSession.ConnectEvent(aSocket)
      else
        ConnectEvent(aSocket);
    end;
  end;
end;

procedure TLTcp.AcceptAction(aSocket: TLHandle);
var
  Tmp: TLSocket;
begin
  Tmp := InitSocket(SocketClass.Create);
  
  if Tmp.Accept(FRootSock.FHandle) then begin
    if Assigned(FRootSock.FNextSock) then begin
      Tmp.FNextSock := FRootSock.FNextSock;
      FRootSock.FNextSock.FPrevSock := Tmp;
    end;
    
    FRootSock.FNextSock := Tmp;
    Tmp.FPrevSock := FRootSock;
    
    if not Assigned(FIterator)      // if we don't have (bug?) an iterator yet
    or (ssServerSocket in FIterator.SocketState) then // or if it's the first socket accepted
      FIterator := Tmp;  // assign it as iterator (don't assign later acceptees)
      
    Inc(FCount);
    FEventer.AddHandle(Tmp);
    
    Tmp.FConnectionStatus := scConnected;
    Tmp.IgnoreWrite := True;

    if Assigned(FSession) then
      FSession.AcceptEvent(Tmp)
    else
      AcceptEvent(Tmp);
  end else
    Tmp.Free;
end;

procedure TLTcp.ReceiveAction(aSocket: TLHandle);
begin
  if (TLSocket(aSocket) = FRootSock) and (ssServerSocket in TLSocket(aSocket).SocketState) then
    AcceptAction(aSocket)
  else with TLSocket(aSocket) do begin
    if FConnectionStatus in [scConnected, scDisconnecting] then begin
      SetState(ssCanReceive);
      if Assigned(FSession) then
        FSession.ReceiveEvent(aSocket)
      else
        ReceiveEvent(aSocket);

      if not (FConnectionStatus = scConnected) then begin
        DisconnectEvent(aSocket);
        aSocket.Free;
      end;
    end;
  end;
end;

procedure TLTcp.SendAction(aSocket: TLHandle);
begin
  with TLSocket(aSocket) do begin
    if FConnectionStatus = scConnecting then
      ConnectAction(aSocket)
    else
      inherited;
  end;
end;

procedure TLTcp.ErrorAction(aSocket: TLHandle; const msg: string);
begin
  if TLSocket(aSocket).ConnectionStatus = scConnecting then begin
    Self.Bail('Error on connect: connection refused', TLSocket(aSocket));
    Exit;
  end;
  
  if Assigned(FSession) then
    FSession.ErrorEvent(aSocket, msg)
  else
    ErrorEvent(aSocket, msg);
end;

function TLTcp.GetConnected: Boolean;
var
  Tmp: TLSocket;
begin
  Result := False;
  Tmp := FRootSock;
  while Assigned(Tmp) do begin
    if Tmp.ConnectionStatus = scConnected then begin
      Result := True;
      Exit;
    end else Tmp := Tmp.NextSock;
  end;
end;

function TLTcp.GetConnecting: Boolean;
begin
  Result := False;
  if Assigned(FRootSock) then
    Result := FRootSock.ConnectionStatus = scConnecting;
end;

function TLTcp.GetCount: Integer;
begin
  Result := FCount;
end;

function TLTcp.GetValidSocket: TLSocket;
begin
  Result := nil;
  
  if Assigned(FIterator) and not (ssServerSocket in FIterator.SocketState) then
    Result := FIterator
  else if Assigned(FRootSock) and Assigned(FRootSock.FNextSock) then
    Result := FRootSock.FNextSock;
end;

procedure TLTcp.SetReuseAddress(const aValue: Boolean);
begin
  if not Assigned(FRootSock)
  or (FRootSock.FConnectionStatus = scNone) then
    FReuseAddress := aValue;
end;

procedure TLTcp.SetSocketNet(const aValue: Integer);
begin
  if GetConnected then
    raise Exception.Create('Cannot set socket network on a connected system');

  FSocketNet := aValue;
end;

function TLTcp.Get(out aData; const aSize: Integer; aSocket: TLSocket): Integer;
begin
  Result := 0;

  if not Assigned(aSocket) then
    aSocket := GetValidSocket;

  if Assigned(aSocket) then
    Result := aSocket.Get(aData, aSize)
  else
    Bail('No connected socket to get through', nil);
end;

function TLTcp.GetMessage(out msg: string; aSocket: TLSocket): Integer;
begin
  Result := 0;

  if not Assigned(aSocket) then
    aSocket := GetValidSocket;

  if Assigned(aSocket) then
    Result := aSocket.GetMessage(msg)
  else
    Bail('No connected socket to get through', nil);
end;

function TLTcp.Send(const aData; const aSize: Integer; aSocket: TLSocket): Integer;
begin
  Result := 0;

  if not Assigned(aSocket) then
    aSocket := GetValidSocket;

  if Assigned(aSocket) then
    Result := aSocket.Send(aData, aSize)
  else
    Bail('No connected socket to send through', nil);
end;

function TLTcp.SendMessage(const msg: string; aSocket: TLSocket): Integer;
begin
  Result := Send(PChar(msg)^, Length(msg), aSocket);
end;

//*******************************TLSession*********************************

procedure TLSession.RegisterWithComponent(aConnection: TLConnection);
begin
  if not Assigned(aConnection) then
    raise Exception.Create('Cannot register session with nil connection');
end;

procedure TLSession.InitHandle(aHandle: TLHandle);
begin
  TLSocket(aHandle).FSession := Self;
end;

procedure TLSession.ReceiveEvent(aHandle: TLHandle);
begin
  FActive := True;
  CallReceiveEvent(aHandle);
end;

procedure TLSession.SendEvent(aHandle: TLHandle);
begin
  FActive := True;
  CallSendEvent(aHandle);
end;

procedure TLSession.ErrorEvent(aHandle: TLHandle; const msg: string);
begin
  FActive := True;
  CallErrorEvent(aHandle, msg);
end;

procedure TLSession.ConnectEvent(aHandle: TLHandle);
begin
  FActive := True;
  CallConnectEvent(aHandle);
end;

procedure TLSession.AcceptEvent(aHandle: TLHandle);
begin
  FActive := True;
  CallAcceptEvent(aHandle);
end;

procedure TLSession.DisconnectEvent(aHandle: TLHandle);
begin
  FActive := True;
  CallDisconnectEvent(aHandle);
end;

procedure TLSession.CallReceiveEvent(aHandle: TLHandle); inline;
begin
  TLSocket(aHandle).FConnection.ReceiveEvent(TLSocket(aHandle));
end;

procedure TLSession.CallSendEvent(aHandle: TLHandle); inline;
begin
  TLSocket(aHandle).FConnection.CanSendEvent(TLSocket(aHandle));
end;

procedure TLSession.CallErrorEvent(aHandle: TLHandle; const msg: string);
  inline;
begin
  TLSocket(aHandle).FConnection.ErrorEvent(TLSocket(aHandle), msg);
end;

procedure TLSession.CallConnectEvent(aHandle: TLHandle); inline;
begin
  TLSocket(aHandle).FConnection.ConnectEvent(TLSocket(aHandle));
end;

procedure TLSession.CallAcceptEvent(aHandle: TLHandle); inline;
begin
  TLSocket(aHandle).FConnection.AcceptEvent(TLSocket(aHandle));
end;

procedure TLSession.CallDisconnectEvent(aHandle: TLHandle); inline;
begin
  TLSocket(aHandle).FConnection.DisconnectEvent(TLSocket(aHandle));
end;


end.

