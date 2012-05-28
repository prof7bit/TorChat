unit lNetSSL;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, cTypes, OpenSSL,
  lNet, lEvents;
  
type
  TLSSLMethod = (msSSLv2or3, msSSLv2, msSSLv3, msTLSv1);
  TLSSLStatus = (slNone, slConnect, slActivateTLS, slShutdown);

  TLPasswordCB = function(buf: pChar; num, rwflag: cInt; userdata: Pointer): cInt; cdecl;

  { TLSSLSocket }

  TLSSLSocket = class(TLSocket)
   protected
    FSSL: PSSL;
    FSSLContext: PSSL_CTX;
    FSSLStatus: TLSSLStatus;
    function GetConnected: Boolean; override; deprecated;
    function GetConnectionStatus: TLSocketConnectionStatus; override;

    function DoSend(const aData; const aSize: Integer): Integer; override;
    function DoGet(out aData; const aSize: Integer): Integer; override;
    
    function HandleResult(const aResult: Integer; aOp: TLSocketOperation): Integer; override;

    function SetActiveSSL(const AValue: Boolean): Boolean;

    procedure SetupSSLSocket;
    procedure ActivateTLSEvent;
    procedure ConnectEvent;
    procedure AcceptEvent;
    procedure ConnectSSL;
    procedure AcceptSSL;
    procedure ShutdownSSL;

    function LogError(const msg: string; const ernum: Integer): Boolean; override;
   public
    destructor Destroy; override;
    
    function SetState(const aState: TLSocketState; const TurnOn: Boolean = True): Boolean; override;

    procedure Disconnect(const Forced: Boolean = False); override;
   public
    property SSLStatus: TLSSLStatus read FSSLStatus;
  end;

  { TLSSLSession }

  TLSSLSession = class(TLSession)
   protected
    FOnSSLConnect: TLSocketEvent;
    FOnSSLAccept: TLSocketEvent;
    FSSLActive: Boolean;
    FSSLContext: PSSL_CTX;
    FPassword: string;
    FCAFile: string;
    FKeyFile: string;
    FMethod: TLSSLMethod;
    FPasswordCallback: TLPasswordCB;
    
    procedure CallOnSSLConnect(aSocket: TLSocket);
    procedure CallOnSSLAccept(aSocket: TLSocket);

    procedure SetSSLActive(const AValue: Boolean);
    procedure SetCAFile(AValue: string);
    procedure SetKeyFile(AValue: string);
    procedure SetPassword(const AValue: string);
    procedure SetMethod(const AValue: TLSSLMethod);
    procedure SetPasswordCallback(const AValue: TLPasswordCB);
    
    procedure CreateSSLContext; virtual;
   public
    constructor Create(aOwner: TComponent); override;
    
    procedure RegisterWithComponent(aConnection: TLConnection); override;
    
    procedure InitHandle(aHandle: TLHandle); override;
    
    procedure ConnectEvent(aHandle: TLHandle); override;
    procedure ReceiveEvent(aHandle: TLHandle); override;
    procedure AcceptEvent(aHandle: TLHandle); override;
    function HandleSSLConnection(aSocket: TLSSLSocket): Boolean;
   public
    property Password: string read FPassword write SetPassword;
    property CAFile: string read FCAFile write SetCAFile;
    property KeyFile: string read FKeyFile write SetKeyFile;
    property Method: TLSSLMethod read FMethod write SetMethod;
    property PasswordCallback: TLPasswordCB read FPasswordCallback write SetPasswordCallback;
    property SSLContext: PSSL_CTX read FSSLContext;
    property SSLActive: Boolean read FSSLActive write SetSSLActive;
    property OnSSLConnect: TLSocketEvent read FOnSSLConnect write FOnSSLConnect;
    property OnSSLAccept: TLSocketEvent read FOnSSLAccept write FOnSSLAccept;
  end;
  
  function IsSSLBlockError(const anError: Longint): Boolean; inline;
  function IsSSLNonFatalError(const anError, aRet: Longint): Boolean; inline;
  
implementation

uses
  {Math,} lCommon;

function GetSSLErrorStr(e: cInt): string;
var
  buf: string;
begin
  Result := '';
  SetLength(buf, 2048);

  repeat
    ErrErrorString(e, buf, Length(buf));
    Result := Result + buf + LineEnding;
    e := ErrGetError;
  until e = 0;
end;

function PasswordCB(buf: pChar; num, rwflag: cInt; userdata: Pointer): cInt; cdecl;
var
  S: TLSSLSession;
begin
  S := TLSSLSession(userdata);
  
  if num < Length(S.Password) + 1 then
    Exit(0);

  Move(S.Password[1], buf[0], Length(S.Password));
  Result := Length(S.Password);
end;

function IsSSLBlockError(const anError: Longint): Boolean; inline;
begin
  Result := (anError = SSL_ERROR_WANT_READ) or (anError = SSL_ERROR_WANT_WRITE);
end;

function IsSSLNonFatalError(const anError, aRet: Longint): Boolean; inline;
var
  tmp: Longint;
begin
  Result := False;
  if anError = SSL_ERROR_SYSCALL then repeat
    tmp := ErrGetError();
    if tmp = 0 then begin // we neet to check the ret
      if aRet <= 0 then Exit; // EOF or BIO crap, we skip those
      Result := IsNonFatalError(aRet);
    end else // check what exactly
      Result := IsNonFatalError(tmp);
  until tmp <= 0; // we need to empty the queue
end;

{ TLSSLSocket }

function TLSSLSocket.SetActiveSSL(const AValue: Boolean): Boolean;
begin
  Result := False;
  
  if (ssSSLActive in FSocketState) = AValue then Exit(True);
  case aValue of
    True  : FSocketState := FSocketState + [ssSSLActive];
    False : FSocketState := FSocketState - [ssSSLActive];
  end;
  
  if aValue and (FConnectionStatus = scConnected) then
    ActivateTLSEvent;

  if not aValue then begin
    if ConnectionStatus = scConnected then
      ShutdownSSL
    else if FSSLStatus in [slConnect, slActivateTLS] then
      raise Exception.Create('Switching SSL mode on socket during SSL handshake is not supported');
  end;
  
  Result := True;
end;

procedure TLSSLSocket.SetupSSLSocket;
begin
  if Assigned(FSSL) then
    SslFree(FSSL);

  FSSL := SSLNew(FSSLContext);
  if not Assigned(FSSL) then begin
    Bail('SSLNew error', -1);
    Exit;
  end;

  if SslSetFd(FSSL, FHandle) = 0 then begin
    FSSL := nil;
    Bail('SSL setFD error', -1);
    Exit;
  end;
end;

procedure TLSSLSocket.ActivateTLSEvent;
begin
  SetupSSLSocket;
  FSSLStatus := slActivateTLS;
  if FIsAcceptor then
    AcceptSSL
  else
    ConnectSSL;
end;

function TLSSLSocket.GetConnected: Boolean;
begin
  if ssSSLActive in FSocketState then
    Result := Assigned(FSSL) and (FSSLStatus = slNone)
  else
    Result := inherited;
end;

function TLSSLSocket.GetConnectionStatus: TLSocketConnectionStatus;
begin
  if ssSSLActive in FSocketState then case FSSLStatus of
    slNone        : if Assigned(FSSL) then
                      Result := scConnected
                    else
                      Result := scNone;
    slConnect,
    slActivateTLS : Result := scConnecting;
    slShutdown    : Result := scDisconnecting;
  end else
    Result := inherited;
end;

function TLSSLSocket.DoSend(const aData; const aSize: Integer): Integer;
begin
  if ssSSLActive in FSocketState then begin
{    if FSSLSendSize = 0 then begin
      FSSLSendSize := Min(aSize, Length(FSSLSendBuffer));
      Move(aData, FSSLSendBuffer[0], FSSLSendSize);
    end;
      
    Result := SSLWrite(FSSL, @FSSLSendBuffer[0], FSSLSendSize);
    if Result > 0 then
      FSSLSendSize := 0;}

    Result := SSLWrite(FSSL, @aData, aSize);
  end else
    Result := inherited DoSend(aData, aSize);
end;

function TLSSLSocket.DoGet(out aData; const aSize: Integer): Integer;
begin
  if ssSSLActive in FSocketState then
    Result := SSLRead(FSSL, @aData, aSize)
  else
    Result := inherited DoGet(aData, aSize);
end;

function TLSSLSocket.HandleResult(const aResult: Integer; aOp: TLSocketOperation): Integer;
const
  GSStr: array[TLSocketOperation] of string = ('SSLWrite', 'SSLRead');
var
  LastError: cInt;
begin
  if not (ssSSLActive in FSocketState) then
    Exit(inherited HandleResult(aResult, aOp));
    
  Result := aResult;
  if Result <= 0 then begin
    LastError := SslGetError(FSSL, Result);
    if IsSSLBlockError(LastError) then case aOp of
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
    end else if IsSSLNonFatalError(LastError, Result) then
      LogError(GSStr[aOp] + ' error', LastError)
    else if (aOp = soSend) and (IsPipeError(LastError)) then
      HardDisconnect(True)
    else
      Bail(GSStr[aOp] + ' error', LastError);
    Result := 0;
  end;
end;

procedure TLSSLSocket.ConnectEvent;
begin
  SetupSSLSocket;
  FSSLStatus := slConnect;
  ConnectSSL;
end;

procedure TLSSLSocket.AcceptEvent;
begin
  SetupSSLSocket;
  FSSLStatus := slConnect;
  AcceptSSL;
end;

function TLSSLSocket.LogError(const msg: string; const ernum: Integer): Boolean;
var
  s: string;
begin
  Result := False;
  if not (ssSSLActive in FSocketState) then
    Result := inherited LogError(msg, ernum)
  else if Assigned(FOnError) then begin
    if ernum > 0 then begin
      SetLength(s, 1024);
      ErrErrorString(ernum, s, Length(s));
      FOnError(Self, msg + ': ' + s);
    end else
      FOnError(Self, msg);
  end;
end;

destructor TLSSLSocket.Destroy;
begin
  inherited Destroy;
  SslFree(FSSL);
end;

function TLSSLSocket.SetState(const aState: TLSocketState; const TurnOn: Boolean
  ): Boolean;
begin
  case aState of
    ssSSLActive: Result := SetActiveSSL(TurnOn);
  else
    Result := inherited SetState(aState, TurnOn);
  end;
end;

procedure TLSSLSocket.ConnectSSL;
var
  c, e: cInt;
begin
  c := SSLConnect(FSSL);
  if c <= 0 then begin
    e := SslGetError(FSSL, c);
    case e of
      SSL_ERROR_WANT_READ  : begin // make sure we're watching for reads and flag status
                               FSocketState := FSocketState - [ssCanReceive];
                               IgnoreRead := False;
                             end;
      SSL_ERROR_WANT_WRITE : begin // make sure we're watching for writes and flag status
                               FSocketState := FSocketState - [ssCanSend];
                               IgnoreWrite := False;
                             end;
    else
      begin
        Bail('SSL connect errors: ' + LineEnding + GetSSLErrorStr(e), -1);
        Exit;
      end;
    end;
  end else begin
    FSSLStatus := slNone;
    TLSSLSession(FSession).CallOnSSLConnect(Self);
  end;
end;

procedure TLSSLSocket.AcceptSSL;
var
  c, e: cInt;
begin
  c := SSLAccept(FSSL);
  if c <= 0 then begin
    e := SslGetError(FSSL, c);
    case e of
      SSL_ERROR_WANT_READ  : begin // make sure we're watching for reads and flag status
                               FSocketState := FSocketState - [ssCanReceive];
                               IgnoreRead := False;
                             end;
      SSL_ERROR_WANT_WRITE : begin // make sure we're watching for writes and flag status
                               FSocketState := FSocketState - [ssCanSend];
                               IgnoreWrite := False;
                             end;
    else
      begin
        Bail('SSL accept errors: ' + LineEnding + GetSSLErrorStr(e), -1);
        Exit;
      end;
    end;
  end else begin
    FSSLStatus := slNone;
    TLSSLSession(FSession).CallOnSSLAccept(Self);
  end;
end;

procedure TLSSLSocket.ShutdownSSL;
var
  n: Integer;
begin
  if Assigned(FSSL) then begin
    FSSLStatus := slNone; // for now
    n := SSLShutdown(FSSL); // don't care for now, unless it fails badly
    if n <= 0 then begin
      n := SslGetError(FSSL, n);
      case n of
        SSL_ERROR_WANT_READ,
        SSL_ERROR_WANT_WRITE,
        SSL_ERROR_SYSCALL     : begin end; // ignore
      else
        Bail('SSL shutdown errors: ' + LineEnding + GetSSLErrorStr(n), -1);
      end;
    end else begin
      FSSLStatus := slNone; // success from our end
    end;
  end;
end;

procedure TLSSLSocket.Disconnect(const Forced: Boolean = False);
begin
  if FDispose
  and (FConnectionStatus = scNone)
  and (not (ssSSLActive in FSocketState)) then // don't do anything when already invalid
    Exit;

  if ssSSLActive in FSocketState then begin
    if ConnectionStatus = scConnected then // don't make SSL inactive just yet, we might get a shutdown response
      ShutdownSSL;
    FSSLStatus := slShutdown;
  end;

  if Forced // if this is forced
  or (FSSLStatus = slNone) then begin // or we successfuly sent the shutdown
    SetActiveSSL(False); // make sure to update status
    inherited Disconnect(Forced); // then proceed with TCP discon
  end;
end;

{ TLSSLSession }

procedure TLSSLSession.SetSSLActive(const AValue: Boolean);
begin
  if aValue = FSSLActive then Exit;
  FSSLActive := aValue;
  if aValue then
    CreateSSLContext;
end;

procedure TLSSLSession.CallOnSSLConnect(aSocket: TLSocket);
begin
  if Assigned(FOnSSLConnect) then
    FOnSSLConnect(aSocket);
end;

procedure TLSSLSession.CallOnSSLAccept(aSocket: TLSocket);
begin
  if Assigned(FOnSSLAccept) then
    FOnSSLAccept(aSocket);
end;

procedure TLSSLSession.SetCAFile(AValue: string);
begin
  DoDirSeparators(aValue);
  if aValue = FCAFile then Exit;
  FCAFile := aValue;
  CreateSSLContext;
end;

procedure TLSSLSession.SetKeyFile(AValue: string);
begin
  DoDirSeparators(aValue);
  if aValue = FKeyFile then Exit;
  FKeyFile := aValue;
  CreateSSLContext;
end;

procedure TLSSLSession.SetPassword(const AValue: string);
begin
  if aValue = FPassword then Exit;
  FPassword := aValue;
  CreateSSLContext;
end;

procedure TLSSLSession.SetMethod(const AValue: TLSSLMethod);
begin
  if aValue = FMethod then Exit;
  FMethod := aValue;
  CreateSSLContext;
end;

procedure TLSSLSession.SetPasswordCallback(const AValue: TLPasswordCB);
begin
  if aValue = FPasswordCallback then Exit;
  FPasswordCallback := aValue;
  CreateSSLContext;
end;

procedure TLSSLSession.CreateSSLContext;
var
  aMethod: PSSL_METHOD;
begin
  if not IsSSLloaded then
    raise Exception.Create('Unable to initialize OpenSSL library, please check your OpenSSL installation');

  if Assigned(FSSLContext) then
    SSLCTXFree(FSSLContext);
    
  if not FSSLActive then
    Exit;

  case FMethod of
    msSSLv2or3 : aMethod := SslMethodV23;
    msSSLv2    : aMethod := SslMethodV2;
    msSSLv3    : aMethod := SslMethodV3;
    msTLSv1    : aMethod := SslMethodTLSV1;
  end;

  FSSLContext := SSLCTXNew(aMethod);
  if not Assigned(FSSLContext) then
    raise Exception.Create('Error creating SSL CTX: SSLCTXNew');
    
  if SSLCTXSetMode(FSSLContext, SSL_MODE_ENABLE_PARTIAL_WRITE) and SSL_MODE_ENABLE_PARTIAL_WRITE <> SSL_MODE_ENABLE_PARTIAL_WRITE then
    raise Exception.Create('Error setting partial write mode on CTX');
  if SSLCTXSetMode(FSSLContext, SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER) and SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER <> SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER then
    raise Exception.Create('Error setting accept moving buffer mode on CTX');

  if Length(FCAFile) > 0 then
    if SslCtxUseCertificateChainFile(FSSLContext, FCAFile) = 0 then
      raise Exception.Create('Error creating SSL CTX: SSLCTXLoadVerifyLocations');

  if Length(FKeyFile) > 0 then begin
    SslCtxSetDefaultPasswdCb(FSSLContext, FPasswordCallback);
    SslCtxSetDefaultPasswdCbUserdata(FSSLContext, Self);
  
    if SSLCTXUsePrivateKeyFile(FSSLContext, FKeyfile, SSL_FILETYPE_PEM) = 0 then
      raise Exception.Create('Error creating SSL CTX: SSLCTXUsePrivateKeyFile');
  end;

  OPENSSLaddallalgorithms;
end;

constructor TLSSLSession.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FPasswordCallback := @PasswordCB;
  FSSLActive := True;
  CreateSSLContext;
end;

procedure TLSSLSession.RegisterWithComponent(aConnection: TLConnection);
begin
  inherited RegisterWithComponent(aConnection);
  
  if not aConnection.SocketClass.InheritsFrom(TLSSLSocket) then
    aConnection.SocketClass := TLSSLSocket;
end;

procedure TLSSLSession.InitHandle(aHandle: TLHandle);
begin
  inherited;
  
  TLSSLSocket(aHandle).FSSLContext := FSSLContext;
  TLSSLSocket(aHandle).SetState(ssSSLActive, FSSLActive);
end;

procedure TLSSLSession.ConnectEvent(aHandle: TLHandle);
begin
  if not (ssSSLActive in TLSSLSocket(aHandle).SocketState) then
    inherited ConnectEvent(aHandle)
  else if HandleSSLConnection(TLSSLSocket(aHandle)) then
    CallConnectEvent(aHandle);
end;

procedure TLSSLSession.ReceiveEvent(aHandle: TLHandle);
begin
  if not (ssSSLActive in TLSSLSocket(aHandle).SocketState) then
    inherited ReceiveEvent(aHandle)
  else case TLSSLSocket(aHandle).SSLStatus of
    slConnect:
      if HandleSSLConnection(TLSSLSocket(aHandle)) then
      case ssServerSocket in TLSSLSocket(aHandle).SocketState of
        True  : CallAcceptEvent(aHandle);
        False : CallConnectEvent(aHandle);
      end;
    slActivateTLS:
      HandleSSLConnection(TLSSLSocket(aHandle));
  else
    CallReceiveEvent(aHandle);
  end;
end;

procedure TLSSLSession.AcceptEvent(aHandle: TLHandle);
begin
  if not (ssSSLActive in TLSSLSocket(aHandle).SocketState) then
    inherited AcceptEvent(aHandle)
  else if HandleSSLConnection(TLSSLSocket(aHandle)) then
    CallAcceptEvent(aHandle);
end;

function TLSSLSession.HandleSSLConnection(aSocket: TLSSLSocket): Boolean;

  procedure HandleNone;
  begin
    if aSocket.FIsAcceptor then
      aSocket.AcceptEvent
    else
      aSocket.ConnectEvent;
  end;

  procedure HandleConnect;
  begin
    if aSocket.FIsAcceptor then
      aSocket.AcceptSSL
    else
      aSocket.ConnectSSL;
  end;

begin
  Result := False;
  
  if not Assigned(FSSLContext) then
    raise Exception.Create('Context not created during SSL connect/accept');

  case aSocket.FSSLStatus of
    slNone        : HandleNone;
    slActivateTLS,
    slConnect     : HandleConnect;
    slShutdown    : raise Exception.Create('Got ConnectEvent or AcceptEvent on socket with ssShutdown status');
  end;
  
  Result := aSocket.SSLStatus = slNone;
end;

initialization
  SslLibraryInit;
  SslLoadErrorStrings;

finalization
  DestroySSLInterface;

end.

