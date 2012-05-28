{ Web server component, built on the HTTP server component

  Copyright (C) 2006-2008 Micha Nelissen

  This library is Free software; you can redistribute it and/or modify it
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
  
  This license has been modified. See file LICENSE.ADDON for more information.
  Should you find these sources without a LICENSE File, please contact
  me at ales@chello.sk
}

unit lwebserver;

{$mode objfpc}{$h+}
{$inline on}

interface

uses
  sysutils, classes, lhttp, lhttputil, lmimetypes, levents,
  lprocess, process, lfastcgi, fastcgi_base;

type
  TLMultipartParameter = (mpContentType, mpContentDisposition, mpContentTransferEncoding,
    mpContentID, mpContentDescription);
  TLMultipartState = (msStart, msBodypartHeader, msBodypartData);

const
  URIParamSepChar: char = '&';
  CookieSepChar: char = ';';
  FormURLContentType: pchar = 'application/x-www-form-urlencoded';
  MultipartContentType: pchar = 'multipart/form-data';
  MPParameterStrings: array[TLMultipartParameter] of string =
    ('Content-Type', 'Content-Disposition', 'Content-Transfer-Encoding',
     'Content-ID', 'Content-Discription');

type
  TDocumentHandler = class;
  TFileHandler = class;

  TFileOutput = class(TBufferOutput)
  protected
    FFile: file;

    function GetSize: integer;
    function FillBuffer: TWriteBlockStatus; override;
  public
    constructor Create(ASocket: TLHTTPSocket);
    destructor Destroy; override;

    function  Open(const AFileName: string): boolean;

    property Size: integer read GetSize;
  end;

  TCGIOutput = class(TBufferOutput)
  protected
    FParsePos: pchar;
    FReadPos: integer;
    FParsingHeaders: boolean;
   
    procedure AddEnvironment(const AName, AValue: string); virtual; abstract;
    procedure AddHTTPParam(const AName: string; AParam: TLHTTPParameter);
    function  ParseHeaders: boolean;
    procedure CGIOutputError; virtual; abstract;
    procedure WriteCGIBlock;
    function  WriteCGIData: TWriteBlockStatus; virtual; abstract;
  public
    FDocumentRoot: string;
    FExtraPath: string;
    FEnvPath: string;
    FScriptFileName: string;
    FScriptName: string;

    constructor Create(ASocket: TLHTTPSocket);
    destructor Destroy; override;

    function  FillBuffer: TWriteBlockStatus; override;
    procedure StartRequest; virtual;
  end;

  TSimpleCGIOutput = class(TCGIOutput)
  protected
    FProcess: TLProcess;

    procedure AddEnvironment(const AName, AValue: string); override;
    procedure CGIProcNeedInput(AHandle: TLHandle);
    procedure CGIProcHasOutput(AHandle: TLHandle);
    procedure CGIProcHasStderr(AHandle: TLHandle);
    procedure DoneInput; override;
    function  HandleInput(ABuffer: pchar; ASize: integer): integer; override;
    procedure CGIOutputError; override;
    function  WriteCGIData: TWriteBlockStatus; override;
  public
    constructor Create(ASocket: TLHTTPSocket);
    destructor Destroy; override;

    procedure  StartRequest; override;

    property Process: TLProcess read FProcess;
  end;

  TFastCGIOutput = class(TCGIOutput)
  protected
    FRequest: TLFastCGIRequest;

    procedure AddEnvironment(const AName, AValue: string); override;
    procedure CGIOutputError; override;
    procedure DoneInput; override;
    procedure RequestEnd(ARequest: TLFastCGIRequest);
    procedure RequestNeedInput(ARequest: TLFastCGIRequest);
    procedure RequestHasOutput(ARequest: TLFastCGIRequest);
    procedure RequestHasStderr(ARequest: TLFastCGIRequest);
    function  HandleInput(ABuffer: pchar; ASize: integer): integer; override;
    function  WriteCGIData: TWriteBlockStatus; override;
    function  WriteBlock: TWriteBlockStatus; override;
  public
    constructor Create(ASocket: TLHTTPSocket);
    destructor Destroy; override;

    procedure StartRequest; override;

    property Request: TLFastCGIRequest read FRequest write FRequest;
  end;

  TCGIHandler = class(TURIHandler)
  protected
    function HandleURI(ASocket: TLHTTPServerSocket): TOutputItem; override;
  public
    FCGIRoot: string;
    FEnvPath: string;
    FDocumentRoot: string;
    FScriptPathPrefix: string;
  end;

  TDocumentRequest = record
    Socket: TLHTTPServerSocket;
    Document: string;
    URIPath: string;
    ExtraPath: string;
    Info: TSearchRec;
    InfoValid: boolean;
  end;

  TDocumentHandler = class(TObject)
  private
    FNext: TDocumentHandler;
  protected
    FFileHandler: TFileHandler;

    procedure RegisterWithEventer(AEventer: TLEventer); virtual;
  public
    function HandleDocument(const ARequest: TDocumentRequest): TOutputItem; virtual; abstract;

    property FileHandler: TFileHandler read FFileHandler;
  end;

  { TFileHandler }

  TFileHandler = class(TURIHandler)
  protected
    FDocHandlerList: TDocumentHandler;
    FDirIndexList: TStrings;
    FMimeTypeFile: string;

    procedure SetMimeTypeFile(const AValue: string);
    function HandleFile(const ARequest: TDocumentRequest): TOutputItem;
    function HandleURI(ASocket: TLHTTPServerSocket): TOutputItem; override;
    procedure RegisterWithEventer(AEventer: TLEventer); override;
  public
    DocumentRoot: string;

    constructor Create;
    destructor Destroy; override;
    
    procedure RegisterHandler(AHandler: TDocumentHandler);

    property DirIndexList: TStrings read FDirIndexList;
    property MimeTypeFile: string read FMimeTypeFile write SetMimeTypeFile;
  end;

  TPHPCGIHandler = class(TDocumentHandler)
  protected
    FAppName: string;
    FEnvPath: string;
  public
    function HandleDocument(const ARequest: TDocumentRequest): TOutputItem; override;

    property AppName: string read FAppName write FAppName;
    property EnvPath: string read FEnvPath write FEnvPath;
  end;

  TPHPFastCGIHandler = class(TDocumentHandler)
  protected
    FPool: TLFastCGIPool;
    FEnvPath: string;

    function  GetAppEnv: string;
    function  GetAppName: string;
    function  GetHost: string;
    function  GetPort: integer;
    procedure RegisterWithEventer(AEventer: TLEventer); override;
    procedure SetAppEnv(NewEnv: string);
    procedure SetAppName(NewName: string);
    procedure SetHost(NewHost: string);
    procedure SetPort(NewPort: integer);
  public
    constructor Create;
    destructor Destroy; override;

    function HandleDocument(const ARequest: TDocumentRequest): TOutputItem; override;

    property AppEnv: string read GetAppEnv write SetAppEnv;
    property AppName: string read GetAppName write SetAppName;
    property EnvPath: string read FEnvPath write FEnvPath;
    property Host: string read GetHost write SetHost;
    property Pool: TLFastCGIPool read FPool;
    property Port: integer read GetPort write SetPort;
  end;

  { Forms }

  TFormOutput = class;

  TFillBufferEvent = procedure(AFormOutput: TFormOutput; var AStatus: TWriteBlockStatus);
  THandleInputMethod = function(ABuffer: pchar; ASize: integer): integer of object;

  TFormOutput = class(TBufferOutput)
  protected
    FBoundary: pchar;
    FRequestVars: TStrings;
    FMPParameters: array[TLMultipartParameter] of pchar;
    FMPState: TLMultipartState;
    FOnExtraHeaders: TNotifyEvent;
    FOnFillBuffer: TFillBufferEvent;
    FHandleInput: THandleInputMethod;

    procedure DoneInput; override;
    function  FillBuffer: TWriteBlockStatus; override;
    function  FindBoundary(ABuffer: pchar): pchar;
    function  HandleInput(ABuffer: pchar; ASize: integer): integer; override;
    function  HandleInputDiscard(ABuffer: pchar; ASize: integer): integer;
    function  HandleInputFormURL(ABuffer: pchar; ASize: integer): integer;
    function  HandleInputMultipart(ABuffer: pchar; ASize: integer): integer;
    procedure ParseMultipartHeader(ABuffer, ALineEnd: pchar);
  public
    constructor Create(ASocket: TLHTTPSocket);
    destructor Destroy; override;

    function AddVariables(Variables: pchar; ASize: integer; SepChar: char): integer;
    procedure DeleteCookie(const AName: string; const APath: string = '/'; 
        const ADomain: string = '');
    procedure SetCookie(const AName, AValue: string; const AExpires: TDateTime; 
        const APath: string = '/'; const ADomain: string = '');

    property OnExtraHeaders: TNotifyEvent read FOnExtraHeaders write FOnExtraHeaders;
    property OnFillBuffer: TFillBufferEvent read FOnFillBuffer write FOnFillBuffer;
  end;

  THandleURIEvent = function(ASocket: TLHTTPServerSocket): TFormOutput;

  TFormHandler = class(TURIHandler)
  protected
    FOnHandleURI: THandleURIEvent;

    function HandleURI(ASocket: TLHTTPServerSocket): TOutputItem; override;
    procedure SelectMultipart(AFormOutput: TFormOutput; AContentType: pchar);
  public
    property OnHandleURI: THandleURIEvent read FOnHandleURI write FOnHandleURI;
  end;

var
  EnableWriteln: Boolean = True;

implementation

uses
  lstrbuffer;

{ Example handlers }

const
  InputBufferEmptyToWriteStatus: array[boolean] of TWriteBlockStatus =
    (wsPendingData, wsWaitingData);
  
procedure InternalWrite(const s: string);
begin
  if EnableWriteln then
    Writeln(s);
end;

procedure TDocumentHandler.RegisterWithEventer(AEventer: TLEventer);
begin
end;

function TCGIHandler.HandleURI(ASocket: TLHTTPServerSocket): TOutputItem;
var
  lOutput: TSimpleCGIOutput;
  lExecPath: string;
begin
  if StrLComp(ASocket.FRequestInfo.Argument, PChar(FScriptPathPrefix),
      Length(FScriptPathPrefix)) = 0 then
  begin
    lOutput := TSimpleCGIOutput.Create(ASocket);
    lOutput.FDocumentRoot := FDocumentRoot;
    lOutput.FEnvPath := FEnvPath;
    lOutput.Process.CurrentDirectory := FCGIRoot;
    lExecPath := ASocket.FRequestInfo.Argument+Length(FScriptPathPrefix);
    DoDirSeparators(lExecPath);
    lExecPath := FCGIRoot+lExecPath;
    if SeparatePath(lExecPath, lOutput.FExtraPath, faAnyFile and not faDirectory) then
    begin
      lOutput.Process.CommandLine := lExecPath;
      lOutput.FScriptFileName := lExecPath;
      lOutput.FScriptName := Copy(lExecPath, Length(FCGIRoot), 
        Length(lExecPath)-Length(FCGIRoot)+1);
      lOutput.StartRequest;
    end else
      ASocket.FResponseInfo.Status := hsNotFound;
    Result := lOutput;
  end else
    Result := nil;
end;

constructor TFileHandler.Create;
begin
  inherited;

  FDirIndexList := TStringList.Create;
end;

destructor TFileHandler.Destroy;
begin
  FreeAndNil(FDirIndexList);

  inherited;
end;

procedure TFileHandler.RegisterWithEventer(AEventer: TLEventer);
var
  lHandler: TDocumentHandler;
begin
  lHandler := FDocHandlerList;
  while lHandler <> nil do
  begin
    lHandler.RegisterWithEventer(AEventer);
    lHandler := lHandler.FNext;
  end;
end;

procedure TFileHandler.SetMimeTypeFile(const AValue: string);
begin
  FMimeTypeFile:=AValue;
  InitMimeList(aValue);
end;

function TFileHandler.HandleFile(const ARequest: TDocumentRequest): TOutputItem;
var
  lFileOutput: TFileOutput;
  lReqInfo: PRequestInfo;
  lRespInfo: PResponseInfo;
  lHeaderOut: PHeaderOutInfo;
  lIndex: integer;
begin
  Result := nil;
  if ARequest.InfoValid then
  begin
    lReqInfo := @ARequest.Socket.FRequestInfo;
    lRespInfo := @ARequest.Socket.FResponseInfo;
    lHeaderOut := @ARequest.Socket.FHeaderOut;
    if not (lReqInfo^.RequestType in [hmHead, hmGet]) then
    begin
      lRespInfo^.Status := hsNotAllowed;
    end else begin
      lFileOutput := TFileOutput.Create(ARequest.Socket);
      if lFileOutput.Open(ARequest.Document) then
      begin
        lRespInfo^.Status := hsOK;
        lHeaderOut^.ContentLength := ARequest.Info.Size;
        lRespInfo^.LastModified := LocalTimeToGMT(FileDateToDateTime(ARequest.Info.Time));
        lIndex := MimeList.IndexOf(ExtractFileExt(ARequest.Document));
        if lIndex >= 0 then
          lRespInfo^.ContentType := TStringObject(MimeList.Objects[lIndex]).Str;
        Result := lFileOutput;
        ARequest.Socket.StartResponse(lFileOutput);
      end else
        lFileOutput.Free;
    end;
  end;
end;

function TFileHandler.HandleURI(ASocket: TLHTTPServerSocket): TOutputItem;
var
  lDocRequest: TDocumentRequest;
  lHandler: TDocumentHandler;
  lTempDoc: string;
  lDirIndexFound: boolean;
  I: integer;
begin
  Result := nil;
  lDocRequest.Socket := ASocket;
  lDocRequest.URIPath := ASocket.FRequestInfo.Argument;
  lDocRequest.Document := lDocRequest.URIPath;
  DoDirSeparators(LDocRequest.Document);
  lDocRequest.Document := IncludeTrailingPathDelimiter(DocumentRoot)
    + lDocRequest.Document;
  lDocRequest.InfoValid := SeparatePath(lDocRequest.Document,lDocRequest.ExtraPath, 
    faAnyFile, @lDocRequest.Info);
  if not lDocRequest.InfoValid then
    exit;
  if (lDocRequest.Info.Attr and faDirectory) <> 0 then
  begin
    lDirIndexFound := false;
    { if non-trivial ExtraPath, then it's not a pure directory request, so do
      not show default directory document }
    if lDocRequest.ExtraPath = PathDelim then
    begin
      lDocRequest.Document := IncludeTrailingPathDelimiter(lDocRequest.Document);
      for I := 0 to FDirIndexList.Count - 1 do
      begin
        lTempDoc := lDocRequest.Document + FDirIndexList.Strings[I];
        lDocRequest.InfoValid := FindFirst(lTempDoc, 
          faAnyFile and not faDirectory, lDocRequest.Info) = 0;
        FindClose(lDocRequest.Info);
        if lDocRequest.InfoValid and ((lDocRequest.Info.Attr and faDirectory) = 0) then
        begin
          lDocRequest.Document := lTempDoc;
          lDirIndexFound := true;
          break;
        end;
      end;
    end;
    { requested a directory, but no source to show }
    if not lDirIndexFound then exit;
  end;

  lHandler := FDocHandlerList;
  while lHandler <> nil do
  begin
    Result := lHandler.HandleDocument(lDocRequest);
    if Result <> nil then exit;
    if ASocket.FResponseInfo.Status <> hsOK then exit;
    lHandler := lHandler.FNext;
  end;

  { no dynamic handler, see if it's a plain file }
  Result := HandleFile(lDocRequest);
end;

procedure TFileHandler.RegisterHandler(AHandler: TDocumentHandler);
begin
  if AHandler = nil then exit;
  AHandler.FFileHandler := Self;
  AHandler.FNext := FDocHandlerList;
  FDocHandlerList := AHandler;
end;

function TPHPCGIHandler.HandleDocument(const ARequest: TDocumentRequest): TOutputItem;
var
  lOutput: TSimpleCGIOutput;
begin
  if ExtractFileExt(ARequest.Document) = '.php' then
  begin
    lOutput := TSimpleCGIOutput.Create(ARequest.Socket);
    lOutput.FDocumentRoot := FFileHandler.DocumentRoot;
    lOutput.Process.CommandLine := FAppName;
    lOutput.FScriptName := ARequest.URIPath;
    lOutput.FScriptFileName := ARequest.Document;
    lOutput.FExtraPath := ARequest.ExtraPath;
    lOutput.FEnvPath := FEnvPath;
    lOutput.StartRequest;
    Result := lOutput;
  end else
    Result := nil;
end;

constructor TPHPFastCGIHandler.Create;
begin
  inherited;
  FPool := TLFastCGIPool.Create;
end;

destructor TPHPFastCGIHandler.Destroy;
begin
  inherited;
  FPool.Free;
end;

function  TPHPFastCGIHandler.GetAppEnv: string;
begin
  Result := FPool.AppEnv;
end;

function  TPHPFastCGIHandler.GetAppName: string;
begin
  Result := FPool.AppName;
end;

function  TPHPFastCGIHandler.GetHost: string;
begin
  Result := FPool.Host;
end;

function  TPHPFastCGIHandler.GetPort: integer;
begin
  Result := FPool.Port;
end;

procedure TPHPFastCGIHandler.SetAppEnv(NewEnv: string);
begin
  FPool.AppEnv := NewEnv;
end;

procedure TPHPFastCGIHandler.SetAppName(NewName: string);
begin
  FPool.AppName := NewName;
end;

procedure TPHPFastCGIHandler.SetHost(NewHost: string);
begin
  FPool.Host := NewHost;
end;

procedure TPHPFastCGIHandler.SetPort(NewPort: integer);
begin
  FPool.Port := NewPort;
end;

procedure TPHPFastCGIHandler.RegisterWithEventer(AEventer: TLEventer);
begin
  FPool.Eventer := AEventer;
end;

function TPHPFastCGIHandler.HandleDocument(const ARequest: TDocumentRequest): TOutputItem;
var
  lOutput: TFastCGIOutput;
  fcgiRequest: TLFastCGIRequest;
begin
  if ExtractFileExt(ARequest.Document) = '.php' then
  begin
    fcgiRequest := FPool.BeginRequest(FCGI_RESPONDER);
    if fcgiRequest <> nil then
    begin
      lOutput := TFastCGIOutput.Create(ARequest.Socket);
      lOutput.FDocumentRoot := FFileHandler.DocumentRoot;
      lOutput.FScriptName := ARequest.URIPath;
      lOutput.FScriptFileName := ARequest.Document;
      lOutput.FExtraPath := ARequest.ExtraPath;
      lOutput.FEnvPath := FEnvPath;
      lOutput.Request := fcgiRequest;
      ARequest.Socket.SetupEncoding(lOutput);
      lOutput.StartRequest;
      Result := lOutput;
    end else begin
      ARequest.Socket.FResponseInfo.Status := hsInternalError;
      ARequest.Socket.StartResponse(nil);
      Result := nil;
    end;
  end else
    Result := nil;
end;

{ Output Items }

constructor TFileOutput.Create(ASocket: TLHTTPSocket);
begin
  inherited;
  FEof := true;
end;

destructor TFileOutput.Destroy;
begin
  inherited;
  
  if not FEof then
    Close(FFile);
end;

function TFileOutput.Open(const AFileName: string): boolean;
begin
  {$I-}
  FileMode := 0;
  Assign(FFile, AFileName);
  Reset(FFile,1);
  {$I+}
  Result := IOResult = 0;
  FEof := false;
end;

function TFileOutput.GetSize: integer; inline;
begin
  Result := FileSize(FFile);
end;

function TFileOutput.FillBuffer: TWriteBlockStatus;
var
  lRead: integer;
begin
  if FEof then 
    exit(wsDone);
  BlockRead(FFile, FBuffer[FBufferPos], FBufferSize-FBufferPos, lRead);
  Inc(FBufferPos, lRead);
  if lRead = 0 then
  begin
    { EOF reached }
    Close(FFile);
    exit(wsDone);
  end;
  Result := wsPendingData;
end;

constructor TCGIOutput.Create(ASocket: TLHTTPSocket);
begin
  inherited;
end;

destructor TCGIOutput.Destroy;
begin
  inherited;
end;

procedure TCGIOutput.AddHTTPParam(const AName: string; AParam: TLHTTPParameter);
var
  lValue: pchar;
begin
  lValue := FSocket.Parameters[AParam];
  if lValue = nil then exit;
  AddEnvironment(AName, lValue);
end;

procedure TCGIOutput.StartRequest;
var
  lServerSocket: TLHTTPServerSocket;
  tempStr: string;
begin
  lServerSocket := TLHTTPServerSocket(FSocket);
{
  FProcess.Environment.Add('SERVER_ADDR=');
  FProcess.Environment.Add('SERVER_ADMIN=');
  FProcess.Environment.Add('SERVER_NAME=');
  FProcess.Environment.Add('SERVER_PORT=');
}
  Self := nil;
  tempStr := TLHTTPServer(lServerSocket.Creator).ServerSoftware;
  if Length(tempStr) > 0 then
    AddEnvironment('SERVER_SOFTWARE', tempStr);

  AddEnvironment('GATEWAY_INTERFACE', 'CGI/1.1'); 
  AddEnvironment('SERVER_PROTOCOL', lServerSocket.FRequestInfo.VersionStr);
  AddEnvironment('REQUEST_METHOD', lServerSocket.FRequestInfo.Method);
  AddEnvironment('REQUEST_URI', '/'+lServerSocket.FRequestInfo.Argument);

  if Length(FExtraPath) > 0 then
  begin
    AddEnvironment('PATH_INFO', FExtraPath);
    { do not set PATH_TRANSLATED: bug in PHP }
//    AddEnvironment('PATH_TRANSLATED', DocumentRoot+FExtraPath);
  end;

  AddEnvironment('SCRIPT_NAME', FScriptName);
  AddEnvironment('SCRIPT_FILENAME', FScriptFileName);
  
  AddEnvironment('QUERY_STRING', lServerSocket.FRequestInfo.QueryParams);
  AddHTTPParam('CONTENT_TYPE', hpContentType);
  AddHTTPParam('CONTENT_LENGTH', hpContentLength);

  AddEnvironment('REMOTE_ADDR', FSocket.PeerAddress);
  AddEnvironment('REMOTE_PORT', IntToStr(FSocket.LocalPort));

  { used when user has authenticated in some way to server }
//  AddEnvironment('AUTH_TYPE='+...);
//  AddEnvironment('REMOTE_USER='+...);
  
  AddEnvironment('DOCUMENT_ROOT', FDocumentRoot);
  AddEnvironment('REDIRECT_STATUS', '200');
  AddHTTPParam('HTTP_HOST', hpHost);
  AddHTTPParam('HTTP_COOKIE', hpCookie);
  AddHTTPParam('HTTP_CONNECTION', hpConnection);
  AddHTTPParam('HTTP_REFERER', hpReferer);
  AddHTTPParam('HTTP_USER_AGENT', hpUserAgent);
  AddHTTPParam('HTTP_ACCEPT', hpAccept);
  AddEnvironment('PATH', FEnvPath);

  FParsingHeaders := true;
  FReadPos := FBufferPos;
  FParsePos := FBuffer+FReadPos;
end;

function  TCGIOutput.ParseHeaders: boolean;
var
  lHttpStatus: TLHTTPStatus;
  iEnd, lCode: integer;
  lStatus, lLength: dword;
  pLineEnd, pNextLine, pValue: pchar;
  lServerSocket: TLHTTPServerSocket;

  procedure AddExtraHeader;
  begin
    AppendString(lServerSocket.FHeaderOut.ExtraHeaders, 
      FParsePos + ': ' + pValue + #13#10);
  end;

begin
  lServerSocket := TLHTTPServerSocket(FSocket);
  repeat
    iEnd := IndexByte(FParsePos^, @FBuffer[FReadPos]-FParsePos, 10);
    if iEnd = -1 then exit(false);
    pNextLine := FParsePos+iEnd+1;
    if (iEnd > 0) and (FParsePos[iEnd-1] = #13) then
      dec(iEnd);
    pLineEnd := FParsePos+iEnd;
    pLineEnd^ := #0;
    if pLineEnd = FParsePos then
    begin
      { empty line signals end of headers }
      FParsingHeaders := false;
      FBufferOffset := pNextLine-FBuffer;
      FBufferPos := FReadPos;
      FReadPos := 0;
      lServerSocket.StartResponse(Self, true);
      exit(false);
    end;
    iEnd := IndexByte(FParsePos^, iEnd, ord(':'));
    if (iEnd = -1) or (FParsePos[iEnd+1] <> ' ') then
      break;
    FParsePos[iEnd] := #0;
    pValue := FParsePos+iEnd+2;
    if StrIComp(FParsePos, 'Content-type') = 0 then
    begin
      lServerSocket.FResponseInfo.ContentType := pValue;
    end else 
    if StrIComp(FParsePos, 'Location') = 0 then
    begin
      if StrLIComp(pValue, 'http://', 7) = 0 then
      begin
        lServerSocket.FResponseInfo.Status := hsMovedPermanently;
        { add location header as-is to response }
        AddExtraHeader;
      end else
        InternalWrite('WARNING: unimplemented ''Location'' response received from CGI script');
    end else 
    if StrIComp(FParsePos, 'Status') = 0 then
    begin
      { sometimes we get '<status code> space <reason>' }
      iEnd := IndexByte(pValue^, pLineEnd-pValue, ord(' '));
      if iEnd <> -1 then
        pValue[iEnd] := #0;
      Val(pValue, lStatus, lCode);
      if lCode <> 0 then
        break;
      for lHttpStatus := Low(TLHTTPStatus) to High(TLHTTPStatus) do
        if HTTPStatusCodes[lHttpStatus] = lStatus then
          lServerSocket.FResponseInfo.Status := lHttpStatus;
    end else
    if StrIComp(FParsePos, 'Content-Length') = 0 then
    begin
      Val(pValue, lLength, lCode);
      if lCode <> 0 then
        break;
      lServerSocket.FHeaderOut.ContentLength := lLength;
    end else
    if StrIComp(FParsePos, 'Last-Modified') = 0 then
    begin
      if not TryHTTPDateStrToDateTime(pValue, 
          lServerSocket.FResponseInfo.LastModified) then
        InternalWrite('WARNING: unable to parse last-modified string from CGI script: ' + pValue);
    end else
      AddExtraHeader;
    FParsePos := pNextLine;
  until false;

  { error happened }
  lServerSocket.FResponseInfo.Status := hsInternalError;
  exit(true);
end;

function TCGIOutput.FillBuffer: TWriteBlockStatus;
begin
  if not FParsingHeaders then
    FReadPos := FBufferPos;
  Result := WriteCGIData;
  if FParsingHeaders then
  begin
    if ParseHeaders then
    begin
      { error while parsing }
      FEof := true;
      exit(wsDone);
    end;
  end else
    FBufferPos := FReadPos;
end;

procedure TCGIOutput.WriteCGIBlock;
begin
  { CGI process has output pending, we can write a block to socket }
  if FParsingHeaders then
  begin
    if (FillBuffer = wsDone) and FParsingHeaders then
    begin
      { still parsing headers ? something's wrong }
      FParsingHeaders := false;
      CGIOutputError;
      TLHTTPServerSocket(FSocket).StartResponse(Self);
    end;
  end;
  if not FParsingHeaders then
    FSocket.WriteBlock;
end;

{ TSimpleCGIOutput }

constructor TSimpleCGIOutput.Create(ASocket: TLHTTPSocket);
begin
  inherited;
  FProcess := TLProcess.Create(nil);
  FProcess.Options := FProcess.Options + [poUsePipes];
  FProcess.OnNeedInput := @CGIProcNeedInput;
  FProcess.OnHasOutput := @CGIProcHasOutput;
  FProcess.OnHasStderr := @CGIProcHasStderr;
end;

destructor TSimpleCGIOutput.Destroy;
begin
  inherited;
  FProcess.Free;
end;

function TSimpleCGIOutput.WriteCGIData: TWriteBlockStatus;
var
  lRead: integer;
begin
  lRead := FProcess.Output.Read(FBuffer[FReadPos], FBufferSize-FReadPos);
  if lRead = 0 then exit(wsDone);
  Inc(FReadPos, lRead);
  Result := InputBufferEmptyToWriteStatus[lRead = 0];
end;

procedure TSimpleCGIOutput.AddEnvironment(const AName, AValue: string);
begin
  FProcess.Environment.Add(AName+'='+AValue);
end;

procedure TSimpleCGIOutput.DoneInput;
begin
  FProcess.CloseInput;
end;

function TSimpleCGIOutput.HandleInput(ABuffer: pchar; ASize: integer): integer;
begin
  if ASize > 0 then
    Result := FProcess.Input.Write(ABuffer^, ASize)
  else
    Result := 0;
  FProcess.InputEvent.IgnoreWrite := ASize = 0;
end;

procedure TSimpleCGIOutput.StartRequest;
begin
  inherited;
  
  FProcess.Eventer := FSocket.Eventer;
  FProcess.Execute;
end;

procedure TSimpleCGIOutput.CGIOutputError;
var
  ServerSocket: TLHTTPServerSocket;
begin
  ServerSocket := TLHTTPServerSocket(FSocket);
  if FProcess.ExitStatus = 127 then
    ServerSocket.FResponseInfo.Status := hsNotFound
  else
    ServerSocket.FResponseInfo.Status := hsInternalError;
end;

procedure TSimpleCGIOutput.CGIProcNeedInput(AHandle: TLHandle);
begin
  FProcess.InputEvent.IgnoreWrite := true;
  FSocket.ParseBuffer;
end;

procedure TSimpleCGIOutput.CGIProcHasOutput(AHandle: TLHandle);
begin
  WriteCGIBlock;
end;

procedure TSimpleCGIOutput.CGIProcHasStderr(AHandle: TLHandle);
var
  lBuf: array[0..1023] of char;
  lRead: integer;
begin
  lRead := FProcess.Stderr.Read(lBuf, sizeof(lBuf)-1);
  lBuf[lRead] := #0;
  write(pchar(@lBuf[0]));
end;

{ TFastCGIOutput }

constructor TFastCGIOutput.Create(ASocket: TLHTTPSocket);
begin
  inherited;
end;

destructor TFastCGIOutput.Destroy;
begin
  if FRequest <> nil then
  begin
    FRequest.OnInput := nil;
    FRequest.OnOutput := nil;
    FRequest.OnStderr := nil;
    FRequest.OnEndRequest := nil;
    FRequest.AbortRequest;
  end;
  inherited;
end;

procedure TFastCGIOutput.AddEnvironment(const AName, AValue: string);
begin
  FRequest.SendParam(AName, AValue);
end;

procedure TFastCGIOutput.CGIOutputError;
begin
  TLHTTPServerSocket(FSocket).FResponseInfo.Status := hsInternalError;
end;

procedure TFastCGIOutput.DoneInput;
begin
  if FRequest <> nil then
    FRequest.DoneInput;
end;

procedure TFastCGIOutput.RequestEnd(ARequest: TLFastCGIRequest);
begin
  FRequest.OnEndRequest := nil;
  FRequest.OnInput := nil;
  FRequest.OnOutput := nil;
  FRequest := nil;
  { trigger final write, to flush output to socket }
  WriteCGIBlock;
end;

procedure TFastCGIOutput.RequestNeedInput(ARequest: TLFastCGIRequest);
begin
  FSocket.ParseBuffer;
end;

procedure TFastCGIOutput.RequestHasOutput(ARequest: TLFastCGIRequest);
begin
  WriteCGIBlock;
end;

procedure TFastCGIOutput.RequestHasStderr(ARequest: TLFastCGIRequest);
var
  lBuf: array[0..1023] of char;
  lRead: integer;
begin
  lRead := ARequest.Get(lBuf, sizeof(lBuf)-1);
  lBuf[lRead] := #0;
  write(pchar(@lBuf[0]));
end;

function  TFastCGIOutput.HandleInput(ABuffer: pchar; ASize: integer): integer;
begin
  Result := FRequest.SendInput(ABuffer, ASize);
end;

function  TFastCGIOutput.WriteCGIData: TWriteBlockStatus;
var
  lRead: integer;
begin
  if FRequest = nil then exit(wsDone);
  if FRequest.OutputDone then exit(wsDone);
  lRead := FRequest.Get(@FBuffer[FReadPos], FBufferSize-FReadPos);
  Inc(FReadPos, lRead);
  Result := InputBufferEmptyToWriteStatus[lRead = 0];
end;

function  TFastCGIOutput.WriteBlock: TWriteBlockStatus;
begin
  if (FRequest <> nil) and FRequest.OutputPending then
  begin
    FRequest.ParseClientBuffer;
    Result := wsWaitingData;
  end else
    Result := inherited;
end;

procedure TFastCGIOutput.StartRequest;
begin
  FRequest.OnEndRequest := @RequestEnd;
  FRequest.OnInput := @RequestNeedInput;
  FRequest.OnOutput := @RequestHasOutput;
  FRequest.OnStderr := @RequestHasStderr;
  inherited;
  FRequest.DoneParams;
end;

{ TFormOutput } 

constructor TFormOutput.Create(ASocket: TLHTTPSocket);
begin
  inherited;
  FRequestVars := TStringList.Create;
end;

destructor TFormOutput.Destroy;
var
  I: integer;
  tmpObj: TObject;
begin
  for I := 0 to FRequestVars.Count - 1 do
  begin
    tmpObj := FRequestVars.Objects[I];
    Finalize(string(tmpObj));
    FRequestVars.Objects[I] := nil;
  end;
  FRequestVars.Free;
  inherited;
end;

function TFormOutput.AddVariables(Variables: pchar; ASize: integer; SepChar: char): integer;
var
  varname, sep, next: pchar;
  strName, strValue: string;
  tmpObj: TObject;
  i: integer;
begin
  if Variables = nil then
    exit(0);
  if ASize = -1 then
    ASize := StrLen(Variables);
  varname := Variables;
  repeat
    sep := varname + IndexChar(varname^, ASize, '=');
    if sep < varname then
      break;
    dec(ASize, sep-varname);
    next := sep + IndexChar(sep^, ASize, SepChar);
    if next < sep then
    begin
      next := sep + ASize;
      ASize := 0;
    end else
      dec(ASize, next+1-sep);
    if sep > varname then
    begin
      setlength(strName, sep-varname);
      move(varname[0], strName[1], sep-varname);
      setlength(strValue, next-sep-1);
      move(sep[1], strValue[1], next-sep-1);
      i := FRequestVars.Add(strName);
      tmpObj := nil;
      string(tmpObj) := strValue;
      FRequestVars.Objects[i] := tmpObj; 
    end;
    varname := next+1;
  until false;
  Result := ASize;
end;

procedure TFormOutput.DoneInput;
begin
  if Assigned(FOnExtraHeaders) then
    FOnExtraHeaders(Self);
  TLHTTPServerSocket(FSocket).StartResponse(Self);
end;

function TFormOutput.HandleInputFormURL(ABuffer: pchar; ASize: integer): integer;
begin
  Result := ASize-AddVariables(ABuffer, ASize, URIParamSepChar)
end;

procedure TFormOutput.ParseMultipartHeader(ABuffer, ALineEnd: pchar);
var
  I: TLMultipartParameter;
  len: integer;
begin
  for I := Low(TLMultipartParameter) to High(TLMultipartParameter) do
  begin
    len := Length(MPParameterStrings[I]);
    if ABuffer+len >= ALineEnd then
      continue;
    if (ABuffer[len] = ':')
      and (StrLIComp(ABuffer, PChar(MPParameterStrings[I]), len) = 0) then
    begin
      Inc(ABuffer, len+2);
      repeat
        if ABuffer = ALineEnd then exit;
        if ABuffer^ <> ' ' then break;
        inc(ABuffer);
      until false;
      FMPParameters[I] := ABuffer;
      if I = mpContentType then
      begin
        repeat
          if ABuffer = ALineEnd then exit;
          if ABuffer = ';' then break;
          inc(ABuffer);
        until false;

      end;
      break;
    end;
  end;
end;

function TFormOutput.FindBoundary(ABuffer: pchar): pchar;
begin
  {$warning TODO}
  Result := nil;
end;

function TFormOutput.HandleInputMultipart(ABuffer: pchar; ASize: integer): integer;
var
  pos, next, endline: pchar;
begin
  pos := ABuffer;
  repeat
    case FMPState of
      msStart:
      begin
        { discard until first boundary }
        next := FindBoundary(pos);
        if next = nil then
          exit(ASize);
        FMPState := msBodypartHeader;
      end;
      msBodypartHeader:
      begin
        endline := pos + IndexChar(pos, ASize, #10);
        if endline < pos then
          exit(pos-ABuffer);
        next := endline+1;
        if (endline > pos) and ((endline-1)^ = #13) then
          dec(endline);
        endline^ := #0;
        if endline > pos then
          ParseMultipartHeader(pos, endline)
        else
          FMPState := msBodypartData;
      end;
      msBodypartData:
      begin
        { decode based on content-transfer-encoding ? }
        { CRLF before boundary, belongs to boundary, not data! }
        next := FindBoundary(ABuffer);
      end;
    else
      exit(ASize);
    end;
    dec(ASize, next-pos);
    pos := next;
  until false;
end;

function TFormOutput.HandleInputDiscard(ABuffer: pchar; ASize: integer): integer;
begin
  Result := ASize;
end;

function TFormOutput.HandleInput(ABuffer: pchar; ASize: integer): integer;
begin
  Result := FHandleInput(ABuffer, ASize);
end;

function TFormOutput.FillBuffer: TWriteBlockStatus;
begin
  Result := wsDone;
  if Assigned(FOnFillBuffer) then
    FOnFillBuffer(Self, Result);
end;

procedure TFormOutput.DeleteCookie(const AName: string; const APath: string = '/'; 
  const ADomain: string = '');
begin
  { cookies expire when expires is in the past, duh }
  SetCookie(AName, '', Now - 7.0, APath, ADomain);
end;

procedure TFormOutput.SetCookie(const AName, AValue: string; const AExpires: TDateTime;
  const APath: string = '/'; const ADomain: string = '');
var
  headers: PStringBuffer;
begin
  headers := @TLHTTPServerSocket(FSocket).FHeaderOut.ExtraHeaders;
  AppendString(headers^, 'Set-Cookie: ' + HTTPEncode(AName) + '=' + HTTPEncode(AValue));
  AppendString(headers^, ';path=' + APath + ';expires=' + FormatDateTime(HTTPDateFormat, AExpires));
  if Length(ADomain) > 0 then
  begin
    AppendString(headers^, ';domain=');
    AppendString(headers^, ADomain);
  end;
  AppendString(headers^, #13#10);
end;

{ TFormHandler }

procedure TFormHandler.SelectMultipart(AFormOutput: TFormOutput; AContentType: pchar);
var
  boundary, endquote: pchar;
begin
  boundary := StrScan(AContentType, '=');
  if boundary <> nil then
  begin
    Inc(boundary);
    if boundary^ = '"' then
    begin
      Inc(boundary);
      endquote := StrScan(boundary, '"');
      if endquote <> nil then
        endquote^ := #0;
    end;
  end;

  AFormOutput.FBoundary := boundary;
  AFormOutput.FHandleInput := @AFormOutput.HandleInputMultipart;
end;

function TFormHandler.HandleURI(ASocket: TLHTTPServerSocket): TOutputItem;
var
  newFormOutput: TFormOutput;
  contentType: pchar;
begin
  if not Assigned(FOnHandleURI) then
    exit(nil);

  newFormOutput := FOnHandleURI(ASocket);
  if newFormOutput = nil then
    exit(nil);

  newFormOutput.AddVariables(ASocket.FRequestInfo.QueryParams, -1, URIParamSepChar);
  newFormOutput.AddVariables(ASocket.Parameters[hpCookie], -1, CookieSepChar);
  contentType := TLHTTPServerSocket(ASocket).Parameters[hpContentType];
  if StrIComp(contentType, FormURLContentType) = 0 then
    newFormOutput.FHandleInput := @newFormOutput.HandleInputFormURL
  else if StrIComp(contentType, MultipartContentType) = 0 then
    SelectMultipart(newFormOutput, contentType)
  else
    newFormOutput.FHandleInput := @newFormOutput.HandleInputDiscard;

  Result := newFormOutput;
end;

end.
