{ HTTP server and client components

  Copyright (C) 2006-2008 Micha Nelissen

  This library is Free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
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

unit lhttp;

{$mode objfpc}{$h+}
{$inline on}

interface

uses
  classes, sysutils, lnet, lnetssl, levents, lhttputil, lstrbuffer;

type
  TLHTTPMethod = (hmHead, hmGet, hmPost, hmUnknown);
  TLHTTPMethods = set of TLHTTPMethod;
  TLHTTPParameter = (hpConnection, hpContentLength, hpContentType,
    hpAccept, hpAcceptCharset, hpAcceptEncoding, hpAcceptLanguage, hpHost,
    hpFrom, hpReferer, hpUserAgent, hpRange, hpTransferEncoding,
    hpIfModifiedSince, hpIfUnmodifiedSince, hpCookie, hpXRequestedWith);
  TLHTTPStatus = (hsUnknown, hsOK, hsNoContent, hsMovedPermanently, hsFound, hsNotModified, 
    hsBadRequest, hsForbidden, hsNotFound, hsPreconditionFailed, hsRequestTooLong,
    hsInternalError, hsNotImplemented, hsNotAllowed);
  TLHTTPTransferEncoding = (teIdentity, teChunked);
  TLHTTPClientError = (ceNone, ceMalformedStatusLine, ceVersionNotSupported,
    ceUnsupportedEncoding);

const
  HTTPDisconnectStatuses = [hsBadRequest, hsRequestTooLong, hsForbidden, 
    hsInternalError, hsNotAllowed];
  HTTPMethodStrings: array[TLHTTPMethod] of string =
    ('HEAD', 'GET', 'POST', '');
  HTTPParameterStrings: array[TLHTTPParameter] of string =
    ('CONNECTION', 'CONTENT-LENGTH', 'CONTENT-TYPE', 'ACCEPT', 
     'ACCEPT-CHARSET', 'ACCEPT-ENCODING', 'ACCEPT-LANGUAGE', 'HOST',
     'FROM', 'REFERER', 'USER-AGENT', 'RANGE', 'TRANSFER-ENCODING',
     'IF-MODIFIED-SINCE', 'IF-UNMODIFIED-SINCE', 'COOKIE', 'X-REQUESTED-WITH');
  HTTPStatusCodes: array[TLHTTPStatus] of dword =
    (0, 200, 204, 301, 302, 304, 400, 403, 404, 412, 414, 500, 501, 405);
  HTTPTexts: array[TLHTTPStatus] of string = 
    ('', 'OK', 'No Content', 'Moved Permanently', 'Found', 'Not Modified', 'Bad Request', 'Forbidden', 
     'Not Found', 'Precondition Failed', 'Request Too Long', 'Internal Error',
     'Method Not Implemented', 'Method Not Allowed');
  HTTPDescriptions: array[TLHTTPStatus] of string = (
      { hsUnknown }
    '',
      { hsOK }
    '',
      { hsNoContent }
    '',
      { hsMovedPermanently }
    '',
      { hsFound }
    '',
      { hsNotModified }
    '',
      { hsBadRequest }
    '<html><head><title>400 Bad Request</title></head><body>'+#10+
    '<h1>Bad Request</h1>'+#10+
    '<p>Your browser did a request this server did not understand.</p>'+#10+
    '</body></html>'+#10,
      { hsForbidden }
    '<html><head><title>403 Forbidden</title></head><body>'+#10+
    '<h1>Forbidden</h1>'+#10+
    '<p>You do not have permission to access this resource.</p>'+#10+
    '</body></html>'+#10,
      { hsNotFound }
    '<html><head><title>404 Not Found</title></head><body>'+#10+
    '<h1>Not Found</h1>'+#10+
    '<p>The requested URL was not found on this server.</p>'+#10+
    '</body></html>'+#10,
      { hsPreconditionFailed }
    '<html><head><title>412 Precondition Failed</title></head><body>'+#10+
    '<h1>Precondition Failed</h1>'+#10+
    '<p>The precondition on the request evaluated to false.</p>'+#10+
    '</body></html>'+#10,
      { hsRequestTooLong }
    '<html><head><title>414 Request Too Long</title></head><body>'+#10+
    '<h1>Bad Request</h1>'+#10+
    '<p>Your browser did a request that was too long for this server to parse.</p>'+#10+
    '</body></html>'+#10,
      { hsInternalError }
    '<html><head><title>500 Internal Error</title></head><body>'+#10+
    '<h1>Internal Error</h1>'+#10+
    '<p>An error occurred while generating the content for this request.</p>'+#10+
    '</body></html>'+#10,
      { hsNotImplemented }
    '<html><head><title>501 Method Not Implemented</title></head><body>'+#10+
    '<h1>Method Not Implemented</h1>'+#10+
    '<p>The method used in the request is invalid.</p>'+#10+
    '</body></html>'+#10,
      { hsNotAllowed }
    '<html><head><title>405 Method Not Allowed</title></head><body>'+#10+
    '<h1>Method Not Allowed</h1>'+#10+
    '<p>The method used in the request is not allowed on the resource specified in the URL.</p>'+#10+
    '</body></html>'+#10);


type
  TLHTTPSocket = class;
  TLHTTPConnection = class;
  TLHTTPClientSocket = class;
  
  PRequestInfo = ^TRequestInfo;
  TRequestInfo = record
    RequestType: TLHTTPMethod;
    DateTime: TDateTime;
    Method: pchar;
    Argument: pchar;
    QueryParams: pchar;
    VersionStr: pchar;
    Version: dword;
  end;

  PClientRequest = ^TClientRequest;
  TClientRequest = record
    Method: TLHTTPMethod;
    URI: string;
    QueryParams: string;
    RangeStart: qword;
    RangeEnd: qword;
  end;

  PClientResponse = ^TClientResponse;
  TClientResponse = record
    Status: TLHTTPStatus;
    Version: dword;
    Reason: string;
  end;

  PHeaderOutInfo = ^THeaderOutInfo;
  THeaderOutInfo = record
    ContentLength: integer;
    TransferEncoding: TLHTTPTransferEncoding;
    ExtraHeaders: TStringBuffer;
    Version: dword;
  end;

  PResponseInfo = ^TResponseInfo;
  TResponseInfo = record
    Status: TLHTTPStatus;
    ContentType: string;
    ContentCharset: string;
    LastModified: TDateTime;
  end;

  TWriteBlockStatus = (wsPendingData, wsWaitingData, wsDone);
  TWriteBlockMethod = function: TWriteBlockStatus of object;

  TOutputItem = class(TObject)
  protected
    FBuffer: pchar;
    FBufferPos: integer;
    FBufferSize: integer;
    FBufferOffset: integer;
    FOutputPending: boolean;
    FEof: boolean;
    FPrev: TOutputItem;
    FNext: TOutputItem;
    FPrevDelayFree: TOutputItem;
    FNextDelayFree: TOutputItem;
    FSocket: TLHTTPSocket;
    FWriteBlock: TWriteBlockMethod;

    procedure DoneInput; virtual;
    function  HandleInput(ABuffer: pchar; ASize: integer): integer; virtual;
    function  WriteBlock: TWriteBlockStatus; virtual;
  public
    constructor Create(ASocket: TLHTTPSocket);
    destructor Destroy; override;

    procedure LogError(const AMessage: string);

    property Socket: TLHTTPSocket read FSocket;
  end;

  TProcMethod = procedure of object;

  TBufferOutput = class(TOutputItem)
  protected
    FPrepareBuffer: TProcMethod;
    FFinishBuffer: TProcMethod;
    FBufferMemSize: integer;

    procedure PrepareBuffer;
    procedure PrepareChunk;
    procedure FinishBuffer;
    procedure FinishChunk;
    procedure SelectChunked;
    procedure SelectBuffered;
    procedure SelectPlain;
    procedure PrependBufferOutput(MinBufferSize: integer);
    procedure PrependStreamOutput(AStream: TStream; AFree: boolean);
    function FillBuffer: TWriteBlockStatus; virtual; abstract;
    function WriteChunk: TWriteBlockStatus;
    function WriteBuffer: TWriteBlockStatus;
    function WritePlain: TWriteBlockStatus;
    function WriteBlock: TWriteBlockStatus; override;
  public
    constructor Create(ASocket: TLHTTPSocket);
    destructor Destroy; override;

    procedure Add(ABuf: pointer; ASize: integer);
    procedure Add(const AStr: string);
    procedure Add(AStream: TStream; AQueue: boolean = false; AFree: boolean = true);
  end;

  TMemoryOutput = class(TOutputItem)
  protected
    FFreeBuffer: boolean;
  public
    constructor Create(ASocket: TLHTTPSocket; ABuffer: pointer; 
      ABufferOffset, ABufferSize: integer; AFreeBuffer: boolean);
    destructor Destroy; override;
  end;

  TStreamOutput = class(TBufferOutput)
  protected
    FStream: TStream;
    FFreeStream: boolean;
    FStreamSize: integer;

    function FillBuffer: TWriteBlockStatus; override;
  public
    constructor Create(ASocket: TLHTTPSocket; AStream: TStream; AFreeStream: boolean);
    destructor Destroy; override;
  end;
  
  TMemoryStreamOutput = class(TOutputItem)
  protected
    FFreeStream: boolean;
    FStream: TMemoryStream;

    function WriteBlock: TWriteBlockStatus; override;
  public
    constructor Create(ASocket: TLHTTPSocket; AStream: TMemoryStream; AFreeStream: boolean);
    destructor Destroy; override;
  end;

  TChunkState = (csInitial, csData, csDataEnd, csTrailer, csFinished);
  TLHTTPParameterArray = array[TLHTTPParameter] of pchar;
  
  TParseBufferMethod = function: boolean of object;
  TLInputEvent = function(ASocket: TLHTTPClientSocket; ABuffer: pchar; ASize: integer): integer of object;
  TLCanWriteEvent = procedure(ASocket: TLHTTPClientSocket; var OutputEof: TWriteBlockStatus) of object;
  TLHTTPClientEvent = procedure(ASocket: TLHTTPClientSocket) of object;

  TLHTTPConnection = class(TLTcp)
  protected
    procedure CanSendEvent(aSocket: TLHandle); override;
    procedure LogAccess(const AMessage: string); virtual;
    procedure ReceiveEvent(aSocket: TLHandle); override;
  public
    destructor Destroy; override;
  end;

  TLHTTPSocket = class(TLSSLSocket)
  protected
    FBuffer: pchar;
    FBufferPos: pchar;
    FBufferEnd: pchar;
    FBufferSize: integer;
    FRequestBuffer: pchar;
    FRequestPos: pchar;
    FRequestInputDone: boolean;
    FRequestHeaderDone: boolean;
    FOutputDone: boolean;
    FInputRemaining: integer;
    FChunkState: TChunkState;
    FCurrentInput: TOutputItem;
    FCurrentOutput: TOutputItem;
    FLastOutput: TOutputItem;
    FKeepAlive: boolean;
    FParseBuffer: TParseBufferMethod;
    FParameters: TLHTTPParameterArray;
    FDelayFreeItems: TOutputItem;

    procedure AddContentLength(ALength: integer); virtual; abstract;
    function  CalcAvailableBufferSpace: integer;
    procedure DelayFree(AOutputItem: TOutputItem);
    procedure DoneBuffer(AOutput: TBufferOutput); virtual;
    procedure FreeDelayFreeItems;
    procedure LogAccess(const AMessage: string); virtual;
    procedure LogMessage; virtual;
    procedure FlushRequest; virtual;
    procedure PackRequestBuffer;
    procedure PackInputBuffer;
    function  ParseRequest: boolean;
    function  ParseEntityPlain: boolean;
    function  ParseEntityChunked: boolean;
    procedure ParseLine(pLineEnd: pchar); virtual;
    procedure ParseParameterLine(pLineEnd: pchar);
    function  ProcessEncoding: boolean;
    procedure ProcessHeaders; virtual; abstract;
    procedure RelocateVariable(var AVar: pchar);
    procedure RelocateVariables; virtual;
    procedure ResetDefaults; virtual;
    function  SetupEncoding(AOutputItem: TBufferOutput; AHeaderOut: PHeaderOutInfo): boolean;
    procedure WriteError(AStatus: TLHTTPStatus); virtual;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure AddToOutput(AOutputItem: TOutputItem);
    procedure Disconnect(const Forced: Boolean = True); override;
    procedure PrependOutput(ANewItem, AItem: TOutputItem);
    procedure RemoveOutput(AOutputItem: TOutputItem);
    procedure HandleReceive;
    function  ParseBuffer: boolean;
    procedure WriteBlock;
    
    property Parameters: TLHTTPParameterArray read FParameters;
  end;

  { http server }

  TSetupEncodingState = (seNone, seWaitHeaders, seStartHeaders);
  
  TLHTTPServerSocket = class(TLHTTPSocket)
  protected
    FLogMessage: TStringBuffer;
    FSetupEncodingState: TSetupEncodingState;

    procedure AddContentLength(ALength: integer); override;
    procedure DoneBuffer(AOutput: TBufferOutput); override;
    procedure FlushRequest; override;
    function  HandleURI: TOutputItem; virtual;
    procedure LogAccess(const AMessage: string); override;
    procedure LogMessage; override;
    procedure RelocateVariables; override;
    procedure ResetDefaults; override;
    procedure ParseLine(pLineEnd: pchar); override;
    procedure ParseRequestLine(pLineEnd: pchar);
    function  PrepareResponse(AOutputItem: TOutputItem; ACustomErrorMessage: boolean): boolean;
    procedure ProcessHeaders; override;
    procedure WriteError(AStatus: TLHTTPStatus); override;
    procedure WriteHeaders(AHeaderResponse, ADataResponse: TOutputItem);
  public
    FHeaderOut: THeaderOutInfo;
    FRequestInfo: TRequestInfo;
    FResponseInfo: TResponseInfo;

    constructor Create; override;
    destructor Destroy; override;

    function  SetupEncoding(AOutputItem: TBufferOutput): boolean;
    procedure StartMemoryResponse(AOutputItem: TMemoryOutput; ACustomErrorMessage: boolean = false);
    procedure StartResponse(AOutputItem: TBufferOutput; ACustomErrorMessage: boolean = false);
  end;
  
  TURIHandler = class(TObject)
  private
    FNext: TURIHandler;
    FMethods: TLHTTPMethods;
  protected
    function HandleURI(ASocket: TLHTTPServerSocket): TOutputItem; virtual; abstract;
    procedure RegisterWithEventer(AEventer: TLEventer); virtual;
  public
    constructor Create;

    property Methods: TLHTTPMethods read FMethods write FMethods;
  end;

  TLAccessEvent = procedure(AMessage: string) of object;

  TLHTTPServer = class(TLHTTPConnection)
  protected
    FHandlerList: TURIHandler;
    FLogMessageTZString: string;
    FServerSoftware: string;
    FOnAccess: TLAccessEvent;

    function HandleURI(ASocket: TLHTTPServerSocket): TOutputItem;
  protected
    procedure LogAccess(const AMessage: string); override;
    procedure RegisterWithEventer; override;
  public
    constructor Create(AOwner: TComponent); override;

    procedure RegisterHandler(AHandler: TURIHandler);

    property ServerSoftware: string read FServerSoftware write FServerSoftware;
    property OnAccess: TLAccessEvent read FOnAccess write FOnAccess;
  end;

  { http client }

  TLHTTPClientSocket = class(TLHTTPSocket)
  protected
    FRequest: PClientRequest;
    FResponse: PClientResponse;
    FHeaderOut: PHeaderOutInfo;
    FError: TLHTTPClientError;
    
    procedure AddContentLength(ALength: integer); override;
    function  GetResponseReason: string;
    function  GetResponseStatus: TLHTTPStatus;
    procedure Cancel(AError: TLHTTPClientError);
    procedure ParseLine(pLineEnd: pchar); override;
    procedure ParseStatusLine(pLineEnd: pchar);
    procedure ProcessHeaders; override;
    procedure ResetDefaults; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure SendRequest;

    property Error: TLHTTPClientError read FError write FError;
    property Response: PClientResponse read FResponse;
    property ResponseReason: string read GetResponseReason;
    property ResponseStatus: TLHTTPStatus read GetResponseStatus;
  end;

  TLHTTPClientState = (hcsIdle, hcsWaiting, hcsReceiving);

  TLHTTPClient = class(TLHTTPConnection)
  protected
    FRequest: TClientRequest;
    FResponse: TClientResponse;
    FHeaderOut: THeaderOutInfo;
    FState: TLHTTPClientState;
    FPendingResponses: integer;
    FOutputEof: boolean;
    FOnCanWrite: TLCanWriteEvent;
    FOnDoneInput: TLHTTPClientEvent;
    FOnInput: TLInputEvent;
    FOnProcessHeaders: TLHTTPClientEvent;
    
    procedure ConnectEvent(aSocket: TLHandle); override;
    procedure DoDoneInput(ASocket: TLHTTPClientSocket);
    function  DoHandleInput(ASocket: TLHTTPClientSocket; ABuffer: pchar; ASize: integer): integer;
    procedure DoProcessHeaders(ASocket: TLHTTPClientSocket);
    function  DoWriteBlock(ASocket: TLHTTPClientSocket): TWriteBlockStatus;
    function  InitSocket(aSocket: TLSocket): TLSocket; override;
    procedure InternalSendRequest;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AddExtraHeader(const AHeader: string);
    procedure AddCookie(const AName, AValue: string; const APath: string = '';
      const ADomain: string = ''; const AVersion: string = '0');
    procedure ResetRange;
    procedure SendRequest;

    property ContentLength: integer read FHeaderOut.ContentLength write FHeaderOut.ContentLength;
    property Method: TLHTTPMethod read FRequest.Method write FRequest.Method;
    property PendingResponses: integer read FPendingResponses;
    property RangeStart: qword read FRequest.RangeStart write FRequest.RangeStart;
    property RangeEnd: qword read FRequest.RangeEnd write FRequest.RangeEnd;
    property Request: TClientRequest read FRequest;
    property State: TLHTTPClientState read FState;
    property URI: string read FRequest.URI write FRequest.URI;
    property Response: TClientResponse read FResponse;
    property OnCanWrite: TLCanWriteEvent read FOnCanWrite write FOnCanWrite;
    property OnDoneInput: TLHTTPClientEvent read FOnDoneInput write FOnDoneInput;
    property OnInput: TLInputEvent read FOnInput write FOnInput;
    property OnProcessHeaders: TLHTTPClientEvent read FOnProcessHeaders write FOnProcessHeaders;
  end;

implementation

uses
  lCommon;

const
  RequestBufferSize = 1024;
  DataBufferSize = 16*1024;

  BufferEmptyToWriteStatus: array[boolean] of TWriteBlockStatus =
    (wsPendingData, wsDone);
  EofToWriteStatus: array[boolean] of TWriteBlockStatus =
    (wsWaitingData, wsDone);

{ helper functions }

function TrySingleDigit(ADigit: char; out OutDigit: byte): boolean;
begin
  Result := (ord(ADigit) >= ord('0')) and (ord(ADigit) <= ord('9'));
  if not Result then exit;
  OutDigit := ord(ADigit) - ord('0');
end;

function HTTPVersionCheck(AStr, AStrEnd: pchar; out AVersion: dword): boolean;
var
  lMajorVersion, lMinorVersion: byte;
begin
  Result := ((AStrEnd-AStr) = 8) 
    and CompareMem(AStr, pchar('HTTP/'), 5)
    and TrySingleDigit(AStr[5], lMajorVersion) 
    and (AStr[6] = '.')
    and TrySingleDigit(AStr[7], lMinorVersion);
  AVersion := lMajorVersion * 10 + lMinorVersion;
end;

function CodeToHTTPStatus(ACode: dword): TLHTTPStatus;
begin
  for Result := Low(TLHTTPStatus) to High(TLHTTPStatus) do
    if HTTPStatusCodes[Result] = ACode then exit;
  Result := hsUnknown;
end;

const
   HexDigits: array[0..15] of char = '0123456789ABCDEF';

function HexReverse(AValue: dword; ABuffer: pchar): integer;
begin
  Result := 0;
  repeat
    ABuffer^ := HexDigits[AValue and $F];
    AValue := AValue shr 4;
    Dec(ABuffer);
    Inc(Result);
  until AValue = 0;
end;

procedure HexToInt(ABuffer: pchar; out AValue: dword; out ACode: integer);
var
  Val, Incr: dword;
  Start: pchar;
begin
  Val := 0;
  ACode := 0;
  Start := ABuffer;
  while ABuffer^ <> #0 do
  begin
    if (ABuffer^ >= '0') and (ABuffer^ <= '9') then
      Incr := ord(ABuffer^) - ord('0')
    else if (ABuffer^ >= 'A') and (ABuffer^ <= 'F') then
      Incr := ord(ABuffer^) - ord('A') + 10
    else if (ABuffer^ >= 'a') and (ABuffer^ <= 'f') then
      Incr := ord(ABuffer^) - ord('a') + 10
    else begin
      ACode := ABuffer - Start + 1;
      break;
    end;
    Val := (Val shl 4) + Incr;
    Inc(ABuffer);
  end;
  AValue := Val;
end;

function EscapeCookie(const AInput: string): string;
begin
  Result := StringReplace(AInput, ';', '%3B', [rfReplaceAll]);
end;

{ TURIHandler }

constructor TURIHandler.Create;
begin
  FMethods := [hmHead, hmGet, hmPost];
end;

procedure TURIHandler.RegisterWithEventer(AEventer: TLEventer);
begin
end;

{ TOutputItem }

constructor TOutputItem.Create(ASocket: TLHTTPSocket);
begin
  FSocket := ASocket;
  inherited Create;
end;

destructor TOutputItem.Destroy;
begin
  if FSocket.FCurrentInput = Self then
    FSocket.FCurrentInput := nil;
    
  if FPrevDelayFree = nil then
    FSocket.FDelayFreeItems := FNextDelayFree
  else
    FPrevDelayFree.FNextDelayFree := FNextDelayFree;
  if FNextDelayFree <> nil then
    FNextDelayFree.FPrevDelayFree := FPrevDelayFree;

  inherited;
end;

procedure TOutputItem.DoneInput;
begin
end;

function TOutputItem.HandleInput(ABuffer: pchar; ASize: integer): integer;
begin
  { discard input }
  Result := ASize;
end;

procedure TOutputItem.LogError(const AMessage: string);
begin
  FSocket.LogError(AMessage, 0);
end;

function TOutputItem.WriteBlock: TWriteBlockStatus;
var
  lWritten: integer;
begin
  if FOutputPending then
  begin
    if FBufferSize > FBufferPos then
    begin
      lWritten := FSocket.Send(FBuffer[FBufferPos], FBufferSize-FBufferPos);
      Inc(FBufferPos, lWritten);
    end;
    FOutputPending := FBufferPos < FBufferSize;
    Result := BufferEmptyToWriteStatus[not FOutputPending];
  end else
    Result := EofToWriteStatus[FEof];
end;

const
  ReserveChunkBytes = 12;

constructor TBufferOutput.Create(ASocket: TLHTTPSocket);
begin
  inherited;
  GetMem(FBuffer, DataBufferSize);
  FWriteBlock := @WritePlain;
  FPrepareBuffer := @PrepareBuffer;
  FFinishBuffer := @FinishBuffer;
  FBufferMemSize := DataBufferSize;
end;

destructor TBufferOutput.Destroy;
begin
  inherited;
  FreeMem(FBuffer);
end;

procedure TBufferOutput.Add(ABuf: pointer; ASize: integer);
var
  copySize: integer;
begin
  repeat
    copySize := FBufferSize-FBufferPos;
    if copySize > ASize then
      copySize := ASize;
    Move(ABuf^, FBuffer[FBufferPos], copySize);
    Inc(FBufferPos, copySize);
    Dec(ASize, copySize);
    if ASize = 0 then
      break;
    PrependBufferOutput(ASize);
  until false;
end;

procedure TBufferOutput.Add(const AStr: string);
begin
  Add(PChar(AStr), Length(AStr));
end;

procedure TBufferOutput.PrependStreamOutput(AStream: TStream; AFree: boolean);
begin
  if AStream is TMemoryStream then
    FSocket.PrependOutput(TMemoryStreamOutput.Create(FSocket, TMemoryStream(AStream), AFree), Self)
  else
    FSocket.PrependOutput(TStreamOutput.Create(FSocket, AStream, AFree), Self);
end;

procedure TBufferOutput.Add(AStream: TStream; AQueue: boolean = false; 
  AFree: boolean = true);
var
  size, copySize: integer;
begin
  size := AStream.Size - AStream.Position;
  repeat
    copySize := FBufferSize-FBufferPos;
    if copySize > size then
      copySize := size;
    AStream.Read(FBuffer[FBufferPos], copySize);
    Inc(FBufferPos, copySize);
    Dec(size, copySize);
    if size = 0 then
      break;
    if AQueue then
    begin
      PrependBufferOutput(0);
      PrependStreamOutput(AStream, AFree);
    end else begin
      PrependBufferOutput(size);
    end;
  until false;
end;

procedure TBufferOutput.PrepareChunk;
begin
  { 12 bytes for starting space, 7 bytes to end: <CR><LF>0<CR><LF><CR><LF> }
  FBufferPos := ReserveChunkBytes;
  FBufferOffset := FBufferPos;
  FBufferSize := FBufferMemSize-7;
end;

procedure TBufferOutput.FinishChunk;
var
  lOffset: integer;
begin
  lOffset := HexReverse(FBufferPos-FBufferOffset, FBuffer+FBufferOffset-3);
  FBuffer[FBufferOffset-2] := #13;
  FBuffer[FBufferOffset-1] := #10;
  FBuffer[FBufferPos] := #13;
  FBuffer[FBufferPos+1] := #10;
  FBufferSize := FBufferPos+2;
  FBufferPos := FBufferOffset-lOffset-2;
end;

procedure TBufferOutput.PrepareBuffer;
  { also for "plain" encoding }
begin
  FBufferPos := 0;
  FBufferOffset := 0;
  FBufferSize := FBufferMemSize;
end;

procedure TBufferOutput.FinishBuffer;
begin
  { nothing to do }
end;

procedure TBufferOutput.PrependBufferOutput(MinBufferSize: integer);
begin
  FFinishBuffer();
  FSocket.PrependOutput(TMemoryOutput.Create(FSocket, FBuffer, FBufferOffset,
    FBufferPos, true), Self);
  FBufferMemSize := MinBufferSize;
  if FBufferMemSize < DataBufferSize then
    FBufferMemSize := DataBufferSize;
  FBuffer := GetMem(FBufferMemSize);
  FPrepareBuffer();
end;

function TBufferOutput.WriteChunk: TWriteBlockStatus;
begin
  if not FOutputPending and not FEof then
  begin
    Result := FillBuffer;
    FEof := Result = wsDone;
    FOutputPending := FBufferPos > FBufferOffset;
    if FOutputPending then
      FinishChunk;
    if FEof then
    begin
      if not FOutputPending then
      begin
        { FBufferPos/Size still in "fill mode" }
        FBufferSize := 0;
        FBufferPos := 0;
        FOutputPending := true;
      end;
      FBuffer[FBufferSize] := '0';
      FBuffer[FBufferSize+1] := #13;
      FBuffer[FBufferSize+2] := #10;
      { no trailer }
      FBuffer[FBufferSize+3] := #13;
      FBuffer[FBufferSize+4] := #10;
      inc(FBufferSize, 5);
    end;
  end else   
    Result := EofToWriteStatus[FEof];
  if FOutputPending then
  begin
    Result := inherited WriteBlock;
    if (Result = wsDone) and not FEof then
    begin
      Result := wsPendingData;
      PrepareChunk;
    end;
  end;
end;
  
function TBufferOutput.WriteBuffer: TWriteBlockStatus;
begin
  if not FOutputPending then
  begin
    Result := FillBuffer;
    FEof := Result = wsDone;
    FOutputPending := FEof;
    if FOutputPending or (FBufferPos = FBufferSize) then
    begin
      if FBufferPos > FBufferOffset then
      begin
        FSocket.AddContentLength(FBufferPos-FBufferOffset);
        if not FEof then
          PrependBufferOutput(0)
        else begin
          FBufferSize := FBufferPos;
          FBufferPos := FBufferOffset;
        end;
      end else begin
        FBufferPos := 0;
        FBufferSize := 0;
      end;
      if FEof then
        FSocket.DoneBuffer(Self);
    end;
  end else
    Result := EofToWriteStatus[FEof];
  if Result = wsDone then
    Result := inherited WriteBlock;
end;

function TBufferOutput.WritePlain: TWriteBlockStatus;
begin
  if not FOutputPending then
  begin
    Result := FillBuffer;
    FEof := Result = wsDone;
    if FBufferPos > FBufferOffset then
    begin
      FOutputPending := true;
      FBufferSize := FBufferPos;
      FBufferPos := FBufferOffset;
    end else begin
      FBufferSize := 0;
      FBufferPos := 0;
    end;
  end;
  Result := inherited WriteBlock;
  if Result <> wsPendingData then
  begin
    PrepareBuffer;
    if not FEof then
      Result := wsPendingData;
  end;
end;

function TBufferOutput.WriteBlock: TWriteBlockStatus;
begin
  Result := FWriteBlock();
end;

procedure TBufferOutput.SelectChunked;
begin
  FPrepareBuffer := @PrepareChunk;
  FWriteBlock := @WriteChunk;
  FFinishBuffer := @FinishChunk;
  PrepareChunk;
end;
  
procedure TBufferOutput.SelectBuffered;
begin
  FPrepareBuffer := @PrepareBuffer;
  FWriteBlock := @WriteBuffer;
  FFinishBuffer := @FinishBuffer;
  PrepareBuffer;
end;
  
procedure TBufferOutput.SelectPlain;
begin
  FPrepareBuffer := @PrepareBuffer;
  FWriteBlock := @WritePlain;
  FFinishBuffer := @FinishBuffer;
  PrepareBuffer;
end;

{ TMemoryOutput }

constructor TMemoryOutput.Create(ASocket: TLHTTPSocket; ABuffer: pointer; 
  ABufferOffset, ABufferSize: integer; AFreeBuffer: boolean);
begin
  inherited Create(ASocket);

  FBuffer := ABuffer;
  FBufferPos := ABufferOffset;
  FBufferSize := ABufferSize;
  FFreeBuffer := AFreeBuffer;
  FOutputPending := true;
end;

destructor TMemoryOutput.Destroy;
begin
  inherited;
  if FFreeBuffer then
    FreeMem(FBuffer);
end;

{ TStreamOutput }

constructor TStreamOutput.Create(ASocket: TLHTTPSocket; AStream: TStream; AFreeStream: boolean);
begin
  inherited Create(ASocket);
  FStream := AStream;
  FFreeStream := AFreeStream;
  FStreamSize := AStream.Size;
end;

destructor TStreamOutput.Destroy;
begin
  if FFreeStream then
    FStream.Free;
  inherited;
end;

function TStreamOutput.FillBuffer: TWriteBlockStatus;
var
  lRead: integer;
begin
  lRead := FStream.Read(FBuffer[FBufferPos], FBufferSize-FBufferPos);
  Inc(FBufferPos, lRead);
  Result := BufferEmptyToWriteStatus[FStream.Position >= FStreamSize];
end;

{ TMemoryStreamOutput }

constructor TMemoryStreamOutput.Create(ASocket: TLHTTPSocket; AStream: TMemoryStream; 
  AFreeStream: boolean);
begin
  inherited Create(ASocket);
  FStream := AStream;
  FFreeStream := AFreeStream;
  FOutputPending := true;
end;

destructor TMemoryStreamOutput.Destroy;
begin
  if FFreeStream then
    FStream.Free;
  inherited;
end;

function TMemoryStreamOutput.WriteBlock: TWriteBlockStatus;
var
  lWritten: integer;
begin
  if not FOutputPending then
    exit(wsDone);

  lWritten := FSocket.Send(PByte(FStream.Memory)[FStream.Position], FStream.Size-FStream.Position);
  FStream.Position := FStream.Position + lWritten;
  FOutputPending := FStream.Position < FStream.Size;
  FEof := not FOutputPending;
  Result := EofToWriteStatus[FEof];
end;

{ TLHTTPSocket }

constructor TLHTTPSocket.Create;
begin
  inherited;

  FBuffer := GetMem(RequestBufferSize);
  FBufferSize := RequestBufferSize;
  FBufferPos := FBuffer;
  FBufferEnd := FBufferPos;
  FBuffer[0] := #0;
  FKeepAlive := true;
end;

destructor TLHTTPSocket.Destroy;
begin
  FreeDelayFreeItems;
  inherited;
  FreeMem(FBuffer);
end;

procedure TLHTTPSocket.Disconnect(const Forced: Boolean = True);
var
  lOutput: TOutputItem;
begin
  inherited Disconnect(Forced);

  while FCurrentOutput <> nil do
  begin
    lOutput := FCurrentOutput;
    FCurrentOutput := FCurrentOutput.FNext;
    lOutput.Free;
  end;
  if FCurrentInput <> nil then
    FreeAndNil(FCurrentInput);
end;

procedure TLHTTPSocket.FreeDelayFreeItems;
var
  lItem: TOutputItem;
begin
  while FDelayFreeItems <> nil do
  begin
    lItem := FDelayFreeItems;
    FDelayFreeItems := FDelayFreeItems.FNextDelayFree;
    lItem.Free;
  end;
end;

procedure TLHTTPSocket.DelayFree(AOutputItem: TOutputItem);
begin
  if AOutputItem = nil then exit;
  { check whether already in delayed free list }
  if AOutputItem = FDelayFreeItems then exit;
  if AOutputItem.FPrevDelayFree <> nil then exit;
  if FDelayFreeItems <> nil then
    FDelayFreeItems.FPrevDelayFree := AOutputItem;
  AOutputItem.FNextDelayFree := FDelayFreeItems;
  FDelayFreeItems := AOutputItem;
end;

procedure TLHTTPSocket.DoneBuffer(AOutput: TBufferOutput);
begin
end;

procedure TLHTTPSocket.LogMessage;
begin
end;

procedure TLHTTPSocket.LogAccess(const AMessage: string);
begin
end;

procedure TLHTTPSocket.WriteError(AStatus: TLHTTPStatus);
begin
end;

procedure TLHTTPSocket.AddToOutput(AOutputItem: TOutputItem);
begin
  AOutputItem.FPrev := FLastOutput;
  if FLastOutput <> nil then
  begin
    FLastOutput.FNext := AOutputItem;
  end else begin
    FCurrentOutput := AOutputItem;
  end;
  FLastOutput := AOutputItem;
end;

procedure TLHTTPSocket.PrependOutput(ANewItem, AItem: TOutputItem);
begin
  ANewItem.FPrev := AItem.FPrev;
  ANewItem.FNext := AItem;
  AItem.FPrev := ANewItem;
  if FCurrentOutput = AItem then
    FCurrentOutput := ANewItem;
end;

procedure TLHTTPSocket.RemoveOutput(AOutputItem: TOutputItem);
begin
  if AOutputItem.FPrev <> nil then
    AOutputItem.FPrev.FNext := AOutputItem.FNext;
  if AOutputItem.FNext <> nil then
    AOutputItem.FNext.FPrev := AOutputItem.FPrev;
  if FLastOutput = AOutputItem then
    FLastOutput := AOutputItem.FPrev;
  if FCurrentOutput = AOutputItem then
    FCurrentOutput := AOutputItem.FNext;
  AOutputItem.FPrev := nil;
  AOutputItem.FNext := nil;
end;

procedure TLHTTPSocket.ResetDefaults;
begin
  FParseBuffer := @ParseRequest;
end;

procedure TLHTTPSocket.FlushRequest;
begin
  FillDWord(FParameters, sizeof(FParameters) div 4, 0);
  ResetDefaults;
end;

function TLHTTPSocket.CalcAvailableBufferSpace: integer;
begin
  Result := FBufferSize-(FBufferEnd-FBuffer)-1;
end;

procedure TLHTTPSocket.HandleReceive;
var
  lRead: integer;
begin
  if FRequestInputDone then 
  begin
    IgnoreRead := true;
    exit;
  end;

  lRead := CalcAvailableBufferSpace;
  { if buffer has filled up, keep ignoring and continue parsing requests }
  if lRead > 0 then
  begin
    IgnoreRead := false;
    lRead := Get(FBufferEnd^, lRead);
    if lRead = 0 then exit;
    Inc(FBufferEnd, lRead);
    FBufferEnd^ := #0;
  end;
  ParseBuffer;

  if FIgnoreWrite then
    WriteBlock;
end;

procedure TLHTTPSocket.RelocateVariable(var AVar: pchar);
begin
  if AVar = nil then exit;
  AVar := FBuffer + (AVar - FRequestPos);
end;

procedure TLHTTPSocket.RelocateVariables;
var
  I: TLHTTPParameter;
begin
  for I := Low(TLHTTPParameter) to High(TLHTTPParameter) do
    RelocateVariable(FParameters[I]);
end;

procedure TLHTTPSocket.PackRequestBuffer;
var
  lBytesLeft: integer;
  lFreeBuffer: pchar;
begin
  if (FRequestBuffer <> nil) and (FBufferEnd-FBufferPos <= RequestBufferSize) then
  begin
    { switch back to normal size buffer }
    lFreeBuffer := FBuffer;
    FBuffer := FRequestBuffer;
    FBufferSize := RequestBufferSize;
    FRequestBuffer := nil;
  end else
    lFreeBuffer := nil;
  if FRequestPos <> nil then
  begin
    lBytesLeft := FBufferEnd-FRequestPos;
    FBufferEnd := FBuffer+lBytesLeft;
    RelocateVariable(FBufferPos);
    RelocateVariables;
    { include null-terminator, where FBufferEnd is pointing at }
    Move(FRequestPos^, FBuffer^, lBytesLeft+1);
    FRequestPos := nil;
  end;
  if lFreeBuffer <> nil then
    FreeMem(lFreeBuffer);
end;

procedure TLHTTPSocket.PackInputBuffer;
var
  lBytesLeft: integer;
begin
  { use bigger buffer for more speed }
  if FRequestBuffer = nil then
  begin
    FRequestBuffer := FBuffer;
    FBuffer := GetMem(DataBufferSize);
    FBufferSize := DataBufferSize;
    FRequestPos := nil;
  end;
  lBytesLeft := FBufferEnd-FBufferPos;
  Move(FBufferPos^, FBuffer^, lBytesLeft);
  FBufferEnd := FBuffer+lBytesLeft;
  FBufferPos := FBuffer;
end;

function TLHTTPSocket.ParseEntityPlain: boolean;
var
  lNumBytes: integer;
begin
  lNumBytes := FBufferEnd - FBufferPos;
  if lNumBytes > FInputRemaining then
    lNumBytes := FInputRemaining;
  { if no output item to feed into, discard }
  if FCurrentInput <> nil then
    lNumBytes := FCurrentInput.HandleInput(FBufferPos, lNumBytes);
  inc(FBufferPos, lNumBytes);
  dec(FInputRemaining, lNumBytes);
  Result := FInputRemaining > 0;
  { prepare for more data, if more data coming }
  if Result and (FBufferPos+FInputRemaining > FBuffer+FBufferSize) then
    PackInputBuffer;
end;

function TLHTTPSocket.ParseEntityChunked: boolean;
var
  lLineEnd, lNextLine: pchar;
  lCode: integer;
begin
  repeat
    if FChunkState = csFinished then
      exit(false);
    if FChunkState = csData then
      if ParseEntityPlain then 
        exit(true)
      else
        FChunkState := csDataEnd;
    
    lLineEnd := StrScan(FBufferPos, #10);
    if lLineEnd = nil then
      exit(true);
    
    lNextLine := lLineEnd+1;
    if (lLineEnd > FBufferPos) and ((lLineEnd-1)^ = #13) then
      dec(lLineEnd);
    case FChunkState of 
      csInitial:
      begin
        lLineEnd^ := #0;
        HexToInt(FBufferPos, dword(FInputRemaining), lCode);
        if lCode = 1 then
        begin
          FChunkState := csFinished;
          Disconnect;
          exit(false);
        end;
        if FInputRemaining = 0 then
          FChunkState := csTrailer
        else
          FChunkState := csData;
      end;
      csDataEnd:
      begin
        { skip empty line }
        FChunkState := csInitial;
      end;
      csTrailer:
      begin
        { trailer is optional, empty line indicates end }
        if lLineEnd = FBufferPos then
          FChunkState := csFinished
        else
          ParseParameterLine(lLineEnd);
      end;
    end;
    FBufferPos := lNextLine;
  until false;
end;

function TLHTTPSocket.ParseRequest: boolean;
var
  pNextLine, pLineEnd: pchar;
begin
  if FRequestHeaderDone then exit(not FRequestInputDone);
  repeat
    pLineEnd := StrScan(FBufferPos, #10);
    if pLineEnd = nil then
    begin
      if (FRequestBuffer <> nil) or (FRequestPos <> nil) then
        PackRequestBuffer
      else if CalcAvailableBufferSpace = 0 then
        WriteError(hsRequestTooLong);
      exit(true);
    end;
  
    pNextLine := pLineEnd+1;
    if (pLineEnd > FBufferPos) and ((pLineEnd-1)^ = #13) then
      dec(pLineEnd);
    pLineEnd^ := #0;
    ParseLine(pLineEnd);
    FBufferPos := pNextLine;
    if FRequestHeaderDone then
      exit(not FRequestInputDone);
  until false;
end;

procedure TLHTTPSocket.ParseParameterLine(pLineEnd: pchar);
var
  lPos: pchar;
  I: TLHTTPParameter;
  lLen: integer;
begin
  lPos := StrScan(FBufferPos, ' ');
  if (lPos = nil) or (lPos = FBufferPos) or ((lPos-1)^ <> ':') then
  begin
    WriteError(hsBadRequest);
    exit;
  end;

  { null-terminate at colon }
  (lPos-1)^ := #0;
  StrUpper(FBufferPos);
  lLen := lPos-FBufferPos-1;
  for I := Low(TLHTTPParameter) to High(TLHTTPParameter) do
    if (Length(HTTPParameterStrings[I]) = lLen)
    and CompareMem(FBufferPos, PChar(HTTPParameterStrings[I]), lLen) then
    begin
      repeat
        inc(lPos);
      until lPos^ <> ' ';
      FParameters[I] := lPos;
      break;
    end;
end;

procedure TLHTTPSocket.ParseLine(pLineEnd: pchar);
begin
  if FBufferPos[0] = #0 then
  begin
    FRequestHeaderDone := true;
    ProcessHeaders;
  end else
    ParseParameterLine(pLineEnd);
end;
        
function TLHTTPSocket.ParseBuffer: boolean;
var
  lParseFunc: TParseBufferMethod;
begin
  repeat
    lParseFunc := FParseBuffer;
    Result := FParseBuffer();
    if not Result and not FRequestInputDone then
    begin
      FRequestInputDone := true;
      if FCurrentInput <> nil then
        FCurrentInput.DoneInput;
    end;
    { if parse func changed mid-run, then we should continue calling the new 
      one: header + data }
  until (lParseFunc = FParseBuffer) or not Result;
end;

function TLHTTPSocket.ProcessEncoding: boolean;
var
  lCode: integer;
  lParam: pchar;
begin
  Result := true;
  lParam := FParameters[hpContentLength];
  if lParam <> nil then
  begin
    FParseBuffer := @ParseEntityPlain;
    Val(lParam, FInputRemaining, lCode);
    if lCode <> 0 then
      WriteError(hsBadRequest);
    exit;
  end;

  lParam := FParameters[hpTransferEncoding];
  if lParam <> nil then
  begin
    if StrIComp(lParam, 'chunked') = 0 then
    begin
      FParseBuffer := @ParseEntityChunked;
      FChunkState := csInitial;
    end else
      Result := false;
    exit;
  end;

  { only if keep-alive, then user must specify either of above headers to 
    indicate next header's start }
  lParam := FParameters[hpConnection];
  FRequestInputDone := (lParam <> nil) and (StrIComp(lParam, 'keep-alive') = 0);
  if not FRequestInputDone then
  begin
    FParseBuffer := @ParseEntityPlain;
    FInputRemaining := high(FInputRemaining);
  end;
end;

function TLHTTPSocket.SetupEncoding(AOutputItem: TBufferOutput; AHeaderOut: PHeaderOutInfo): boolean;
begin
  if AHeaderOut^.ContentLength = 0 then
  begin
    if AHeaderOut^.Version >= 11 then
    begin
      { we can use chunked encoding }
      AHeaderOut^.TransferEncoding := teChunked;
      AOutputItem.SelectChunked;
    end else begin
      { we need to buffer the response to find its length }
      AHeaderOut^.TransferEncoding := teIdentity;
      AOutputItem.SelectBuffered;
      { need to accumulate data before starting header output }
      AddToOutput(AOutputItem);
      exit(false);
    end;
  end else begin
    AHeaderOut^.TransferEncoding := teIdentity;
    AOutputItem.SelectPlain;
  end;
  Result := true;
end;

procedure TLHTTPSocket.WriteBlock;
begin
  while true do
  begin
    if FCurrentOutput = nil then
    begin
      if not FOutputDone or (not FRequestInputDone and FKeepAlive) then
        break;

      if not FKeepAlive then
      begin
        Disconnect;
        exit;
      end;

      { next request }
      FRequestInputDone := false;
      FRequestHeaderDone := false;
      FOutputDone := false;
      FRequestPos := FBufferPos;
      FlushRequest;
      { rewind buffer pointers if at end of buffer anyway }
      if FBufferPos = FBufferEnd then
        PackRequestBuffer;

      if ParseBuffer and IgnoreRead then 
      begin
        { end of input buffer reached, try reading more }
        HandleReceive;
      end;

      if FCurrentOutput = nil then 
        break;
    end;

    { if we cannot send, then the send buffer is full }
    if (FConnectionStatus <> scConnected) or not (ssCanSend in FSocketState) then
      break;

    case FCurrentOutput.WriteBlock of
      wsDone:
      begin
        if FCurrentOutput = FLastOutput then
          FLastOutput := nil;
        { some output items may trigger this parse/write loop }
        DelayFree(FCurrentOutput);
        FCurrentOutput := FCurrentOutput.FNext;
      end;
      wsWaitingData:
      begin
        { wait for more data from external source }
        break;
      end;
    end;
    { nothing left to write, request was busy and now completed }
    if FCurrentOutput = nil then
    begin
      LogMessage;
      FOutputDone := true;
    end;
  end;
end;

{ TLHTTPServerSocket }

constructor TLHTTPServerSocket.Create;
begin
  inherited;

  FLogMessage := InitStringBuffer(256);
  FHeaderOut.ExtraHeaders := InitStringBuffer(256);
  ResetDefaults;
end;

destructor TLHTTPServerSocket.Destroy;
begin
  FreeMem(FLogMessage.Memory);
  FreeMem(FHeaderOut.ExtraHeaders.Memory);
  inherited;
end;

procedure TLHTTPServerSocket.AddContentLength(ALength: integer);
begin
  Inc(FHeaderOut.ContentLength, ALength);
end;

procedure TLHTTPServerSocket.DoneBuffer(AOutput: TBufferOutput);
begin
  if FCurrentOutput <> AOutput then
  begin
    RemoveOutput(AOutput);
    AOutput.FNext := FCurrentOutput;
    FCurrentOutput := AOutput;
  end;
  WriteHeaders(AOutput, nil);
end;

procedure TLHTTPServerSocket.LogAccess(const AMessage: string);
begin
  TLHTTPConnection(FCreator).LogAccess(AMessage);
end;

procedure TLHTTPServerSocket.LogMessage;
begin
  { log a message about this request, 
    '<StatusCode> <Length> "<Referer>" "<User-Agent>"' }
  AppendString(FLogMessage, IntToStr(HTTPStatusCodes[FResponseInfo.Status]));
  AppendChar(FLogMessage, ' ');
  AppendString(FLogMessage, IntToStr(FHeaderOut.ContentLength));
  AppendString(FLogMessage, ' "');
  AppendString(FLogMessage, FParameters[hpReferer]);
  AppendString(FLogMessage, '" "');
  AppendString(FLogMessage, FParameters[hpUserAgent]);
  AppendChar(FLogMessage, '"');
  AppendChar(FLogMessage, #0);
  LogAccess(FLogMessage.Memory);
end;

procedure TLHTTPServerSocket.ResetDefaults;
begin
  inherited;
  FRequestInfo.RequestType := hmUnknown;
  FSetupEncodingState := seNone;
  with FResponseInfo do
  begin
    Status := hsOK;
    ContentType := 'application/octet-stream';
    ContentCharset := '';
    LastModified := 0.0;
  end;
end;

procedure TLHTTPServerSocket.FlushRequest;
  { reset structure to zero, not called from constructor }
begin
  with FRequestInfo do
  begin
    { request }
    Argument := nil;
    QueryParams := nil;
    Version := 0;
  end;
  with FHeaderOut do
  begin
    ContentLength := 0;
    TransferEncoding := teIdentity;
    ExtraHeaders.Pos := ExtraHeaders.Memory;
    Version := 0;
  end;
  inherited;
end;
  
procedure TLHTTPServerSocket.RelocateVariables;
begin
  RelocateVariable(FRequestInfo.Method);
  RelocateVariable(FRequestInfo.Argument);
  RelocateVariable(FRequestInfo.QueryParams);
  RelocateVariable(FRequestInfo.VersionStr);
  inherited;
end;

procedure TLHTTPServerSocket.ParseLine(pLineEnd: pchar);
begin
  if FRequestInfo.RequestType = hmUnknown then
  begin
    ParseRequestLine(pLineEnd);
    exit;
  end;

  inherited;
end;

procedure TLHTTPServerSocket.ParseRequestLine(pLineEnd: pchar);
var
  lPos: pchar;
  I: TLHTTPMethod;
  NowLocal: TDateTime;
begin
  { make a timestamp for this request }
  NowLocal := Now;
  FRequestInfo.DateTime := LocalTimeToGMT(NowLocal);
  { begin log message }
  FLogMessage.Pos := FLogMessage.Memory;
  AppendString(FLogMessage, PeerAddress);
  AppendString(FLogMessage, ' - [');
  AppendString(FLogMessage, FormatDateTime('dd/mmm/yyyy:hh:nn:ss', NowLocal));
  AppendString(FLogMessage, TLHTTPServer(FCreator).FLogMessageTZString);
  AppendString(FLogMessage, FBufferPos, pLineEnd-FBufferPos);
  AppendString(FLogMessage, '" ');

  { decode version }
  lPos := pLineEnd;
  repeat
    if lPos^ = ' ' then break;
    dec(lPos);
    if lPos < FBufferPos then
    begin
      WriteError(hsBadRequest);
      exit;
    end;
  until false;

  lPos^ := #0;
  inc(lPos);
  { lPos = version string }
  if not HTTPVersionCheck(lPos, pLineEnd, FRequestInfo.Version) then
  begin
    WriteError(hsBadRequest);
    exit;
  end;
  FRequestInfo.VersionStr := lPos;
  FHeaderOut.Version := FRequestInfo.Version;
  
  { trim spaces at end of URI }
  dec(lPos);
  repeat
    if lPos = FBufferPos then break;
    dec(lPos);
    if lPos^ <> ' ' then break;
    lPos^ := #0;
  until false;

  { decode method }
  FRequestInfo.Method := FBufferPos;
  lPos := StrScan(FBufferPos, ' ');
  if lPos = nil then
  begin
    WriteError(hsBadRequest);
    exit;
  end;

  lPos^ := #0;
  for I := Low(TLHTTPMethod) to High(TLHTTPMethod) do
  begin
    if (I = hmUnknown) or (((lPos-FBufferPos) = Length(HTTPMethodStrings[I]))
      and CompareMem(FBufferPos, PChar(HTTPMethodStrings[I]), lPos-FBufferPos)) then
    begin
      repeat
        inc(lPos);
      until lPos^ <> ' ';
      FRequestInfo.Argument := lPos;
      FRequestInfo.RequestType := I;
      break;
    end;
  end;

  if ((pLineEnd-FRequestInfo.Argument) > 7) and (StrIComp(FRequestInfo.Argument, 'http://') = 0) then
  begin
    { absolute URI }
    lPos := FRequestInfo.Argument+7;
    while (lPos^ = '/') do 
      Inc(lPos);
    FParameters[hpHost] := lPos;
    lPos := StrScan(lPos, '/');
    FRequestInfo.Argument := lPos;
  end;
  { FRequestInfo.Argument now points to an "abs_path" }
  if FRequestInfo.Argument[0] <> '/' then
  begin
    WriteError(hsBadRequest);
    exit;
  end;
  repeat
    Inc(FRequestInfo.Argument);
  until FRequestInfo.Argument[0] <> '/';
end;

procedure TLHTTPServerSocket.ProcessHeaders;
  { process request }
var
  lPos, lConnParam: pchar;
begin
  { do HTTP/1.1 Host-field present check }
  if (FRequestInfo.Version > 10) and (FParameters[hpHost] = nil) then
  begin
    WriteError(hsBadRequest);
    exit;
  end;
      
  lPos := StrScan(FRequestInfo.Argument, '?');
  if lPos <> nil then
  begin
    lPos^ := #0;
    FRequestInfo.QueryParams := lPos+1;
  end;

  FKeepAlive := FRequestInfo.Version > 10;
  lConnParam := FParameters[hpConnection];
  if lConnParam <> nil then
  begin
    if StrIComp(lConnParam, 'keep-alive') = 0 then
      FKeepAlive := true
    else
    if StrIComp(lConnParam, 'close') = 0 then
      FKeepAlive := false;
  end;
  
  HTTPDecode(FRequestInfo.Argument);
  if not CheckPermission(FRequestInfo.Argument) then
  begin
    WriteError(hsForbidden);
  end else begin
    if not ProcessEncoding then
    begin
      WriteError(hsNotImplemented);
      exit;
    end;
      
    FCurrentInput := HandleURI;
    { if we have a valid outputitem, wait until it is ready 
      to produce its response }
    if FCurrentInput = nil then
    begin
      if FResponseInfo.Status = hsOK then
        WriteError(hsNotFound)
      else
        WriteError(FResponseInfo.Status);
    end else if FRequestInputDone then
      FCurrentInput.DoneInput;
  end;
end;

function TLHTTPServerSocket.PrepareResponse(AOutputItem: TOutputItem; ACustomErrorMessage: boolean): boolean;
var
  lDateTime: TDateTime;
begin
  { check modification date }
  if FResponseInfo.Status < hsBadRequest then
  begin
    if (FParameters[hpIfModifiedSince] <> nil) 
      and (FResponseInfo.LastModified <> 0.0) then
    begin
      if TryHTTPDateStrToDateTime(FParameters[hpIfModifiedSince], lDateTime) then
      begin
        if lDateTime > FRequestInfo.DateTime then
          FResponseInfo.Status := hsBadRequest
        else
        if FResponseInfo.LastModified <= lDateTime then
          FResponseInfo.Status := hsNotModified;
      end;
    end else
    if (FParameters[hpIfUnmodifiedSince] <> nil) then
    begin
      if TryHTTPDateStrToDateTime(FParameters[hpIfUnmodifiedSince], lDateTime) then
      begin
        if (FResponseInfo.LastModified = 0.0) 
          or (lDateTime < FResponseInfo.LastModified) then
          FResponseInfo.Status := hsPreconditionFailed;
      end;
    end;
  end;

  if (FResponseInfo.Status < hsOK) or (FResponseInfo.Status in [hsNoContent, hsNotModified]) then
  begin
    { RFC says we MUST not include a response for these statuses }
    ACustomErrorMessage := false;
    FHeaderOut.ContentLength := 0;
  end;
  
  Result := (FResponseInfo.Status = hsOK) or ACustomErrorMessage;
  if not Result then
  begin
    WriteError(FResponseInfo.Status);
    DelayFree(AOutputItem);
  end;
end;

procedure TLHTTPServerSocket.StartMemoryResponse(AOutputItem: TMemoryOutput; ACustomErrorMessage: boolean = false);
begin
  if PrepareResponse(AOutputItem, ACustomErrorMessage) then
  begin
    if FRequestInfo.RequestType <> hmHead then
      FHeaderOut.ContentLength := AOutputItem.FBufferSize
    else
      FHeaderOut.ContentLength := 0;
    WriteHeaders(nil, AOutputItem);
  end;
end;

function TLHTTPServerSocket.SetupEncoding(AOutputItem: TBufferOutput): boolean;
const
  SetupEncodingToState: array[boolean] of TSetupEncodingState = (seWaitHeaders, seStartHeaders);
begin
  if FSetupEncodingState > seNone then
    exit(FSetupEncodingState = seStartHeaders);
  Result := inherited SetupEncoding(AOutputItem, @FHeaderOut);
  FSetupEncodingState := SetupEncodingToState[Result];
end;

procedure TLHTTPServerSocket.StartResponse(AOutputItem: TBufferOutput; ACustomErrorMessage: boolean = false);
begin
  if PrepareResponse(AOutputItem, ACustomErrorMessage) then
    if (FRequestInfo.RequestType = hmHead) or SetupEncoding(AOutputItem) then
      WriteHeaders(nil, AOutputItem);
end;

function TLHTTPServerSocket.HandleURI: TOutputItem; {inline;} {<--- triggers IE}
begin
  Result := TLHTTPServer(FCreator).HandleURI(Self);
end;

procedure TLHTTPServerSocket.WriteError(AStatus: TLHTTPStatus);
var
  lMessage: string;
  lMsgOutput: TMemoryOutput;
begin
  if AStatus in HTTPDisconnectStatuses then
    FKeepAlive := false;
  lMessage := HTTPDescriptions[AStatus];
  FRequestHeaderDone := true;
  FResponseInfo.Status := AStatus;
  FHeaderOut.ContentLength := Length(lMessage);
  FHeaderOut.TransferEncoding := teIdentity;
  if Length(lMessage) > 0 then
  begin
    FResponseInfo.ContentType := 'text/html';
    lMsgOutput := TMemoryOutput.Create(Self, PChar(lMessage), 0, Length(lMessage), false)
  end else begin
    FResponseInfo.ContentType := '';
    lMsgOutput := nil;
  end;
  WriteHeaders(nil, lMsgOutput);
end;

procedure TLHTTPServerSocket.WriteHeaders(AHeaderResponse, ADataResponse: TOutputItem);
var
  lTemp: string[23];
  lMessage: TStringBuffer;
  tempStr: string;
begin
  lMessage := InitStringBuffer(504);
  
  AppendString(lMessage, 'HTTP/1.1 ');
  Str(HTTPStatusCodes[FResponseInfo.Status], lTemp);
  AppendString(lMessage, lTemp);
  AppendChar(lMessage, ' ');
  AppendString(lMessage, HTTPTexts[FResponseInfo.Status]);
  AppendString(lMessage, #13#10+'Date: ');
  AppendString(lMessage, FormatDateTime(HTTPDateFormat, FRequestInfo.DateTime));
  AppendString(lMessage, ' GMT');
  tempStr := TLHTTPServer(FCreator).ServerSoftware;
  if Length(tempStr) > 0 then
  begin
    AppendString(lMessage, #13#10+'Server: ');
    AppendString(lMessage, tempStr);
  end;
  if Length(FResponseInfo.ContentType) > 0 then
  begin
    AppendString(lMessage, #13#10+'Content-Type: ');
    AppendString(lMessage, FResponseInfo.ContentType);
    if Length(FResponseInfo.ContentCharset) > 0 then
    begin
      AppendString(lMessage, '; charset=');
      AppendString(lMessage, FResponseInfo.ContentCharset);
    end;
  end;
  if FHeaderOut.TransferEncoding = teIdentity then
  begin
    AppendString(lMessage, #13#10+'Content-Length: ');
    Str(FHeaderOut.ContentLength, lTemp);
    AppendString(lMessage, lTemp);
  end else begin
    { only other possibility: teChunked }
    AppendString(lMessage, #13#10+'Transfer-Encoding: chunked');
  end;
  if FResponseInfo.LastModified <> 0.0 then
  begin
    AppendString(lMessage, #13#10+'Last-Modified: ');
    AppendString(lMessage, FormatDateTime(HTTPDateFormat, FResponseInfo.LastModified));
    AppendString(lMessage, ' GMT');
  end;
  AppendString(lMessage, #13#10+'Connection: ');
  if FKeepAlive then
    AppendString(lMessage, 'keep-alive')
  else
    AppendString(lMessage, 'close');
  AppendString(lMessage, #13#10);
  with FHeaderOut.ExtraHeaders do
    AppendString(lMessage, Memory, Pos-Memory);
  AppendString(lMessage, #13#10);
  if AHeaderResponse <> nil then
  begin
    AHeaderResponse.FBuffer := lMessage.Memory;
    AHeaderResponse.FBufferSize := lMessage.Pos-lMessage.Memory;
  end else
    AddToOutput(TMemoryOutput.Create(Self, lMessage.Memory, 0,
      lMessage.Pos-lMessage.Memory, true));

  if ADataResponse <> nil then
  begin
    if FRequestInfo.RequestType = hmHead then
      DelayFree(ADataResponse)
    else
      AddToOutput(ADataResponse);
  end;
end;

{ TLHTTPConnection }

destructor TLHTTPConnection.Destroy;
begin
  inherited;
end;

procedure TLHTTPConnection.LogAccess(const AMessage: string);
begin
end;

procedure TLHTTPConnection.ReceiveEvent(aSocket: TLHandle);
begin
  TLHTTPSocket(aSocket).HandleReceive;
  TLHTTPSocket(aSocket).FreeDelayFreeItems;
end;

procedure TLHTTPConnection.CanSendEvent(aSocket: TLHandle);
begin
  TLHTTPSocket(aSocket).WriteBlock;
  TLHTTPSocket(aSocket).FreeDelayFreeItems;
end;

{ TLHTTPServer }

constructor TLHTTPServer.Create(AOwner: TComponent);
var
  TZSign: char;
  TZSecsAbs: integer;
begin
  inherited Create(AOwner);

  FPort := 80; // default port
  SocketClass := TLHTTPServerSocket;
  if TZSeconds >= 0 then
    TZSign := '+'
  else
    TZSign := '-';
  TZSecsAbs := Abs(TZSeconds);
  FLogMessageTZString := Format(' %s%.2d%.2d] "', 
    [TZSign, TZSecsAbs div 3600, (TZSecsAbs div 60) mod 60]);
end;

function TLHTTPServer.HandleURI(ASocket: TLHTTPServerSocket): TOutputItem;
var
  lHandler: TURIHandler;
begin
  Result := nil;
  lHandler := FHandlerList;
  while lHandler <> nil do
  begin
    if ASocket.FRequestInfo.RequestType in lHandler.Methods then
    begin
      Result := lHandler.HandleURI(ASocket);
      if ASocket.FResponseInfo.Status <> hsOK then break;
      if Result <> nil then break;
    end;
    lHandler := lHandler.FNext;
  end;
end;

procedure TLHTTPServer.LogAccess(const AMessage: string);
begin
  if Assigned(FOnAccess) then
    FOnAccess(AMessage);
end;

procedure TLHTTPServer.RegisterHandler(AHandler: TURIHandler);
begin
  if AHandler = nil then exit;
  AHandler.FNext := FHandlerList;
  FHandlerList := AHandler;
  if Eventer <> nil then
    AHandler.RegisterWithEventer(Eventer);
end;

procedure TLHTTPServer.RegisterWithEventer;
var
  lHandler: TURIHandler;
begin
  inherited;
  lHandler := FHandlerList;
  while lHandler <> nil do
  begin
    lHandler.RegisterWithEventer(Eventer);
    lHandler := lHandler.FNext;
  end;
end;

{ TClientInput }

type
  TClientOutput = class(TOutputItem)
  protected
    FPersistent: boolean;
    
    procedure DoneInput; override;
  public
    constructor Create(ASocket: TLHTTPClientSocket);
    destructor Destroy; override;
    procedure FreeInstance; override;

    function  HandleInput(ABuffer: pchar; ASize: integer): integer; override;
    function  WriteBlock: TWriteBlockStatus; override;
  end;

constructor TClientOutput.Create(ASocket: TLHTTPClientSocket);
begin
  inherited Create(ASocket);
  FPersistent := true;
end;

destructor TClientOutput.Destroy;
begin
  if FPersistent then exit; 
  inherited;
end;

procedure TClientOutput.FreeInstance;
begin
  if FPersistent then exit; 
  inherited;
end;

procedure TClientOutput.DoneInput;
begin
  TLHTTPClient(TLHTTPClientSocket(FSocket).FCreator).
    DoDoneInput(TLHTTPClientSocket(FSocket));
end;

function  TClientOutput.HandleInput(ABuffer: pchar; ASize: integer): integer;
begin
  Result := TLHTTPClient(TLHTTPClientSocket(FSocket).FCreator).
    DoHandleInput(TLHTTPClientSocket(FSocket), ABuffer, ASize);
end;

function  TClientOutput.WriteBlock: TWriteBlockStatus;
begin
  Result := TLHTTPClient(TLHTTPClientSocket(FSocket).FCreator).
    DoWriteBlock(TLHTTPClientSocket(FSocket));
end;

{ TLHTTPClientSocket }

constructor TLHTTPClientSocket.Create;
begin
  inherited Create;

  FCurrentInput := TClientOutput.Create(Self);
  ResetDefaults;
end;

destructor TLHTTPClientSocket.Destroy;
begin
  if Assigned(FCurrentInput) then begin
    TClientOutput(FCurrentInput).FPersistent := false;
    FreeAndNil(FCurrentInput);
  end;
  inherited;
end;

procedure TLHTTPClientSocket.AddContentLength(ALength: integer);
begin
  Inc(TLHTTPClient(FCreator).FHeaderOut.ContentLength, ALength);
end;

procedure TLHTTPClientSocket.Cancel(AError: TLHTTPClientError);
begin
  FError := AError;
  Disconnect;
end;

function TLHTTPClientSocket.GetResponseReason: string;
begin
  Result := FResponse^.Reason;
end;

function TLHTTPClientSocket.GetResponseStatus: TLHTTPStatus;
begin
  Result := FResponse^.Status;
end;

procedure TLHTTPClientSocket.SendRequest;
var
  lMessage: TStringBuffer;
  lTemp: string[23];
  hasRangeStart, hasRangeEnd: boolean;
begin
  lMessage := InitStringBuffer(504);

  AppendString(lMessage, HTTPMethodStrings[FRequest^.Method]);
  AppendChar(lMessage, ' ');
  AppendString(lMessage, FRequest^.URI);
  AppendChar(lMessage, ' ');
  AppendString(lMessage, 'HTTP/1.1'+#13#10+'Host: ');
  AppendString(lMessage, TLHTTPClient(FCreator).Host);
  if TLHTTPClient(FCreator).Port <> 80 then
  begin
    AppendChar(lMessage, ':');
    Str(TLHTTPClient(FCreator).Port, lTemp);
    AppendString(lMessage, lTemp);
  end;
  AppendString(lMessage, #13#10);
  if FHeaderOut^.ContentLength > 0 then
  begin
    AppendString(lMessage, 'Content-Length: ');
    Str(FHeaderOut^.ContentLength, lTemp);
    AppendString(lMessage, lTemp+#13#10);
  end;
  hasRangeStart := TLHTTPClient(FCreator).RangeStart <> high(qword);
  hasRangeEnd := TLHTTPClient(FCreator).RangeEnd <> high(qword);
  if hasRangeStart or hasRangeEnd then
  begin
    AppendString(lMessage, 'Range: bytes=');
    if hasRangeStart then
    begin
      Str(TLHTTPClient(FCreator).RangeStart, lTemp);
      AppendString(lMessage, lTemp);
    end;
    AppendChar(lMessage, '-');
    if hasRangeEnd then
    begin
      Str(TLHTTPClient(FCreator).RangeEnd, lTemp);
      AppendString(lMessage, lTemp);
    end;
    AppendString(lMessage, #13#10);
  end;
  with FHeaderOut^.ExtraHeaders do
    AppendString(lMessage, Memory, Pos-Memory);
  AppendString(lMessage, #13#10);
  AddToOutput(TMemoryOutput.Create(Self, lMessage.Memory, 0,
    lMessage.Pos-lMessage.Memory, true));
  AddToOutput(FCurrentInput);
  
  WriteBlock;
end;

procedure TLHTTPClientSocket.ParseLine(pLineEnd: pchar);
begin
  if FError <> ceNone then
    exit;

  if FResponse^.Status = hsUnknown then
  begin
    ParseStatusLine(pLineEnd);
    exit;
  end;

  inherited;
end;

procedure TLHTTPClientSocket.ParseStatusLine(pLineEnd: pchar);
var
  lPos: pchar;
begin
  lPos := FBufferPos;
  repeat
    if lPos >= pLineEnd then
    begin
      Cancel(ceMalformedStatusLine);
      exit;
    end;
    if lPos^ = ' ' then
      break;
    Inc(lPos);
  until false;
  if not HTTPVersionCheck(FBufferPos, lPos, FResponse^.Version) then
  begin
    Cancel(ceMalformedStatusLine);
    exit;
  end;

  if (FResponse^.Version > 11) then
  begin
    Cancel(ceVersionNotSupported);
    exit;
  end;

  { status code }
  Inc(lPos);
  if (lPos+3 >= pLineEnd) or (lPos[3] <> ' ') then
  begin
    Cancel(ceMalformedStatusLine);
    exit;
  end;
  FResponse^.Status := CodeToHTTPStatus((ord(lPos[0])-ord('0'))*100
    + (ord(lPos[1])-ord('0'))*10 + (ord(lPos[2])-ord('0')));
  if FResponse^.Status = hsUnknown then
  begin
    Cancel(ceMalformedStatusLine);
    exit;
  end;

  Inc(lPos, 4);
  if lPos < pLineEnd then
    FResponse^.Reason := lPos;
end;

procedure TLHTTPClientSocket.ProcessHeaders;
begin
  if not ProcessEncoding then
    Cancel(ceUnsupportedEncoding);

  TLHTTPClient(FCreator).DoProcessHeaders(Self);
end;

procedure TLHTTPClientSocket.ResetDefaults;
begin
  inherited;

  FError := ceNone;
end;

{ TLHTTPClient }

constructor TLHTTPClient.Create(AOwner: TComponent);
begin
  inherited;

  FPort := 80;
  SocketClass := TLHTTPClientSocket;
  FRequest.Method := hmGet;
  FHeaderOut.ExtraHeaders := InitStringBuffer(256);
  ResetRange;
end;

destructor TLHTTPClient.Destroy;
begin
  FreeMem(FHeaderOut.ExtraHeaders.Memory);
  inherited;
end;

procedure TLHTTPClient.AddExtraHeader(const AHeader: string);
begin
  AppendString(FHeaderOut.ExtraHeaders, AHeader);
  AppendString(FHeaderOut.ExtraHeaders, #13#10);
end;

procedure TLHTTPClient.AddCookie(const AName, AValue: string; const APath: string = '';
  const ADomain: string = ''; const AVersion: string = '0');
var
  lHeader: string;
begin
  lHeader := 'Cookie: $Version='+AVersion+'; '+AName+'='+EscapeCookie(AValue);
  if Length(APath) > 0 then
    lHeader := lHeader+';$Path='+APath;
  if Length(ADomain) > 0 then
    lHeader := lHeader+';$Domain='+ADomain;
  AddExtraHeader(lHeader);
end;

procedure TLHTTPClient.ConnectEvent(aSocket: TLHandle);
begin
  inherited;
  InternalSendRequest;
end;

procedure TLHTTPClient.DoDoneInput(ASocket: TLHTTPClientSocket);
begin
  Dec(FPendingResponses);
  if FPendingResponses = 0 then
    FState := hcsIdle
  else
    FState := hcsWaiting;
  if Assigned(FOnDoneInput) then
    FOnDoneInput(ASocket);
end;

function  TLHTTPClient.DoHandleInput(ASocket: TLHTTPClientSocket; ABuffer: pchar; ASize: integer): integer;
begin
  FState := hcsReceiving;
  if Assigned(FOnInput) then
    Result := FOnInput(ASocket, ABuffer, ASize)
  else
    Result := ASize;
end;

procedure TLHTTPClient.DoProcessHeaders(ASocket: TLHTTPClientSocket);
begin
  if Assigned(FOnProcessHeaders) then
    FOnProcessHeaders(ASocket);
end;

function  TLHTTPClient.DoWriteBlock(ASocket: TLHTTPClientSocket): TWriteBlockStatus;
begin
  Result := wsDone;
  if not FOutputEof then
    if Assigned(FOnCanWrite) then
      FOnCanWrite(ASocket, Result)
end;

function  TLHTTPClient.InitSocket(aSocket: TLSocket): TLSocket;
begin
  TLHTTPClientSocket(aSocket).FHeaderOut := @FHeaderOut;
  TLHTTPClientSocket(aSocket).FRequest := @FRequest;
  TLHTTPClientSocket(aSocket).FResponse := @FResponse;
  Result := inherited;
end;

procedure TLHTTPClient.InternalSendRequest;
begin
  FOutputEof := false;
  TLHTTPClientSocket(FIterator).SendRequest;
  Inc(FPendingResponses);
  if FState = hcsIdle then
    FState := hcsWaiting;
end;

procedure TLHTTPClient.ResetRange;
begin
  FRequest.RangeStart := High(FRequest.RangeStart);
  FRequest.RangeEnd := High(FRequest.RangeEnd);
end;

procedure TLHTTPClient.SendRequest;
begin
  if not Connected then
    Connect(FHost, FPort)
  else
    InternalSendRequest;
end;

end.

