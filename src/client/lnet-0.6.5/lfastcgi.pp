{ FastCGI requester support for lNet

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

unit lfastcgi;

{$mode objfpc}{$h+}

interface

uses
  classes, sysutils, fastcgi_base, lnet, levents, lstrbuffer, ltimer;

type
  TLFastCGIClient = class;
  TLFastCGIRequest = class;
  TLFastCGIPool = class;

  TLFastCGIRequestEvent = procedure(ARequest: TLFastCGIRequest) of object;

  PLFastCGIRequest = ^TLFastCGIRequest;
  TLFastCGIRequest = class(TObject)
  protected
    FID: integer;
    FClient: TLFastCGIClient;
    FBuffer: TStringBuffer;
    FBufferSendPos: integer;
    FHeader: FCGI_Header;
    FHeaderPos: integer;
    FContentLength: integer;
    FInputBuffer: pchar;
    FInputSize: integer;
    FOutputDone: boolean;
    FStderrDone: boolean;
    FOutputPending: boolean;
    FNextFree: TLFastCGIRequest;
    FNextSend: TLFastCGIRequest;
    FOnEndRequest: TLFastCGIRequestEvent;
    FOnInput: TLFastCGIRequestEvent;
    FOnOutput: TLFastCGIRequestEvent;
    FOnStderr: TLFastCGIRequestEvent;

    procedure HandleReceive;
    procedure HandleReceiveEnd;
    function  HandleSend: boolean;
    procedure DoEndRequest;
    procedure DoOutput;
    procedure DoStderr;
    procedure EndRequest;
    procedure RewindBuffer;
    procedure SetContentLength(NewLength: integer);
    procedure SendEmptyRec(AType: integer);
    procedure SendGetValues;
    procedure SetID(const NewID: integer);
  public
    constructor Create;
    destructor Destroy; override;
    
    procedure AbortRequest;
    function  Get(ABuffer: pchar; ASize: integer): integer;
    procedure ParseClientBuffer;
    function  SendBuffer: integer;
    function  SendPrivateBuffer: boolean;
    procedure SendBeginRequest(AType: integer);
    procedure SendParam(const AName, AValue: string; AReqType: integer = FCGI_PARAMS);
    function  SendInput(const ABuffer: pchar; ASize: integer): integer;
    procedure DoneParams;
    procedure DoneInput;

    property ID: integer read FID write SetID;
    property StderrDone: boolean read FStderrDone;
    property OutputDone: boolean read FOutputDone;
    property OutputPending: boolean read FOutputPending;
    property OnEndRequest: TLFastCGIRequestEvent read FOnEndRequest write FOnEndRequest;
    property OnInput: TLFastCGIRequestEvent read FOnInput write FOnInput;
    property OnOutput: TLFastCGIRequestEvent read FOnOutput write FOnOutput;
    property OnStderr: TLFastCGIRequestEvent read FOnStderr write FOnStderr;
  end;

  TFastCGIClientState = (fsIdle, fsConnecting, fsConnectingAgain, 
    fsStartingServer, fsHeader, fsData, fsFlush);
  
  PLFastCGIClient = ^TLFastCGIClient;
  TLFastCGIClient = class(TLTcp)
  protected
    FRequests: PLFastCGIRequest;
    FRequestsCount: integer;
    FNextRequestID: integer;
    FRequestsSent: integer;
    FFreeRequest: TLFastCGIRequest;
    FSendRequest: TLFastCGIRequest;
    FRequest: TLFastCGIRequest;
    FState: TFastCGIClientState;
    FNextFree: TLFastCGIClient;
    FPool: TLFastCGIPool;
    FBuffer: pchar;
    FBufferEnd: pchar;
    FBufferPos: pchar;
    FBufferSize: dword;
    FReqType: byte;
    FContentLength: integer;
    FPaddingLength: integer;

    function Connect: Boolean; override;
    procedure ConnectEvent(ASocket: TLHandle); override;
    procedure DisconnectEvent(ASocket: TLHandle); override;
    procedure ErrorEvent(ASocket: TLHandle; const msg: string); override;
    function  CreateRequester: TLFastCGIRequest;
    procedure HandleGetValuesResult;
    procedure HandleReceive(ASocket: TLSocket);
    procedure HandleSend(ASocket: TLSocket);
    procedure ParseBuffer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AddToSendQueue(ARequest: TLFastCGIRequest);
    function  BeginRequest(AType: integer): TLFastCGIRequest;
    procedure EndRequest(ARequest: TLFastCGIRequest);
    procedure Flush;
    function  GetBuffer(ABuffer: pchar; ASize: integer): integer;

    property ReqType: byte read FReqType;
    property RequestsSent: integer read FRequestsSent;
  end;

  TSpawnState = (ssNone, ssSpawning, ssSpawned);

  TLFastCGIPool = class(TObject)
  protected
    FClients: PLFastCGIClient;
    FClientsCount: integer;
    FClientsAvail: integer;
    FClientsMax: integer;
    FMaxRequestsConn: integer;
    FFreeClient: TLFastCGIClient;
    FTimer: TLTimer;
    FEventer: TLEventer;
    FAppName: string;
    FAppEnv: string;
    FHost: string;
    FPort: integer;
    FSpawnState: TSpawnState;
    
    procedure AddToFreeClients(AClient: TLFastCGIClient);
    function  CreateClient: TLFastCGIClient;
    procedure ConnectClients(Sender: TObject);
    procedure StartServer;
  public
    constructor Create;
    destructor Destroy; override;

    function  BeginRequest(AType: integer): TLFastCGIRequest;
    procedure EndRequest(AClient: TLFastCGIClient);

    property AppEnv: string read FAppEnv write FAppEnv;
    property AppName: string read FAppName write FAppName;
    property ClientsMax: integer read FClientsMax write FClientsMax;
    property Eventer: TLEventer read FEventer write FEventer;
    property MaxRequestsConn: integer read FMaxRequestsConn write FMaxRequestsConn;
    property Host: string read FHost write FHost;
    property Port: integer read FPort write FPort;
    property Timer: TLTimer read FTimer;
  end;

implementation

uses
  lSpawnFCGI;

{ TLFastCGIRequest }

constructor TLFastCGIRequest.Create;
begin
  inherited;

  FBuffer := InitStringBuffer(504);
  FHeader.Version := FCGI_VERSION_1;
  FHeaderPos := -1;
end;

destructor TLFastCGIRequest.Destroy;
begin
  inherited;
  FreeMem(FBuffer.Memory);
end;

procedure TLFastCGIRequest.HandleReceive;
begin
  case FClient.ReqType of
    FCGI_STDOUT: DoOutput;
    FCGI_STDERR: DoStderr;
    FCGI_END_REQUEST: EndRequest;
    FCGI_GET_VALUES_RESULT: FClient.HandleGetValuesResult;
  else
    FClient.Flush;
  end;
end;

procedure TLFastCGIRequest.HandleReceiveEnd;
begin
  case FClient.ReqType of
    FCGI_STDOUT: FOutputDone := true;
    FCGI_STDERR: FStderrDone := true;
  end;
end;

function TLFastCGIRequest.HandleSend: boolean;
begin
  if FOnInput <> nil then
    FOnInput(Self);
  Result := FInputBuffer = nil;
end;

procedure TLFastCGIRequest.DoOutput;
begin
  if FOnOutput <> nil then
    FOnOutput(Self);
end;

procedure TLFastCGIRequest.DoStderr;
begin
  if FOnStderr <> nil then
    FOnStderr(Self);
end;

procedure TLFastCGIRequest.DoEndRequest;
begin
  if FOnEndRequest <> nil then
    FOnEndRequest(Self);
end;

procedure TLFastCGIRequest.EndRequest;
begin
  FOutputDone := false;
  FStderrDone := false;
  FClient.EndRequest(Self);
  FClient.Flush;
  RewindBuffer;
  DoEndRequest;
end;

function TLFastCGIRequest.Get(ABuffer: pchar; ASize: integer): integer;
begin
  Result := FClient.GetBuffer(ABuffer, ASize);
end;

procedure TLFastCGIRequest.ParseClientBuffer;
begin
  FOutputPending := false;
  if (FClient.Iterator <> nil) and FClient.Iterator.IgnoreRead then
    FClient.HandleReceive(nil)
  else
    FClient.ParseBuffer;
end;

procedure TLFastCGIRequest.SetID(const NewID: integer);
begin
  FID := NewID;
  FHeader.RequestIDB0 := byte(NewID and $FF);
  FHeader.RequestIDB1 := byte((NewID shr 8) and $FF);
end;

procedure TLFastCGIRequest.SetContentLength(NewLength: integer);
begin
  FContentLength := NewLength;
  FHeader.ContentLengthB0 := byte(NewLength and $FF);
  FHeader.ContentLengthB1 := byte((NewLength shr 8) and $FF);
  FHeader.PaddingLength := byte(7-((NewLength+7) and 7));
end;

const
  PaddingBuffer: array[0..7] of char = (#0, #0, #0, #0, #0, #0, #0, #0);
type
  TLFastCGIStringSize = record
    Size: integer;
    SizeBuf: array[0..3] of char;
  end;

function GetFastCGIStringSize(ABufferPos: pbyte; var ASize: integer): integer;
begin
  ASize := ABufferPos[0];
  if ASize >= 128 then
  begin
    ASize := ((ABufferPos[0] shl 24) and $7f) or (ABufferPos[1] shl 16)
      or (ABufferPos[2] shl 8) or ABufferPos[3];
    Result := 4;
  end else
    Result := 1;
end;
  
procedure FillFastCGIStringSize(const AStr: string; var AFastCGIStr: TLFastCGIStringSize);
var
  lLen: dword;
begin
  lLen := dword(Length(AStr));
  if lLen > 127 then
  begin
    AFastCGIStr.Size := 4;
    AFastCGIStr.SizeBuf[0] := char($80 + ((lLen shr 24) and $ff));
    AFastCGIStr.SizeBuf[1] := char((lLen shr 16) and $ff);
    AFastCGIStr.SizeBuf[2] := char((lLen shr 8) and $ff);
    AFastCGIStr.SizeBuf[3] := char(lLen and $ff);
  end else begin
    AFastCGIStr.Size := 1;
    AFastCGIStr.SizeBuf[0] := char(lLen);
  end;
end;

procedure TLFastCGIRequest.SendBeginRequest(AType: integer);
var
  lBody: FCGI_BeginRequestBody;
begin
  lBody.roleB1 := byte((AType shr 8) and $ff);
  lBody.roleB0 := byte(AType and $ff);
  lBody.flags := FCGI_KEEP_CONN;
  FHeader.ReqType := FCGI_BEGIN_REQUEST;
  SetContentLength(sizeof(lBody));
  AppendString(FBuffer, @FHeader, sizeof(FHeader));
  AppendString(FBuffer, @lBody, sizeof(lBody));
end;

procedure TLFastCGIRequest.SendParam(const AName, AValue: string; AReqType: integer = FCGI_PARAMS);
var
  lNameLen: TLFastCGIStringSize;
  lValueLen: TLFastCGIStringSize;
  lTotalLen: integer;
begin
  FillFastCGIStringSize(AName, lNameLen);
  FillFastCGIStringSize(AValue, lValueLen);
  lTotalLen := lNameLen.Size+lValueLen.Size+Length(AName)+Length(AValue);
  if (FHeader.ReqType = AReqType) and (FBufferSendPos = 0) 
    and (0 <= FHeaderPos) and (FHeaderPos < FBuffer.Pos - FBuffer.Memory) then
  begin
    { undo padding }
    Dec(FBuffer.Pos, FHeader.PaddingLength);
    SetContentLength(FContentLength+lTotalLen);
    Move(FHeader, FBuffer.Memory[FHeaderPos], sizeof(FHeader));
  end else begin
    FHeader.ReqType := AReqType;
    SetContentLength(lTotalLen);
    FHeaderPos := FBuffer.Pos - FBuffer.Memory;
    AppendString(FBuffer, @FHeader, sizeof(FHeader));
  end;
  AppendString(FBuffer, @lNameLen.SizeBuf[0], lNameLen.Size);
  AppendString(FBuffer, @lValueLen.SizeBuf[0], lValueLen.Size);
  AppendString(FBuffer, AName);
  AppendString(FBuffer, AValue);
  AppendString(FBuffer, @PaddingBuffer[0], FHeader.PaddingLength);
end;

procedure TLFastCGIRequest.SendGetValues;
var
  lRequestID: integer;
begin
  { management record type has request id 0 }
  lRequestID := ID;
  ID := 0;
  SendParam('FCGI_MAX_REQS', '', FCGI_GET_VALUES);
  { if we're the first connection, ask max. # connections }
  if FClient.FPool.FClientsAvail = 1 then
    SendParam('FCGI_MAX_CONNS', '', FCGI_GET_VALUES);
  ID := lRequestID;
end;

function  TLFastCGIRequest.SendInput(const ABuffer: pchar; ASize: integer): integer;
begin
  { first send current buffer if any }
  if FInputBuffer <> nil then
  begin
    Result := SendBuffer;
    if FInputBuffer <> nil then exit;
  end else Result := 0;
  if Result >= ASize then exit;
  if FInputBuffer = nil then
  begin
    FInputBuffer := ABuffer+Result;
    FInputSize := ASize-Result;
    FHeader.ReqType := FCGI_STDIN;
    SetContentLength(FInputSize);
    AppendString(FBuffer, @FHeader, sizeof(FHeader));
  end;
  Inc(Result, SendBuffer);
end;

procedure TLFastCGIRequest.RewindBuffer;
begin
  FBufferSendPos := 0;
  FHeaderPos := -1;
  { rewind stringbuffer }
  FBuffer.Pos := FBuffer.Memory;
end;

function TLFastCGIRequest.SendPrivateBuffer: boolean;
var
  lWritten: integer;
begin
  { nothing to send ? }
  if FBuffer.Pos-FBuffer.Memory = FBufferSendPos then
    exit(true);
  { already a queue and we are not first in line ? no use in trying to send then }
  if (FClient.FSendRequest = nil) or (FClient.FSendRequest = Self) then
  begin
    lWritten := FClient.Send(FBuffer.Memory[FBufferSendPos], 
      FBuffer.Pos-FBuffer.Memory-FBufferSendPos);
    Inc(FBufferSendPos, lWritten);
    Result := FBufferSendPos = FBuffer.Pos-FBuffer.Memory;
    { do not rewind buffer, unless remote side has had chance to disconnect }
    if Result then
      RewindBuffer;
  end else
    Result := false;
  if not Result then
    FClient.AddToSendQueue(Self);
end;

function TLFastCGIRequest.SendBuffer: integer;
var
  lWritten: integer;
begin
  { already a queue and we are not first in line ? no use in trying to send then }
  if (FClient.FSendRequest <> nil) and (FClient.FSendRequest <> Self) then 
    exit(0);

  { header to be sent? }
  if not SendPrivateBuffer then exit(0);
  { first write request header, then wait for possible disconnect }
  if FBufferSendPos > 0 then exit(0);
  if FInputBuffer = nil then exit(0);

  lWritten := FClient.Send(FInputBuffer^, FInputSize);
  Inc(FInputBuffer, lWritten);
  Dec(FInputSize, lWritten);
  if FInputSize = 0 then
  begin
    FInputBuffer := nil;
    AppendString(FBuffer, @PaddingBuffer[0], FHeader.PaddingLength);
  end else
    FClient.AddToSendQueue(Self);
  Result := lWritten;
end;

procedure TLFastCGIRequest.SendEmptyRec(AType: integer);
begin
  FHeader.ReqType := AType;
  SetContentLength(0);
  AppendString(FBuffer, @FHeader, sizeof(FHeader));
  { no padding needed for empty string }
end;

procedure TLFastCGIRequest.DoneParams;
begin
  SendEmptyRec(FCGI_PARAMS);
end;

procedure TLFastCGIRequest.DoneInput;
begin
  SendEmptyRec(FCGI_STDIN);
  SendPrivateBuffer;
end;

procedure TLFastCGIRequest.AbortRequest;
begin
  FHeader.ReqType := FCGI_ABORT_REQUEST;
  SetContentLength(0);
  AppendString(FBuffer, @FHeader, sizeof(FHeader));
  SendPrivateBuffer;
end;

{ TLFastCGIClient }

const
  DataBufferSize = 64*1024-1;

constructor TLFastCGIClient.Create(AOwner: TComponent);
begin
  inherited;

  FBuffer := GetMem(DataBufferSize+1);
  FBufferPos := FBuffer;
  FBufferEnd := FBuffer;
  FRequests := AllocMem(sizeof(TLFastCGIRequest));
  FRequestsCount := 1;
  FFreeRequest := nil;
  OnReceive := @HandleReceive;
  OnCanSend := @HandleSend;
end;

destructor TLFastCGIClient.Destroy;
var
  I: integer;
begin
  for I := 0 to FNextRequestID-1 do
    FRequests[I].Free;
  FreeMem(FRequests);
  FreeMem(FBuffer);
  inherited;
end;

function TLFastCGIClient.GetBuffer(ABuffer: pchar; ASize: integer): integer;
begin
  Result := FBufferEnd - FBufferPos;
  if Result > FContentLength then 
    Result := FContentLength;
  if Result > ASize then
    Result := ASize;
  Move(FBufferPos^, ABuffer^, Result);
  Inc(FBufferPos, Result);
  Dec(FContentLength, Result);
  { buffer empty? reset }
  if FBufferPos = FBufferEnd then
  begin
    FBufferPos := FBuffer;
    FBufferEnd := FBuffer;
  end;
end;

procedure TLFastCGIClient.ConnectEvent(ASocket: TLHandle);
begin
  if FState = fsStartingServer then
    FPool.FSpawnState := ssSpawned;
  FState := fsHeader;
  if FPool <> nil then
    FPool.AddToFreeClients(Self);

  inherited;
end;

procedure TLFastCGIClient.DisconnectEvent(ASocket: TLHandle);
var
  I: integer;
  needReconnect: boolean;
begin
  inherited;
  FRequestsSent := 0;
  needReconnect := false;
  for I := 0 to FNextRequestID-1 do
    if FRequests[I].FNextFree = nil then
    begin
      { see if buffer contains request, then assume we can resend that }
      if FRequests[I].FBufferSendPos > 0 then
      begin
        needReconnect := true;
        FRequests[I].FBufferSendPos := 0;
        FRequests[I].SendPrivateBuffer;
      end else
      if FRequests[I].FBuffer.Memory = FRequests[I].FBuffer.Pos then
        needReconnect := true
      else
        FRequests[I].EndRequest;
    end;
  if needReconnect then 
    Connect;
end;

procedure TLFastCGIClient.ErrorEvent(ASocket: TLHandle; const msg: string);
begin
  if (FState = fsConnectingAgain) 
    or ((FState = fsConnecting) and (FPool.FSpawnState = ssSpawned)) then
  begin
    FRequest.DoEndRequest;
    EndRequest(FRequest);
    FState := fsIdle;
  end else
  if FState = fsConnecting then
  begin
    FPool.StartServer;
    FState := fsStartingServer;
  end;
end;

procedure TLFastCGIClient.HandleGetValuesResult;
var
  lNameLen, lValueLen, lIntVal, lCode: integer;
  lBufferPtr: pchar;
  lPrevChar: char;

  procedure GetIntVal;
  begin
    lPrevChar := lBufferPtr[lNameLen+lValueLen];
    lBufferPtr[lNameLen+lValueLen] := #0;
    Val(lBufferPtr+lNameLen, lIntVal, lCode);
    lBufferPtr[lNameLen+lValueLen] := lPrevChar;
  end;

begin
  repeat
    lBufferPtr := FBufferPos;
    Inc(lBufferPtr, GetFastCGIStringSize(PByte(lBufferPtr), lNameLen));
    Inc(lBufferPtr, GetFastCGIStringSize(PByte(lBufferPtr), lValueLen));
    if lBufferPtr + lNameLen + lValueLen > FBufferEnd then exit;
    if StrLComp(lBufferPtr, 'FCGI_MAX_REQS', lNameLen) = 0 then
    begin
      GetIntVal;
      if (lCode = 0) and (FRequestsCount <> lIntVal) then
      begin
        FRequestsCount := lIntVal;
        ReallocMem(FRequests, sizeof(TLFastCGIRequest)*lIntVal);
      end;
    end else
    if StrLComp(lBufferPtr, 'FCGI_MAX_CONNS', lNameLen) = 0 then
    begin
      GetIntVal;
      if lCode = 0 then
        FPool.ClientsMax := lIntVal;
    end;
    Inc(lBufferPtr, lNameLen+lValueLen);
    Dec(FContentLength, lBufferPtr-FBufferPos);
    FBufferPos := lBufferPtr;
  until FContentLength = 0;
end;

procedure TLFastCGIClient.HandleReceive(ASocket: TLSocket);
var
  lRead: integer;
begin
  lRead := Get(FBufferEnd^, DataBufferSize-PtrUInt(FBufferEnd-FBuffer));
  if lRead = 0 then exit;
  { remote side has had chance to disconnect, clear buffer }
  Inc(FBufferEnd, lRead);
  ParseBuffer;
end;

procedure TLFastCGIClient.HandleSend(ASocket: TLSocket);
var
  lRequest: TLFastCGIRequest;
begin
  if FSendRequest = nil then exit;
  lRequest := FSendRequest.FNextSend;
  repeat
    if not lRequest.SendPrivateBuffer or not lRequest.HandleSend then
      exit;

    lRequest.FNextSend := nil;
    { only this one left in list ? }
    if FSendRequest = lRequest then
    begin
      FSendRequest := nil;
      exit;
    end else begin
      lRequest := lRequest.FNextSend;
      FSendRequest.FNextSend := lRequest;
    end;
  until false;
end;

procedure TLFastCGIClient.AddToSendQueue(ARequest: TLFastCGIRequest);
begin
  if ARequest.FNextSend <> nil then exit;

  if FSendRequest = nil then
    FSendRequest := ARequest
  else
    ARequest.FNextSend := FSendRequest.FNextSend;
  FSendRequest.FNextSend := ARequest;
end;

procedure TLFastCGIClient.ParseBuffer;
var
  lHeader: PFCGI_Header;
  lReqIndex: integer;
begin
  repeat
    case FState of
      fsHeader:
      begin
        if FBufferEnd-FBufferPos < sizeof(FCGI_Header) then
          exit;
        lHeader := PFCGI_Header(FBufferPos);
        FReqType := lHeader^.ReqType;
        lReqIndex := (lHeader^.RequestIDB1 shl 8) or lHeader^.RequestIDB0;
        FContentLength := (lHeader^.ContentLengthB1 shl 8) or lHeader^.ContentLengthB0;
        FPaddingLength := lHeader^.PaddingLength;
        Inc(FBufferPos, sizeof(lHeader^));
        if lReqIndex > 0 then
          Dec(lReqIndex);
        if (lReqIndex < FRequestsCount) and (FRequests[lReqIndex] <> nil) then
        begin
          FRequest := FRequests[lReqIndex];
          if FContentLength > 0 then
            FState := fsData
          else begin
            FRequest.HandleReceiveEnd;
            Flush;
          end;
        end else
          Flush;
      end;
      fsData: 
      begin
        FRequest.HandleReceive;
        if FContentLength = 0 then 
          Flush
        else begin
          FRequest.FOutputPending := true;
          exit;
        end;
      end;
      fsFlush: Flush;
    end;
  until FBufferPos = FBufferEnd;
end;

procedure TLFastCGIClient.Flush;

  function FlushSize(var ANumBytes: integer): boolean;
  var
    lFlushBytes: integer;
  begin
    lFlushBytes := ANumBytes;
    if lFlushBytes > FBufferEnd - FBufferPos then
      lFlushBytes := FBufferEnd - FBufferPos;
    Dec(ANumBytes, lFlushBytes);
    Inc(FBufferPos, lFlushBytes);
    Result := ANumBytes = 0;
  end;

begin
  FState := fsFlush;
  if FlushSize(FContentLength) and FlushSize(FPaddingLength) then
  begin
    { buffer empty? reset }
    if FBufferPos = FBufferEnd then
    begin
      FBufferPos := FBuffer;
      FBufferEnd := FBuffer;
    end;
    FState := fsHeader;
    FRequest := nil;
  end;
end;

function TLFastCGIClient.CreateRequester: TLFastCGIRequest;
begin
  if FRequests[FNextRequestID] = nil then
    FRequests[FNextRequestID] := TLFastCGIRequest.Create;
  Result := FRequests[FNextRequestID];
  Inc(FNextRequestID);
  Result.FClient := Self;
  Result.ID := FNextRequestID;  { request ids start at 1 }
end;

function TLFastCGIClient.Connect: Boolean;
begin
  Result := inherited Connect(FPool.Host, FPool.Port);
  FRequest := FRequests[0];
  if FRequest.FBuffer.Pos = FRequest.FBuffer.Memory then
    FRequest.SendGetValues;
  if FState <> fsStartingServer then
    FState := fsConnecting
  else
    FState := fsConnectingAgain;
end;

function TLFastCGIClient.BeginRequest(AType: integer): TLFastCGIRequest;
begin
  if FFreeRequest <> nil then
  begin
    Result := FFreeRequest.FNextFree;
    if FFreeRequest = FFreeRequest.FNextFree then
      FFreeRequest := nil
    else
      FFreeRequest.FNextFree := FFreeRequest.FNextFree.FNextFree;
    Result.FNextFree := nil;
  end else
  if FNextRequestID = FRequestsCount then
    exit(nil)
  else begin
    Result := CreateRequester;
  end;

  if not Connected then
    Connect;

  Result.SendBeginRequest(AType);
  Inc(FRequestsSent);
end;

procedure TLFastCGIClient.EndRequest(ARequest: TLFastCGIRequest);
begin
  if FFreeRequest <> nil then
    ARequest.FNextFree := FFreeRequest.FNextFree
  else
    FFreeRequest := ARequest;
  FFreeRequest.FNextFree := ARequest;
  if FPool <> nil then
    FPool.EndRequest(Self);
end;
   
{ TLFastCGIPool }

constructor TLFastCGIPool.Create;
begin
  FClientsMax := 1;
  FMaxRequestsConn := 1;
  inherited;
end;

destructor TLFastCGIPool.Destroy;
var
  I: integer;
begin
  for I := 0 to FClientsAvail-1 do
    FClients[I].Free;
  FreeMem(FClients);
  if FTimer <> nil then
    FTimer.Free;
  inherited;
end;

function  TLFastCGIPool.CreateClient: TLFastCGIClient;
begin
  if FClientsAvail = FClientsCount then
  begin
    Inc(FClientsCount, 64);
    ReallocMem(FClients, FClientsCount*sizeof(TLFastCGIRequest));
  end;
  Result := TLFastCGIClient.Create(nil);
  Result.FPool := Self;
  Result.Eventer := FEventer;
  FClients[FClientsAvail] := Result;
  Inc(FClientsAvail);
end;

function  TLFastCGIPool.BeginRequest(AType: integer): TLFastCGIRequest;
var
  lTempClient: TLFastCGIClient;
begin
  Result := nil;
  while FFreeClient <> nil do
  begin
    lTempClient := FFreeClient.FNextFree;
    Result := lTempClient.BeginRequest(AType);
    if Result <> nil then break;
    { Result = nil -> no free requesters on next free client }
    if lTempClient = FFreeClient then
      FFreeClient := nil
    else
      FFreeClient.FNextFree := lTempClient.FNextFree;
    lTempClient.FNextFree := nil;
  end;

  { all clients busy }
  if Result = nil then
    if FClientsAvail < FClientsMax then
      Result := CreateClient.BeginRequest(AType);
end;

procedure TLFastCGIPool.EndRequest(AClient: TLFastCGIClient);
begin
  { TODO: wait for other requests to be completed }
  if AClient.RequestsSent = FMaxRequestsConn then
    AClient.Disconnect;
  AddToFreeClients(AClient);
end;

procedure TLFastCGIPool.AddToFreeClients(AClient: TLFastCGIClient);
begin
  if AClient.FNextFree <> nil then exit;
  
  if FFreeClient = nil then
    FFreeClient := AClient
  else
    AClient.FNextFree := FFreeClient.FNextFree;
  FFreeClient.FNextFree := AClient;
end;

procedure TLFastCGIPool.ConnectClients(Sender: TObject);
var
  I: integer;
begin
  for I := 0 to FClientsAvail-1 do
    if FClients[I].FState = fsStartingServer then
      FClients[I].Connect;
end;

procedure TLFastCGIPool.StartServer;
begin
  if FSpawnState = ssNone then
  begin
    FSpawnState := ssSpawning;
    SpawnFCGIProcess(FAppName, FAppEnv, FPort);
    if FTimer = nil then
      FTimer := TLTimer.Create;
    FTimer.OneShot := true;
    FTimer.OnTimer := @ConnectClients;
  end;
  FTimer.Interval := 2000;
end;

end.
