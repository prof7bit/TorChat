{ TorChat - TTorChatClient, this component is implememting the client

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
unit torchatclient;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  contnrs,
  fpjson,
  jsonparser,
  torchatabstract,
  buddy,
  clientconfig,
  torprocess,
  networking,
  connection,
  miscfunc;

type
  { TBuddyList contains all the buddy objects and implements all the boring
    CRUD mechanisms, persisting on disk, etc. Its essentialy an array of
    TABuddy with a few helper methods to manage it. TBuddyList is thread safe.}
  TBuddyList = class(TABuddyList)
  strict protected
    FCritical: TRTLCriticalSection;
  public
    constructor Create(AClient: TAClient); reintroduce;
    destructor Destroy; override;
    procedure CheckState; override;
    procedure SetOwnID(AID: String); override;
    procedure Lock; override;
    procedure Unlock; override;
    procedure Load; override;
    procedure Save; override;
    procedure RemoveBuddy(ABuddy: TABuddy); override;
    procedure AddBuddy(ABuddy: TABuddy); override;
    function FindBuddy(AID: String): TABuddy; override;
    function FindBuddyByCookie(ACookie: String): TABuddy; override;
    procedure DoDisconnectAll; override;
    function Count: Integer; override;
  end;

  { TTorChatClient implements the abstract TAClient. Together with all its
    contained objects this represents a fully functional TorChat client.
    The GUI (or libpurpletorchat or a command line client) will derive a class
    from TTorChatClient overriding the virtual event methods (the methods that
    start with On (see the definition of TAClient) to hook into the events and
    then create an instance of it.
    The GUI also must call the TorChatClient.ProcessMessages method in regular
    intervals (1 second or so) from within the GUI-thread. Aditionally whenever
    there is an incoming chat message, status change or other event then
    TorChatClient will fire the OnNotifyGui event and as a response the GUI should
    schedule an additional call to ProcessMessages a soon as possible. All other
    event method calls will then always originate from the thread that is
    calling TorChatClient.ProcessMessages. This method will process one queued
    message per call, it will never block and if there are no messages and
    nothing else to do it will just return.}
  TTorChatClient = class(TAClient)
  strict protected
    FIsDestriying: Boolean;
    FTor: TTor;
    FQueue: TObjectQueue;
    CS: TRTLCriticalSection;
    FTimeStarted: TDateTime;
    FHSNameOK: Boolean;
    procedure CbNetIn(AStream: TTCPStream; E: Exception);
    procedure PopNextMessage;
    procedure CheckHiddenServiceName;
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;
    procedure Enqueue(AMessage: TAMessage); override;
    procedure ProcessMessages; override;
    procedure SetStatus(AStatus: TTorchatStatus); override;
  end;

implementation

{ TBuddyList }

constructor TBuddyList.Create(AClient: TAClient);
begin
  InitCriticalSection(FCritical);
  inherited Create(AClient);
  FClient := AClient;
  Load;
end;

destructor TBuddyList.Destroy;
begin
  inherited Destroy;
  DoneCriticalsection(FCritical);
end;

procedure TBuddyList.SetOwnID(AID: String);
var
  Buddy : TABuddy;
begin
  FOwnID := AID;
  if FindBuddy(AID) = nil then begin
    writeln('adding "myself"-buddy ' + AID + ' to the list');
    Buddy := TBuddy.Create(FClient);
    Buddy.InitID(AID);
    Buddy.FriendlyName := 'myself';
    AddBuddy(Buddy);
    Save;
  end;
end;

procedure TBuddyList.Lock;
begin
  EnterCriticalsection(FCritical);
end;

procedure TBuddyList.Unlock;
begin
  LeaveCriticalsection(FCritical);
end;

procedure TBuddyList.CheckState;
var
  Buddy: TABuddy;
begin
  EnterCriticalsection(FCritical);
  for Buddy in FList do begin
    Buddy.CheckState;
  end;
  LeaveCriticalsection(FCritical);
end;

procedure TBuddyList.Load;
var
  FS: TFileStream = nil;
  JParser: TJSONParser = nil;
  JList: TJSONArray = nil;
  Last, I: Integer;
  Buddy: TABuddy;
begin
  try
    writeln('trying to load saved buddy list');
    FS := TFileStream.Create(ConcatPaths([ConfGetDataDir, 'buddylist.json']), fmOpenRead);
    JParser :=TJSONParser.Create(FS);
    JList := JParser.Parse as TJSONArray;
    Last := JList.Count - 1;
    EnterCriticalsection(FCritical);
    for I := 0 to Last do begin
      try
        Buddy := TBuddy.Create(FClient);
        Buddy.InitFromJsonObect(JList.Objects[I]); // this may raise exception
        AddBuddy(Buddy);
        writeln('buddy ' + Buddy.ID + ' loaded');
      except
        FreeAndNil(Buddy);
        writeln('E error while parsing buddy from saved list');
      end;
    end;
    LeaveCriticalsection(FCritical);
  except
    on E: Exception do begin
      WriteLn('(1) could not load buddy list: ' + E.Message);
    end;
  end;
  if assigned(JList) then FreeAndNil(JList);
  if assigned(JParser) then FreeAndNil(JParser);
  if assigned(FS) then FreeAndNil(FS);
  Save;
end;

procedure TBuddyList.Save;
var
  Buddy: TABuddy;
  JArr : TJSONArray;
  JData: String;
  FS: TFileStream = nil;
begin
  writeln('saving buddy list');
  JArr := TJSONArray.Create;
  EnterCriticalsection(FCritical);
  for Buddy in FList do begin
    JArr.Add(Buddy.AsJsonObject);
  end;
  LeaveCriticalsection(FCritical);
  JData := JArr.FormatJSON([foSingleLineObject]);
  JArr.Free;
  try
    FS := TFileStream.Create(ConcatPaths([ConfGetDataDir, 'buddylist.json']), fmCreate + fmOpenWrite);
    FS.Write(JData[1], Length(JData));
  except
    on E: Exception do begin
      writeln('E could not save buddy list: ' + E.Message);
    end;
  end;
  if assigned(FS) then FreeAndNil(FS);
end;

procedure TBuddyList.RemoveBuddy(ABuddy: TABuddy);
var
  I,J,Last : Integer;
begin
  EnterCriticalsection(FCritical);
  Last := Length(FList) - 1;
  for I := 0 to Last do begin
    if FList[I] = ABuddy then begin
      for J := I to Last-1 do begin
        FList[J] := FList[J+1];
      end;
      SetLength(FList, Last);
      break;
    end;
  end;
  LeaveCriticalsection(FCritical);
end;

procedure TBuddyList.AddBuddy(ABuddy: TABuddy);
var
  P : Integer;
begin
  EnterCriticalsection(FCritical);
  P := Length(FList);
  SetLength(FList, P+1);
  FList[P] := ABuddy;
  LeaveCriticalsection(FCritical);
end;

function TBuddyList.FindBuddy(AID: String): TABuddy;
begin
  Result := nil;
  EnterCriticalsection(FCritical);
  for Result in FList do begin
    if Result.ID = AID then
      break;
  end;
  LeaveCriticalsection(FCritical);
end;

function TBuddyList.FindBuddyByCookie(ACookie: String): TABuddy;
begin
  Result := nil;
  EnterCriticalsection(FCritical);
  for Result in FList do begin
    if Result.Cookie = ACookie then
      break;
  end;
  LeaveCriticalsection(FCritical);
end;

procedure TBuddyList.DoDisconnectAll;
var
  Buddy: TABuddy;
begin
  EnterCriticalsection(FCritical);
  for Buddy in FList do begin
    Buddy.DoDisconnect;
  end;
  LeaveCriticalsection(FCritical);
end;

function TBuddyList.Count: Integer;
begin
  Result := Length(FList);
end;

{ TTorChatClient }

constructor TTorChatClient.Create(AOwner: TComponent);
//var
//  C : TAHiddenConnection;
begin
  FIsDestriying := False;
  Inherited Create(AOwner);
  FMainThread := ThreadID;
  Randomize;
  self.FStandardOut := Output;
  InitCriticalSection(CS);
  FHSNameOK := False;
  FTimeStarted := 0; // we will initialize it on first ProcessMessages() call
  FQueue := TObjectQueue.Create;
  FTor := TTor.Create(self);
  FNetwork := TSocketWrapper.Create(Self);
  FBuddyList := TBuddyList.Create(self);
  with FNetwork do begin
    SocksProxyAddress := ConfGetTorHost;
    SocksProxyPort := ConfGetTorPort;
    IncomingCallback := @CbNetIn;
    Bind(ConfGetListenPort);
  end;
end;

destructor TTorChatClient.Destroy;
var
  Msg: TAMessage;
begin
  WriteLn(MilliTime, ' start destroying TorChatClient');
  FIsDestriying := True;
  BuddyList.DoDisconnectAll;
  EnterCriticalsection(CS);
  while FQueue.Count > 0 do begin
    Msg := FQueue.Pop as TAMessage;
    Msg.Free;
  end;
  FQueue.Free;
  LeaveCriticalsection(CS);
  DoneCriticalsection(CS);
  WriteLn(MilliTime, ' start destroying child components');
  inherited Destroy;
end;

procedure TTorChatClient.Enqueue(AMessage: TAMessage);
begin
  if FIsDestriying then begin
    // no more messages during destruction, they won't be processed
    // anyways and we also don't want to generate any new timer events.
    // just free the message, throw it away.
    WriteLn('TTorChatClient.Enqueue() not enqueuing message during shutdown');
    AMessage.Free;
  end
  else begin
    EnterCriticalsection(CS);
    FQueue.Push(AMessage);
    LeaveCriticalsection(CS);
    OnNotifyGui;
  end;
end;

procedure TTorChatClient.ProcessMessages;
begin
  if FIsDestriying then exit;
  if FTimeStarted = 0 then FTimeStarted := Now;
  CheckHiddenServiceName;
  PopNextMessage;
  BuddyList.CheckState;
end;

procedure TTorChatClient.SetStatus(AStatus: TTorchatStatus);
begin
  writeln('setting own status to ', AStatus);
end;

procedure TTorChatClient.CbNetIn(AStream: TTCPStream; E: Exception);
var
  C : THiddenConnection;
begin
  Output := self.StandardOut; // make writeln redirect work in this thread
  writeln('TTorChatClient.CbNetIn()');
  C := THiddenConnection.Create(self, AStream);
  Ignore(C);
  Ignore(E);
end;

procedure TTorChatClient.PopNextMessage;
var
  Msg: TAMessage;
begin
  if FQueue.Count > 0 then begin
    EnterCriticalsection(CS);
    Msg := FQueue.Pop as TAMessage;
    LeaveCriticalsection(CS);
    try
      Msg.Execute;
    except
      on E: Exception do begin
        WriteLn(E.Message);
      end;
    end;
    Msg.Free;
  end;
end;

procedure TTorChatClient.CheckHiddenServiceName;
var
  HSName: String;
begin
  if not FHSNameOK then begin;
    if SecondsSince(FTimeStarted) < SECONDS_WAIT_FOR_HOSTNAME_FILE then begin
      HSName := ConfGetHiddenServiceName;
      if HSName <> '' then begin
        writeln('found own name: ' + HSName);
        BuddyList.OwnID := HSName;
        FHSNameOK := True;
      end;
    end;
  end;
end;

end.

