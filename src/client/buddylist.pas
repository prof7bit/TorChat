unit buddylist;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  torchatabstract;

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


implementation
uses
  sysutils,
  buddy,
  clientconfig,
  fpjson,
  jsonparser;

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

end.

