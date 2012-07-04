unit tc_roster;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  tc_interface,
  tc_templist;

type
  { TRoster contains all the buddy objects and implements all the boring
  CRUD mechanisms, persisting on disk, etc. Its essentialy an array of
  IBuddy with a few helper methods to manage it. TRoster is thread safe.}
  TRoster = class(TTempList, IRoster)
  strict protected
    FOwnID: String;
    FShowMyself: Boolean;
    FIsLoading: Boolean;
  public
    constructor Create(AClient: IClient); reintroduce;
    procedure SetOwnID(AID: String);
    function OwnID: String;
    function GroupName: String;
    procedure AddBuddy(ABuddy: IBuddy); override;
    procedure AddBuddyNoCallback(ABuddy: IBuddy);
    procedure RemoveBuddy(ABuddy: IBuddy); override;
    procedure RemoveBuddyNoCallback(ABuddy: IBuddy);
    procedure Load;
    procedure Save;
  end;


implementation
uses
  sysutils,
  fpjson,
  jsonparser,
  tc_buddy,
  tc_misc;

{ TRoster }

constructor TRoster.Create(AClient: IClient);
begin
  Inherited Create(AClient);
  FShowMyself := True;//False;
  FIsLoading := False;
end;

procedure TRoster.SetOwnID(AID: String);
var
  Buddy : IBuddy;
begin
  FOwnID := AID;
  if FShowMyself then begin
    if (ByID(AID) = nil) then begin
      writeln('TRoster.SetOwnID() adding "myself"-buddy ' + AID);
      Buddy := TBuddy.Create(FClient);
      Buddy.InitID(AID);
      Buddy.SetLocalAlias('myself');
      AddBuddy(Buddy);
    end;
  end
  else begin
    Buddy := ByID(AID);
    if Assigned(Buddy) then
      RemoveBuddy(Buddy);
  end;
end;

function TRoster.OwnID: String;
begin
  Result := FOwnID;
end;

function TRoster.GroupName: String;
begin
  Result := Format('%s (%s)', [FClient.ProfileName, FOwnID]);
end;

procedure TRoster.AddBuddy(ABuddy: IBuddy);
begin
  AddBuddyNoCallback(ABuddy);
  FClient.OnBuddyAdded(ABuddy);
  FClient.OnBuddyAvatarChange(ABuddy);
  FClient.OnBuddyStatusChange(ABuddy);
end;

procedure TRoster.AddBuddyNoCallback(ABuddy: IBuddy);
begin
  inherited AddBuddy(ABuddy);
  if not FIsLoading then
    Save;
end;

procedure TRoster.RemoveBuddy(ABuddy: IBuddy);
begin
  RemoveBuddyNoCallback(ABuddy);
  FClient.OnBuddyRemoved(ABuddy);
end;

procedure TRoster.RemoveBuddyNoCallback(ABuddy: IBuddy);
begin
  WriteLnF('I removing %s from roster', [ABuddy.ID]);
  inherited RemoveBuddy(ABuddy);
  Save;
end;

procedure TRoster.Load;
var
  FileName: String;
  FS: TFileStream = nil;
  JParser: TJSONParser = nil;
  JList: TJSONArray = nil;
  LastI, I: Integer;
  tc_buddy: IBuddy;
  Success: Boolean;
begin
  Success := True;
  FIsLoading := True; // prevent autosave while adding buddies
  FileName := ConcatPaths([FClient.Config.DataDir, 'buddylist.json']);
  try
    writeln('TRoster.Load()');
    FS := TFileStream.Create(FileName, fmOpenRead);
    JParser :=TJSONParser.Create(FS);
    JList := JParser.Parse as TJSONArray;
    LastI := JList.Count - 1;
    for I := 0 to LastI do begin
      try
        tc_buddy := TBuddy.Create(FClient);
        tc_buddy.InitFromJsonObect(JList.Objects[I]); // this may raise exception
        AddBuddyNoCallback(tc_buddy);
        writeln('TRoster.Load() ' + tc_buddy.ID + ' loaded');
      except
        Success := False;
        FreeAndNil(tc_buddy);
        writeln('E TRoster.Load() error while parsing buddy');
      end;
    end;
  except
    on E: Exception do begin
      Success := False;
      WriteLn('W TRoster.Load() could not load: ' + E.Message);
    end;
  end;
  if assigned(FS) then FreeAndNil(FS);
  if assigned(JList) then FreeAndNil(JList);
  if assigned(JParser) then FreeAndNil(JParser);
  if not Success then
    if RenameFile(FileName, FileName + '.broken') then
      WriteLn('E renamed broken buddy list to ' + FileName + '.broken');
  FIsLoading := False;
  Save;
end;

procedure TRoster.Save;
var
  Path, FileName, TempName: String;
  Buddy: IBuddy;
  JArr : TJSONArray;
  JData: String;
  FS: TFileStream = nil;
  Success: Boolean;
begin
  writeln('TRoster.Save()');
  Success := False;
  Path := FClient.Config.DataDir;
  TempName := ConcatPaths([Path,'_buddylist.json']);
  FileName := ConcatPaths([Path,'buddylist.json']);
  JArr := TJSONArray.Create;

  for Buddy in Self do
    JArr.Add(Buddy.AsJsonObject);

  JData := JArr.FormatJSON([foSingleLineObject]);
  JArr.Free;
  try
    FS := TFileStream.Create(TempName, fmCreate + fmOpenWrite);
    FS.Write(JData[1], Length(JData));
    Success := True;
  except
    on E: Exception do begin
      writeln('E TRoster.Save() could not save: ' + E.Message);
    end;
  end;
  if Assigned(FS) then FreeAndNil(FS);

  if Success then begin
    SafeDelete(FileName);
    RenameFile(TempName, FileName);
  end
  else
    SafeDelete(TempName);
end;

end.

