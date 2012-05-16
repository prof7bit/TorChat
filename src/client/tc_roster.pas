unit tc_roster;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  tc_interface,
  tc_templist;

type
  { TBuddyList contains all the buddy objects and implements all the boring
  CRUD mechanisms, persisting on disk, etc. Its essentialy an array of
  IBuddy with a few helper methods to manage it. TBuddyList is thread safe.}
  TBuddyList = class(TBuddyListTemp, IBuddyList)
  strict protected
    FOwnID: String;
  public
    constructor Create(AClient: IClient); reintroduce;
    procedure SetOwnID(AID: String); virtual;
    function OwnID: String; virtual;
    procedure Load; virtual;
    procedure Save; virtual;
  end;


implementation
uses
  sysutils,
  tc_buddy,
  fpjson,
  jsonparser;

{ TBuddyList }

constructor TBuddyList.Create(AClient: IClient);
begin
  Inherited Create(AClient);
  Load;
end;

procedure TBuddyList.SetOwnID(AID: String);
var
  tc_buddy : IBuddy;
begin
  FOwnID := AID;
  if FindBuddy(AID) = nil then begin
    writeln('TBuddyList.SetOwnID() adding "myself"-buddy ' + AID);
    tc_buddy := TBuddy.Create(FClient);
    tc_buddy.InitID(AID);
    tc_buddy.SetFriendlyName('myself');
    AddBuddy(tc_buddy);
    Save;
  end;
end;

function TBuddyList.OwnID: String;
begin
  Result := FOwnID;
end;

procedure TBuddyList.Load;
var
  FS: TFileStream = nil;
  JParser: TJSONParser = nil;
  JList: TJSONArray = nil;
  LastI, I: Integer;
  tc_buddy: IBuddy;
begin
  try
    writeln('TBuddyList.Load()');
    FS := TFileStream.Create(ConcatPaths([FClient.Config.DataDir, 'buddylist.json']), fmOpenRead);
    JParser :=TJSONParser.Create(FS);
    JList := JParser.Parse as TJSONArray;
    LastI := JList.Count - 1;
    for I := 0 to LastI do begin
      try
        tc_buddy := TBuddy.Create(FClient);
        tc_buddy.InitFromJsonObect(JList.Objects[I]); // this may raise exception
        AddBuddy(tc_buddy);
        writeln('TBuddyList.Load() ' + tc_buddy.ID + ' loaded');
      except
        FreeAndNil(tc_buddy);
        writeln('E TBuddyList.Load() error while parsing buddy');
      end;
    end;
  except
    on E: Exception do begin
      WriteLn('W TBuddyList.Load() could not load: ' + E.Message);
    end;
  end;
  if assigned(JList) then FreeAndNil(JList);
  if assigned(JParser) then FreeAndNil(JParser);
  if assigned(FS) then FreeAndNil(FS);
  Save;
end;

procedure TBuddyList.Save;
var
  tc_buddy: IBuddy;
  JArr : TJSONArray;
  JData: String;
  FS: TFileStream = nil;
begin
  writeln('TBuddyList.Save()');
  JArr := TJSONArray.Create;

  for tc_buddy in self do
    JArr.Add(tc_buddy.AsJsonObject);

  JData := JArr.FormatJSON([foSingleLineObject]);
  JArr.Free;
  try
    FS := TFileStream.Create(ConcatPaths([FClient.Config.DataDir, 'buddylist.json']), fmCreate + fmOpenWrite);
    FS.Write(JData[1], Length(JData));
  except
    on E: Exception do begin
      writeln('E TBuddyList.Save() could not save: ' + E.Message);
    end;
  end;
  if assigned(FS) then FreeAndNil(FS);
end;

end.

