unit tc_templist;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  tc_interface;

type

  { TTempList }

  TTempList = class(TInterfaceList, ITempList)
  strict protected
    FClient: IClient;
  public
    constructor Create(AClient: IClient);
    destructor Destroy; override;
    procedure CheckState; virtual;
    procedure AddBuddy(ABuddy: IBuddy); virtual;
    procedure RemoveBuddy(ABuddy: IBuddy); virtual;
    function FindBuddy(AName: String): IBuddy; virtual;
    function FindBuddyByCookie(ACookie: String): IBuddy; virtual;
    function HasBuddy(ABuddy: IBuddy): Boolean; virtual;
    procedure DoDisconnectAll; virtual;
    function GetEnumerator: TABuddyEnumerator; virtual;
  end;

implementation
uses
  tc_misc;

type
  { TBuddyEnumerator }
  TBuddyEnumerator = class(TABuddyEnumerator)
  strict private
    FPosition: Integer;
    FList: IInterfaceList;
  public
    constructor Create(AList: IInterfaceList);
    function GetCurrent: IBuddy; override;
    function MoveNext: Boolean; override;
    property Current: IBuddy read GetCurrent;
  end;

{ TBuddyEnumerator }

constructor TBuddyEnumerator.Create(AList: IInterfaceList);
begin
  FList := AList;
  FPosition := -1;
end;

function TBuddyEnumerator.GetCurrent: IBuddy;
begin
  Result := IBuddy(FList[FPosition]);
end;

function TBuddyEnumerator.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FList.Count;
end;

{ TTempList }

constructor TTempList.Create(AClient: IClient);
begin
  FClient := AClient;
  inherited Create;
end;

destructor TTempList.Destroy;
begin
  inherited Destroy;
  WriteLn(MilliTime, ' TBuddyListTemp.Destroy() finished');
end;

procedure TTempList.CheckState;
var
  Buddy: IBuddy;
begin
  for Buddy in Self do begin
    Buddy.CheckState;
  end;
end;

procedure TTempList.AddBuddy(ABuddy: IBuddy);
begin
  Self.Add(ABuddy);
  FClient.OnBuddyAdded(ABuddy);
end;

procedure TTempList.RemoveBuddy(ABuddy: IBuddy);
begin
  Self.Remove(ABuddy);
end;

function TTempList.FindBuddy(AName: String): IBuddy;
var
  Buddy: IBuddy;
begin
  Result := nil;
  for Buddy in Self do
    if Buddy.ID = AName then
      exit(Buddy);
end;

function TTempList.FindBuddyByCookie(ACookie: String): IBuddy;
var
  Buddy: IBuddy;
begin
  Result := nil;
  for Buddy in Self do
    if Buddy.Cookie = ACookie then
      exit(Buddy);
end;

function TTempList.HasBuddy(ABuddy: IBuddy): Boolean;
var
  Buddy: IBuddy;
begin
  Result := False;
  for Buddy in Self do
    if Buddy = ABuddy then
      Exit(True);
end;

procedure TTempList.DoDisconnectAll;
var
  Buddy: IBuddy;
begin
  for Buddy in Self do
    Buddy.DoDisconnect;
end;

function TTempList.GetEnumerator: TABuddyEnumerator;
begin
  Result := TBuddyEnumerator.Create(self);
end;


end.

