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
    procedure CheckState;
    procedure AddBuddy(ABuddy: IBuddy);
    procedure RemoveBuddy(ABuddy: IBuddy);
    function ByID(ABuddyID: String): IBuddy;
    function ByCookie(ACookie: String): IBuddy;
    procedure DoDisconnectAll;
    function GetEnumerator: TABuddyEnumerator;
  end;

implementation

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
  WriteLn('TBuddyListTemp.Destroy() finished');
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

function TTempList.ByID(ABuddyID: String): IBuddy;
var
  Buddy: IBuddy;
begin
  Result := nil;
  for Buddy in Self do
    if Buddy.ID = ABuddyID then
      exit(Buddy);
end;

function TTempList.ByCookie(ACookie: String): IBuddy;
var
  Buddy: IBuddy;
begin
  Result := nil;
  for Buddy in Self do
    if Buddy.Cookie = ACookie then
      exit(Buddy);
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
  Result := TBuddyEnumerator.Create(Self);
end;


end.

