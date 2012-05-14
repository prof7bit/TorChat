unit buddylisttemp;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  torchatabstract;

type

  { TBuddyListTemp }

  TBuddyListTemp = class(TInterfaceList, IBuddyListTemp)
  strict protected
    FClient: TAClient;
  public
    constructor Create(AClient: TAClient);
    destructor Destroy; override;
    procedure CheckState; virtual;
    procedure AddBuddy(ABuddy: IBuddy); virtual;
    procedure RemoveBuddy(ABuddy: IBuddy); virtual;
    function FindBuddy(AName: String): IBuddy; virtual;
    function FindBuddyByCookie(ACookie: String): IBuddy; virtual;
    procedure DoDisconnectAll; virtual;
    function GetEnumerator: TABuddyEnumerator; virtual;
  end;

implementation
uses
  miscfunc;

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

{ TBuddyListTemp }

constructor TBuddyListTemp.Create(AClient: TAClient);
begin
  FClient := AClient;
  inherited Create;
end;

destructor TBuddyListTemp.Destroy;
begin
  inherited Destroy;
  WriteLn(MilliTime, ' TBuddyListTemp.Destroy() finished');
end;

procedure TBuddyListTemp.CheckState;
var
  Buddy: IBuddy;
begin
  for Buddy in Self do begin
    Buddy.CheckState;
  end;
end;

procedure TBuddyListTemp.AddBuddy(ABuddy: IBuddy);
begin
  Self.Add(ABuddy);
  FClient.OnBuddyAdded(ABuddy);
end;

procedure TBuddyListTemp.RemoveBuddy(ABuddy: IBuddy);
begin
  Self.Remove(ABuddy);
end;

function TBuddyListTemp.FindBuddy(AName: String): IBuddy;
var
  Buddy: IBuddy;
begin
  Result := nil;
  for Buddy in Self do
    if Buddy.ID = AName then
      exit(Buddy);
end;

function TBuddyListTemp.FindBuddyByCookie(ACookie: String): IBuddy;
var
  Buddy: IBuddy;
begin
  Result := nil;
  for Buddy in Self do
    if Buddy.Cookie = ACookie then
      exit(Buddy);
end;

procedure TBuddyListTemp.DoDisconnectAll;
var
  Buddy: IBuddy;
begin
  for Buddy in Self do
    Buddy.DoDisconnect;
end;

function TBuddyListTemp.GetEnumerator: TABuddyEnumerator;
begin
  Result := TBuddyEnumerator.Create(self);
end;


end.

