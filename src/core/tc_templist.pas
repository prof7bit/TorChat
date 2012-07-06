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
    procedure AddBuddy(ABuddy: IBuddy); virtual;
    procedure RemoveBuddy(ABuddy: IBuddy); virtual;
    function ByID(ABuddyID: String): IBuddy;
    function ByCookie(ACookie: String): IBuddy;
    procedure DoDisconnectAll;
    function GetEnumerator: TBuddyEnumerator;
  end;

implementation

{ TTempList }

constructor TTempList.Create(AClient: IClient);
begin
  FClient := AClient;
  inherited Create;
end;

destructor TTempList.Destroy;
begin
  inherited Destroy;
  WriteLn('TTempList.Destroy() finished');
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

function TTempList.GetEnumerator: TBuddyEnumerator;
begin
  Result := TBuddyEnumerator.Create(Self);
end;


end.

