unit tc_cookie_list;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface
uses
  Classes,
  tc_interface;

type
  { TCookieEntry }

  TCookieEntry = class(TInterfacedObject, ICookieEntry)
    FID: String;
    FCookie: String;
    constructor Create(ABuddy, ACookie: String); reintroduce;
    function ID: String;
    function Cookie: String;
  end;

  { TCookieList }

  TCookieList = class(TInterfaceList, ICookieList)
    function Add(ABuddyID, ACookie: String): Boolean;
    procedure Remove(ACookie: String);
    function CountByID(ABuddyID: String): Integer;
    function GetEnumerator: TCookieEnumerator;
  end;

implementation
uses
  tc_misc;

{ TCookieEntry }

constructor TCookieEntry.Create(ABuddy, ACookie: String);
begin
  FID := ABuddy;
  FCookie := ACookie;
end;

function TCookieEntry.ID: String;
begin
  Result := FID;
end;

function TCookieEntry.Cookie: String;
begin
  Result := FCookie;
end;


{ TCookieList }

function TCookieList.Add(ABuddyID, ACookie: String): Boolean;
var
  CE: ICookieEntry;
  C: Integer;
begin
  Result := False;
  C := 0;
  for CE in Self do begin
    if CE.ID = ABuddyID then begin
      Inc(C);
      if CE.Cookie = ACookie then
        exit;
    end;
  end;
  CE := TCookieEntry.Create(ABuddyID, ACookie);
  inherited Add(CE);
  if C > 0 then begin
    WriteLnF('W %d different cookies from the same ID %s', [C+1, ABuddyID]);
    Result := True;
  end;
end;

procedure TCookieList.Remove(ACookie: String);
var
  C: ICookieEntry;
begin
  for C in Self do begin
    if C.Cookie = ACookie then begin
      inherited Remove(C);
      exit;
    end;;
  end;
end;

function TCookieList.CountByID(ABuddyID: String): Integer;
var
  Item: ICookieEntry;
begin
  Result := 0;
  for Item in self do
    if Item.ID = ABuddyID then
      inc(Result);
end;

function TCookieList.GetEnumerator: TCookieEnumerator;
begin
  Result := TCookieEnumerator.Create(Self);
end;


end.

