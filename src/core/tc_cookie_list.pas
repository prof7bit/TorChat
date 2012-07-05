unit tc_cookie_list;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface
uses
  tc_gen_list;

type
  { TCookieEntry }

  TCookieEntry = record
    ID: String;
    Cookie: String;
    class operator = (A,B: TCookieEntry): Boolean;
  end;

  { TCookieList }

  TCookieList = class(specialize GList<TCookieEntry>)
    function Add(ABuddyID, ACookie: String): Boolean;
    procedure Remove(ACookie: String);
    function CountByID(ABuddyID: String): Integer;
  end;

implementation
uses
  tc_misc;

{ TCookieEntry }

class operator TCookieEntry.= (A, B: TCookieEntry): Boolean;
begin
  Result := (A.ID = B.ID) and (A.Cookie = B.Cookie);
end;

{ TCookieList }

function TCookieList.Add(ABuddyID, ACookie: String): Boolean;
var
  CE: TCookieEntry;
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
  CE.Cookie := ACookie;
  CE.ID := ABuddyID;
  inherited Add(CE);
  if C > 0 then begin
    WriteLnF('W %d different cookies from the same ID %s', [C+1, ABuddyID]);
    Result := True;
  end;
end;

procedure TCookieList.Remove(ACookie: String);
var
  C: TCookieEntry;
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
  Item: TCookieEntry;
begin
  Result := 0;
  for Item in self do
    if Item.ID = ABuddyID then
      inc(Result);
end;


end.

