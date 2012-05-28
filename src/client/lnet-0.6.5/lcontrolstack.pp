{ Control stack

  CopyRight (C) 2004-2008 Ales Katona

  This library is Free software; you can rediStribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is diStributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; withOut even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a Copy of the GNU Library General Public License
  along with This library; if not, Write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

  This license has been modified. See File LICENSE for more inFormation.
  Should you find these sources withOut a LICENSE File, please contact
  me at ales@chello.sk
}

unit lControlStack;

{$mode objfpc}

interface

const
  TL_CSLENGTH = 3;

type
  TLOnFull = procedure of object;
  
  TLControlStack = class
   private
    FItems: array of Char;
    FIndex: Byte;
    FOnFull: TLOnFull;
    function GetFull: Boolean;
    function GetItem(const i: Byte): Char;
    procedure SetItem(const i: Byte; const Value: Char);
   public
    constructor Create;
    procedure Clear;
    procedure Push(const Value: Char);
    property ItemIndex: Byte read FIndex;
    property Items[i: Byte]: Char read GetItem write SetItem; default;
    property Full: Boolean read GetFull;
    property OnFull: TLOnFull read FOnFull write FOnFull;
  end;

implementation

uses
  lTelnet;
  
constructor TLControlStack.Create;
begin
  FOnFull:=nil;
  FIndex:=0;
  SetLength(FItems, TL_CSLENGTH);
end;

function TLControlStack.GetFull: Boolean;
begin
  Result:=False;
  if FIndex >= TL_CSLENGTH then
    Result:=True;
end;

function TLControlStack.GetItem(const i: Byte): Char;
begin
  Result:=TS_NOP;
  if i < TL_CSLENGTH then
    Result:=FItems[i];
end;

procedure TLControlStack.SetItem(const i: Byte; const Value: Char);
begin
  if i < TL_CSLENGTH then
    FItems[i]:=Value;
end;

procedure TLControlStack.Clear;
begin
  FIndex:=0;
end;

procedure TLControlStack.Push(const Value: Char);
begin
  if FIndex < TL_CSLENGTH then begin
    FItems[FIndex]:=Value;
    Inc(FIndex);
    if Full and Assigned(FOnFull) then
      FOnFull;
  end;
end;

end.

