{ Mime types helper

  Copyright (C) 2006-2008 Micha Nelissen

  This library is Free software; you can redistribute it and/or modify it
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
  
  This license has been modified. See file LICENSE.ADDON for more information.
  Should you find these sources without a LICENSE File, please contact
  me at ales@chello.sk
}

unit lMimeTypes;

{$mode objfpc}{$h+}

interface

uses
  classes, sysutils, strutils;

type
  TStringObject = class(TObject)
    Str: string;
  end;
  
  procedure InitMimeList(const aFileName: string);

var
  MimeList: TStringList = nil;

implementation

var
  MimeFileName: string;

procedure InitMimeList(const aFileName: string);
var
  MimeFile: Text;
  lPos, lNextPos: integer;
  lLine, lName: string;
  lStrObj: TStringObject;
  lBuffer: array[1..32*1024] of byte;
begin
  if not Assigned(MimeList) then begin
    MimeFileName := aFileName;
    MimeList := TStringList.Create;
    if FileExists(MimeFileName) then
    begin
      Assign(MimeFile, MimeFileName);
      Reset(MimeFile);
      SetTextBuf(MimeFile, lBuffer);
      while not Eof(MimeFile) do
      begin
        ReadLn(MimeFile, lLine);
        if (Length(lLine) = 0) or (lLine[1] = '#') then
          continue;

        lPos := Pos(#9, lLine);
        if lPos = 0 then
          continue;
        lName := Copy(lLine, 1, lPos-1);

        while (lPos <= Length(lLine)) and (lLine[lPos] in [#9,' ']) do
          Inc(lPos);
        if lPos > Length(lLine) then
          continue;
        repeat
          lNextPos := PosEx(' ', lLine, lPos);
          if lNextPos = 0 then
            lNextPos := Length(lLine)+1;
          lStrObj := TStringObject.Create;
          lStrObj.Str := lName;
          MimeList.AddObject('.'+Copy(lLine, lPos, lNextPos-lPos), lStrObj);
          lPos := lNextPos+1;
        until lPos > Length(lLine);
      end;
      close(MimeFile);
    end;
    MimeList.Sorted := true;
  end;
end;

procedure FreeMimeList;
var
  I: integer;
begin
  if Assigned(MimeList) then begin
    for I := 0 to MimeList.Count - 1 do
      MimeList.Objects[I].Free;
    FreeAndNil(MimeList);
  end;
end;

finalization
  FreeMimeList;
end.
