{ lNet Timer

  CopyRight (C) 2006-2008 Micha Nelissen

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

  This license has been modified. See File LICENSE.ADDON for more inFormation.
  Should you find these sources without a LICENSE File, please contact
  me at ales@chello.sk
}

unit ltimer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TLTimer }

  TLTimer = class(TObject)
  protected
    FOnTimer: TNotifyEvent;
    FInterval: TDateTime;
    FStarted: TDateTime;
    FOneShot: Boolean;
    FEnabled: Boolean;

    function  GetInterval: Integer;
    procedure SetInterval(const aValue: Integer);
  public
    procedure CallAction;
    property Enabled: Boolean read FEnabled write FEnabled;
    property Interval: Integer read GetInterval write SetInterval;
    property OneShot: Boolean read FOneShot write FOneShot;
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
  end;

implementation

{ TLTimer }

function TLTimer.GetInterval: Integer;
begin
  Result := Round(FInterval * MSecsPerDay);
end;

procedure TLTimer.SetInterval(const aValue: Integer);
begin
  FInterval := AValue / MSecsPerDay;
  FStarted := Now;
  FEnabled := true;
end;

procedure TLTimer.CallAction;
begin
  if FEnabled and Assigned(FOnTimer) and (Now - FStarted >= FInterval) then 
  begin
    FOnTimer(Self);
    if not FOneShot then
      FStarted := Now
    else
      FEnabled := false;
  end;
end;

end.

