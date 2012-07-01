{ TorChat - Main window

  Copyright (C) 2012 Bernd Kreuss <prof7bit@gmail.com>

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 3 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}
unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  Forms,
  Controls,
  ExtCtrls,
  ComCtrls,
  LMessages,
  gui_interface,
  roster_manager;

type
  { TFMain }
  TFMain = class(TForm)
    ImageList: TImageList;
    PumpTimer: TIdleTimer;
    RosterView: TTreeView;
    procedure FormCreate(Sender: TObject);
    procedure PumpTimerTimer(Sender: TObject);
    procedure OnNeedPump(var Msg: TLMessage); message WM_NEEDPUMP;
  strict private
    FRosterManager: TGuiRosterManager;
  end;

var
  FMain: TFMain;

implementation
uses
  language;


{$R *.lfm}

{ TFMain }

procedure TFMain.FormCreate(Sender: TObject);
begin
  FRosterManager := TGuiRosterManager.Create(RosterView, FMain.Handle);
end;

procedure TFMain.PumpTimerTimer(Sender: TObject);
begin
  FRosterManager.Pump;
end;

procedure TFMain.OnNeedPump(var Msg: TLMessage);
begin
  FRosterManager.Pump;
end;

end.

