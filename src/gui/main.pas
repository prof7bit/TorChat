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
  tc_interface,
  tc_client;

type
  { TGuiClient }
  TGuiClient = class(TTorChatClient)
    procedure OnBuddyAdded(ABuddy: IBuddy); override;
    procedure OnBuddyRemoved(ABuddy: IBuddy); override;
    procedure OnBuddyStatusChange(ABuddy: IBuddy); override;
    procedure OnBuddyAvatarChange(ABuddy: IBuddy); override;
    procedure OnInstantMessage(ABuddy: IBuddy; AText: String); override;
    procedure OnGotOwnID; override;
    procedure OnNeedPump; override;
  end;

  { TFMain }
  TFMain = class(TForm)
    PumpTimer: TIdleTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PumpTimerTimer(Sender: TObject);
  strict private
    FClient : TGuiClient;
  end;

var
  FMain: TFMain;

implementation
uses
  language;

{ TGuiClient }

procedure TGuiClient.OnBuddyAdded(ABuddy: IBuddy);
begin

end;

procedure TGuiClient.OnBuddyRemoved(ABuddy: IBuddy);
begin

end;

procedure TGuiClient.OnBuddyStatusChange(ABuddy: IBuddy);
begin

end;

procedure TGuiClient.OnBuddyAvatarChange(ABuddy: IBuddy);
begin

end;

procedure TGuiClient.OnInstantMessage(ABuddy: IBuddy; AText: String);
begin

end;

procedure TGuiClient.OnGotOwnID;
begin

end;

procedure TGuiClient.OnNeedPump;
begin

end;

{$R *.lfm}

{ TFMain }

procedure TFMain.FormCreate(Sender: TObject);
begin
  FClient := TGuiClient.Create(Self, 'guiclient');
end;

procedure TFMain.FormDestroy(Sender: TObject);
begin
  FClient.Free;
end;

procedure TFMain.PumpTimerTimer(Sender: TObject);
begin
  FClient.Pump;
end;


end.

