unit client_object;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  tc_client,
  tc_interface,
  gui_interface;

type
  { TGuiClient }
  TGuiClient = class(TTorChatClient)
  public
    FRosterManager: IGuiRosterManager;
    constructor Create(AOwner: TComponent; ARosterManager: IGuiRosterManager; AProfileName: String); reintroduce;
    procedure OnBuddyAdded(ABuddy: IBuddy); override;
    procedure OnBuddyRemoved(ABuddy: IBuddy); override;
    procedure OnBuddyStatusChange(ABuddy: IBuddy); override;
    procedure OnBuddyAvatarChange(ABuddy: IBuddy); override;
    procedure OnBuddyAliasChange(ABuddy: IBuddy); override;
    procedure OnInstantMessage(ABuddy: IBuddy; AText: String); override;
    procedure OnIncomingFileTransfer(ABuddy: IBuddy; AID: String; AFileName: String; AFileSize: UInt64; ABlockSize: Integer); override;
    procedure OnGotOwnID; override;
    procedure OnNeedPump; override;
  end;

implementation

{ TGuiClient }

constructor TGuiClient.Create(AOwner: TComponent; ARosterManager: IGuiRosterManager; AProfileName: String);
begin
  FRosterManager := ARosterManager;
  inherited Create(AOwner, AProfileName);
end;

procedure TGuiClient.OnBuddyAdded(ABuddy: IBuddy);
begin

end;

procedure TGuiClient.OnBuddyRemoved(ABuddy: IBuddy);
begin

end;

procedure TGuiClient.OnBuddyStatusChange(ABuddy: IBuddy);
begin
  FRosterManager.SetBuddyStatus(ABuddy.ID, ABuddy.Status);
end;

procedure TGuiClient.OnBuddyAvatarChange(ABuddy: IBuddy);
begin

end;

procedure TGuiClient.OnBuddyAliasChange(ABuddy: IBuddy);
begin

end;

procedure TGuiClient.OnInstantMessage(ABuddy: IBuddy; AText: String);
begin

end;

procedure TGuiClient.OnIncomingFileTransfer(ABuddy: IBuddy; AID: String; AFileName: String; AFileSize: UInt64; ABlockSize: Integer);
begin

end;

procedure TGuiClient.OnGotOwnID;
var
  Buddy: IBuddy;
  GroupName: String;
begin
  GroupName := Self.Roster.GroupName;
  for Buddy in Roster do begin
    FRosterManager.FindOrAddBuddy(GroupName, Buddy.ID, Buddy.LocalAlias);
  end;
end;

procedure TGuiClient.OnNeedPump;
begin
  FRosterManager.OnNeedPump;
end;

end.

