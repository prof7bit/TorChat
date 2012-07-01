unit gui_interface;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Controls,
  ComCtrls,
  LMessages,
  tc_interface;

const
  WM_NEEDPUMP = WM_USER + 1;

type
  { IGuiRosterManager }
  IGuiRosterManager = interface
    procedure Pump;
    procedure OnNeedPump;
    function FindOrAddGroup(AGroupName: String): TTreeNode;
    function FindOrAddBuddy(AGroupName, ABuddyID, ABuddyAlias: String): TTreeNode;
    procedure SetBuddyStatus(ID: String; Status: TTorchatStatus);
  end;

implementation

end.

