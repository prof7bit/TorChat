unit gui_interface;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Controls,
  ComCtrls,
  tc_interface;

type
  { IGuiRosterManager }
  IGuiRosterManager = interface
    procedure Pump;
    function FindOrAddGroup(AGroupName: String): TTreeNode;
    function FindOrAddBuddy(AGroupName, ABuddyID, ABuddyAlias: String): TTreeNode;
    procedure SetBuddyStatus(ID: String; Status: TTorchatStatus);
  end;

implementation

end.

