unit gui_interface;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Controls,
  ComCtrls;

type
  { IGuiRosterManager }
  IGuiRosterManager = interface
    procedure Pump;
    function FindOrAddGroup(AGroupName: String): TTreeNode;
    function FindOrAddBuddy(AGroupName, ABuddyID, ABuddyAlias: String): TTreeNode;
  end;

implementation

end.

