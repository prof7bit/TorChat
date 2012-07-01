unit roster_manager;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Controls,
  ComCtrls,
  gui_interface,
  client_object;

const
  // index in the ImageList
  IMG_OFFLINE = 0;
  IMG_CONNECTING = 1;
  IMG_AVAILBLE = 2;
  IMG_AWAY = 3;
  IMG_XA = 4;
  IMG_MESSAGE = 5;

type
  TNodeType = (
    NODE_GROUP,
    NODE_BUDDY
  );

  TNodeData = class
    NodeType: TNodeType;
    NodeName: String;
    GroupName: String; // only for buddies
  end;

  { TNodeHelper }

  TNodeHelper = class helper for TTreeNode
    function GetData: TNodeData;
  end;

  { TGuiRosterManager }

  TGuiRosterManager = class(TComponent, IGuiRosterManager)
  strict private
    TV: TTreeView;
    FClient: TGuiClient;
  public
    constructor Create(ATreeView: TTreeView); reintroduce;
    function FindOrAddGroup(AGroupName: String): TTreeNode;
    function FindOrAddBuddy(AGroupName, ABuddyID, ABuddyAlias: String): TTreeNode;
    function Find(AType: TNodeType; AName: String): TTreeNode;
    procedure OnDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure OnDeleteNode(Sender: TObject; Node: TTreeNode);
    procedure Pump;
  end;

implementation

{ TNodeHelper }

function TNodeHelper.GetData: TNodeData;
begin
  Result := TNodeData(Self.Data);
end;

{ TGuiRosterManager }

constructor TGuiRosterManager.Create(ATreeView: TTreeView);
begin
  Inherited Create(ATreeView);
  TV := ATreeView;
  FClient := TGuiClient.Create(Self, Self, 'zzzzz');
  TV.OnDeletion := @OnDeleteNode;
  TV.OnDragOver := @OnDragOver;
end;

function TGuiRosterManager.FindOrAddGroup(AGroupName: String): TTreeNode;
var
  NodeData: TNodeData;
begin
  Result := Find(NODE_GROUP, AGroupName);
  if not Assigned(Result) then begin
    Result := TV.Items.AddChild(nil, AGroupName);
    NodeData := TNodeData.Create;
    NodeData.NodeType := NODE_GROUP;
    NodeData.NodeName := AGroupName;
    Result.Data := NodeData;
  end;
end;

function TGuiRosterManager.FindOrAddBuddy(AGroupName, ABuddyID, ABuddyAlias: String): TTreeNode;
var
  Group: TTreeNode;
  NodeData: TNodeData;
begin
  Group := FindOrAddGroup(AGroupName);
  Result := Find(NODE_BUDDY, ABuddyID);
  if not Assigned(Result) then begin
    Result := TV.Items.AddChild(Group, ABuddyID);
    NodeData := TNodeData.Create;
    NodeData.NodeType := NODE_BUDDY;
    NodeData.NodeName := ABuddyID;
    NodeData.GroupName := AGroupName;
    Result.Data := NodeData;
    Result.ImageIndex := IMG_OFFLINE;
    Result.SelectedIndex := IMG_OFFLINE;
  end
  else
    Result.MoveTo(Group, naAddChild);
end;

function TGuiRosterManager.Find(AType: TNodeType; AName: String): TTreeNode;
var
  Data: TNodeData;
begin
  for Result in TV.Items do begin
    Data := Result.GetData;
    if Data.NodeType = AType then
      if Data.NodeName = AName then
        exit;
  end;
  Result := nil;
end;

procedure TGuiRosterManager.OnDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := True;
end;

procedure TGuiRosterManager.OnDeleteNode(Sender: TObject; Node: TTreeNode);
begin
  Node.GetData.Free;
end;

procedure TGuiRosterManager.Pump;
begin
  FClient.Pump;
end;

end.

