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

  TNodeExtraData = class
    NodeType: TNodeType;
    NodeName: String;
    GroupName: String; // only for buddies
  end;

  { TNodeHelper }

  TNodeHelper = class helper for TTreeNode
    function ExtraData: TNodeExtraData;
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
    procedure OnAddNode(Sender: TObject; Node: TTreeNode);
    procedure OnDeleteNode(Sender: TObject; Node: TTreeNode);
    procedure Pump;
  end;

implementation

{ TNodeHelper }

function TNodeHelper.ExtraData: TNodeExtraData;
begin
  Result := TNodeExtraData(Self.Data);
end;

{ TGuiRosterManager }

constructor TGuiRosterManager.Create(ATreeView: TTreeView);
begin
  Inherited Create(ATreeView);
  TV := ATreeView;
  FClient := TGuiClient.Create(Self, Self, 'zzzzz');
  TV.OnAddition := @OnAddNode;
  TV.OnDeletion := @OnDeleteNode;
  TV.OnDragOver := @OnDragOver;
end;

function TGuiRosterManager.FindOrAddGroup(AGroupName: String): TTreeNode;
begin
  Result := Find(NODE_GROUP, AGroupName);
  if not Assigned(Result) then begin
    Result := TV.Items.AddChild(nil, AGroupName);
    with Result.ExtraData do begin
      NodeType := NODE_GROUP;
      NodeName := AGroupName;
    end;
  end;
end;

function TGuiRosterManager.FindOrAddBuddy(AGroupName, ABuddyID, ABuddyAlias: String): TTreeNode;
var
  Group: TTreeNode;
begin
  Group := FindOrAddGroup(AGroupName);
  Result := Find(NODE_BUDDY, ABuddyID);
  if not Assigned(Result) then begin
    Result := TV.Items.AddChild(Group, ABuddyID);
    with Result do begin
      ImageIndex := IMG_OFFLINE;
      SelectedIndex := IMG_OFFLINE;
      with ExtraData do begin
        NodeType := NODE_BUDDY;
        NodeType := NODE_BUDDY;
        NodeName := ABuddyID;
        GroupName := AGroupName;
      end;
    end;
  end
  else
    Result.MoveTo(Group, naAddChild);
end;

function TGuiRosterManager.Find(AType: TNodeType; AName: String): TTreeNode;
var
  Data: TNodeExtraData;
begin
  for Result in TV.Items do begin
    Data := Result.ExtraData;
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

procedure TGuiRosterManager.OnAddNode(Sender: TObject; Node: TTreeNode);
begin
  Node.Data := TNodeExtraData.Create;
end;

procedure TGuiRosterManager.OnDeleteNode(Sender: TObject; Node: TTreeNode);
begin
  Node.ExtraData.Free;
end;

procedure TGuiRosterManager.Pump;
begin
  FClient.Pump;
end;

end.

