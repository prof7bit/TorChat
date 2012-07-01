unit roster_manager;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  LCLType,
  LCLIntf,
  Controls,
  ComCtrls,
  tc_interface,
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

  { TNodeExtraData }

  TNodeExtraData = class
    NodeType: TNodeType;
    NodeName: String;
    GroupName: String;    // only for buddies
    HasMessage: Boolean;  // blink because of new message
    TorChatStatus: TTorchatStatus;
    constructor Create;
  end;

  { TNodeHelper }

  TNodeHelper = class helper for TTreeNode
    function ExtraData: TNodeExtraData;
    procedure SetIcon(I: Integer);
  end;

  { TGuiRosterManager }

  TGuiRosterManager = class(TComponent, IGuiRosterManager)
  strict private
    TV: TTreeView;
    FMainHandle: HWND;
    FClient: TGuiClient;
  public
    constructor Create(ATreeView: TTreeView; AMainHandle: HWND); reintroduce;
    function FindOrAddGroup(AGroupName: String): TTreeNode;
    function FindOrAddBuddy(AGroupName, ABuddyID, ABuddyAlias: String): TTreeNode;
    function Find(AType: TNodeType; AName: String): TTreeNode;
    procedure SetBuddyStatus(ID: String; Status: TTorchatStatus);
    procedure OnDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure OnAddNode(Sender: TObject; Node: TTreeNode);
    procedure OnDeleteNode(Sender: TObject; Node: TTreeNode);
    procedure Pump;
    procedure OnNeedPump;
  end;

implementation

{ TNodeExtraData }

constructor TNodeExtraData.Create;
begin
  TorChatStatus := TORCHAT_OFFLINE;
  HasMessage := False;
end;

{ TNodeHelper }

function TNodeHelper.ExtraData: TNodeExtraData;
begin
  Result := TNodeExtraData(Self.Data);
end;

procedure TNodeHelper.SetIcon(I: Integer);
begin
  ImageIndex := I;
  SelectedIndex := I;
end;

{ TGuiRosterManager }

constructor TGuiRosterManager.Create(ATreeView: TTreeView; AMainHandle: HWND);
begin
  Inherited Create(ATreeView);
  TV := ATreeView;
  FMainHandle := AMainHandle;
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
      SetIcon(IMG_OFFLINE);
      with ExtraData do begin
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

procedure TGuiRosterManager.SetBuddyStatus(ID: String; Status: TTorchatStatus);
var
  N: TTreeNode;
begin
  if csDestroying in ComponentState then
    exit;
  N := Find(NODE_BUDDY, ID);
  if Assigned(N) then begin
    N.ExtraData.TorChatStatus := Status;
    case Status of
      TORCHAT_OFFLINE     : N.SetIcon(IMG_OFFLINE);
      TORCHAT_AVAILABLE   : N.SetIcon(IMG_AVAILBLE);
      TORCHAT_AWAY        : N.SetIcon(IMG_AWAY);
      TORCHAT_XA          : N.SetIcon(IMG_XA);
      TORCHAT_CONNECTING  : N.SetIcon(IMG_CONNECTING);
    end;
  end;
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

procedure TGuiRosterManager.OnNeedPump;
begin
  PostMessage(FMainHandle, WM_NEEDPUMP, 0, 0);
end;

end.

