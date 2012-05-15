{ TorChat - TTorChatClient, this component is implememting the client

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
unit torchatclient;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  interfaces,
  torprocess,
  networking;

type
  { TTorChatClient implements the abstract IClient. Together with all its
    contained objects this represents a fully functional TorChat client.
    The GUI (or libpurpletorchat or a command line client) will derive a class
    from TTorChatClient overriding the virtual event methods (the methods that
    start with On (see the definition of IClient) to hook into the events and
    then create an instance of it.
    The GUI also must call the TorChatClient.ProcessMessages method in regular
    intervals (1 second or so) from within the GUI-thread. Aditionally whenever
    there is an incoming chat message, status change or other event then
    TorChatClient will fire the OnNotifyGui event and as a response the GUI should
    schedule an additional call to ProcessMessages a soon as possible. All other
    event method calls will then always originate from the thread that is
    calling TorChatClient.ProcessMessages. This method will process one queued
    message per call, it will never block and if there are no messages and
    nothing else to do it will just return.}
  TTorChatClient = class(TComponent, IClient)
  strict protected
    FMainThread: TThreadID;
    FBuddyList: IBuddyList;
    FNetwork: TSocketWrapper;
    FIsDestroying: Boolean;
    FTor: TTor;
    FQueue: IInterfaceList;
    FCSQueue: TRTLCriticalSection;
    FCSConnList: TRTLCriticalSection;
    FTimeStarted: TDateTime;
    FHSNameOK: Boolean;
    FConnInList: IInterfaceList;
    procedure CbNetIn(AStream: TTCPStream; E: Exception);
    procedure PopNextMessage;
    procedure CheckHiddenServiceName;
  public
    procedure OnNotifyGui; virtual; abstract;
    procedure OnBuddyStatusChange(ABuddy: IBuddy); virtual; abstract;
    procedure OnBuddyAdded(ABuddy: IBuddy); virtual; abstract;
    procedure OnBuddyRemoved(ABuddy: IBuddy); virtual; abstract;
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;
    function MainThread: TThreadID; virtual;
    function BuddyList: IBuddyList; virtual;
    function Network: TSocketWrapper; virtual;
    procedure Enqueue(AMessage: IMessage); virtual;
    procedure ProcessMessages; virtual;
    procedure SetStatus(AStatus: TTorchatStatus); virtual;
    procedure RegisterConnection(AConn: IHiddenConnection); virtual;
    procedure UnregisterConnection(AConn: IHiddenConnection); virtual;
  end;


implementation
uses
  buddylist,
  clientconfig,
  miscfunc,
  connection;

{ TTorChatClient }

constructor TTorChatClient.Create(AOwner: TComponent);
//var
//  C : IHiddenConnection;
begin
  FIsDestroying := False;
  Inherited Create(AOwner);
  FMainThread := ThreadID;
  Randomize;
  InitCriticalSection(FCSQueue);
  InitCriticalSection(FCSConnList);
  FHSNameOK := False;
  FTimeStarted := 0; // we will initialize it on first ProcessMessages() call
  FConnInList := TInterfaceList.Create;
  FQueue := TInterfaceList.Create;
  FTor := TTor.Create(self);
  FNetwork := TSocketWrapper.Create(self);
  FBuddyList := TBuddyList.Create(self);
  with FNetwork do begin
    SocksProxyAddress := ConfGetTorHost;
    SocksProxyPort := ConfGetTorPort;
    IncomingCallback := @CbNetIn;
    Bind(ConfGetListenPort);
  end;
end;

destructor TTorChatClient.Destroy;
var
  Conn: IHiddenConnection;
begin
  WriteLn(MilliTime, ' start destroying TorChatClient');
  FIsDestroying := True;

  // disconnect all buddies
  BuddyList.DoDisconnectAll;

  // disconnect all remaining incoming connections
  while FConnInList.Count > 0 do begin
    Conn := IHiddenConnection(FConnInList.Items[0]);
    Conn.DoClose;
  end;

  FBuddyList.Clear;
  FQueue.Clear;

  DoneCriticalsection(FCSQueue);
  DoneCriticalsection(FCSConnList);

  WriteLn(MilliTime, ' start destroying child components');
  //FTor.Free;
  //FNetwork.Free;
  inherited Destroy;
  WriteLn(MilliTime, ' done destroying child components');
end;

function TTorChatClient.MainThread: TThreadID;
begin
  Result := FMainThread;
end;

function TTorChatClient.BuddyList: IBuddyList;
begin
  Result := FBuddyList;
end;

function TTorChatClient.Network: TSocketWrapper;
begin
  Result := FNetwork;
end;

procedure TTorChatClient.Enqueue(AMessage: IMessage);
begin
  if FIsDestroying then begin
    // no more messages during destruction, they won't be processed
    // anyways and we also don't want to generate any new timer events.
    // just free the message, throw it away.
    WriteLn('TTorChatClient.Enqueue() not enqueuing message during shutdown');
  end
  else begin
    EnterCriticalsection(FCSQueue);
    FQueue.Add(AMessage);
    LeaveCriticalsection(FCSQueue);
    OnNotifyGui;
  end;
end;

procedure TTorChatClient.ProcessMessages;
begin
  if FIsDestroying then exit;
  if FTimeStarted = 0 then FTimeStarted := Now;
  CheckHiddenServiceName;
  PopNextMessage;
  BuddyList.CheckState;
end;

procedure TTorChatClient.SetStatus(AStatus: TTorchatStatus);
begin
  writeln('TTorChatClient.SetStatus(', AStatus, ')');
end;

procedure TTorChatClient.RegisterConnection(AConn: IHiddenConnection);
begin
  EnterCriticalsection(FCSConnList);
  FConnInList.Add(AConn);
  LeaveCriticalsection(FCSConnList);
  WriteLn(_F(
    'TTorChatClient.RegisterConnection() have now %d incoming connections',
    [FConnInList.Count]));
end;

procedure TTorChatClient.UnregisterConnection(AConn: IHiddenConnection);
begin
  EnterCriticalsection(FCSConnList);
  // only incoming connections are in this list
  if FConnInList.IndexOf(AConn) <> -1 then begin
    FConnInList.Remove(AConn);
    LeaveCriticalsection(FCSConnList);
    WriteLn(_F(
      'TTorChatClient.UnregisterConnection() removed %s, %d incoming connections left',
      [AConn.DebugInfo, FConnInList.Count]));
  end
  else
    LeaveCriticalsection(FCSConnList);
end;

procedure TTorChatClient.CbNetIn(AStream: TTCPStream; E: Exception);
var
  C : THiddenConnection;
begin
  writeln('TTorChatClient.CbNetIn()');
  Ignore(E);
  if FIsDestroying then
    AStream.Free
  else begin
    C := THiddenConnection.Create(self, AStream, nil);
    RegisterConnection(C);
  end;
end;

procedure TTorChatClient.PopNextMessage;
var
  Msg: IMessage;
begin
  if FQueue.Count > 0 then begin
    EnterCriticalsection(FCSQueue);
    Msg := IMessage(FQueue.First);
    FQueue.Remove(Msg);
    LeaveCriticalsection(FCSQueue);
    try
      Msg.Execute;
    except
      on E: Exception do begin
        WriteLn(E.Message);
      end;
    end;
  end;
end;

procedure TTorChatClient.CheckHiddenServiceName;
var
  HSName: String;
begin
  if not FHSNameOK then begin;
    if SecondsSince(FTimeStarted) < SECONDS_WAIT_FOR_HOSTNAME_FILE then begin
      HSName := ConfGetHiddenServiceName;
      if HSName <> '' then begin
        writeln('TTorChatClient.CheckHiddenServiceName() found: ' + HSName);
        BuddyList.SetOwnID(HSName);
        FHSNameOK := True;
      end
      else
        writeln('TTorChatClient.CheckHiddenServiceName() not found');
    end;
  end;
end;

end.

