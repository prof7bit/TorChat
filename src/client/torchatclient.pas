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
  contnrs,
  torchatabstract,
  torprocess,
  networking;

type
  { TTorChatClient implements the abstract TAClient. Together with all its
    contained objects this represents a fully functional TorChat client.
    The GUI (or libpurpletorchat or a command line client) will derive a class
    from TTorChatClient overriding the virtual event methods (the methods that
    start with On (see the definition of TAClient) to hook into the events and
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
  TTorChatClient = class(TAClient)
  strict protected
    FIsDestroying: Boolean;
    FTor: TTor;
    FQueue: TObjectQueue;
    FCSQueue: TRTLCriticalSection;
    FCSConnList: TRTLCriticalSection;
    FTimeStarted: TDateTime;
    FHSNameOK: Boolean;
    FConnInList: IInterfaceList;
    procedure CbNetIn(AStream: TTCPStream; E: Exception);
    procedure PopNextMessage;
    procedure CheckHiddenServiceName;
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;
    procedure Enqueue(AMessage: TAMessage); override;
    procedure ProcessMessages; override;
    procedure SetStatus(AStatus: TTorchatStatus); override;
    procedure RegisterConnection(AConn: TAHiddenConnection); override;
    procedure UnregisterConnection(AConn: TAHiddenConnection); override;
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
//  C : TAHiddenConnection;
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
  FQueue := TObjectQueue.Create;
  FTor := TTor.Create(self);
  FNetwork := TSocketWrapper.Create(Self);
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
  Msg: TAMessage;
  Conn: TAHiddenConnection;
begin
  WriteLn(MilliTime, ' start destroying TorChatClient');
  FIsDestroying := True;

  // disconnect all buddies
  BuddyList.DoDisconnectAll;

  // disconnect all remaining incoming connections
  while FConnInList.Count > 0 do begin
    Conn := TAHiddenConnection(FConnInList.Items[0]);
    Conn.DoClose;
  end;

  FBuddyList.Clear;

  // empty the message queue
  EnterCriticalsection(FCSQueue);
  while FQueue.Count > 0 do begin
    Msg := FQueue.Pop as TAMessage;
    Msg.Free;
  end;
  FQueue.Free;
  LeaveCriticalsection(FCSQueue);

  DoneCriticalsection(FCSQueue);
  DoneCriticalsection(FCSConnList);
  WriteLn(MilliTime, ' start destroying child components');
  inherited Destroy;
  WriteLn(MilliTime, ' done destroying child components');
end;

procedure TTorChatClient.Enqueue(AMessage: TAMessage);
begin
  if FIsDestroying then begin
    // no more messages during destruction, they won't be processed
    // anyways and we also don't want to generate any new timer events.
    // just free the message, throw it away.
    WriteLn('TTorChatClient.Enqueue() not enqueuing message during shutdown');
    AMessage.Free;
  end
  else begin
    EnterCriticalsection(FCSQueue);
    FQueue.Push(AMessage);
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

procedure TTorChatClient.RegisterConnection(AConn: TAHiddenConnection);
begin
  EnterCriticalsection(FCSConnList);
  FConnInList.Add(AConn);
  LeaveCriticalsection(FCSConnList);
  WriteLn(_F(
    'TTorChatClient.RegisterConnection() have now %d incoming connections',
    [FConnInList.Count]));
end;

procedure TTorChatClient.UnregisterConnection(AConn: TAHiddenConnection);
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
  Msg: TAMessage;
begin
  if FQueue.Count > 0 then begin
    EnterCriticalsection(FCSQueue);
    Msg := FQueue.Pop as TAMessage;
    LeaveCriticalsection(FCSQueue);
    try
      Msg.Execute;
    except
      on E: Exception do begin
        WriteLn(E.Message);
      end;
    end;
    Msg.Free;
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

