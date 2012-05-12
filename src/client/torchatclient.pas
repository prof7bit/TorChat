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
    FIsDestriying: Boolean;
    FTor: TTor;
    FQueue: TObjectQueue;
    CS: TRTLCriticalSection;
    FTimeStarted: TDateTime;
    FHSNameOK: Boolean;
    procedure CbNetIn(AStream: TTCPStream; E: Exception);
    procedure PopNextMessage;
    procedure CheckHiddenServiceName;
  public
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;
    procedure Enqueue(AMessage: TAMessage); override;
    procedure ProcessMessages; override;
    procedure SetStatus(AStatus: TTorchatStatus); override;
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
  FIsDestriying := False;
  Inherited Create(AOwner);
  FMainThread := ThreadID;
  Randomize;
  self.FStandardOut := Output;
  InitCriticalSection(CS);
  FHSNameOK := False;
  FTimeStarted := 0; // we will initialize it on first ProcessMessages() call
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
begin
  WriteLn(MilliTime, ' start destroying TorChatClient');
  FIsDestriying := True;
  BuddyList.DoDisconnectAll;
  EnterCriticalsection(CS);
  while FQueue.Count > 0 do begin
    Msg := FQueue.Pop as TAMessage;
    Msg.Free;
  end;
  FQueue.Free;
  LeaveCriticalsection(CS);
  DoneCriticalsection(CS);
  WriteLn(MilliTime, ' start destroying child components');
  inherited Destroy;
end;

procedure TTorChatClient.Enqueue(AMessage: TAMessage);
begin
  if FIsDestriying then begin
    // no more messages during destruction, they won't be processed
    // anyways and we also don't want to generate any new timer events.
    // just free the message, throw it away.
    WriteLn('TTorChatClient.Enqueue() not enqueuing message during shutdown');
    AMessage.Free;
  end
  else begin
    EnterCriticalsection(CS);
    FQueue.Push(AMessage);
    LeaveCriticalsection(CS);
    OnNotifyGui;
  end;
end;

procedure TTorChatClient.ProcessMessages;
begin
  if FIsDestriying then exit;
  if FTimeStarted = 0 then FTimeStarted := Now;
  CheckHiddenServiceName;
  PopNextMessage;
  BuddyList.CheckState;
end;

procedure TTorChatClient.SetStatus(AStatus: TTorchatStatus);
begin
  writeln('setting own status to ', AStatus);
end;

procedure TTorChatClient.CbNetIn(AStream: TTCPStream; E: Exception);
var
  C : THiddenConnection;
begin
  Output := self.StandardOut; // make writeln redirect work in this thread
  writeln('TTorChatClient.CbNetIn()');
  C := THiddenConnection.Create(self, AStream);
  Ignore(C);
  Ignore(E);
end;

procedure TTorChatClient.PopNextMessage;
var
  Msg: TAMessage;
begin
  if FQueue.Count > 0 then begin
    EnterCriticalsection(CS);
    Msg := FQueue.Pop as TAMessage;
    LeaveCriticalsection(CS);
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
        writeln('found own name: ' + HSName);
        BuddyList.OwnID := HSName;
        FHSNameOK := True;
      end;
    end;
  end;
end;

end.

