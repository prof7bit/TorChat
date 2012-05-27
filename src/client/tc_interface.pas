{ TorChat - interfaces and other common definitions for TorChat classes

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

{ This unit defines the interfaces between the various classes
  of the TorChat client.
}
unit tc_interface;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  fpjson,
  tc_sock;

type
  TTorchatStatus = (
    TORCHAT_OFFLINE,
    TORCHAT_CONNECTING,
    TORCHAT_AVAILABLE,
    TORCHAT_AWAY,
    TORCHAT_XA
  );

  TMethodOfObject = procedure of object;

  IBuddy = interface;
  IRoster = interface;
  ITempList = interface;
  IHiddenConnection = interface;
  IMessage = interface;
  IProtocolMessage = interface;
  IMsgQueue = interface;
  TAReceiver = class;

  TABuddyEnumerator = class
    function GetCurrent: IBuddy; virtual; abstract;
    function MoveNext: Boolean; virtual; abstract;
    property Current: IBuddy read GetCurrent;
  end;

  IClientConfig = interface
    function DataDir: String;
    function PathTorExe: String;
    function ListenPort: DWord;
    function TorHostName: String;
    function TorPort: DWord;
  end;

  IClient = interface
    procedure Pump;       // must be called from the GUI thread.
    procedure OnNeedPump; // ask the GUI to schedule a Pump() asap.
    procedure OnGotOwnID;
    procedure OnBuddyStatusChange(ABuddy: IBuddy);
    procedure OnBuddyAdded(ABuddy: IBuddy);
    procedure OnBuddyRemoved(ABuddy: IBuddy);
    procedure SetStatus(AStatus: TTorchatStatus);
    procedure RegisterAnonConnection(AConn: IHiddenConnection);
    procedure UnregisterAnonConnection(AConn: IHiddenConnection);
    function  MainThread: TThreadID;
    function Roster: IRoster;
    function TempList: ITempList;
    function Queue: IMsgQueue;
    function Network: TSocketWrapper;
    function Config: IClientConfig;
    function IsDestroying: Boolean;
    function ProfileName: String;
    function TorHost: String;
    function TorPort: DWord;
    function HSNameOK: Boolean;
    function Status: TTorchatStatus;
  end;

  { a temporary list of buddies}
  ITempList = interface(IInterfaceList)
    procedure CheckState;
    procedure AddBuddy(ABuddy: IBuddy);
    procedure RemoveBuddy(ABuddy: IBuddy);
    function ByID(ABuddyID: String): IBuddy;
    function ByCookie(ACookie: String): IBuddy;
    procedure DoDisconnectAll;
    function GetEnumerator: TABuddyEnumerator;
  end;

  { the buddy list }
  IRoster = interface(ITempList)
    procedure Load;
    procedure Save;
    procedure AddBuddyNoCallback(ABuddy: IBuddy);
    procedure RemoveBuddyNoCallback(ABuddy: IBuddy);
    function OwnID: String;
    function GroupName: String;
    procedure SetOwnID(AID: String);
  end;

  IBuddy = interface
    procedure CheckState;
    function AsJsonObject: TJSONObject;
    procedure InitFromJsonObect(AObject: TJSONObject);
    function  InitID(AID: String): Boolean;
    procedure OnOutgoingConnection;
    procedure OnOutgoingConnectionFail;
    procedure OnIncomingConnection;
    procedure OnIncomingConnectionFail;
    procedure MustSendPong(ACookie: String);
    procedure ForgetLastPing;
    procedure ResetConnectInterval;
    procedure ResetTimeout;
    procedure DoDisconnect;
    procedure RemoveYourself;
    function Client: IClient;
    function ID: String;
    function Cookie: String;
    function FriendlyName: String;
    function ConnIncoming: IHiddenConnection;
    function ConnOutgoing: IHiddenConnection;
    function IsFullyConnected: Boolean;
    function Status: TTorchatStatus;
    procedure SetFriendlyName(AName: String);
    procedure SetIncoming(AConn: IHiddenConnection);
    procedure SetOutgoing(AConn: IHiddenConnection);
    procedure SetStatus(AStatus: TTorchatStatus);
    procedure SendPong;
    procedure SendAddMe;
    procedure SendStatus;
  end;

  IHiddenConnection = interface
    procedure SetBuddy(ABuddy: IBuddy);
    procedure SetPingBuddyID(AID: String);
    procedure Send(AData: String);
    procedure SendLine(AEncodedLine: String);
    procedure OnTCPFail; // called by the receiver
    function IsOutgoing: Boolean;
    function DebugInfo: String;
    function Buddy: IBuddy;
    function Client: IClient;
    function Stream: TTCPStream;
    function TimeCreated: TDateTime;
    function PingBuddyID: String;
    procedure Disconnect;
  end;

  IMessage = interface
    procedure Execute;
  end;

  IProtocolMessage = interface(IMessage)
    procedure Parse;
    procedure Send;
  end;

  IMsgQueue = interface
    procedure Put(Msg: IMessage);
    procedure PumpNext;
    procedure Clear;
  end;

  TAReceiver = class(TThread)
  strict protected
    FClient: IClient;
    FConnection: IHiddenConnection;
  public
    property Client: IClient read FClient;
  end;

  operator in(ABuddy: IBuddy; AList: ITempList): Boolean;

implementation

operator in(ABuddy: IBuddy; AList: ITempList): Boolean;
begin
  Result := (AList.IndexOf(ABuddy) > -1);
end;

end.

