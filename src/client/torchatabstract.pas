{ TorChat - Abstract base classes

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
  of the client through abstract base classes. This is a lot of
  Bolierplate but now we have a clear definition of what all the
  objects are allowed to know about each other in one single place.
  This file also serves as some kind of documentation about the
  containment hierarchy and the internal API.
}
unit torchatabstract;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  fpjson,
  networking;

type
  TTorchatStatus = (
    TORCHAT_OFFLINE,
    TORCHAT_CONNECTING,
    TORCHAT_AVAILABLE,
    TORCHAT_AWAY,
    TORCHAT_EXTENDED_AWAY
  );

  TBuddyConnectionState = (
    STATE_DISCONNECTED,
    STATE_TRYING,
    STATE_CONNECTED
  );

  TABuddy = class;
  TABuddyList = class;
  TAHiddenConnection = class;
  TAReceiver = class;
  TAMessage = class;

  TBuddyArray = array of TABuddy;

  TAClient = class(TComponent)
  strict protected
    FMainThread: TThreadID;
    FStandardOut: text;
    FBuddyList: TABuddyList;
    FNetwork: TSocketWrapper;
  public
    property MainThread: TThreadID read FMainThread;
    property StandardOut: Text read FStandardOut;
    procedure ProcessMessages; virtual; abstract;
    procedure SetStatus(AStatus: TTorchatStatus); virtual; abstract;
    procedure OnNotifyGui; virtual; abstract;
    procedure OnBuddyStatusChange(ABuddy: TABuddy); virtual; abstract;
    procedure Enqueue(AMessage: TAMessage); virtual; abstract;
    property BuddyList: TABuddyList read FBuddyList;
    property Network: TSocketWrapper read FNetwork;
  end;

  TABuddyList = class(TComponent)
  strict protected
    FClient: TAClient;
    FOwnID: String;
    FList: TBuddyArray;
  public
    procedure CheckState; virtual; abstract;
    procedure SetOwnID(AID: String); virtual; abstract;
    procedure AddBuddy(ABuddy: TABuddy); virtual; abstract;
    procedure RemoveBuddy(ABuddy: TABuddy); virtual; abstract;
    function FindBuddy(AName: String): TABuddy; virtual; abstract;
    function FindBuddyByCookie(ACookie: String): TABuddy; virtual; abstract;
    procedure DoDisconnectAll; virtual; abstract;
    procedure Lock; virtual; abstract;
    procedure Unlock; virtual; abstract;
    procedure Load; virtual; abstract;
    procedure Save; virtual; abstract;
    function Count: Integer; virtual; abstract;
    property Buddies: TBuddyArray read FList;
    property OwnID: String read FOwnID write SetOwnID;
  end;

  TABuddy = class(TComponent)
  strict protected
    FClient: TAClient;
    FID: String;
    FOwnCookie: String;
    FFriendlyName: String;
    FStatus: TTorchatStatus;
    FLastDisconnect: TDateTime;
    FStateOut: TBuddyConnectionState;
    FStateIn: TBuddyConnectionState;
    FConnIncoming: TAHiddenConnection;
    FConnOutgoing: TAHiddenConnection;
  public
    procedure CheckState; virtual; abstract;
    function AsJsonObject: TJSONObject; virtual; abstract;
    procedure InitFromJsonObect(AObject: TJSONObject); virtual; abstract;
    procedure InitID(AID: String); virtual; abstract;
    procedure SetIncoming(AConn: TAHiddenConnection); virtual; abstract;
    procedure SetOutgoing(AConn: TAHiddenConnection); virtual; abstract;
    procedure SetStatus(AStatus: TTorchatStatus); virtual; abstract;
    procedure OnOutgoingConnection; virtual; abstract;
    procedure OnOutgoingConnectionFail; virtual; abstract;
    procedure OnIncomingConnection; virtual; abstract;
    procedure OnIncomingConnectionFail; virtual; abstract;
    procedure MustSendPong(ACookie: String); virtual; abstract;
    procedure DoDisconnect; virtual; abstract;
    property Client: TAClient read FClient;
    property ID: String read FID;
    property Cookie: String read FOwnCookie;
    property FriendlyName: String read FFriendlyName write FFriendlyName;
    property ConnIncoming: TAHiddenConnection read FConnIncoming write SetIncoming;
    property ConnOutgoing: TAHiddenConnection read FConnOutgoing write SetOutgoing;
    property Status: TTorchatStatus read FStatus write SetStatus;
  end;

  TAHiddenConnection = class
  strict protected
    FTCPStream: TTCPStream;
    FClient: TAClient;
    FBuddy: TABuddy;
    FReceiver: TAReceiver;
    procedure SetBuddy(ABuddy: TABuddy); virtual; abstract;
  public
    procedure Send(AData: String); virtual; abstract;
    procedure SendLine(AEncodedLine: String); virtual; abstract;
    procedure OnTCPFail; virtual; abstract; // called by the receiver
    function IsOutgoing: Boolean; virtual; abstract;
    property Buddy: TABuddy read FBuddy write SetBuddy;
    property Client: TAClient read FClient;
    property Stream: TTCPStream read FTCPStream;
  end;

  { TAMessage represents a protocol message }
  TAMessage = class
  strict protected
    FConnection: TAHiddenConnection;
    FClient: TAClient;
    FBuddy: TABuddy;
  public
    class function GetCommand: String; virtual; abstract;
    constructor Create(AConnection: TAHiddenConnection; AEncodedContent: String); virtual; abstract;
    procedure Parse; virtual; abstract;
    procedure Execute; virtual; abstract;
    procedure Send; virtual; abstract;
    property Client: TAClient read FClient write FClient;
    property Buddy: TABuddy read FBuddy write FBuddy;
  end;

  TAReceiver = class(TThread)
  strict protected
    FClient: TAClient;
    FConnection: TAHiddenConnection;
  public
    property Client: TAClient read FClient;
  end;

implementation

end.

