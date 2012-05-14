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
    TORCHAT_XA
  );

  IBuddy = interface;
  IBuddyList = interface;
  TAHiddenConnection = interface;
  TAReceiver = class;
  TAMessage = class;

  TABuddyEnumerator = class
    function GetCurrent: IBuddy; virtual; abstract;
    function MoveNext: Boolean; virtual; abstract;
    property Current: IBuddy read GetCurrent;
  end;

  TAClient = class(TComponent)
  strict protected
    FMainThread: TThreadID;
    FBuddyList: IBuddyList;
    FNetwork: TSocketWrapper;
  public
    procedure ProcessMessages; virtual; abstract;
    procedure OnNotifyGui; virtual; abstract;
    procedure OnBuddyStatusChange(ABuddy: IBuddy); virtual; abstract;
    procedure OnBuddyAdded(ABuddy: IBuddy); virtual; abstract;
    procedure OnBuddyRemoved(ABuddy: IBuddy); virtual; abstract;
    procedure SetStatus(AStatus: TTorchatStatus); virtual; abstract;
    procedure RegisterConnection(AConn: TAHiddenConnection); virtual; abstract;
    procedure UnregisterConnection(AConn: TAHiddenConnection); virtual; abstract;
    property MainThread: TThreadID read FMainThread;
    procedure Enqueue(AMessage: TAMessage); virtual; abstract;
    property BuddyList: IBuddyList read FBuddyList;
    property Network: TSocketWrapper read FNetwork;
  end;

  IBuddyListTemp = interface(IInterfaceList)
    procedure CheckState;
    procedure AddBuddy(ABuddy: IBuddy);
    procedure RemoveBuddy(ABuddy: IBuddy);
    function FindBuddy(AName: String): IBuddy;
    function FindBuddyByCookie(ACookie: String): IBuddy;
    procedure DoDisconnectAll;
    function GetEnumerator: TABuddyEnumerator;
  end;

  IBuddyList = interface(IBuddyListTemp)
    procedure Load;
    procedure Save;
    function OwnID: String;
    procedure SetOwnID(AID: String);
  end;

  IBuddy = interface
    procedure CheckState;
    function AsJsonObject: TJSONObject;
    procedure InitFromJsonObect(AObject: TJSONObject);
    procedure InitID(AID: String);
    procedure OnOutgoingConnection;
    procedure OnOutgoingConnectionFail;
    procedure OnIncomingConnection;
    procedure OnIncomingConnectionFail;
    procedure MustSendPong(ACookie: String);
    procedure DoDisconnect;
    function Client: TAClient;
    function ID: String;
    function Cookie: String;
    function FriendlyName: String;
    function ConnIncoming: TAHiddenConnection;
    function ConnOutgoing: TAHiddenConnection;
    function Status: TTorchatStatus;
    procedure SetFriendlyName(AName: String);
    procedure SetIncoming(AConn: TAHiddenConnection);
    procedure SetOutgoing(AConn: TAHiddenConnection);
    procedure SetStatus(AStatus: TTorchatStatus);
  end;

  TAHiddenConnection = interface
    procedure SetBuddy(ABuddy: IBuddy);
    procedure Send(AData: String);
    procedure SendLine(AEncodedLine: String);
    procedure OnTCPFail;// called by the receiver
    procedure DoClose;
    function IsOutgoing: Boolean;
    function DebugInfo: String;
    function Buddy: IBuddy;
    function Client: TAClient;
    function Stream: TTCPStream;
  end;

  { TAMessage represents a protocol message }
  TAMessage = class
  strict protected
    FConnection: TAHiddenConnection;
    FClient: TAClient;
    FBuddy: IBuddy;
  public
    class function GetCommand: String; virtual; abstract;
    constructor Create(AConnection: TAHiddenConnection; AEncodedContent: String); virtual; abstract;
    procedure Parse; virtual; abstract;
    procedure Execute; virtual; abstract;
    procedure Send; virtual; abstract;
    property Client: TAClient read FClient write FClient;
    property Buddy: IBuddy read FBuddy write FBuddy;
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

