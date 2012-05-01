{ TorChat - Abstract base classes

  Copyright (C) 2012 Bernd Kreuss <prof7bit@googlemail.com>

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
  Classes, networking;

type
  TABuddy = class;
  TABuddyList = class;
  TAHiddenConnection = class;
  TAReceiver = class;
  TAMessage = class;

  TAClient = class(TComponent)
  strict protected
    FBuddyList: TABuddyList;
  public
    procedure ProcessMessages; virtual; abstract;
    procedure OnNotifyGui; virtual; abstract;
    procedure Enqueue(AMessage: TAMessage); virtual; abstract;
    property BuddyList: TABuddyList read FBuddyList;
  end;

  TABuddyList = class(TComponent)
  strict protected
    FList: array of TABuddy;
  public
    procedure CheckState; virtual; abstract;
    procedure AddBuddy(ABuddy: TABuddy); virtual; abstract;
    procedure RemoveBuddy(ABuddy: TABuddy); virtual; abstract;
    procedure Load; virtual; abstract;
    procedure Save; virtual; abstract;
    function Count: Integer; virtual; abstract;
  end;

  TABuddy = class(TComponent)
  strict protected
    FClient: TAClient;
    FConnIncoming: TAHiddenConnection;
    FConnOutgoing: TAHiddenConnection;
    procedure SetIncoming(AConn: TAHiddenConnection); virtual; abstract;
    procedure SetOutgoing(AConn: TAHiddenConnection); virtual; abstract;
  public
    procedure CheckState; virtual; abstract;
    procedure OnOutgoingConnection; virtual; abstract;
    procedure OnOutgoingConnectionFail; virtual; abstract;
    property ConnIncoming: TAHiddenConnection read FConnIncoming write SetIncoming;
    property ConnOutgoing: TAHiddenConnection read FConnOutgoing write SetOutgoing;
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
    procedure SendLine(ALine: String); virtual; abstract;
    procedure OnConnectionClose; virtual; abstract; // called by the receiver
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
    constructor Create(AConnection: TAHiddenConnection; AContent: String); virtual; abstract;
    procedure Parse; virtual; abstract;
    procedure Execute; virtual; abstract;
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

