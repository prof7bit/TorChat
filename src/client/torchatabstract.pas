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

  TAClient = class(TComponent)
  strict protected
    FBuddyList: TABuddyList;
  public
    property BuddyList: TABuddyList read FBuddyList;
  end;

  TABuddyList = class(TComponent)
  strict protected
    FList: array of TABuddy;
  public
    procedure AddBuddy(ABuddy: TABuddy); virtual; abstract;
    procedure RemoveBuddy(ABuddy: TABuddy); virtual; abstract;
    procedure Save; virtual; abstract;
    function Count: Integer; virtual; abstract;
  end;

  TABuddy = class
  strict protected
    FClient: TAClient;
    FConnIncoming: TAHiddenConnection;
    FConnOutgoing: TAHiddenConnection;
    procedure SetIncoming(AConn: TAHiddenConnection); virtual; abstract;
    procedure SetOutgoing(AConn: TAHiddenConnection); virtual; abstract;
  public
    property ConnIncoming: TAHiddenConnection read FConnIncoming write SetIncoming;
    property ConnOutgoing: TAHiddenConnection read FConnOutgoing write SetOutgoing;
  end;

  TAHiddenConnection = class(TConnection)
  strict protected
    FBuddy: TABuddy;
    FReceiver: TAReceiver;
    procedure SetBuddy(ABuddy: TABuddy); virtual; abstract;
  public
    procedure Send(AData: String); virtual; abstract;
    procedure SendLine(ALine: String); virtual; abstract;
    procedure OnConnectionClose; virtual; abstract; // called by the receiver
    property Buddy: TABuddy read FBuddy write SetBuddy;
  end;

  { TAMessage represents a protocol message }
  TAMessage = class
  strict protected
    FConnection: TAHiddenConnection;
  public
    class function GetCommand: String; virtual; abstract;
    constructor Create(AConnection: TAHiddenConnection; AContent: String); virtual; abstract;
    function Parse: Boolean; virtual; abstract;  // True = success
    function Execute: Boolean; virtual; abstract;// True = success
  end;

  TAReceiver = class(TThread)
  strict protected
    FConnection: TAHiddenConnection;
  end;

implementation

end.

