{ This unit defines the interfaces between the various classes
  of the client through abstract base classes. They will all be
  implemented in separate units and all they need to know about
  each other is clearly defined below.
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

  TAClient = class(TObject)
  strict protected
    FBuddyList: TABuddyList;
  public
    property BuddyList: TABuddyList read FBuddyList;
  end;

  TABuddyList = class(TObject)
  strict protected
    FList: array of TABuddy;
  public
    procedure AddBuddy(ABuddy: TABuddy); virtual abstract;
    procedure RemoveBuddy(ABuddy: TABuddy); virtual abstract;
    procedure Save; virtual abstract;
    function Count: Integer; virtual abstract;
  end;

  TABuddy = class(TObject)
  strict protected
    FClient: TAClient;
    FConnIncoming: TAHiddenConnection;
    FConnOutgoing: TAHiddenConnection;
    procedure SetIncoming(AConn: TAHiddenConnection); virtual abstract;
    procedure SetOutgoing(AConn: TAHiddenConnection); virtual abstract;
  public
    property ConnIncoming: TAHiddenConnection read FConnIncoming write SetIncoming;
    property ConnOutgoing: TAHiddenConnection read FConnOutgoing write SetOutgoing;
  end;

  TAHiddenConnection = class(TConnection)
  strict protected
    FBuddy: TABuddy;
    procedure SetBuddy(ABuddy: TABuddy); virtual abstract;
  public
    property Buddy: TABuddy read FBuddy write SetBuddy;
  end;

implementation

end.

