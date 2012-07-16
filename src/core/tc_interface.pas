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
{$modeswitch nestedprocvars}
{$modeswitch advancedrecords}

interface

uses
  Classes,
  fpjson,
  lEvents,
  tc_generic;

type
  TTorchatStatus = (
    TORCHAT_OFFLINE,
    TORCHAT_CONNECTING,
    TORCHAT_AVAILABLE,
    TORCHAT_AWAY,
    TORCHAT_XA
  );

  P24Pixel = ^T24Pixel;
  T24Pixel = packed record
    Red: Byte;
    Green: Byte;
    Blue: Byte;
  end;

  TMethodOfObject = procedure of object;
  TStringArray = array of String;
  TFindFunc = function(O: TObject): Boolean is nested;

  ICookieEntry = interface;
  IBuddy = interface;
  IRoster = interface;
  ITempList = interface;
  IHiddenConnection = interface;
  IMessage = interface;
  IProtocolMessage = interface;
  IMsgQueue = interface;
  IFileTransfer = interface;

  TBuddyEnumerator = specialize TGenericInterfaceEnumerator<IBuddy>;
  TCookieEnumerator = specialize TGenericInterfaceEnumerator<ICookieEntry>;

  { Owned by ICookieList, implemented in tc_cookie_list }
  ICookieEntry = interface
    function ID: String;
    function Cookie: String;
  end;

  { The client maintains a list of incoming ping cookies from
    all incoming connections, so it can detect when one ID is
    trying to send multiple different cookies.
    Owned by the client, implemented in tc_cookie_list }
  ICookieList = interface(IInterfaceList)
    function Add(ABuddyID, ACookie: String): Boolean; overload;
    procedure Remove(ACookie: String); overload;
    function CountByID(ABuddyID: String): Integer;
    function GetEnumerator: TCookieEnumerator;
  end;

  { Configuration settings.
    Owned by the client, implemented in tc_config }
  IClientConfig = interface
    procedure Load;
    procedure Save;
    procedure SetString(AKey: String; AValue: String; Encoded: Boolean=False);
    function GetString(AKey: String; Encoded: Boolean=False): String;
    function GetStringArray(AKey: String): TStringArray;
    function DataDir: String;
    function ListenPort: DWord;
    function ListenInterface: String;
    function TorHostName: String;
    function TorPort: DWord;
  end;

  { The client, ultimately owning all other objects. The GUI will
    derive from this and implement all the OnXxxx() event methods.
    Implemented in tc_client. }
  IClient = interface
    procedure Pump;       // must be called from the GUI thread.
    procedure OnNeedPump; // ask the GUI to schedule a Pump() asap.
    procedure OnGotOwnID;
    procedure OnBuddyStatusChange(ABuddy: IBuddy);
    procedure OnBuddyAvatarChange(ABuddy: IBuddy);
    procedure OnBuddyAliasChange(ABuddy: IBuddy);
    procedure OnBuddyAdded(ABuddy: IBuddy);
    procedure OnBuddyRemoved(ABuddy: IBuddy);
    procedure OnInstantMessage(ABuddy: IBuddy; AText: String);
    procedure OnIncomingFileTransfer(ABuddy: IBuddy; AID: String; AFileName: String; AFileSize: UInt64; ABlockSize: Integer);
    function UserAddBuddy(AID, AAlias: String): IBuddy;
    procedure SetStatus(AStatus: TTorchatStatus);
    procedure SetOwnAvatarData(RGB, Alpha: String);
    procedure SetOwnProfile(AName, AText: String);
    procedure RegisterAnonConnection(AConn: IHiddenConnection);
    procedure UnregisterAnonConnection(AConn: IHiddenConnection);
    function  MainThread: TThreadID;
    procedure DummySocketEvent(AHandle: TLHandle);
    procedure DummySocketError(AHandle: TLHandle; const Error: String);
    procedure AddFileTransfer(ATransfer: IFileTransfer);
    procedure RemoveFileTransfer(ATransfer: IFileTransfer);
    function FindFileTransferSend(Id: String): IFileTransfer;
    function FindFileTransferRecv(Id: String): IFileTransfer;
    function FindFileTransfer(GuiID: Pointer): IFileTransfer;
    function Roster: IRoster;
    function TempList: ITempList;
    function Queue: IMsgQueue;
    function CookieList: ICookieList;
    function Config: IClientConfig;
    function IsDestroying: Boolean;
    function ProfileName: String;
    function TorHost: String;
    function TorPort: DWord;
    function HSNameOK: Boolean;
    function Status: TTorchatStatus;
    function LNetEventer: TLEventer;
  end;

  { A temporary list of buddies, also used as base class for the roster.
    Owned by the client, implemented in tc_templist }
  ITempList = interface(IInterfaceList)
    procedure CheckState;
    procedure AddBuddy(ABuddy: IBuddy);
    procedure RemoveBuddy(ABuddy: IBuddy);
    function ByID(ABuddyID: String): IBuddy;
    function ByCookie(ACookie: String): IBuddy;
    procedure DoDisconnectAll;
    function GetEnumerator: TBuddyEnumerator;
  end;

  { the buddy list.
    Owned by the client, implemented in tc_roster }
  IRoster = interface(ITempList)
    procedure Load;
    procedure Save;
    procedure AddBuddyNoCallback(ABuddy: IBuddy);
    procedure RemoveBuddyNoCallback(ABuddy: IBuddy);
    function OwnID: String;
    function GroupName: String;
    procedure SetOwnID(AID: String);
  end;

  { Owned by templist or roster, implemented in tc_buddy }
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
    procedure ResetKeepaliveTimeout;
    procedure ResetAllTimes; // make it look like new (for the templist)
    procedure DoDisconnect;
    procedure RemoveYourself;
    function Client: IClient;
    function Software: String;
    function SoftwareVersion: String;
    function ID: String;
    function Cookie: String;
    function LocalAlias: String;
    function ConnIncoming: IHiddenConnection;
    function ConnOutgoing: IHiddenConnection;
    function IsFullyConnected: Boolean;
    function MaySendText: Boolean;
    function Status: TTorchatStatus;
    function AvatarData: String; // raw 64*64*24 bitmap (or '' if empty)
    function AvatarAlphaData: String; // raw 64*64*8 bitmap (or '' if empty)
    function ProfileName: String;
    function ProfileText: String;
    procedure SetLocalAlias(AName: String);
    procedure SetIncoming(AConn: IHiddenConnection);
    procedure SetOutgoing(AConn: IHiddenConnection);
    procedure SetStatus(AStatus: TTorchatStatus);
    procedure SetSoftware(ASoftware: String);
    procedure SetSoftwareVersion(AVersion: String);
    procedure SetAvatarData(ABitmap: String);
    procedure SetAvatarAlphaData(ABitmap: String);
    procedure SetProfileName(AName: String);
    procedure SetProfileText(AText: String);
    function SendIM(AText: String): Boolean;
    procedure SendAddMe;
    procedure SendStatus;
    procedure SendAvatar;
    procedure SendProfile;
  end;

  { Each buddy owns two of them, incoming anonymous connections
    are owned by the client until authenticated and then assigned
    to the buddy. Implemented in tc_conn }
  IHiddenConnection = interface
    procedure SetBuddy(ABuddy: IBuddy);
    procedure SetPingData(AID: String; ACookie: String);
    procedure Send(AData: String);
    procedure SendLine(AEncodedLine: String);
    function IsOutgoing: Boolean;
    function DebugInfo: String;
    function Buddy: IBuddy;
    function Client: IClient;
    function TimeCreated: TDateTime;
    function PingBuddyID: String;
    function PingCookie: String;
    procedure Disconnect;
  end;

  { A message that can be enqueued for execution on the main thread.
    This interface is also implemented in all protocol messages.
    Classes that implement only IMessage are for internal messaging,
    they are implemented in tc_msgqueue }
  IMessage = interface
    procedure Execute;
  end;

  { A protocol message. All protocol messages implement this interface
    because they are derived from TMsg, implemented in tc_potocol.
    Incoming messages are created and parsed by the connection object
    from within the network thread, then enqueued and later executed
    in the main thread duing Pump().
    Implementations are in tc_protocol and all the tc_prot_xxx units }
  IProtocolMessage = interface(IMessage)
    procedure Parse;
    procedure Send;
  end;

  { The queue for all incoming protocol messages and internal messages,
    owned by the client, implemented in tc_msgqueue }
  IMsgQueue = interface
    procedure Put(Msg: IMessage);
    procedure PumpNext;
    procedure Clear;
  end;

  { Represents an incoming or outgoing file transfer. The OnXxxx()
    methods must be implemnted by the GUI to hook into the events
    that each file transfer will generate.
    Created and owned by the GUI, created upon request from the user
    or when the client fires the event for incoming transfer.
    Implemented in tc_filetransfer }
  IFileTransfer = interface
    function ID: String;
    function Client: IClient;
    function Buddy: IBuddy;
    function IsSender: Boolean;
    function IsComplete: Boolean;
    function BytesCompleted: Int64;
    function FileSize: Int64;
    function GuiID: Pointer; // optional, can be some related GUI object
    procedure SetGuiID(AGuiID: Pointer);
    procedure CheckState;
    procedure OnStartSending;
    procedure OnProgressSending;
    procedure OnCancelSending;
    procedure OnCompleteSending;
    procedure OnStartReceiving;
    procedure OnProgressReceiving;
    procedure OnCancelReceiving;
    procedure OnCompleteReceiving;
    procedure MoveReceivedFile(DestName: String);
    procedure ReceivedFileChunk(StartByte: Int64; FileChunk: String);
    procedure ReceivedBrokenChunk(StartByte: Int64);
    procedure ReceivedOk(StartByte: Int64);
    procedure ReceivedError(StartByte: Int64);
    procedure ReceivedCancel;
  end;

  operator in(ABuddy: IBuddy; AList: ITempList): Boolean;

implementation

operator in(ABuddy: IBuddy; AList: ITempList): Boolean;
begin
  Result := (AList.IndexOf(ABuddy) > -1);
end;


end.

