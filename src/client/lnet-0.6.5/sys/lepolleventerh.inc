{% lepolleventerh.inc included by levents.pas }

{$ifdef Linux}

  PEpollEvent = ^epoll_event;
  TEpollEvent = epoll_event;
  PEpollData = ^epoll_data;
  TEpollData = epoll_data;
  
  { TLEpollEventer }
  
  TLEpollEventer = class(TLEventer)
   protected
    FTimeout: cInt;
    FEvents: array of TEpollEvent;
    FEventsRead: array of TEpollEvent;
    FEpollReadFD: THandle;   // this one monitors LT style for READ
    FEpollFD: THandle;       // this one monitors ET style for other
    FEpollMasterFD: THandle; // this one monitors the first two
    FFreeList: TFPObjectList;
    function GetTimeout: Integer; override;
    procedure SetTimeout(const Value: Integer); override;
    procedure HandleIgnoreRead(aHandle: TLHandle); override;
    procedure Inflate;
   public
    constructor Create; override;
    destructor Destroy; override;
    function AddHandle(aHandle: TLHandle): Boolean; override;
    function CallAction: Boolean; override;
  end;

{$endif} // linux
