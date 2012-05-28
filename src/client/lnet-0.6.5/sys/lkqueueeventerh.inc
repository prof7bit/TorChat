{% lkqueueeventerh.inc included by levents.pas }

{$ifdef BSD}

  { TLKQueueEventer }

  TLKQueueEventer = class(TLEventer)
   protected
    FTimeout: TTimeSpec;
    FEvents: array of TKEvent;
    FChanges: array of TKEvent;
    FFreeSlot: Integer;
    FQueue: THandle;
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
  
{$endif} // bsd
