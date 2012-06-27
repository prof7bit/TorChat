{$ifdef _type}
  PPurpleXfer = ^TPurpleXfer;

  TPurpleXferType = (
    PURPLE_XFER_UNKNOWN = 0,  (**< Unknown file transfer type.  *)
    PURPLE_XFER_SEND,         (**< File sending.                *)
    PURPLE_XFER_RECEIVE       (**< File receiving.              *)
  );

  TPurpleXferStatusType = (
    PURPLE_XFER_STATUS_UNKNOWN = 0,   (**< Unknown, the xfer may be null.  *)
    PURPLE_XFER_STATUS_NOT_STARTED,   (**< It hasn't started yet.  *)
    PURPLE_XFER_STATUS_ACCEPTED,      (**< Receive accepted, but destination file not selected yet  *)
    PURPLE_XFER_STATUS_STARTED,       (**< purple_xfer_start has been called.  *)
    PURPLE_XFER_STATUS_DONE,          (**< The xfer completed successfully.  *)
    PURPLE_XFER_STATUS_CANCEL_LOCAL,  (**< The xfer was cancelled by us.  *)
    PURPLE_XFER_STATUS_CANCEL_REMOTE  (**< The xfer was cancelled by the other end, or we couldn't connect.  *)
  );

  PXferCb = procedure(xfer: PPurpleXfer); cdecl;
  PXferAckCb = procedure(xfer: PPurpleXfer; par1: Pchar; par2: csize_t); cdecl;

  { TPurpleXfer }

  TPurpleXfer = Object
    class function Create(Account: PPurpleAccount; Typ: TPurpleXferType; Who: String): PPurpleXfer;
    procedure Free; // will call purple_xfer_end()
    procedure CancelRemote;
    procedure Request;
    procedure RequestAccepted(FileName: String);
    procedure SetAckFnc(Fnc: PXferAckCb);
    procedure SetBytesSent(Bytes: csize_t);
    procedure SetCancelSendFnc(Fnc: PXferCb);
    procedure SetCancelRecvFnc(Fnc: PXferCb);
    procedure SetCompleted(Completed: Boolean);
    procedure SetFileName(FileName: String);
    procedure SetInitFnc(Fnc: PXferCb);
    procedure Start(fd: cint; ip: String; port: cuint);
    procedure SetRequestDeniedFnc(Fnc: PXferCb);
    procedure SetSize(Size: csize_t);
    procedure UpdateProgress;
    function GetAccount: PPurpleAccount;
    function GetFileName: String;
    function GetLocalFileName: String;
    function GetRemoteUser: String;
    function GetStatus: TPurpleXferStatusType;
  end;
{$endif}


{$ifdef _func}
function  purple_xfer_new(account: PPurpleAccount; type_: TPurpleXferType; who: PChar): PPurpleXfer; cdecl; external LIBPURPLE;
procedure purple_xfer_end(xfer: PPurpleXfer); cdecl; external LIBPURPLE;
procedure purple_xfer_cancel_remote(xfer: PPurpleXfer); cdecl; external LIBPURPLE;
procedure purple_xfer_request(xfer: PPurpleXfer); cdecl; external LIBPURPLE;
procedure purple_xfer_request_accepted(xfer: PPurpleXfer; filename: PChar); cdecl; external LIBPURPLE;
procedure purple_xfer_set_ack_fnc(xfer: PPurpleXfer; fnc: PXferAckCb); cdecl; external LIBPURPLE;
procedure purple_xfer_set_bytes_sent(xfer: PPurpleXfer; bytes_sent: csize_t); cdecl; external LIBPURPLE;
procedure purple_xfer_set_cancel_send_fnc(xfer: PPurpleXfer; fnc: PXferCb); cdecl; external LIBPURPLE;
procedure purple_xfer_set_cancel_recv_fnc(xfer: PPurpleXfer; fnc: PXferCb); cdecl; external LIBPURPLE;
procedure purple_xfer_set_completed(xfer: PPurpleXfer; completed: gboolean); cdecl; external LIBPURPLE;
procedure purple_xfer_set_end_fnc(xfer: PPurpleXfer; fnc: PXferCb); cdecl; external LIBPURPLE;
procedure purple_xfer_set_filename(xfer: PPurpleXfer; filename: PChar); cdecl; external LIBPURPLE;
procedure purple_xfer_set_init_fnc(xfer: PPurpleXfer; fnc: PXferCb); cdecl; external LIBPURPLE;
procedure purple_xfer_start(xfer: PPurpleXfer; fd: cint; ip: PChar;
  port: cuint); cdecl; external LIBPURPLE;
procedure purple_xfer_set_request_denied_fnc(xfer: PPurpleXfer; fnc: PXferCb); cdecl; external LIBPURPLE;
procedure purple_xfer_set_size(xfer: PPurpleXfer; size: csize_t); cdecl; external LIBPURPLE;
procedure purple_xfer_update_progress(xfer: PPurpleXfer); cdecl; external LIBPURPLE;
function  purple_xfer_get_account(xfer: PPurpleXfer): PPurpleAccount; cdecl; external LIBPURPLE;
function  purple_xfer_get_filename(xfer: PPurpleXfer): PChar; cdecl; external LIBPURPLE;
function  purple_xfer_get_local_filename(xfer: PPurpleXfer): PChar; cdecl; external LIBPURPLE;
function  purple_xfer_get_remote_user(xfer: PPurpleXfer): PChar; cdecl; external LIBPURPLE;
function purple_xfer_get_status(xfer: PPurpleXfer): TPurpleXferStatusType; cdecl; external LIBPURPLE;
{$endif}


{$ifdef _impl}
class function TPurpleXfer.Create(Account: PPurpleAccount; Typ: TPurpleXferType; Who: String): PPurpleXfer;
begin
  Result := purple_xfer_new(Account, Typ, Pointer(Who));
end;

procedure TPurpleXfer.Free;
begin
  purple_xfer_end(@Self);
end;

procedure TPurpleXfer.CancelRemote;
begin
  purple_xfer_cancel_remote(@Self);
end;

procedure TPurpleXfer.Request;
begin
  purple_xfer_request(@Self);
end;

procedure TPurpleXfer.RequestAccepted(FileName: String);
begin
  purple_xfer_request_accepted(@Self, Pointer(FileName));
end;

procedure TPurpleXfer.SetAckFnc(Fnc: PXferAckCb);
begin
  purple_xfer_set_ack_fnc(@Self, Fnc);
end;

procedure TPurpleXfer.SetBytesSent(Bytes: csize_t);
begin
  purple_xfer_set_bytes_sent(@Self, Bytes);
end;

procedure TPurpleXfer.SetCancelSendFnc(Fnc: PXferCb);
begin
  purple_xfer_set_cancel_send_fnc(@Self, Fnc);
end;

procedure TPurpleXfer.SetCancelRecvFnc(Fnc: PXferCb);
begin
  purple_xfer_set_cancel_recv_fnc(@Self, Fnc);
end;

procedure TPurpleXfer.SetCompleted(Completed: Boolean);
begin
  purple_xfer_set_completed(@Self, Completed);
end;

procedure TPurpleXfer.SetFileName(FileName: String);
begin
  purple_xfer_set_filename(@Self, Pointer(FileName));
end;

procedure TPurpleXfer.SetInitFnc(Fnc: PXferCb);
begin
  purple_xfer_set_init_fnc(@Self, Fnc);
end;

procedure TPurpleXfer.Start(fd: cint; ip: String; port: cuint);
begin
  purple_xfer_start(@Self, fd, Pointer(ip), port);
end;

procedure TPurpleXfer.SetRequestDeniedFnc(Fnc: PXferCb);
begin
  purple_xfer_set_request_denied_fnc(@Self, Fnc);
end;

procedure TPurpleXfer.SetSize(Size: csize_t);
begin
  purple_xfer_set_size(@Self, Size);
end;

procedure TPurpleXfer.UpdateProgress;
begin
  purple_xfer_update_progress(@Self);
end;

function TPurpleXfer.GetAccount: PPurpleAccount;
begin
  Result := purple_xfer_get_account(@Self);
end;

function TPurpleXfer.GetFileName: String;
begin
  Result := purple_xfer_get_filename(@Self);
end;

function TPurpleXfer.GetLocalFileName: String;
begin
  Result := purple_xfer_get_local_filename(@Self);
end;

function TPurpleXfer.GetRemoteUser: String;
begin
  Result := purple_xfer_get_remote_user(@Self);
end;

function TPurpleXfer.GetStatus: TPurpleXferStatusType;
begin
  Result := purple_xfer_get_status(@Self);
end;

{$endif}
