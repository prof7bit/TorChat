{ TorChat - libpurpletorchat, a libpurple (Pidgin) plugin for TorChat

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
library purpletorchat;

{ Naming Conventions:

  Type names:
  -----------
  Normally in Pascal we use Uppercase/CamelCase identifiers and
  type names start with 'T', pointer types with 'P', interfaces
  with 'I', which will then look like TSomeThing or PSomeThing.

  C programmers on the other hand normally don't define separate
  names to denote pointer types, they just use the asterisk
  everywhere in their code and write SomeType and SomeType*

  I don't do the Pascal equivalent of * here (which would be
  ^SomeType) because it would make it totally inconsistent.
  Type names are the most important names of all, they should
  be clear and consistent. I have translated the purple types:

    PurpleSomething  is now TPurpleSomething
    PurpleSomething* is now PPurpleSomething

  and I am trying to avoid using the name PurpleSomething (without
  T or P) entirely. (PurpleSomething would be a good name for a
  variable in Pascal but this would confuse the hell out of every
  libpurple dev who is used to know this as a type name. So I'm
  not using it at all.)

  Variable names and function names
  ---------------------------------
  libpurple callback functions and variables that hold libpurple
  or glib types are named with lowercase and under_score and all
  other variables are using the normal Pascal naming conventions.

  The TorChat engine will call overridden virtual methods when
  TorChat events happen, these are the methods that start with

    TTorChatPurpleClient.OnXxxx()

  and the callback fuctions that are called by libpurple when
  Pidgin/libpurple wants something to happen are the procedures
  and functions that start with

    torchat_xxx()

  and the xxx corresponds to the field names in the info records
  where these callbacks have been registered.
}

{$if FPC_FULLVERSION < 20600}
  {$fatal *** You need Free Pascal Compiler version 2.6.0 or higher *** }
{$endif}
{$mode objfpc}{$H+}

uses
  {$ifdef UseHeapTrc} // do it with -dUseHeapTrc, not with -gh
    heaptrc,
  {$endif}
  {$ifdef unix}
    cthreads,
  {$endif}
  Classes,
  sysutils,
  contnrs,
  ctypes,
  glib2,
  purple,
  purplehelper,
  tc_interface,
  tc_client,
  tc_const,
  FPimage,
  FPWritePNG,
  FPReadPNG,
  FPImgCanv,
  tc_misc,
  tc_filetransfer;

const
  PRPL_ID_OFFLINE = 'offline';
  PRPL_ID_AVAILABLE = 'available';
  PRPL_ID_AWAY = 'away';
  PRPL_ID_XA = 'xa';
  PRPL_ID_INVISIBLE = 'invisible';

type
  { TTorChat is a TTorChatClient whose event
    methods know how to speak with libpurple }
  TTorChat = class(TTorChatClient)
  public
    purple_account: PPurpleAccount;
    purple_timer: Integer;
    constructor Create(AOwner: TComponent; AProfileName: String;
      account: PPurpleAccount); reintroduce;
    procedure OnNeedPump; override;
    procedure OnGotOwnID; override;
    procedure OnBuddyStatusChange(ABuddy: IBuddy); override;
    procedure OnBuddyAvatarChange(ABuddy: IBuddy); override;
    procedure OnBuddyAdded(ABuddy: IBuddy); override;
    procedure OnBuddyRemoved(ABuddy: IBuddy); override;
    procedure OnInstantMessage(ABuddy: IBuddy; AText: String); override;
    procedure OnIncomingFileTransfer(ABuddy: IBuddy; AID: String; AFileName: String; AFileSize: UInt64; ABlockSize: Integer); override;
  end;

  { TTransfer is a TFileTransfer whose event
    methods know how to speak with libpuple }
  TTransfer = class(TFileTransfer)
  private
    PurpleProgressStarted: Boolean;
  public
    constructor Create(ABuddy: IBuddy; AFileName: String);
    procedure OnProgress; override;
    procedure OnCancel; override;
    procedure OnComplete; override;
  end;

  { TClients holds a list of clients since we can have
    multiple "accounts" in pidgin at the same time }
  TClients = class(TFPHashObjectList)
    function Find(Account: PPurpleAccount): TTorChat;
  end;

var
  purple_plugin: PPurplePlugin;
  Clients: TClients;


(********************************************************************
 *                                                                  *
 *  The timer functions are called  by libpurple timers, they are   *
 *  fired in regular intervals and all OnXxxx method calls from     *
 *  TorChat into libpurple will ultimately originate from one of    *
 *  these calls to Pump(). This is needed because we may not just   *
 *  call purple_xxx functions from our own threads, everything      *
 *  needs to happen on the main thread.                             *
 *                                                                  *
 ********************************************************************)

function cb_purple_timer(data: Pointer): GBoolean; cdecl;
begin
  TTorChat(data).Pump;
  Result := True;
end;

function cb_purple_timer_oneshot(data: Pointer): GBoolean; cdecl;
begin
  TTorChat(data).Pump;
  Result := False; // purple timer will not fire again
end;

//{$define DebugTimer}
{$ifdef DebugTimer}
const
  DEBUG_TIMER_DELAY = 120;
  DEBUG_TIMER_INTERVAL = 30;
  DEBUG_FUNC_ADDR: TGSourceFunc = nil;
{ this timer function is only used for debugging, it
  can be used to automatically trigger certain events
  at runtime more easily than doing it manually. }
function __debug(Data: Pointer): gboolean; cdecl;
var
  I: Integer;
  TorChat: TTorChatClient;
begin
  WriteLn('W *** debug timer triggered, unexpected events begin...');
  for I := 0 to TorChatClients.Count-1 do begin
    TorChat := TTorChatClient(TorChatClients.Items[I]);
    TorChat.Roster.DoDisconnectAll;
    Sleep(400);
  end;
  WriteLn('W *** debug timer end');
  if DEBUG_TIMER_INTERVAL > 0 then
    purple_timeout_add(DEBUG_TIMER_INTERVAL*1000, DEBUG_FUNC_ADDR, nil);
  Result := False;
end;
{$endif DebugTimer}

(******************************************************************
 *                                                                *
 *  All the following functions are callbacks that are called by  *
 *  libpurple when the user interacts with the application.       *
 *  They have been registered in the two info records during      *
 *  initialization (see the notes at the end of this file).       *
 *                                                                *
 ******************************************************************)

function torchat_load(plugin: PPurplePlugin): GBoolean; cdecl;
begin
  purple_plugin := plugin;
  Clients := TClients.Create(False);
  {$ifdef DebugTimer}
  DEBUG_FUNC_ADDR := @__debug;
  purple_timeout_add(DEBUG_TIMER_DELAY*1000, DEBUG_FUNC_ADDR, nil);
  WriteLn('E Debug timer installed, crazy things will happen unexpectedly...');
  {$endif DebugTimer}
  WriteLn('plugin loaded');
  Result := True;
end;

function torchat_unload(plugin: PPurplePlugin): GBoolean; cdecl;
begin
  Clients.Free;
  WriteLn('plugin unloaded');
  Result := True;
end;

{ this is called when the user clicks ok in th the "set user info..."
  dialog box, this callback was registered in torchat_set_user_info()
  It will red the fields and set the data in TorChat. We will pass the
  account handle as user_data}
procedure torchat_set_user_info_ok(user_data: Pointer; fields: PPurpleRequestFields); cdecl;
var
  TorChat: IClient;
  ProfileName: String;
  ProfileText: String;
begin
  TorChat := Clients.Find(user_data); // user_data = account
  if Assigned(TorChat) then begin
    ProfileName := purple_request_fields_get_string(fields, 'name');
    ProfileText := purple_request_fields_get_string(fields, 'text');
    TorChat.SetOwnProfile(ProfileName, ProfileText);
  end;
end;

{ this is called when the user clicks the "set user info..." menu item
  this callback was registered in torchat_actions(). This function will
  create a dialog box and request the input of profile data. It will
  register torchat_set_user_info_ok() as the cb for the OK-Button }
procedure torchat_set_user_info(act: PPurplePluginAction); cdecl;
var
  fields: PPurpleRequestFields;
  group: PPurpleRequestFieldGroup;
  gc : PPurpleConnection;
  account: PPurpleAccount;
  TorChat: IClient;
begin
  gc := PPurpleConnection(act^.context);
  account := gc^.account;
  TorChat := Clients.Find(account);
  if Assigned(TorChat) then begin
    fields := purple_request_fields_new;
    group := purple_request_field_group_new(
      PChar('User info for ' + TorChat.Roster.OwnID));
    purple_request_fields_add_group(fields, group);

    purple_request_field_group_add_field(
      group,
      purple_request_field_string_new(
        'name',
        'Name',
        PChar(TorChat.Config.GetString('ProfileName')),
        False
      )
    );
    purple_request_field_group_add_field(
      group,
      purple_request_field_string_new(
        'text',
        'About me',
        PChar(TorChat.Config.GetString('ProfileText')),
        True
      )
    );

    purple_request_fields(
      gc,
      'Set User Info',
      nil,
      nil,
      fields,
      'ok',
      @torchat_set_user_info_ok,
      'cancel',
      nil,
      account,
      nil,
      nil,
      account
    );
  end;
end;

{ this callback is registered in the plugin_info record, purple will call
  it when loading the plugin and here we set up the menu items and
  register the callbacks to handle these menu items. }
function torchat_actions(plugin: PPurplePlugin; context: Pointer): PGList; cdecl;
var
  act   : PPurplePluginAction;
  m     : PGList;
begin
  act := purple_plugin_action_new('Set User Info...', @torchat_set_user_info);
  m := g_list_append(nil, act);
  Result := m;
end;

function torchat_status_types(acc: PPurpleAccount): PGList; cdecl;
begin
  // Pidgin has some strange policy regarding usable status types:
  // As soon as there are more than one protocols active it will
  // fall back to a standard list of status types, no matter whether
  // all the protocols support them or any of them requested them,
  // so we are forced to register them all and then map them to
  // TorChat statuses in our torchat_set_status() callback.
  Result := nil;
  Result := g_list_append(Result, purple_status_type_new_full(PURPLE_STATUS_AVAILABLE, PRPL_ID_AVAILABLE, nil, True, True, False));
  Result := g_list_append(Result, purple_status_type_new_full(PURPLE_STATUS_AWAY, PRPL_ID_AWAY, nil, True, True, False));
  Result := g_list_append(Result, purple_status_type_new_full(PURPLE_STATUS_UNAVAILABLE, PRPL_ID_XA, nil, True, True, False));
  Result := g_list_append(Result, purple_status_type_new_full(PURPLE_STATUS_INVISIBLE, PRPL_ID_INVISIBLE, nil, True, True, False));
  Result := g_list_append(Result, purple_status_type_new_full(PURPLE_STATUS_OFFLINE, PRPL_ID_OFFLINE, nil, True, True, False));
end;

procedure torchat_set_status(acc: PPurpleAccount; status: PPurpleStatus); cdecl;
var
  status_prim   : TPurpleStatusPrimitive;
  TorchatStatus : TTorchatStatus;
begin
  status_prim := purple_status_type_get_primitive(purple_status_get_type(status));
  case status_prim of
    PURPLE_STATUS_AVAILABLE: TorchatStatus := TORCHAT_AVAILABLE;
    PURPLE_STATUS_UNAVAILABLE: TorchatStatus := TORCHAT_XA;
    PURPLE_STATUS_AWAY: TorchatStatus := TORCHAT_AWAY;
    PURPLE_STATUS_EXTENDED_AWAY: TorchatStatus := TORCHAT_XA;
    PURPLE_STATUS_INVISIBLE: TorchatStatus := TORCHAT_OFFLINE;
  end;
  Clients.Find(acc).SetStatus(TorchatStatus);
end;

procedure torchat_set_buddy_icon(gc: PPurpleConnection; img: PPurpleStoredImage); cdecl;
var
  HaveImage: Boolean;
  len: PtrUInt;
  data: Pointer;
  ImageOriginal: TFPMemoryImage;
  ImageScaled: TFPMemoryImage;
  CanvasScaled: TFPImageCanvas;
  ImageReader: TFPCustomImageReader;
  ImageStream: TMemoryStream;
  Pixel: TFPColor;
  RGB24: String;
  Alpha8: String;
  PtrRGB: P24Pixel;
  PtrAlpha: PByte;
  X, Y: Integer;
  AllAlphaBits: Byte;
  TorChat: TTorChat;
begin
  TorChat := Clients.Find(gc^.account);
  if Assigned(TorChat) then begin
    len := purple_imgstore_get_size(img);
    data := purple_imgstore_get_data(img);

    // read the PNG image from memory
    ImageOriginal := TFPMemoryImage.Create(0, 0);
    ImageStream := TMemoryStream.Create;
    ImageStream.Write(data^, len);
    ImageStream.Seek(0, soBeginning);
    ImageReader := TFPReaderPNG.Create;
    try
      ImageOriginal.LoadFromStream(ImageStream, ImageReader);
      HaveImage := True;
    except
      WriteLn('I received invalid or empty image from libpurple');
      WriteLn(len);
      HaveImage := False;
    end;

    if HaveImage then begin
      // scale the image to 64*64
      ImageScaled := TFPMemoryImage.Create(64, 64);
      {$warnings off} // contains some abstract methods (must be FCL bug)
      CanvasScaled := TFPImageCanvas.Create(ImageScaled);
      {$warnings on}
      CanvasScaled.StretchDraw(0, 0, 64, 64, ImageOriginal);

      // convert it to TorChat format (uncompressed RGB, separate Alpha)
      SetLength(RGB24, 12288);
      SetLength(Alpha8, 4096);
      PtrRGB := @RGB24[1];
      PtrAlpha := @Alpha8[1];
      AllAlphaBits := $ff;
      for Y := 0 to 63 do begin
        for X := 0 to 63 do begin
          Pixel := ImageScaled.Colors[X, Y];
          PtrRGB^.Red := hi(Pixel.red);
          PtrRGB^.Green := hi(Pixel.green);
          PtrRGB^.Blue := hi(Pixel.blue);
          PtrAlpha^ := hi(Pixel.alpha);
          AllAlphaBits := AllAlphaBits and hi(Pixel.alpha);
          Inc(PtrRGB);
          Inc(PtrAlpha);
        end;
      end;
      CanvasScaled.Free;
      ImageScaled.Free;
      if AllAlphaBits = $ff then
        Alpha8 := '';
    end
    else begin
      RGB24 := '';
      Alpha8 := '';
    end;

    TorChat.SetOwnAvatarData(RGB24, Alpha8);

    ImageStream.Free;
    ImageReader.Free;
    ImageOriginal.Free;
  end;
end;

procedure torchat_add_buddy(gc: PPurpleConnection; purple_buddy: PPurpleBuddy; group: PPurpleGroup); cdecl;
var
  TorChat : TTorChat;
  purple_id: PChar;
  purple_alias: PChar;
begin
  TorChat := Clients.Find(gc^.account);
  if Assigned(TorChat) then begin
    purple_id := purple_buddy_get_name(purple_buddy);
    purple_alias := purple_buddy_get_alias_only(purple_buddy);
    if not TorChat.UserAddBuddy(purple_id, purple_alias) then begin
      purple_notify_message(purple_plugin, PURPLE_NOTIFY_MSG_ERROR,
        'Cannot add buddy',
        'A buddy with this ID cannot be added',
        'Either this ID contains invalid characters or it is incomplete or the ID is already on the list.',
        nil,
        nil);
      purple_blist_remove_buddy(purple_buddy);
    end;
  end;
end;

function torchat_send_im(gc: PPurpleConnection; who, message: PChar; flags: TPurpleMessageFlags): cint; cdecl;
var
  TorChat: TTorChat;
  Buddy: IBuddy;
  Msg: String;
begin
  Msg := Html2Plain(message);
  TorChat := Clients.Find(gc^.account);
  if Assigned(TorChat) then begin
    Buddy := TorChat.Roster.ByID(who);
    if Assigned(Buddy) then
      Result := cint(Buddy.SendIM(Msg)) // wrong return value, see below!
    else begin
      {$note might have sent remove_me in the meantime, write a warning}
      {$warning there are error codes defined for sending IM, use them!}
      Result := 0; // is this ok? No, its not, see $warning above.
    end;
  end;
end;

procedure torchat_remove_buddy(gc: PPurpleConnection; purple_buddy: PPurpleBuddy; group: PPurpleGroup); cdecl;
var
  TorChat: IClient;
  Buddy: IBuddy;
  purple_id: PChar;
begin
  purple_id := purple_buddy_get_name(purple_buddy);
  TorChat := Clients.Find(gc^.account);
  Buddy := TorChat.Roster.ByID(purple_id);
  if Assigned(Buddy) then
    Buddy.RemoveYourself;
end;

procedure torchat_alias_buddy(gc: PPurpleConnection; who, aalias: PChar); cdecl;
var
  TorChat: TTorChat;
  Buddy: IBuddy;
begin
  TorChat := Clients.Find(gc^.account);
  if Assigned(TorChat) then begin
    Buddy := TorChat.Roster.ByID(who);
    Buddy.SetLocalAlias(aalias);
    serv_got_alias(gc, who, aalias);
  end;
end;

procedure torchat_tooltip_text(purple_buddy: PPurpleBuddy; user_info: PPurpleNotifyUserInfo; full: gboolean); cdecl;
var
  buddy_id : PChar;
  TorChat: TTorChat;
  Buddy: IBuddy;
begin
  if not full then exit;
  TorChat := Clients.Find(purple_buddy_get_account(purple_buddy));
  if Assigned(TorChat) then begin
    buddy_id := purple_buddy_get_name(purple_buddy);
    Buddy := TorChat.Roster.ByID(buddy_id);
    if Assigned(Buddy) then begin
      // we escape every < or > from all strings because
      // it would break the entire tooltip window. (It would
      // be completely empty, this is a bug in Pidgin.)
      if Buddy.Software <> '' then begin
        purple_notify_user_info_add_pair(
          user_info,
          'Client',
          PChar(EscapeAngleBrackets(Buddy.Software + '-' + Buddy.SoftwareVersion))
        );
      end;
      if Buddy.ProfileName <> '' then begin
        purple_notify_user_info_add_pair(
          user_info,
          'Name',
          PChar(EscapeAngleBrackets(Buddy.ProfileName))
        );
      end;
      if Buddy.ProfileText <> '' then begin
        purple_notify_user_info_add_pair(
          user_info,
          'Profile',
          PChar(EscapeAngleBrackets(Buddy.ProfileText))
        );
      end;
    end;
  end;
end;

function torchat_get_text_table(acc: PPurpleAccount): PGHashTable; cdecl;
begin
  Result := g_hash_table_new(@g_str_hash, @g_str_equal);
  g_hash_table_insert(Result, PChar('login_label'), PChar('profile name'));
end;

function torchat_list_icon(acc: PPurpleAccount; buddy: PPurpleBuddy): PChar; cdecl;
begin
  Result := 'torchat';
  // now it will look for torchat.png in several resolutions
  // in the folders /usr/share/pixmaps/pidgin/protocols/*/
  // the installer for the plugin must install these files.
  // IMPORTANT: This will also be used for the name of the
  // folder where the log files are stored. It is NOT allowed
  // to return nil, this would break logging beyond all repair!
end;

procedure torchat_login(acc: PPurpleAccount); cdecl;
var
  TorChat: TTorChat;
  purple_status: PPurpleStatus;
begin
  TorChat := TTorChat.Create(nil, acc^.username, acc);
  TorChat.purple_timer := purple_timeout_add(1000, @cb_purple_timer, TorChat);
  Clients.Add(acc^.username, TorChat);

  // it won't call set_status after login, so we have to do it ourselves
  purple_status := purple_presence_get_active_status(acc^.presence);
  torchat_set_status(acc, purple_status);
end;

procedure torchat_close(gc: PPurpleConnection); cdecl;
var
  TorChat: TTorChat;
begin
  TorChat := Clients.Find(gc^.account);
  if Assigned(TorChat) then begin
    purple_timeout_remove(TorChat.purple_timer);
    Clients.Remove(TorChat);
    TorChat.Free;
  end;
end;

{ This will be called to decide whether it should enable the menu
  item "send file" in the buddy list and in the conversation window}
function torchat_can_receive_file(gc: PPurpleConnection; who: PChar): gboolean; cdecl;
var
  TorChat: IClient;
  Buddy: IBuddy;
begin
  Result := False;
  {$ifdef WithFileTransfer}
  TorChat := Clients.Find(gc^.account);
  if Assigned(TorChat) then begin
    Buddy := TorChat.Roster.ByID(who);
    if Assigned(Buddy) then begin
      Result := True;
    end;
  end;
  {$endif}
end;

{ registered during torchat_send_file() and called after the
  "file open" dialog has been finished with OK. Here we create
  a new TPurpleFileTransfer object in TorChat and start the
  file sending }
procedure torchat_xfer_init_send(xfer: PPurpleXfer); cdecl;
var
  FileName: String;
  TorChat: IClient;
  Buddy: IBuddy;
  FT: TTransfer;
begin
  WriteLn('torchat_xfer_init_send() ', FileName);
  TorChat := Clients.Find(purple_xfer_get_account(xfer));
  if Assigned(TorChat) then begin
    FileName := purple_xfer_get_local_filename(xfer);
    Buddy := TorChat.Roster.ByID(purple_xfer_get_remote_user(xfer));
    if Assigned(Buddy) then begin
      FT := TTransfer.Create(Buddy, FileName);
      FT.SetGuiHandle(xfer);
      TorChat.AddFileTransfer(FT);
      FT.StartSending;
    end;
  end;
end;

{ registered during torchat_send_file() }
procedure torchat_xfer_cancel_send(xfer: PPurpleXfer); cdecl;
var
  TorChat: IClient;
  Transfer: IFileTransfer;
begin
  WriteLn('torchat_xfer_cancel_send()');
  TorChat := Clients.Find(purple_xfer_get_account(xfer));
  if Assigned(TorChat) then begin
    Transfer := TorChat.FindFileTransfer(xfer);
    if Assigned(Transfer) then begin
      TorChat.RemoveFileTransfer(Transfer);
    end;
  end;
end;

{ registered during torchat_send_file() }
procedure torchat_xfer_end(xfer: PPurpleXfer); cdecl;
var
  FileName: String;
begin
  FileName := purple_xfer_get_local_filename(xfer);
  WriteLn('torchat_xfer_end() ', FileName);
end;

{ This function is called when the user clicks on the "Send File..."
  menu item. At this time there has not yet been a file dialog to select
  the file and filename will be nil. When the user drops a file to the
  chat window then it WILL be called with filename. In that case we need
  to initiate it a little bit differently. }
procedure torchat_send_file(gc: PPurpleConnection; who, filename: PChar); cdecl;
var
  xfer: PPurpleXfer;
begin
  Writeln(_F('send_file(%s, %s)', [who, filename]));
  if not Assigned(filename) then begin
    xfer := purple_xfer_new(gc^.account, PURPLE_XFER_SEND, who);
    purple_xfer_set_init_fnc(xfer, @torchat_xfer_init_send);
    purple_xfer_set_cancel_send_fnc(xfer, @torchat_xfer_cancel_send);
    purple_xfer_set_end_fnc(xfer, @torchat_xfer_end);
    purple_xfer_request(xfer); // this will trigger the "file open" dialog
    // all the rest will now happen with the above callback functions.
  end
  else begin
    WriteLn('W send_file() we have a file name already, this is not yet implemeted');
    {$warning implement this also}
  end;
end;

{ TPurpleFileTransfer }

constructor TTransfer.Create(ABuddy: IBuddy; AFileName: String);
begin
  inherited Create(Buddy, AFileName);
  PurpleProgressStarted := False;
end;

procedure TTransfer.OnProgress;
var
  xfer: PPurpleXfer;
begin
  xfer := GuiHandle;
  if not PurpleProgressStarted then begin
    purple_xfer_start(xfer, -1, nil, 0);
    PurpleProgressStarted := True;
  end;
  purple_xfer_set_bytes_sent(xfer, BytesCompleted);
  purple_xfer_update_progress(xfer);
end;

procedure TTransfer.OnCancel;
begin

end;

procedure TTransfer.OnComplete;
var
  xfer: PPurpleXfer;
begin
  xfer := GuiHandle;
  purple_xfer_set_bytes_sent(xfer, BytesCompleted);
  purple_xfer_set_completed(xfer, True);
  purple_xfer_update_progress(xfer);
  Client.RemoveFileTransfer(Self);
end;

(********************************************************************
 *                 end of libpurple callbacks                       *
 ********************************************************************)

{ TClients }

function TClients.Find(Account: PPurpleAccount): TTorChat;
begin
  Result := inherited Find(Account^.username) as TTorChat;
end;

{ TTorchatPurpleClient }

constructor TTorChat.Create(AOwner: TComponent; AProfileName: String;
  account: PPurpleAccount);
begin
  purple_account := account;
  inherited Create(AOwner, AProfileName);
end;


(********************************************************************
 *                                                                  *
 *  All the following methods are called when TorChat feels the     *
 *  need to notify libpurple/Pidgin about events that happened.     *
 *                                                                  *
 *  They all ultimately originate from one of the calls to Pump()   *
 *  in the timer functions because everything has to happen on      *
 *  libpurple's main thread. The only exception is OnNeedPump()     *
 *  which can come from any thread and is only used to request      *
 *  another Pump() to be scheduled as soon as posible.              *
 *                                                                  *
 ********************************************************************)

procedure TTorChat.OnNeedPump;
begin
  purple_timeout_add(0, @cb_purple_timer_oneshot, Self);
end;

procedure TTorChat.OnGotOwnID;
var
  Buddy: IBuddy;
  purple_buddy: PPurpleBuddy;
  purple_id: PChar;
  purple_alias: PChar;
  group_name: PChar;
  purple_group: PPurpleGroup;
  purple_list: PGSList;
begin
  WriteLn('Switching accout to "connected", synchronizing buddy lists');
  purple_connection_set_state(purple_account^.gc, PURPLE_CONNECTED);

  group_name := GetMemAndCopy(Roster.GroupName);
  purple_group := purple_find_group(group_name);

  // remove buddies from purple's list that not in TorChat's list
  purple_list := purple_find_buddies(purple_account, nil);
  while Assigned(purple_list) do begin
    purple_id := purple_buddy_get_name(purple_list^.data);
    if not Assigned(Roster.ByID(purple_id)) then begin
      purple_blist_remove_buddy(purple_list^.data);
    end;
    purple_list := g_slist_delete_link(purple_list, purple_list);
  end;

  // add buddies to purple's buddy list that are not in purple's list
  Roster.Lock;
  for Buddy in Roster do begin
    purple_id := GetMemAndCopy(Buddy.ID);
    purple_alias := GetMemAndCopy(Buddy.LocalAlias);
    purple_buddy := purple_find_buddy(purple_account, purple_id);
    if not Assigned(purple_buddy) then begin
      if not Assigned(purple_group) then begin
        purple_group := purple_group_new(group_name);
        purple_blist_add_group(purple_group, nil);
      end;
      purple_buddy := purple_buddy_new(purple_account, purple_id, purple_alias);
      purple_blist_add_buddy(purple_buddy, nil, purple_group, nil);
    end
    else begin
      serv_got_alias(purple_account^.gc, purple_id, purple_alias);
      purple_blist_alias_buddy(purple_buddy, purple_alias);
    end;
    FreeMem(purple_id);
    FreeMem(purple_alias);
  end;
  Roster.Unlock;
  FreeMem(group_name);
end;

procedure TTorChat.OnBuddyStatusChange(ABuddy: IBuddy);
var
  buddy_name: PChar;
  status_id: PChar;
begin
  buddy_name := GetMemAndCopy(ABuddy.ID);
  case ABuddy.Status of
    TORCHAT_AVAILABLE: status_id := GetMemAndCopy(PRPL_ID_AVAILABLE);
    TORCHAT_AWAY: status_id := GetMemAndCopy(PRPL_ID_AWAY);
    TORCHAT_XA: status_id := GetMemAndCopy(PRPL_ID_XA);
    TORCHAT_OFFLINE: status_id := GetMemAndCopy(PRPL_ID_OFFLINE);
  end;
  purple_prpl_got_user_status(purple_account, buddy_name, status_id);
  FreeMem(status_id);
  FreeMem(buddy_name);
end;

procedure TTorChat.OnBuddyAvatarChange(ABuddy: IBuddy);
var
  buddy_name: PChar;
  icon_data: Pointer;
  icon_len: PtrUInt;
  Image: TFPMemoryImage;
  ImageWriter: TFPWriterPNG;
  ImageStream: TMemoryStream;
  Raw24Bitmap: String;
  Raw8Alpha: String;
  HasAlpha: Boolean;
  Pixel: TFPColor;
  X,Y: Integer;
  PtrPixel24: P24Pixel;
  PtrAlpha8: PByte;
begin
  buddy_name := GetMemAndCopy(ABuddy.ID);
  Raw24Bitmap := ABuddy.AvatarData;
  if Length(Raw24Bitmap) = 12288 then begin;
    Raw8Alpha := ABuddy.AvatarAlphaData;
    HasAlpha := (Length(Raw8Alpha) = 4096);

    // we will now create a TFPMemoryImage from our
    // bitmap and use that to convert it into PNG
    Image := TFPMemoryImage.create(64, 64);
    PtrPixel24 := @Raw24Bitmap[1];
    if HasAlpha then PtrAlpha8 := @Raw8Alpha[1];
    for Y := 0 to 63 do begin
      for X := 0 to 63 do begin
        Pixel.red := PtrPixel24^.Red shl 8;
        Pixel.green := PtrPixel24^.Green shl 8;
        Pixel.blue := PtrPixel24^.Blue shl 8;
        Inc(PtrPixel24);
        if HasAlpha then begin
          Pixel.alpha := PtrAlpha8^ shl 8;
          Inc(PtrAlpha8);
        end
        else
          Pixel.alpha := $ffff;     // 100% opaque
        Image.Colors[x,y] := Pixel;
      end;
    end;
    ImageWriter := TFPWriterPNG.create;
    ImageWriter.UseAlpha := HasAlpha;
    ImageWriter.Indexed := False;
    ImageWriter.WordSized := False;
    ImageStream := TMemoryStream.Create;
    Image.SaveToStream(ImageStream, ImageWriter);

    icon_len := ImageStream.Size;
    icon_data := PurpleGetMem(icon_len);
    Move(ImageStream.Memory^, icon_data^, icon_len);

    // libpurple accepts data in PNG format (and possibly many
    // other formats too (its actually handled by the GUI and not
    // by libpurple, libpurple only stores and manages the data
    // without caring what it is). PNG is fine for our purposes
    // because it is well supported and can handle transparency.
    WriteLn(_F('%s setting avatar in libpurple', [ABuddy.ID]));
    purple_buddy_icons_set_for_user(
      purple_account,
      buddy_name,
      icon_data,
      icon_len,
      nil
    );

    ImageStream.Free;
    ImageWriter.Free;
    Image.Free;
  end
  else begin // empty avatar
    WriteLn(_F('%s removing avatar in libpurple', [ABuddy.ID]));
    purple_buddy_icons_set_for_user(
      purple_account,
      buddy_name,
      nil,
      0,
      nil
    );
  end;
  FreeMem(buddy_name);
end;

procedure TTorChat.OnBuddyAdded(ABuddy: IBuddy);
var
  group_name: PChar;
  buddy_name: PChar;
  buddy_alias: PChar;
  purple_group: PPurpleGroup;
  purple_buddy: PPurpleBuddy;
begin
  if not HSNameOk then exit; // because we don't have a group name yet
  buddy_name := GetMemAndCopy(ABuddy.ID);
  buddy_alias := GetMemAndCopy(ABuddy.LocalAlias);
  group_name := GetMemAndCopy(Roster.GroupName);
  if not assigned(purple_find_buddy(purple_account, buddy_name)) then begin
    purple_group := purple_find_group(group_name);
    if not Assigned(purple_group) then begin
      purple_group := purple_group_new(group_name);
      purple_blist_add_group(purple_group, nil);
    end;
    purple_buddy := purple_buddy_new(purple_account, buddy_name, buddy_alias);
    purple_blist_add_buddy(purple_buddy, nil, purple_group, nil);
  end;
  FreeMem(buddy_alias);
  FreeMem(buddy_name);
  FreeMem(group_name);
end;

procedure TTorChat.OnBuddyRemoved(ABuddy: IBuddy);
var
  buddy_name: PChar;
  purple_buddy: PPurpleBuddy;
begin
  buddy_name := GetMemAndCopy(ABuddy.ID);
  purple_buddy := purple_find_buddy(purple_account, buddy_name);
  if Assigned(purple_buddy) then begin
    purple_blist_remove_buddy(purple_buddy);
  end;
  FreeMem(buddy_name);
  {$note must do something when an IM window is currently open}
end;

procedure TTorChat.OnInstantMessage(ABuddy: IBuddy; AText: String);
begin
  serv_got_im(
    purple_account^.gc,
    PChar(ABuddy.ID),
    Pchar(Plain2Html(AText)),
    PURPLE_MESSAGE_RECV,
    NowUTCUnix
  );
end;

procedure TTorChat.OnIncomingFileTransfer(ABuddy: IBuddy; AID: String; AFileName: String; AFileSize: UInt64; ABlockSize: Integer);
begin

end;

procedure Init;
begin
  SOFTWARE_NAME := 'libpurple/TorChat'; // for the 'client' message

  with plugin_info do begin
    magic := PURPLE_PLUGIN_MAGIC;
    major_version := PURPLE_MAJOR_VERSION;
    minor_version := PURPLE_MINOR_VERSION;
    plugintype := PURPLE_PLUGIN_PROTOCOL;
    priority := PURPLE_PRIORITY_DEFAULT;
    id := 'prpl-prof7bit-torchat';
    name := 'TorChat';
    version := PChar(SOFTWARE_VERSION);
    summary := 'TorChat Protocol';
    description := 'TorChat protocol plugin for libpurple / Pidgin';
    author := 'Bernd Kreuss <prof7bit@gmail.com>';
    homepage := 'https://github.com/prof7bit/TorChat';
    load := @torchat_load;
    unload := @torchat_unload;
    actions := @torchat_actions;
    extra_info := @plugin_protocol_info;
  end;

  with plugin_protocol_info do begin
    options := OPT_PROTO_NO_PASSWORD;
    with icon_spec do begin
      format := 'png';
      min_height := 64;
      min_width := 64;
      scale_rules := PURPLE_ICON_SCALE_SEND;
    end;
    list_icon := @torchat_list_icon;
    status_types := @torchat_status_types;
    get_account_text_table := @torchat_get_text_table;
    login := @torchat_login;
    close := @torchat_close;
    set_status := @torchat_set_status;
    set_buddy_icon := @torchat_set_buddy_icon;
    add_buddy := @torchat_add_buddy;
    remove_buddy := @torchat_remove_buddy;
    alias_buddy := @torchat_alias_buddy;
    tooltip_text := @torchat_tooltip_text;
    send_im := @torchat_send_im;
    can_receive_file := @torchat_can_receive_file;
    send_file := @torchat_send_file;
    struct_size := SizeOf(TPurplePluginProtocolInfo);
  end;

  //// add additional fields to the settings dialog
  //TorPath := PChar(DefaultPathTorExe);
  //acc_opt := purple_account_option_string_new('Tor binary', 'tor', TorPath);
  //plugin_protocol_info.protocol_options := g_list_append(nil, acc_opt);

  {$ifdef UseHeapTrc}
    WriteLn('W plugin has been compiled with -dUseHeapTrc. Not recommended.');
  {$endif}
end;

exports
  purple_init_plugin;

initialization
  Init;
end.

{ Things happen in the following order:

    * purple loads this library, unit initialization sections will execute:
      + WriteLn redirection will be installed (by purplehelper.pas)
      + Init() procedure is exected (PluginInfo and PluginProtocolInfo
        will be initialized, see above)

    * libpurple calls purple_init_plugin() (implementd in purple.pas):
      + PluginInfo records are passed to purple, registration complete.

    * torchat_load() callback is called by purple

    the above happens only once during application start.
    Then for every account (TorChat profile) that is configured in
    Pidgin and activated it will call the login function:

    * torchat_login() once for every account when going online
    * torchat_close() once for every account when going offline

    when unloading the plugin (on pidgin shutdown) it will

    * switch all accounts to offline (call the above mentioned
      torchat_close() for every account that is currrently online)

    * torchat_unload() which will be the last function it will
      ever call, after this it will unload the library.

}
