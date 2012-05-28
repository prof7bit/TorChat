{ Asynchronous process support

  Copyright (C) 2006-2008 Micha Nelissen

  This library is Free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is diStributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; withOut even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a Copy of the GNU Library General Public License
  along with This library; if not, Write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
  
  This license has been modified. See file LICENSE.ADDON for more information.
  Should you find these sources without a LICENSE File, please contact
  me at ales@chello.sk
}

unit lProcess;

{$mode objfpc}{$h+}

interface

uses
  sysutils, classes, process, levents, pipes;

type
  TLInputPipeStream = class(TInputPipeStream)
  protected
    FEvent: TLHandle;
  public
    function Read(var Buffer; Count: longint): longint; override;
  end;

  TLOutputPipeStream = class(TOutputPipeStream)
  protected
    FEvent: TLHandle;
  public
    function Write(const Buffer; Count: longint): longint; override;          
  end;

  TLProcess = class(TProcess)
  protected
    FInputEvent: TLHandle;
    FOutputEvent: TLHandle;
    FStderrEvent: TLHandle;
    FEventer: TLEventer;

    function  GetOnNeedInput: TLHandleEvent;
    function  GetOnHasOutput: TLHandleEvent;
    function  GetOnHasStderr: TLHandleEvent;
    procedure SetOnNeedInput(NewOnInput: TLHandleEvent);
    procedure SetOnHasOutput(NewOnOutput: TLHandleEvent);
    procedure SetOnHasStderr(NewOnStderr: TLHandleEvent);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    
    procedure CloseInput; override;
    procedure CloseOutput; override;
    procedure CloseStderr; override;
    procedure Execute; override;

    property InputEvent: TLHandle read FInputEvent;
    property OutputEvent: TLHandle read FOutputEvent;
    property StderrEvent: TLHandle read FStderrEvent;
    property Eventer: TLEventer read FEventer write FEventer;
    property OnNeedInput: TLHandleEvent read GetOnNeedInput write SetOnNeedInput;
    property OnHasOutput: TLHandleEvent read GetOnHasOutput write SetOnHasOutput;
    property OnHasStderr: TLHandleEvent read GetOnHasStderr write SetOnHasStderr;
  end;

implementation

function TLInputPipeStream.Read(var Buffer; Count: longint): longint;
begin
  Result := inherited;
  FEvent.IgnoreRead := false;
end;

function TLOutputPipeStream.Write(const Buffer; Count: longint): longint;
begin
  Result := inherited;
  FEvent.IgnoreWrite := false;
end;

constructor TLProcess.Create(AOwner: TComponent);
begin
  inherited;

  FInputEvent := TLHandle.Create;
  FOutputEvent := TLHandle.Create;
  FStderrEvent := TLHandle.Create;
end;

destructor TLProcess.Destroy;
begin
  inherited;
  FInputEvent.Free;
  FOutputEvent.Free;
  FStderrEvent.Free;
end;

procedure TLProcess.CloseInput;
begin
  FEventer.UnplugHandle(FInputEvent);
  inherited;
end;

procedure TLProcess.CloseOutput;
begin
  FEventer.UnplugHandle(FOutputEvent);
  inherited;
end;

procedure TLProcess.CloseStderr;
begin
  FEventer.UnplugHandle(FStderrEvent);
  inherited;
end;

procedure TLProcess.Execute;
begin
  inherited;

  if (poUsePipes in Options) and (FEventer <> nil) then
  begin
    if Input <> nil then
    begin
      FInputEvent.Handle := Input.Handle;
      FInputEvent.IgnoreRead := true;
      FEventer.AddHandle(FInputEvent);
    end;
    if Output <> nil then
    begin
      FOutputEvent.Handle := Output.Handle;
      FOutputEvent.IgnoreWrite := true;
      FEventer.AddHandle(FOutputEvent);
    end;
    if Stderr <> nil then
    begin
      FStderrEvent.Handle := Stderr.Handle;
      FStderrEvent.IgnoreWrite := true;
      FEventer.AddHandle(FStderrEvent);
    end;
  end;
end;

function TLProcess.GetOnNeedInput: TLHandleEvent;
begin
  Result := FInputEvent.OnWrite;
end;

function TLProcess.GetOnHasOutput: TLHandleEvent;
begin
  Result := FOutputEvent.OnRead;
end;

function TLProcess.GetOnHasStderr: TLHandleEvent;
begin
  Result := FStderrEvent.OnRead;
end;

procedure TLProcess.SetOnNeedInput(NewOnInput: TLHandleEvent);
begin
  FInputEvent.OnWrite := NewOnInput;
end;

procedure TLProcess.SetOnHasOutput(NewOnOutput: TLHandleEvent);
begin
  FOutputEvent.OnRead := NewOnOutput;
end;

procedure TLProcess.SetOnHasStderr(NewOnStderr: TLHandleEvent);
begin
  FStderrEvent.OnRead := NewOnStderr;
end;

end.
