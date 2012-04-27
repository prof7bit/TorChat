{ TorChat - Connection class, representing a connection via Tor

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
unit connection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, torchatabstract, receiver;

type

  { THiddenConnection }
  THiddenConnection = class(TAHiddenConnection)
    constructor Create(AHandle: THandle; AAppRef: TComponent); override;
    procedure Send(AData: String); override;
    procedure SendLine(ALine: String); override;
    procedure OnConnectionClose; override;
    destructor Destroy; override;
  end;

implementation

{ THiddenConnection }

constructor THiddenConnection.Create(AHandle: THandle; AAppRef: TComponent);
begin
  inherited Create(AHandle, AAppRef);
  FClient := TAClient(AppRef);
  FReceiver := TReceiver.Create(Self);
  WriteLn('created connection');
end;

procedure THiddenConnection.Send(AData: String);
begin
  Write(AData[1], Length(AData));
end;

procedure THiddenConnection.SendLine(ALine: String);
begin
  Send(ALine + #10);
end;

procedure THiddenConnection.OnConnectionClose;
begin
  WriteLn('*** OnConnectionClose');
end;

destructor THiddenConnection.Destroy;
begin
  DoClose; // this will also let the receiver leave the blocking recv()
  FReceiver.Terminate;
  FReceiver.Free;
  inherited Destroy;
end;

end.

