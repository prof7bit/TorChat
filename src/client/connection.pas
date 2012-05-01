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
  Classes, SysUtils, torchatabstract, receiver, networking;

type

  { THiddenConnection }
  THiddenConnection = class(TAHiddenConnection)
    constructor Create(AClient: TAClient; AStream: TTCPStream);
    destructor Destroy; override;
    procedure Send(AData: String); override;
    procedure SendLine(ALine: String); override;
    procedure OnConnectionClose; override;
    procedure SetBuddy(ABuddy: TABuddy); override;
  end;

implementation

{ THiddenConnection }

constructor THiddenConnection.Create(AClient: TAClient; AStream: TTCPStream);
begin
  FTCPStream := AStream;
  FClient := AClient;
  FReceiver := TReceiver.Create(Self);
  WriteLn('created connection');
end;

procedure THiddenConnection.Send(AData: String);
begin
  FTCPStream.Write(AData[1], Length(AData));
end;

procedure THiddenConnection.SendLine(ALine: String);
begin
  Send(ALine + #10);
end;

procedure THiddenConnection.OnConnectionClose;
begin
  WriteLn('*** OnConnectionClose');
end;

procedure THiddenConnection.SetBuddy(ABuddy: TABuddy);
begin
  FBuddy := ABuddy;
end;

destructor THiddenConnection.Destroy;
begin
  FTCPStream.Free; // this will also let the receiver leave the blocking recv()
  FReceiver.Terminate;
  FReceiver.Free;
  inherited Destroy;
end;

end.

