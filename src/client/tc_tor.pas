{ TorChat - TTor manages TorChat's own Tor process

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
unit tc_tor;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  process,
  tc_interface;

type

  { TTor }

  TTor = class(TProcess)
  strict protected
    FClient: IClient;
  public
    constructor Create(AOwner: TComponent; AClient: IClient); reintroduce;
    destructor Destroy; override;
  end;

implementation

{ TTor }

constructor TTor.Create(AOwner: TComponent; AClient: IClient);
begin
  inherited Create(AOwner);
  FClient := AClient;
  Options := [poStderrToOutPut];
  CurrentDirectory := ConcatPaths([FClient.Config.DataDir, 'tor']);
  Executable := FClient.Config.PathTorExe;
  Parameters.Add('-f');
  Parameters.Add('torrc.txt');
  try
    Execute;
  except
    on E: Exception do begin
      writeln('E could not start Tor process: ' + Executable);
      writeln('E ' + E.Message);
    end;
  end;
end;

destructor TTor.Destroy;
begin
  Terminate(0);
  inherited Destroy;
end;

end.

