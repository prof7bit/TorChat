{ TorChat - TTor manages TorChat's own Tor process

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
unit torprocess;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process, clientconfig, FileUtil;

type

  { TTor }

  TTor = class(TProcess)
    constructor Create(AOwner: TComponent); override;
  public
    destructor Destroy; override;
  end;

implementation

{ TTor }

constructor TTor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Options := [poStderrToOutPut];
  CurrentDirectory := FileUtil.AppendPathDelim(ConfGetDataDir) + 'tor';
  Executable := ConfGetTorExe;
  Parameters.Add('-f');
  Parameters.Add('torrc.txt');
  Execute;
end;

destructor TTor.Destroy;
begin
  Terminate(0);
  inherited Destroy;
end;

end.

