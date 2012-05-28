uses
  Classes, BaseUnix;

function SpawnFCGIProcess(App, Enviro: string; const aPort: Word): Integer;
var
  TheSocket: TLSocket;
  i: Integer;
  SL: TStringList;
  aNil: Pointer = nil;
  ppEnv, ppArgs: ppChar;
begin
  Result:=FpFork;

  if Result = 0 then begin
    ppArgs:=@aNil;

    for i:=3 to 10000 do
      CloseSocket(i);

    if CloseSocket(StdInputHandle) <> 0 then
      Exit(LSocketError);

    TheSocket:=TLSocket.Create;
    TheSocket.SetState(ssBlocking);

    if not TheSocket.Listen(aPort) then
      Exit(LSocketError);

    ppEnv:=@aNil;

    if Length(Enviro) > 0 then begin
      SL:=TStringList.Create;
      repeat
        i:=Pos(':', Enviro);
        if i > 0 then begin
          SL.Add(Copy(Enviro, 1, i - 1));
          Delete(Enviro, 1, i);
        end else
          SL.Add(Enviro);
      until i = 0;
      GetMem(ppEnv, SizeOf(pChar) * (SL.Count + 1));
      for i:=0 to SL.Count-1 do
        ppEnv[i]:=pChar(SL[i]);
      ppEnv[SL.Count]:=nil;
    end;
    
    FpExecve(pChar(App), ppArgs, ppEnv);
  end else if Result > 0 then
    Result:=0; // it went ok
end;

