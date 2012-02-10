unit networking;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TSocksConnection }

  TSocksConnection = class(TObject)
    constructor Create(AProxy: String; AProxyPort: DWord; AAddress: String; APort: DWord);
    destructor Destroy; override;
  strict private
    FProxy: String;
    FProxyPort: DWord;
    FAddress: String;
    FPort: DWord;
  end;

implementation

{ TSocksConnection }

constructor TSocksConnection.Create(AProxy: String; AProxyPort: DWord; AAddress: String; APort: DWord);
begin
  inherited Create;
  FProxy := AProxy;
  FProxyPort := AProxyPort;
  FAddress := AAddress;
  FPort := APort;
end;

destructor TSocksConnection.Destroy;
begin
  inherited Destroy;
end;

end.

