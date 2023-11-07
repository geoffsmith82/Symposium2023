unit uSTT;

interface

uses
  System.IOUtils,
  System.SysUtils
  ;

type
  TBaseSpeechToText = class
  protected
    FResourceKey : string;
  public
    function TranscribeAudio(const FilePath, ModelName: string): string; virtual; abstract;
    function SupportedFormats(): TArray<string>; virtual; abstract;
    function IsFileSupported(const FilePath: string): Boolean;
    constructor Create(const AResourceKey: string; const AApplicationName: string; const AHost: string);
  end;

implementation

{ TBaseSpeechToText }

constructor TBaseSpeechToText.Create(const AResourceKey, AApplicationName, AHost: string);
begin
  FResourceKey := AResourceKey;
end;

function TBaseSpeechToText.IsFileSupported(const FilePath: string): Boolean;
var
  i : Integer;
  LFormats : TArray<string>;
  LExt : string;
begin
  Result := False;
  LFormats := SupportedFormats();
  LExt := TPath.GetExtension(FilePath).ToLower;
  for i := 0 to High(LFormats) do
  begin
    if ('.' + SupportedFormats[i].ToLower = LExt) then
      Exit(True);
  end;
end;

end.
