unit uBaseSpeechToText;

interface

type
  TBaseSpeechToText = class
  protected
    FResourceKey : string;
  public
    function TranscribeAudio(const FilePath, ModelName: string): string; virtual; abstract;
    constructor Create(const AResourceKey: string; const AApplicationName: string; const AHost: string);
  end;

implementation

{ TBaseSpeechToText }

constructor TBaseSpeechToText.Create(const AResourceKey, AApplicationName, AHost: string);
begin
  FResourceKey := AResourceKey;
end;

end.
