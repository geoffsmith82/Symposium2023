unit uAmazon.SpeechToText;

interface

uses uBaseSpeechToText;

type
  TAmazonSpeechToText = class(TBaseSpeechToText)
  public
    function SupportedFormats: TArray<string>; override;
    function TranscribeAudio(const FilePath, ModelName: string): string; override;
  end;

implementation

{ TAmazonSpeechToText }

function TAmazonSpeechToText.SupportedFormats: TArray<string>;
begin
  SetLength(Result, 1);
  Result[0] := 'wav';
end;
function TAmazonSpeechToText.TranscribeAudio(const FilePath, ModelName: string): string;
begin

end;

end.
