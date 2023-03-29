unit uAmazon.SpeechToText;

interface

uses uBaseSpeechToText;

type
  TAmazonSpeechToText = class(TBaseSpeechToText)
  public
    function TranscribeAudio(const FilePath, ModelName: string): string; override;
    function SpeechEngineName: string; override;
  end;

implementation

{ TAmazonSpeechToText }

function TAmazonSpeechToText.TranscribeAudio(const FilePath, ModelName: string): string;
begin

end;

function TAmazonSpeechToText.SpeechEngineName: string;
begin
  Result := 'AmazonSpeech';
end;

end.
