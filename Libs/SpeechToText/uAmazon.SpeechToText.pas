unit uAmazon.SpeechToText;

interface

uses uBaseSpeechToText;

type
  TAmazonSpeechToText = class(TBaseSpeechToText)

  public
    function TranscribeAudio(const FilePath, ModelName: string): string; override;
  end;

implementation

{ TAmazonSpeechToText }

function TAmazonSpeechToText.TranscribeAudio(const FilePath, ModelName: string): string;
begin

end;

end.
