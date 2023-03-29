unit uBaseSpeechToText;

interface

type
  TBaseSpeechToText = class

  public
    function TranscribeAudio(const FilePath, ModelName: string): string; virtual; abstract;
    function SpeechEngineName: string; virtual; abstract;
  end;

implementation

end.
