unit uWindows.Engine;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Win.ComObj,
  Winapi.ActiveX,
  uBaseSpeech,
  SpeechLib_TLB
  ;

type
  TWindowsSpeechService = class(TBaseTextToSpeech)
  public
    function TextToSpeech(text: string; VoiceName: string = ''): TMemoryStream; override;
    function SpeechEngineName: string; override;
  end;

implementation

{ TWindowsSpeechService }


function TWindowsSpeechService.SpeechEngineName: string;
begin
  Result := 'WindowsVoice';
end;

function TWindowsSpeechService.TextToSpeech(text, VoiceName: string): TMemoryStream;
var
  Voice: OleVariant;
begin
  Voice := CreateOleObject('SAPI.SpVoice');
  Voice.Speak(text, 0);
  Result := nil;
end;

end.
