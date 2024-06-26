program Weather;

uses
  {$IFDEF EurekaLog}
  EMemLeaks,
  EResLeaks,
  EDebugExports,
  EDebugJCL,
  EFixSafeCallException,
  EMapWin32,
  EAppVCL,
  EDialogWinAPIMSClassic,
  EDialogWinAPIEurekaLogDetailed,
  EDialogWinAPIStepsToReproduce,
  ExceptionLog7,
  {$ENDIF EurekaLog}
  Vcl.Forms,
  frmWeatherMain in 'frmWeatherMain.pas' {frmWeatherWindow},
  udmWeather in 'udmWeather.pas' {dmWeather: TDataModule},
  uXMLBOMPrecis in 'uXMLBOMPrecis.pas',
  SpeechLib_TLB in '..\Libs\TextToSpeech\SpeechLib_TLB.pas',
  uTTS.Amazon.Polly in '..\Libs\TextToSpeech\uTTS.Amazon.Polly.pas',
  uTTS in '..\Libs\TextToSpeech\uTTS.pas',
  uTTS.ElevenLabs in '..\Libs\TextToSpeech\uTTS.ElevenLabs.pas',
  uElevenLabs.Voices.DTO in '..\Libs\TextToSpeech\uElevenLabs.Voices.DTO.pas',
  uTTS.GoogleSpeech.DTO in '..\Libs\TextToSpeech\uTTS.GoogleSpeech.DTO.pas',
  uTTS.GoogleSpeech in '..\Libs\TextToSpeech\uTTS.GoogleSpeech.pas',
  uTTS.Microsoft.Cognitive in '..\Libs\TextToSpeech\uTTS.Microsoft.Cognitive.pas',
  uTTS.Microsoft.Cognitive.Voices.DTO in '..\Libs\TextToSpeech\uTTS.Microsoft.Cognitive.Voices.DTO.pas',
  uTTS.Windows.Engine in '..\Libs\TextToSpeech\uTTS.Windows.Engine.pas',
  uLLM.OpenAI in '..\Libs\LLM\uLLM.OpenAI.pas',
  REST.Authenticator.EnhancedOAuth in '..\Libs\REST.Authenticator.EnhancedOAuth.pas',
  uLLM in '..\Libs\LLM\uLLM.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmWeatherWindow, frmWeatherWindow);
  Application.Run;
end.

