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
  uAmazon.Polly in '..\Libs\TextToSpeech\uAmazon.Polly.pas',
  uBaseSpeech in '..\Libs\TextToSpeech\uBaseSpeech.pas',
  uElevenLabs.REST in '..\Libs\TextToSpeech\uElevenLabs.REST.pas',
  uElevenLabs.Voices.DTO in '..\Libs\TextToSpeech\uElevenLabs.Voices.DTO.pas',
  uGoogleSpeech.DTO in '..\Libs\TextToSpeech\uGoogleSpeech.DTO.pas',
  uGoogleSpeech in '..\Libs\TextToSpeech\uGoogleSpeech.pas',
  uMicrosoft.Cognitive.REST in '..\Libs\TextToSpeech\uMicrosoft.Cognitive.REST.pas',
  uMicrosoft.Cognitive.Voices.DTO in '..\Libs\TextToSpeech\uMicrosoft.Cognitive.Voices.DTO.pas',
  uWindows.Engine in '..\Libs\TextToSpeech\uWindows.Engine.pas',
  OpenAI in '..\Libs\OpenAI.pas',
  REST.Authenticator.EnhancedOAuth in '..\Libs\REST.Authenticator.EnhancedOAuth.pas',
  uGPT in '..\Libs\uGPT.pas',
  uDALLe2.DTO in '..\Libs\uDALLe2.DTO.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmWeatherWindow, frmWeatherWindow);
  Application.CreateForm(TdmWeather, dmWeather);
  Application.Run;
end.
