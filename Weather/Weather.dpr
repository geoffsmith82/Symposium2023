program Weather;

uses
  Vcl.Forms,
  frmWeatherMain in 'frmWeatherMain.pas' {frmWeatherWindow},
  udmWeather in 'udmWeather.pas' {dmWeather: TDataModule},
  uXMLBOMPrecis in 'uXMLBOMPrecis.pas',
  SpeechLib_TLB in '..\Libs\Speech\SpeechLib_TLB.pas',
  uAmazon.Polly in '..\Libs\Speech\uAmazon.Polly.pas',
  uBaseSpeech in '..\Libs\Speech\uBaseSpeech.pas',
  uElevenLabs.REST in '..\Libs\Speech\uElevenLabs.REST.pas',
  uElevenLabs.Voices.DTO in '..\Libs\Speech\uElevenLabs.Voices.DTO.pas',
  uGoogleSpeech.DTO in '..\Libs\Speech\uGoogleSpeech.DTO.pas',
  uGoogleSpeech in '..\Libs\Speech\uGoogleSpeech.pas',
  uMicrosoft.Cognitive.REST in '..\Libs\Speech\uMicrosoft.Cognitive.REST.pas',
  uMicrosoft.Cognitive.Voices.DTO in '..\Libs\Speech\uMicrosoft.Cognitive.Voices.DTO.pas',
  uWindows.Engine in '..\Libs\Speech\uWindows.Engine.pas',
  OpenAI in '..\Libs\OpenAI.pas',
  REST.Authenticator.EnhancedOAuth in '..\Libs\REST.Authenticator.EnhancedOAuth.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmWeatherWindow, frmWeatherWindow);
  Application.CreateForm(TdmWeather, dmWeather);
  Application.Run;
end.
