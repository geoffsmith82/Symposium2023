program Weather;

uses
  Vcl.Forms,
  frmWeatherMain in 'frmWeatherMain.pas' {frmWeatherWindow},
  SpeechLib_TLB in '..\Speech\SpeechLib_TLB.pas',
  uWindows.Engine in '..\Speech\uWindows.Engine.pas',
  uMicrosoft.Cognitive.Voices.DTO in '..\Speech\uMicrosoft.Cognitive.Voices.DTO.pas',
  uMicrosoft.Cognitive.REST in '..\Speech\uMicrosoft.Cognitive.REST.pas',
  uGoogleSpeech in '..\Speech\uGoogleSpeech.pas',
  uGoogleSpeech.DTO in '..\Speech\uGoogleSpeech.DTO.pas',
  uElevenLabs.Voices.DTO in '..\Speech\uElevenLabs.Voices.DTO.pas',
  uElevenLabs.REST in '..\Speech\uElevenLabs.REST.pas',
  uBaseSpeech in '..\Speech\uBaseSpeech.pas',
  uAmazon.Polly in '..\Speech\uAmazon.Polly.pas',
  OpenAI in '..\OpenAI.pas',
  udmWeather in 'udmWeather.pas' {dmWeather: TDataModule},
  uXMLBOMPrecis in 'uXMLBOMPrecis.pas',
  REST.Authenticator.EnhancedOAuth in '..\REST.Authenticator.EnhancedOAuth.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmWeatherWindow, frmWeatherWindow);
  Application.CreateForm(TdmWeather, dmWeather);
  Application.Run;
end.
