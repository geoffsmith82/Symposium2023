program DelphiChatGPT;

uses
  Vcl.Forms,
  UChatGPT in 'UChatGPT.pas' {Form1},
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
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

