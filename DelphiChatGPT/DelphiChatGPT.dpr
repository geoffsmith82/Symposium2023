program DelphiChatGPT;

uses
  Vcl.Forms,
  REST.Authenticator.EnhancedOAuth in 'REST.Authenticator.EnhancedOAuth.pas',
  uElevenLabs.Voices.DTO in 'Speech\uElevenLabs.Voices.DTO.pas',
  uElevenLabs.REST in 'Speech\uElevenLabs.REST.pas',
  OpenAI in 'OpenAI.pas',
  uBaseSpeech in 'Speech\uBaseSpeech.pas',
  uMicrosoft.Cognitive.REST in 'Speech\uMicrosoft.Cognitive.REST.pas',
  uAmazon.Polly in 'Speech\uAmazon.Polly.pas',
  uMicrosoft.Cognitive.Voices.DTO in 'Speech\uMicrosoft.Cognitive.Voices.DTO.pas',
  uGoogleSpeech.DTO in 'Speech\uGoogleSpeech.DTO.pas',
  uGoogleSpeech in 'Speech\uGoogleSpeech.pas',
  uWindows.Engine in 'Speech\uWindows.Engine.pas',
  SpeechLib_TLB in 'Speech\SpeechLib_TLB.pas',
  UChatGPT in 'UChatGPT.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

