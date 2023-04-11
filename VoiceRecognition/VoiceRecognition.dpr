program VoiceRecognition;

uses
  Vcl.Forms,
  ufrmVoiceRecognition in 'ufrmVoiceRecognition.pas' {frmVoiceRecognition},
  OpenAI in '..\Libs\OpenAI.pas',
  uDALLe2.DTO in '..\Libs\uDALLe2.DTO.pas',
  uWindows.Engine in '..\Libs\TextToSpeech\uWindows.Engine.pas',
  uMicrosoft.Cognitive.Voices.DTO in '..\Libs\TextToSpeech\uMicrosoft.Cognitive.Voices.DTO.pas',
  uMicrosoft.Cognitive.REST in '..\Libs\TextToSpeech\uMicrosoft.Cognitive.REST.pas',
  uGoogleSpeech in '..\Libs\TextToSpeech\uGoogleSpeech.pas',
  uGoogleSpeech.DTO in '..\Libs\TextToSpeech\uGoogleSpeech.DTO.pas',
  uElevenLabs.Voices.DTO in '..\Libs\TextToSpeech\uElevenLabs.Voices.DTO.pas',
  uElevenLabs.REST in '..\Libs\TextToSpeech\uElevenLabs.REST.pas',
  uBaseSpeech in '..\Libs\TextToSpeech\uBaseSpeech.pas',
  uAmazon.Polly in '..\Libs\TextToSpeech\uAmazon.Polly.pas',
  SpeechLib_TLB in '..\Libs\TextToSpeech\SpeechLib_TLB.pas',
  uAssemblyAI.SpeechToText in '..\Libs\SpeechToText\uAssemblyAI.SpeechToText.pas',
  uDeepGram.SpeechToText in '..\Libs\SpeechToText\uDeepGram.SpeechToText.pas',
  REST.Authenticator.EnhancedOAuth in '..\Libs\REST.Authenticator.EnhancedOAuth.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmVoiceRecognition, frmVoiceRecognition);
  Application.Run;
end.
