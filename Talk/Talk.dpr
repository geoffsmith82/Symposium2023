program Talk;

uses
  System.StartUpCopy,
  FMX.Forms,
  ufrmTalk in 'ufrmTalk.pas' {frmTalk},
  uTTS.Amazon.Polly in '..\Libs\TextToSpeech\uTTS.Amazon.Polly.pas',
  uTTS.Coqui in '..\Libs\TextToSpeech\uTTS.Coqui.pas',
  uTTS.ElevenLabs in '..\Libs\TextToSpeech\uTTS.ElevenLabs.pas',
  uTTS.GoogleSpeech.DTO in '..\Libs\TextToSpeech\uTTS.GoogleSpeech.DTO.pas',
  uTTS.GoogleSpeech in '..\Libs\TextToSpeech\uTTS.GoogleSpeech.pas',
  uTTS.Microsoft.Cognitive in '..\Libs\TextToSpeech\uTTS.Microsoft.Cognitive.pas',
  uTTS.Microsoft.Cognitive.Voices.DTO in '..\Libs\TextToSpeech\uTTS.Microsoft.Cognitive.Voices.DTO.pas',
  uTTS.OpenAI in '..\Libs\TextToSpeech\uTTS.OpenAI.pas',
  uTTS in '..\Libs\TextToSpeech\uTTS.pas',
  uTTS.Windows.Engine in '..\Libs\TextToSpeech\uTTS.Windows.Engine.pas',
  REST.Authenticator.EnhancedOAuth in '..\Libs\REST.Authenticator.EnhancedOAuth.pas',
  SpeechLib_TLB in '..\Libs\TextToSpeech\SpeechLib_TLB.pas',
  ApiKeyStore in '..\Libs\ApiKeyStore\ApiKeyStore.pas',
  FMX.frmApiKeyStore in '..\Libs\ApiKeyStore\FMX.frmApiKeyStore.pas' {frmApiKeyStores},
  uAPIKeyNameList in '..\Libs\ApiKeyStore\uAPIKeyNameList.pas',
  ApiKeyStore.Windows in '..\Libs\ApiKeyStore\ApiKeyStore.Windows.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmTalk, frmTalk);
  Application.Run;
end.


