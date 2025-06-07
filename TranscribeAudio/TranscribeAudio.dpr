program TranscribeAudio;

uses
  Vcl.Forms,
  frmTranscribeAudio in 'frmTranscribeAudio.pas' {VoiceRecognitionForm},
  uSTT in '..\Libs\SpeechToText\uSTT.pas',
  uSTT.Microsoft in '..\Libs\SpeechToText\uSTT.Microsoft.pas',
  uSTT.OpenAI.Whisper.Online in '..\Libs\SpeechToText\uSTT.OpenAI.Whisper.Online.pas',
  uSTT.Google in '..\Libs\SpeechToText\uSTT.Google.pas',
  uSTT.Amazon in '..\Libs\SpeechToText\uSTT.Amazon.pas',
  uSTT.Google.DTO in '..\Libs\SpeechToText\uSTT.Google.DTO.pas',
  REST.Authenticator.EnhancedOAuth in '..\Libs\REST.Authenticator.EnhancedOAuth.pas',
  uEngineManager in '..\Libs\uEngineManager.pas',
  ApiKeyStore in '..\Libs\ApiKeyStore\ApiKeyStore.pas',
  ApiKeyStore.Windows in '..\Libs\ApiKeyStore\ApiKeyStore.Windows.pas',
  frmApiKeyStore in '..\Libs\ApiKeyStore\frmApiKeyStore.pas' {frmApiKeyStores},
  uAPIKeyNameList in '..\Libs\ApiKeyStore\uAPIKeyNameList.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TVoiceRecognitionForm, VoiceRecognitionForm);
  Application.Run;
end.
