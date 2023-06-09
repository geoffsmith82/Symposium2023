program TranscribeAudio;

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
  frmTranscribeAudio in 'frmTranscribeAudio.pas' {VoiceRecognitionForm},
  uBaseSpeechToText in '..\Libs\SpeechToText\uBaseSpeechToText.pas',
  uMicrosoft.SpeechToText in '..\Libs\SpeechToText\uMicrosoft.SpeechToText.pas',
  uOpenAI.Whisper.Online.SpeechToText in '..\Libs\SpeechToText\uOpenAI.Whisper.Online.SpeechToText.pas',
  uGoogle.SpeechToText in '..\Libs\SpeechToText\uGoogle.SpeechToText.pas',
  uAmazon.SpeechToText in '..\Libs\SpeechToText\uAmazon.SpeechToText.pas',
  REST.Authenticator.EnhancedOAuth in '..\Libs\REST.Authenticator.EnhancedOAuth.pas',
  uEngineManager in '..\Libs\uEngineManager.pas',
  uGoogle.SpeechToText.DTO in '..\Libs\SpeechToText\uGoogle.SpeechToText.DTO.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TVoiceRecognitionForm, VoiceRecognitionForm);
  Application.Run;
end.
