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
  uSTT in '..\Libs\SpeechToText\uSTT.pas',
  uSTT.Microsoft in '..\Libs\SpeechToText\uSTT.Microsoft.pas',
  uSTT.OpenAI.Whisper.Online in '..\Libs\SpeechToText\uSTT.OpenAI.Whisper.Online.pas',
  uSTT.Google in '..\Libs\SpeechToText\uSTT.Google.pas',
  uSTT.Amazon in '..\Libs\SpeechToText\uSTT.Amazon.pas',
  uSTT.Google.DTO in '..\Libs\SpeechToText\uSTT.Google.DTO.pas',
  REST.Authenticator.EnhancedOAuth in '..\Libs\REST.Authenticator.EnhancedOAuth.pas',
  uEngineManager in '..\Libs\uEngineManager.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TVoiceRecognitionForm, VoiceRecognitionForm);
  Application.Run;
end.
