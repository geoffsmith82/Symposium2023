program VoiceRecognition;

uses
  Vcl.Forms,
  frmVoiceRecognition in 'frmVoiceRecognition.pas' {VoiceRecognitionForm},
  uBaseSpeechToText in '..\Libs\SpeechToText\uBaseSpeechToText.pas',
  uMicrosoft.SpeechToText in '..\Libs\SpeechToText\uMicrosoft.SpeechToText.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TVoiceRecognitionForm, VoiceRecognitionForm);
  Application.Run;
end.
