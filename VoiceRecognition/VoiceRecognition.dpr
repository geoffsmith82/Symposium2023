program VoiceRecognition;

uses
  Vcl.Forms,
  frmVoiceRecognition in 'frmVoiceRecognition.pas' {VoiceRecognitionForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TVoiceRecognitionForm, VoiceRecognitionForm);
  Application.Run;
end.
