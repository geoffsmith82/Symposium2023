program VoiceDetection;

uses
  Vcl.Forms,
  frmVoiceDetection in 'frmVoiceDetection.pas' {VoiceDetectionForm},
  Picovoice in '..\Picovoice.pas',
  Pv_Porcupine in '..\Pv_Porcupine.pas',
  Pv_Recorder in '..\Pv_Recorder.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TVoiceDetectionForm, VoiceDetectionForm);
  Application.Run;
end.
