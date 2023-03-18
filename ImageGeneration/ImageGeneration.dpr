program ImageGeneration;

uses
  Vcl.Forms,
  frmImageGenWindow in 'frmImageGenWindow.pas' {frmImageGenerator},
  OpenAI in '..\Libs\OpenAI.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmImageGenerator, frmImageGenerator);
  Application.Run;
end.
