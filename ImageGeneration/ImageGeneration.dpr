program ImageGeneration;

uses
  Vcl.Forms,
  frmImageGenWindow in 'frmImageGenWindow.pas' {frmImageGenerator},
  uDALLe2.DTO in '..\Libs\ImageGeneration\uDALLe2.DTO.pas',
  uImageGeneration in '..\Libs\ImageGeneration\uImageGeneration.pas',
  uImageGeneration.OpenAI in '..\Libs\ImageGeneration\uImageGeneration.OpenAI.pas',
  uImageGeneration.Replicate in '..\Libs\ImageGeneration\uImageGeneration.Replicate.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmImageGenerator, frmImageGenerator);
  Application.Run;
end.
