program EmbeddingsDemo;

uses
  Vcl.Forms,
  ufrmEmbeddings in 'ufrmEmbeddings.pas' {frmEmbeddings},
  uGoogleCustomSearch in '..\Libs\Search\uGoogleCustomSearch.pas',
  OpenAI in '..\Libs\OpenAI.pas',
  uLLM in '..\Libs\uLLM.pas',
  udmEmbeddings in 'udmEmbeddings.pas' {dmEmbeddings: TDataModule},
  uDALLe2.DTO in '..\Libs\uDALLe2.DTO.pas',
  uAnthropic in '..\Libs\uAnthropic.pas',
  uGoogle.PaLM in '..\Libs\uGoogle.PaLM.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmEmbeddings, frmEmbeddings);
  Application.CreateForm(TdmEmbeddings, dmEmbeddings);
  Application.Run;
end.
