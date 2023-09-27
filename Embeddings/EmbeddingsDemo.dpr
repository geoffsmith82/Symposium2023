program EmbeddingsDemo;

uses
  Vcl.Forms,
  ufrmEmbeddings in 'ufrmEmbeddings.pas' {frmEmbeddings},
  uGoogleCustomSearch in '..\Libs\Search\uGoogleCustomSearch.pas',
  OpenAI in '..\Libs\LLM\OpenAI.pas',
  uLLM in '..\Libs\LLM\uLLM.pas',
  udmEmbeddings in 'udmEmbeddings.pas' {dmEmbeddings: TDataModule},
  uDALLe2.DTO in '..\Libs\uDALLe2.DTO.pas',
  uAnthropic in '..\Libs\LLM\uAnthropic.pas',
  uGoogle.PaLM in '..\Libs\LLM\uGoogle.PaLM.pas',
  uAzureGPT in '..\Libs\LLM\uAzureGPT.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmEmbeddings, frmEmbeddings);
  Application.CreateForm(TdmEmbeddings, dmEmbeddings);
  Application.Run;
end.
