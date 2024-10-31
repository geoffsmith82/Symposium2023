program EmbeddingsDemo;

uses
  Vcl.Forms,
  ufrmEmbeddings in 'ufrmEmbeddings.pas' {frmEmbeddings},
  uGoogleCustomSearch in '..\Libs\Search\uGoogleCustomSearch.pas',
  uLLM.OpenAI in '..\Libs\LLM\uLLM.OpenAI.pas',
  uLLM in '..\Libs\LLM\uLLM.pas',
  udmEmbeddings in 'udmEmbeddings.pas' {dmEmbeddings: TDataModule},
  uLLM.Anthropic in '..\Libs\LLM\uLLM.Anthropic.pas',
  uLLM.Google.PaLM in '..\Libs\LLM\uLLM.Google.PaLM.pas',
  uLLM.Azure in '..\Libs\LLM\uLLM.Azure.pas',
  uEmbeddings in '..\Libs\Embeddings\uEmbeddings.pas',
  uEmbeddings.OpenAI in '..\Libs\Embeddings\uEmbeddings.OpenAI.pas',
  uEmbeddings.Microsoft.OpenAI in '..\Libs\Embeddings\uEmbeddings.Microsoft.OpenAI.pas',
  uAttributes in '..\Libs\LLM\uAttributes.pas',
  uLLM.Functions in '..\Libs\LLM\uLLM.Functions.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmEmbeddings, frmEmbeddings);
  Application.CreateForm(TdmEmbeddings, dmEmbeddings);
  Application.Run;
end.
