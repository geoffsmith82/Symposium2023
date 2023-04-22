program EmbeddingsDemo;

uses
  Vcl.Forms,
  frmEmbeddings in 'frmEmbeddings.pas' {Form4},
  uGoogleCustomSearch in '..\Libs\Search\uGoogleCustomSearch.pas',
  OpenAI in '..\Libs\OpenAI.pas',
  uDALLe2.DTO in '..\Libs\uDALLe2.DTO.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
