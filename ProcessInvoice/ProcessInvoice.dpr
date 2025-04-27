program ProcessInvoice;

uses
  Vcl.Forms,
  Unit2 in 'Unit2.pas' {Form2},
  uAttributes in '..\Libs\LLM\uAttributes.pas',
  uLLM.Functions in '..\Libs\LLM\uLLM.Functions.pas',
  uLLM.OpenAI.Assistants in '..\Libs\LLM\uLLM.OpenAI.Assistants.pas',
  uLLM.OpenAI in '..\Libs\LLM\uLLM.OpenAI.pas',
  uLLM in '..\Libs\LLM\uLLM.pas',
  ApiKeyStore in '..\Libs\ApiKeyStore\ApiKeyStore.pas',
  ApiKeyStore.Windows in '..\Libs\ApiKeyStore\ApiKeyStore.Windows.pas',
  uAPIKeyNameList in '..\Libs\ApiKeyStore\uAPIKeyNameList.pas',
  frmApiKeyStore in '..\Libs\ApiKeyStore\frmApiKeyStore.pas' {frmApiKeyStores};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
