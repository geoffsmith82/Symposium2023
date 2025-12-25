program StreamingTest;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  uLLM.Streaming.OpenAI in 'uLLM.Streaming.OpenAI.pas',
  uAPIKeyNameList in '..\Libs\ApiKeyStore\uAPIKeyNameList.pas',
  frmApiKeyStore in '..\Libs\ApiKeyStore\frmApiKeyStore.pas' {frmApiKeyStores},
  ApiKeyStore.Windows in '..\Libs\ApiKeyStore\ApiKeyStore.Windows.pas',
  ApiKeyStore in '..\Libs\ApiKeyStore\ApiKeyStore.pas',
  uLLM in '..\Libs\LLM\uLLM.pas',
  uAttributes in '..\Libs\LLM\uAttributes.pas',
  uLLM.Functions in '..\Libs\LLM\uLLM.Functions.pas',
  uLLM.OpenAI in '..\Libs\LLM\uLLM.OpenAI.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
