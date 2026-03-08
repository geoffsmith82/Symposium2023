program StructuredOutputDemo;

uses
  Vcl.Forms,
  frmStructuredOutput in 'frmStructuredOutput.pas' {frmStructuredOutputDemo},
  uSchemaModels in 'uSchemaModels.pas',
  uLLM in '..\Libs\LLM\uLLM.pas',
  uLLM.Functions in '..\Libs\LLM\uLLM.Functions.pas',
  uLLM.OpenAI in '..\Libs\LLM\uLLM.OpenAI.pas',
  uLLM.Anthropic in '..\Libs\LLM\uLLM.Anthropic.pas',
  uLLM.Google.Gemini in '..\Libs\LLM\uLLM.Google.Gemini.pas',
  uLLM.DeepSeek in '..\Libs\LLM\uLLM.DeepSeek.pas',
  uLLM.Groq in '..\Libs\LLM\uLLM.Groq.pas',
  uLLM.Mistral in '..\Libs\LLM\uLLM.Mistral.pas',
  uLLM.X.Ai in '..\Libs\LLM\uLLM.X.Ai.pas',
  uLLM.OpenRouter in '..\Libs\LLM\uLLM.OpenRouter.pas',
  uAttributes in '..\Libs\LLM\uAttributes.pas',
  ApiKeyStore in '..\Libs\ApiKeyStore\ApiKeyStore.pas',
  ApiKeyStore.Windows in '..\Libs\ApiKeyStore\ApiKeyStore.Windows.pas',
  uAPIKeyNameList in '..\Libs\ApiKeyStore\uAPIKeyNameList.pas',
  frmApiKeyStore in '..\Libs\ApiKeyStore\frmApiKeyStore.pas' {frmApiKeyStores};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmStructuredOutputDemo, frmStructuredOutputDemo);
  Application.Run;
end.
