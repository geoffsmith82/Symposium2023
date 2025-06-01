program DescribePicture;

uses
  System.StartUpCopy,
  FMX.Forms,
  frmDescribe in 'frmDescribe.pas' {frmDescribePicture},
  uAPIKeyNameList in '..\Libs\ApiKeyStore\uAPIKeyNameList.pas',
  FMX.frmApiKeyStore in '..\Libs\ApiKeyStore\FMX.frmApiKeyStore.pas' {frmApiKeyStores},
  ApiKeyStore.Windows in '..\Libs\ApiKeyStore\ApiKeyStore.Windows.pas',
  ApiKeyStore in '..\Libs\ApiKeyStore\ApiKeyStore.pas',
  ApiKeyStore.OSX in '..\Libs\ApiKeyStore\ApiKeyStore.OSX.pas',
  uAttributes in '..\Libs\LLM\uAttributes.pas',
  uLLM.Anthropic in '..\Libs\LLM\uLLM.Anthropic.pas',
  uLLM.Azure in '..\Libs\LLM\uLLM.Azure.pas',
  uLLM.Functions in '..\Libs\LLM\uLLM.Functions.pas',
  uLLM.Google.Gemini in '..\Libs\LLM\uLLM.Google.Gemini.pas',
  uLLM.Google.PaLM in '..\Libs\LLM\uLLM.Google.PaLM.pas',
  uLLM.Groq in '..\Libs\LLM\uLLM.Groq.pas',
  uLLM.HuggingFace in '..\Libs\LLM\uLLM.HuggingFace.pas',
  uLLM.Mistral in '..\Libs\LLM\uLLM.Mistral.pas',
  uLLM.OpenAI.Assistants in '..\Libs\LLM\uLLM.OpenAI.Assistants.pas',
  uLLM.OpenAI in '..\Libs\LLM\uLLM.OpenAI.pas',
  uLLM.OpenRouter in '..\Libs\LLM\uLLM.OpenRouter.pas',
  uLLM in '..\Libs\LLM\uLLM.pas',
  uLLM.Replicate in '..\Libs\LLM\uLLM.Replicate.pas',
  uLLM.X.Ai in '..\Libs\LLM\uLLM.X.Ai.pas',
  uEngineManager in '..\Libs\uEngineManager.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmDescribePicture, frmDescribePicture);
  Application.Run;
end.
