program LLMLister;

uses
  System.StartUpCopy,
  FMX.Forms,
  frmLLMList in 'frmLLMList.pas' {frmLLMLister},
  uAPIKeyNameList in '..\Libs\ApiKeyStore\uAPIKeyNameList.pas',
  FMX.frmApiKeyStore in '..\Libs\ApiKeyStore\FMX.frmApiKeyStore.pas' {frmApiKeyStores},
  ApiKeyStore.Windows in '..\Libs\ApiKeyStore\ApiKeyStore.Windows.pas',
  ApiKeyStore in '..\Libs\ApiKeyStore\ApiKeyStore.pas',
  REST.Authenticator.EnhancedOAuth in '..\Libs\REST.Authenticator.EnhancedOAuth.pas',
  uLLM.X.Ai in '..\Libs\LLM\uLLM.X.Ai.pas',
  uLLM.Replicate in '..\Libs\LLM\uLLM.Replicate.pas',
  uLLM in '..\Libs\LLM\uLLM.pas',
  uLLM.OpenAI in '..\Libs\LLM\uLLM.OpenAI.pas',
  uLLM.OpenAI.Assistants in '..\Libs\LLM\uLLM.OpenAI.Assistants.pas',
  uLLM.HuggingFace in '..\Libs\LLM\uLLM.HuggingFace.pas',
  uLLM.Groq in '..\Libs\LLM\uLLM.Groq.pas',
  uLLM.Google.Gemini in '..\Libs\LLM\uLLM.Google.Gemini.pas',
  uLLM.Functions in '..\Libs\LLM\uLLM.Functions.pas',
  uLLM.Azure in '..\Libs\LLM\uLLM.Azure.pas',
  uLLM.Anthropic in '..\Libs\LLM\uLLM.Anthropic.pas',
  uLLM.Mistral in '..\Libs\LLM\uLLM.Mistral.pas',
  uAttributes in '..\Libs\LLM\uAttributes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmLLMLister, frmLLMLister);
  Application.Run;
end.
