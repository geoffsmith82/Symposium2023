program TranslateLang;

uses
  Vcl.Forms,
  frmTranslation in 'frmTranslation.pas' {frmMainTranslationWindow},
  uTranslate.Amazon in '..\Libs\Translate\uTranslate.Amazon.pas',
  uTranslate in '..\Libs\Translate\uTranslate.pas',
  uTranslate.Google in '..\Libs\Translate\uTranslate.Google.pas',
  uTranslatedfn in 'uTranslatedfn.pas',
  REST.Authenticator.EnhancedOAuth in '..\Libs\REST.Authenticator.EnhancedOAuth.pas',
  uOutputChangedLanguageTokens in 'uOutputChangedLanguageTokens.pas',
  uTranslate.Microsoft in '..\Libs\Translate\uTranslate.Microsoft.pas',
  uEngineManager in '..\Libs\uEngineManager.pas',
  uTranslate.LanguageCodes in '..\Libs\Translate\uTranslate.LanguageCodes.pas',
  uLLM.OpenAI in '..\Libs\LLM\uLLM.OpenAI.pas',
  uLLM in '..\Libs\LLM\uLLM.pas',
  uLLM.Functions in '..\Libs\LLM\uLLM.Functions.pas',
  uAttributes in '..\Libs\LLM\uAttributes.pas',
  ApiKeyStore in '..\Libs\ApiKeyStore\ApiKeyStore.pas',
  ApiKeyStore.Windows in '..\Libs\ApiKeyStore\ApiKeyStore.Windows.pas',
  uAPIKeyNameList in '..\Libs\ApiKeyStore\uAPIKeyNameList.pas',
  frmApiKeyStore in '..\Libs\ApiKeyStore\frmApiKeyStore.pas' {frmApiKeyStores};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMainTranslationWindow, frmMainTranslationWindow);
  Application.Run;
end.

