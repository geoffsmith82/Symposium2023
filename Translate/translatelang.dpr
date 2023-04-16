program TranslateLang;

uses
  {$IFDEF EurekaLog}
  EMemLeaks,
  EResLeaks,
  EDebugExports,
  EDebugJCL,
  EFixSafeCallException,
  EMapWin32,
  EAppVCL,
  EDialogWinAPIMSClassic,
  EDialogWinAPIEurekaLogDetailed,
  EDialogWinAPIStepsToReproduce,
  ExceptionLog7,
  {$ENDIF EurekaLog}
  Vcl.Forms,
  frmTranslation in 'frmTranslation.pas' {frmMainTranslationWindow},
  uAmazon.Translate in '..\Libs\Translate\uAmazon.Translate.pas',
  uBaseTranslate in '..\Libs\Translate\uBaseTranslate.pas',
  uGoogle.Translate in '..\Libs\Translate\uGoogle.Translate.pas',
  uTranslatedfn in 'uTranslatedfn.pas',
  REST.Authenticator.EnhancedOAuth in '..\Libs\REST.Authenticator.EnhancedOAuth.pas',
  uOutputChangedLanguageTokens in 'uOutputChangedLanguageTokens.pas',
  uMicrosoft.Translate in '..\Libs\Translate\uMicrosoft.Translate.pas',
  uEngineManager in '..\Libs\uEngineManager.pas',
  LanguageCodes in '..\Libs\Translate\LanguageCodes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMainTranslationWindow, frmMainTranslationWindow);
  Application.Run;
end.
