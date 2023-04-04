program TranslateLang;

uses
  Vcl.Forms,
  frmTranslation in 'frmTranslation.pas' {frmMainTranslationWindow},
  uAmazon.Translate in '..\Libs\Translate\uAmazon.Translate.pas',
  uBaseTranslate in '..\Libs\Translate\uBaseTranslate.pas',
  uGoogle.Translate in '..\Libs\Translate\uGoogle.Translate.pas',
  uTranslatedfn in 'uTranslatedfn.pas',
  REST.Authenticator.EnhancedOAuth in '..\Libs\REST.Authenticator.EnhancedOAuth.pas',
  uOutputChangedLanguageTokens in 'uOutputChangedLanguageTokens.pas',
  uMicrosoft.Translate in '..\Libs\Translate\uMicrosoft.Translate.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMainTranslationWindow, frmMainTranslationWindow);
  Application.Run;
end.
