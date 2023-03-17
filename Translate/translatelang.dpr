program TranslateLang;

uses
  Vcl.Forms,
  Unit3 in 'Unit3.pas' {frmMainTranslationWindow},
  uAmazon.Translate in '..\Libs\Translate\uAmazon.Translate.pas',
  uBaseTranslate in '..\Libs\Translate\uBaseTranslate.pas',
  uGoogle.Translate in '..\Libs\Translate\uGoogle.Translate.pas',
  uMicrosoft.Translate in '..\Libs\Translate\uMicrosoft.Translate.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMainTranslationWindow, frmMainTranslationWindow);
  Application.Run;
end.
