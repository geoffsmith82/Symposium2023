program translatelang;

uses
  Vcl.Forms,
  Unit3 in 'Unit3.pas' {frmMainTranslationWindow},
  uGoogle.Translate in 'translate\uGoogle.Translate.pas',
  uBaseTranslate in 'translate\uBaseTranslate.pas',
  uMicrosoft.Translate in 'translate\uMicrosoft.Translate.pas',
  uAmazon.Translate in 'translate\uAmazon.Translate.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMainTranslationWindow, frmMainTranslationWindow);
  Application.Run;
end.
