program ImageGeneration;

uses
  Vcl.Forms,
  frmImageGenWindow in 'frmImageGenWindow.pas' {frmImageGenerator},
  uDALLe2.DTO in '..\Libs\ImageGeneration\uDALLe2.DTO.pas',
  uImageGeneration in '..\Libs\ImageGeneration\uImageGeneration.pas',
  uImageGeneration.OpenAI in '..\Libs\ImageGeneration\uImageGeneration.OpenAI.pas',
  uImageGeneration.Replicate in '..\Libs\ImageGeneration\uImageGeneration.Replicate.pas',
  uImageGeneration.XAI in '..\Libs\ImageGeneration\uImageGeneration.XAI.pas',
  ApiKeyStore in '..\Libs\ApiKeyStore\ApiKeyStore.pas',
  frmApiKeyStore in '..\Libs\ApiKeyStore\frmApiKeyStore.pas' {frmApiKeyStores},
  uAPIKeyNameList in '..\Libs\ApiKeyStore\uAPIKeyNameList.pas',
  ApiKeyStore.Windows in '..\Libs\ApiKeyStore\ApiKeyStore.Windows.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmImageGenerator, frmImageGenerator);
  Application.Run;
end.

