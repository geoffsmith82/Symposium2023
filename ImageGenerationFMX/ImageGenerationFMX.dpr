program ImageGenerationFMX;

uses
  System.StartUpCopy,
  Fmx.Forms,
  frmImageGenWindow in 'frmImageGenWindow.pas' {frmImageGenerator},
  uDALLe2.DTO in '..\Libs\ImageGeneration\uDALLe2.DTO.pas',
  uImageGeneration in '..\Libs\ImageGeneration\uImageGeneration.pas',
  uImageGeneration.OpenAI in '..\Libs\ImageGeneration\uImageGeneration.OpenAI.pas',
  uImageGeneration.Replicate in '..\Libs\ImageGeneration\uImageGeneration.Replicate.pas',
  uImageGeneration.XAI in '..\Libs\ImageGeneration\uImageGeneration.XAI.pas',
  ApiKeyStore in '..\Libs\ApiKeyStore\ApiKeyStore.pas',
  uAPIKeyNameList in '..\Libs\ApiKeyStore\uAPIKeyNameList.pas',
  ApiKeyStore.Windows in '..\Libs\ApiKeyStore\ApiKeyStore.Windows.pas',
  uImageGeneration.Imagen in '..\Libs\ImageGeneration\uImageGeneration.Imagen.pas',
  ApiKeyStore.OSX in '..\Libs\ApiKeyStore\ApiKeyStore.OSX.pas',
  FMX.frmApiKeyStore in '..\Libs\ApiKeyStore\FMX.frmApiKeyStore.pas' {frmApiKeyStores};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmImageGenerator, frmImageGenerator);
  Application.Run;
end.

