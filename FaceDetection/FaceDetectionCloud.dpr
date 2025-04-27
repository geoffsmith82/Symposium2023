program FaceDetectionCloud;

uses
  Vcl.Forms,
  frmFaceWindow in 'frmFaceWindow.pas' {frmFaceDetection},
  uBaseFaceRecognition in '..\Libs\FaceRecognition\uBaseFaceRecognition.pas',
  uMicrosoft.FaceRecognition in '..\Libs\FaceRecognition\uMicrosoft.FaceRecognition.pas',
  REST.Authenticator.EnhancedOAuth in '..\Libs\REST.Authenticator.EnhancedOAuth.pas',
  uGoogle.FaceRecognition in '..\Libs\FaceRecognition\uGoogle.FaceRecognition.pas',
  uEngineManager in '..\Libs\uEngineManager.pas',
  uCodeProject.FaceRecognition in '..\Libs\FaceRecognition\uCodeProject.FaceRecognition.pas',
  uMicrosoft.FaceRecognition.DTO in '..\Libs\FaceRecognition\uMicrosoft.FaceRecognition.DTO.pas',
  ApiKeyStore in '..\Libs\ApiKeyStore\ApiKeyStore.pas',
  ApiKeyStore.Windows in '..\Libs\ApiKeyStore\ApiKeyStore.Windows.pas',
  uAPIKeyNameList in '..\Libs\ApiKeyStore\uAPIKeyNameList.pas',
  frmApiKeyStore in '..\Libs\ApiKeyStore\frmApiKeyStore.pas' {frmApiKeyStores};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmFaceDetection, frmFaceDetection);
  Application.Run;
end.

