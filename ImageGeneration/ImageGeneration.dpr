program ImageGeneration;

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
  frmImageGenWindow in 'frmImageGenWindow.pas' {frmImageGenerator},
  OpenAI in '..\Libs\LLM\OpenAI.pas',
  uLLM in '..\Libs\LLM\uLLM.pas',
  uDALLe2.DTO in '..\Libs\uDALLe2.DTO.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmImageGenerator, frmImageGenerator);
  Application.Run;
end.
