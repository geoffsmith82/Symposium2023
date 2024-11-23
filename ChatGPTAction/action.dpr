program action;
{$APPTYPE GUI}

uses
  Vcl.Forms,
  Web.WebReq,
  IdHTTPWebBrokerBridge,
  FormUnit1 in 'FormUnit1.pas' {Form1},
  WebModuleUnit1 in 'WebModuleUnit1.pas' {WebModule1: TWebModule},
  Controller.Personal in 'Controller.Personal.pas',
  Controller.Project in 'Controller.Project.pas',
  Files.Extra in 'Files.Extra.pas',
  udmCompiler in 'udmCompiler.pas' {dmCompiler: TDataModule},
  Controller.Project.DataObjects in 'Controller.Project.DataObjects.pas',
  udmFixInsight in 'udmFixInsight.pas' {dmFixInsight: TDataModule},
  CodeExtractor in 'CodeExtractor.pas',
  udmPersonalAlarms in 'udmPersonalAlarms.pas' {dmPersonalAlarms: TDataModule},
  Controller.Personal.DataObjects in 'Controller.Personal.DataObjects.pas',
  Controller.Weather in 'Controller.Weather.pas',
  Controller.Weather.DataObjects in 'Controller.Weather.DataObjects.pas',
  udmWeather in '..\Weather\udmWeather.pas' {dmWeather: TDataModule},
  uXMLBOMPrecis in '..\Weather\uXMLBOMPrecis.pas',
  udmDiff in 'udmDiff.pas' {dmDiff: TDataModule};

{$R *.res}

begin
  if WebRequestHandler <> nil then
    WebRequestHandler.WebModuleClass := WebModuleClass;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
