unit WebModuleUnit1;

interface

uses
  System.SysUtils,
  System.Classes,
  IdContext,
  MVCFramework,
  Web.HTTPApp;

type
  TWebModule1 = class(TWebModule)
    procedure WebModule1DefaultHandlerAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure WebModuleCreate(Sender: TObject);
  private
    { Private declarations }
    FMVC : TMVCEngine;
  public
    { Public declarations }
    class procedure OnParseAuthentication(AContext: TIdContext; const AAuthType,
      AAuthData: string; var VUsername, VPassword: string;
      var VHandled: Boolean);
  end;

var
  WebModuleClass: TComponentClass = TWebModule1;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

uses
  MVCFramework.Commons,
  DelphiIdeProxyController
  ;

class procedure TWebModule1.OnParseAuthentication(
  AContext: TIdContext; const AAuthType, AAuthData: string; var VUsername,
  VPassword: string; var VHandled: Boolean);
begin
  VHandled := True; //SameText(LowerCase(AAuthType), 'Token');
end;

procedure TWebModule1.WebModule1DefaultHandlerAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  Response.Content :=
    '<html>' +
    '<head><title>Web Server Application</title></head>' +
    '<body>Web Server Application</body>' +
    '</html>';
end;

procedure TWebModule1.WebModuleCreate(Sender: TObject);
begin
    FMVC := TMVCEngine.Create(Self,
    procedure(Config: TMVCConfig)
    begin
      // default content-type
      Config[TMVCConfigKey.DefaultContentType] := TMVCConstants.DEFAULT_CONTENT_TYPE;
      // default content charset
      Config[TMVCConfigKey.DefaultContentCharset] := TMVCConstants.DEFAULT_CONTENT_CHARSET;
      // unhandled actions are permitted?
      Config[TMVCConfigKey.AllowUnhandledAction] := 'false';
      // default view file extension
      Config[TMVCConfigKey.DefaultViewFileExtension] := 'html';
      // view path
      Config[TMVCConfigKey.ViewPath] := 'templates';
      // Max Record Count for automatic Entities CRUD
      Config[TMVCConfigKey.MaxEntitiesRecordCount] := '20';
      // Enable Server Signature in response
      Config[TMVCConfigKey.ExposeServerSignature] := 'true';
      // Max request size in bytes
      Config[TMVCConfigKey.MaxRequestSize] := IntToStr(TMVCConstants.DEFAULT_MAX_REQUEST_SIZE);
    end);
  FMVC.AddController(TDelphiIdeProxyController);
end;

end.
