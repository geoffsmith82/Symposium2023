unit uGoogle.Translate;


interface

uses
  System.Generics.Collections,
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.IniFiles,
  System.Net.URLClient,
  ShellApi,
  REST.Types,
  REST.Client,
  REST.Response.Adapter,
  REST.Authenticator.EnhancedOAuth,
  IdHTTPServer,
  IdCustomHTTPServer,
  IdHTTPHeaderInfo,
  IdContext,
  uBaseTranslate
  ;

type
  TGoogleTranslate = class(TBaseTranslate)
  strict private
    FOAuth2 : TEnhancedOAuth2Authenticator;
    FHTTPServer : TIdHttpServer;
    FSettings : TIniFile;
    procedure IdHTTPServer1CommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
  private
    FRESTClient: TRESTClient;
    FRESTRequest: TRESTRequest;
    FRESTResponse: TRESTResponse;
    FAPIKey: string;
    FAPISecret: string;
    FSourceLang: string;
    FTargetLang: string;
  public
    constructor Create(const APIKey: string; const APISecret: string; const SourceLang, TargetLang: string; Settings: TiniFile);
    function Translate(const SourceText: string): string; override;
    function EngineName: string; override;
    function FromLanguages: TArray<string>; override;
    function ToLanguages: TArray<string>; override;
    procedure Authenticate;
  end;

implementation

procedure TGoogleTranslate.Authenticate;
begin
  ShellExecute(0, 'OPEN', PChar(FOAuth2.AuthorizationRequestURI), nil, nil, 0);
end;

constructor TGoogleTranslate.Create(const APIKey: string; const APISecret: string; const SourceLang, TargetLang: string; Settings: TiniFile);
begin
  inherited Create;
  FAPIKey := APIKey;
  FAPISecret := APISecret;
  FSourceLang := SourceLang;
  FTargetLang := TargetLang;

  // Create a new REST client and set the base URL to the Google Translate API endpoint
  FRESTClient := TRESTClient.Create(nil);
  FRESTClient.BaseURL := 'https://translation.googleapis.com';

  // Create a new REST request and set its parameters
  FRESTRequest := TRESTRequest.Create(nil);
  FRESTRequest.Method := rmPOST;
  FRESTRequest.Client := FRESTClient;
  FRESTRequest.AddParameter('source', FSourceLang);
  FRESTRequest.AddParameter('target', FTargetLang);

  // Create a new REST response adapter
  FRESTResponse := TRESTResponse.Create(nil);
  FRESTResponse.ContentType := 'application/json';

  FOAuth2 := TEnhancedOAuth2Authenticator.Create(nil);
  FOAuth2.Scope := 'https://www.googleapis.com/auth/cloud-platform';
  FOAuth2.AuthorizationEndpoint := 'https://accounts.google.com/o/oauth2/auth?access_type=offline';
  FOAuth2.AccessTokenEndpoint := 'https://accounts.google.com/o/oauth2/token';
  FOAuth2.RedirectionEndpoint := 'http://localhost:7777/';
  FOAuth2.ClientID := FAPIKey;
  FOAuth2.ClientSecret := FAPISecret;
  FSettings := Settings;
  FOAuth2.RefreshToken := FSettings.ReadString('GoogleAuthentication', 'RefreshToken', '');
  FHTTPServer := TIdHttpServer.Create;
  FHTTPServer.DefaultPort := 7777;
  FHTTPServer.OnCommandGet := IdHTTPServer1CommandGet;
  FHTTPServer.Active := True;

end;

function TGoogleTranslate.EngineName: string;
begin
  Result := 'Google Translate';
end;

function TGoogleTranslate.FromLanguages: TArray<string>;
var
  ResponseJson: TJSONObject;
  LanguagesArray: TJSONArray;
  I: Integer;
begin
//  RequestUrl := Format('%s/languages?target=%s&key=%s', [Endpoint, DefaultLanguage, ApiKey]);

  FRESTRequest.ResetToDefaults;
  FRESTRequest.Resource := '/language/translate/v2/languages';

  FRESTClient.Authenticator := FOAuth2;
  FOAuth2.RefreshAccessTokenIfRequired;

  FRESTRequest.Client := FRESTClient;
  FRESTRequest.Execute;

  ResponseJson := FRESTResponse.JSONValue  as TJSONObject;

  LanguagesArray := (ResponseJson.GetValue('data') as TJSONObject).GetValue('languages') as TJSONArray;
  SetLength(Result, LanguagesArray.Count);

  for I := 0 to LanguagesArray.Count - 1 do
    Result[I] := LanguagesArray.Items[I].GetValue<string>('language');
end;


procedure TGoogleTranslate.IdHTTPServer1CommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  LCode: string;
  LURL : TURI;
begin
  if ARequestInfo.QueryParams = '' then
    Exit;
  LURL := TURI.Create('https://localhost/?' + ARequestInfo.QueryParams);
  try
    LCode := LURL.ParameterByName['code'];
  except
    Exit;
  end;
  FOAuth2.AuthCode := LCode;
  FOAuth2.ChangeAuthCodeToAccesToken;

  FSettings.WriteString('GoogleAuthentication', 'RefreshToken', FOAuth2.RefreshToken);
end;

function TGoogleTranslate.ToLanguages: TArray<string>;
begin

end;

function TGoogleTranslate.Translate(const SourceText: string): string;
var
  jsonRequest : TJSONObject;
begin
  // Set the source text parameter and execute the REST request
  jsonRequest := TJSONObject.Create;
  jsonRequest.AddPair('q', SourceText);
  jsonRequest.AddPair('target', 'en');

  FRESTClient.Authenticator := FOAuth2;
  FRESTRequest.Resource := '/language/translate/v2?textType=html';

  FOAuth2.RefreshAccessTokenIfRequired;
  FRESTRequest.Method := rmPOST;
  FRESTRequest.AddBody(jsonRequest);
  FRESTRequest.Response := FRESTResponse;
  FRESTRequest.Execute;


  // Get the translated text from the REST response
  Result := TEncoding.UTF8.GetString(FRESTResponse.RawBytes);
//    .GetValue<string>(data.translations[0].translatedText);
end;

end.
