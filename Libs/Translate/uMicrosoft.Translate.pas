unit uMicrosoft.Translate;

interface
uses
  System.Generics.Collections,
  System.SysUtils,
  System.Classes,
  System.JSON,
  REST.Types,
  REST.Client,
  REST.Response.Adapter,
  uBaseTranslate
  ;
type
  TMicrosoftTranslate = class(TBaseTranslate)
  private
    FRESTClient: TRESTClient;
    FRESTRequest: TRESTRequest;
    FRESTResponse: TRESTResponse;
    FSubscriptionKey: string;
    FEndpoint: string;
    FSourceLang: string;
    FTargetLang: string;
    FAccessToken: string;
    FExpiryTime: TDateTime;
    procedure GetAccessToken;
  public
    constructor Create(const SubscriptionKey, Endpoint, SourceLang, TargetLang: string);
    function Translate(const SourceText: string): string; override;
    function EngineName: string; override;
    function FromLanguages: TArray<string>; override;
    function ToLanguages: TArray<string>; override;
  end;
implementation

uses System.DateUtils;


constructor TMicrosoftTranslate.Create(const SubscriptionKey, Endpoint, SourceLang, TargetLang: string);
begin
  FSubscriptionKey := SubscriptionKey;
  FEndpoint := Endpoint;
  FSourceLang := SourceLang;
  FTargetLang := TargetLang;
  // Create a new REST client and set the base URL to the Microsoft Translate API endpoint
  FRESTClient := TRESTClient.Create(nil);
  FRESTClient.BaseURL := FEndpoint;
  // Create a new REST request and set its parameters
  FRESTRequest := TRESTRequest.Create(nil);
  FRESTRequest.Method := rmPOST;
  FRESTRequest.Client := FRESTClient;
  FRESTRequest.AddParameter('api-version', '3.0');
  FRESTRequest.AddParameter('from', FSourceLang);
  FRESTRequest.AddParameter('to', FTargetLang);
  // Set the authorization header using the subscription key
  FRESTRequest.Params.AddItem('Ocp-Apim-Subscription-Key', FSubscriptionKey, TRESTRequestParameterKind.pkHTTPHEADER, [TRESTRequestParameterOption.poDoNotEncode]);
  // Create a new REST response adapter
  FRESTResponse := TRESTResponse.Create(nil);
  FRESTResponse.ContentType := 'application/json';
end;
function TMicrosoftTranslate.EngineName: string;
begin
  Result := 'Microsoft Translate';
end;

function TMicrosoftTranslate.FromLanguages: TArray<string>;
var
  ResponseJson: TJSONObject;
  LanguagesArray: TJSONObject;
  LanguageObj : TJSONObject;
  I: Integer;
  ApiVersion : string;
begin
  ApiVersion := '3.0';
  FRESTRequest.Method := rmGET;
  FRESTRequest.ResetToDefaults;
  FRESTRequest.Resource := '/languages?api-version={version}&scope={scope}';
  FRESTRequest.AddParameter('version', ApiVersion, TRESTRequestParameterKind.pkURLSEGMENT);
  FRESTRequest.AddParameter('scope', 'translation', TRESTRequestParameterKind.pkURLSEGMENT);
  FRESTRequest.Response := FRESTResponse;

  FRESTRequest.Execute;

  ResponseJson := FRESTResponse.JSONValue as TJSONObject; //TJSONObject.ParseJSONValue(ResponseText) as TJSONObject;

  LanguagesArray := ResponseJson.Values['translation'] as TJSONObject;
  SetLength(Result, LanguagesArray.Count);

  for I := 0 to LanguagesArray.Count - 1 do
  begin
    LanguageObj := LanguagesArray.Pairs[i].JsonValue as TJSONObject;
    Result[I] := LanguageObj.Values['name'].Value;
  end;
end;

function TMicrosoftTranslate.ToLanguages: TArray<string>;
var
  ResponseJson: TJSONObject;
  LanguagesArray: TJSONObject;
  LanguageObj : TJSONObject;
  I: Integer;
  ApiVersion : string;
begin
  ApiVersion := '3.0';
  FRESTRequest.Method := rmGET;
  FRESTRequest.ResetToDefaults;
  FRESTRequest.Resource := '/languages?api-version={version}&scope={scope}';
  FRESTRequest.AddParameter('version', ApiVersion, TRESTRequestParameterKind.pkURLSEGMENT);
  FRESTRequest.AddParameter('scope', 'translation', TRESTRequestParameterKind.pkURLSEGMENT);
  FRESTRequest.Response := FRESTResponse;

  FRESTRequest.Execute;

  ResponseJson := FRESTResponse.JSONValue as TJSONObject; //TJSONObject.ParseJSONValue(ResponseText) as TJSONObject;

  LanguagesArray := ResponseJson.Values['translation'] as TJSONObject;
  SetLength(Result, LanguagesArray.Count);

  for I := 0 to LanguagesArray.Count - 1 do
  begin
    LanguageObj := LanguagesArray.Pairs[i].JsonValue as TJSONObject;
    Result[I] := LanguageObj.Values['name'].Value;
  end;
end;

procedure TMicrosoftTranslate.GetAccessToken;
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
begin
  RESTClient := TRESTClient.Create('https://api.cognitive.microsoft.com');
  RESTRequest := TRESTRequest.Create(RESTClient);
  RESTResponse := TRESTResponse.Create(RESTClient);
  try
    RESTRequest.Method := TRESTRequestMethod.rmPOST;
    RESTRequest.Resource := '/sts/v1.0/issueToken';
    RESTRequest.Response := RESTResponse;
    RESTRequest.AddParameter('Ocp-Apim-Subscription-Key', FSubscriptionKey, TRESTRequestParameterKind.pkHTTPHEADER);
    RESTRequest.AddParameter('Content-Type', 'application/x-www-form-urlencoded', TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
    RESTRequest.AddParameter('Content-Length', '0', TRESTRequestParameterKind.pkHTTPHEADER);
    RESTRequest.Execute;
    FAccessToken := RESTResponse.Content;
    FExpiryTime := IncMinute(FExpiryTime, 8);
  finally
    RESTResponse.Free;
    RESTRequest.Free;
    RESTClient.Free;
  end;
end;

function TMicrosoftTranslate.Translate(const SourceText: string): string;
var
  RequestBody: TJSONObject;
  jsonArray : TJSONArray;
  Translations: TJSONArray;
  TranslationsArray: TJSONArray;
  ApiVersion : string;
begin
  if SourceText.Trim.IsEmpty or FTargetLang.Trim.IsEmpty then
    Exit('');

  GetAccessToken;

  ApiVersion := '3.0';
  FRESTRequest.ResetToDefaults;

  // Build the request body JSON using a TJSONObject
  RequestBody := TJSONObject.Create;
  try
    RequestBody.AddPair('text', SourceText);

    // Set the request body and execute the REST request
   // FRESTRequest.AddParameter('application/json', RequestBody.ToJSON, TRESTRequestParameterKind.pkREQUESTBODY);
   jsonArray := TJSONArray.Create(RequestBody);
    FRESTRequest.AddBody(jsonArray.ToJSON, ctAPPLICATION_JSON);
    // Set the target language and URL for the Microsoft Translate API
    FRESTRequest.AddParameter('to', FTargetLang, TRESTRequestParameterKind.pkQUERY);
    FRESTRequest.AddParameter('api-version', ApiVersion, TRESTRequestParameterKind.pkQUERY);
    FRESTRequest.AddParameter('Authorization', 'Bearer ' + FAccessToken, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
    FRESTRequest.Method := rmPOST;
    FRESTRequest.Resource := 'translate';

    FRESTRequest.Execute;

    if FRESTResponse.StatusCode <> 200 then
      Exit('');

    if FRESTResponse.ContentType <> 'application/json' then
      Exit('');

    // Get the translations array from the REST response
    Translations := FRESTResponse.JSONValue as TJSONArray;
    if not Assigned(Translations) or (Translations.Count = 0) then
      Exit('');

    // Get the translated text from the first element of the translations array
    TranslationsArray := Translations.Items[0].GetValue<TJSONArray>('translations');
    if not Assigned(TranslationsArray) or (TranslationsArray.Count = 0) then
      Exit('');

    Result := (TranslationsArray[0] as TJSONObject).GetValue('text').Value;
  except
    on E: Exception do
      Result := '';
  end;

  RequestBody.Free;
end;
end.

