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
  public
    constructor Create(const SubscriptionKey, Endpoint, SourceLang, TargetLang: string);
    function Translate(const SourceText: string): string;
    function EngineName: string; override;
    function FromLanguages: TArray<string>; override;
    function ToLanguages: TArray<string>; override;
  end;

implementation

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

function TMicrosoftTranslate.Translate(const SourceText: string): string;
var
  RequestBody: string;
begin
  // Build the request body JSON
  RequestBody := Format('{"text": "%s"}', [SourceText]);

  // Set the request body and execute the REST request
  FRESTRequest.AddParameter('application/json', RequestBody, TRESTRequestParameterKind.pkREQUESTBODY);
  FRESTRequest.Execute;

  // Get the translated text from the REST response
  Result := FRESTResponse.JSONValue.GetValue<string>('translations[0].text');
end;

end.

