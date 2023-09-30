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
    FAccessToken: string;
    FExpiryTime: TDateTime;
    procedure GetAccessToken;
  public
    constructor Create(const SubscriptionKey, Endpoint: string);
    destructor Destroy; override;
    function Translate(const SourceText: string; const toLang: string; const fromLang: string): string; override;
    function FromLanguages: TObjectList<TLanguageInfo>; override;
    function ToLanguages: TObjectList<TLanguageInfo>; override;
  end;
implementation

uses System.DateUtils;


constructor TMicrosoftTranslate.Create(const SubscriptionKey, Endpoint: string);
begin
  inherited Create(SubscriptionKey);
  FExpiryTime := 0;
  FEndpoint := Endpoint;
  // Create a new REST client and set the base URL to the Microsoft Translate API endpoint
  FRESTClient := TRESTClient.Create(nil);
  FRESTClient.BaseURL := FEndpoint;
  // Create a new REST request and set its parameters
  FRESTRequest := TRESTRequest.Create(nil);
  FRESTRequest.Method := rmPOST;
  FRESTRequest.Client := FRESTClient;
  FRESTRequest.AddParameter('api-version', '3.0');
  // Set the authorization header using the subscription key
  FRESTRequest.Params.AddItem('Ocp-Apim-Subscription-Key', FSubscriptionKey, TRESTRequestParameterKind.pkHTTPHEADER, [TRESTRequestParameterOption.poDoNotEncode]);
  // Create a new REST response adapter
  FRESTResponse := TRESTResponse.Create(nil);
  FRESTResponse.ContentType := 'application/json';
end;

destructor TMicrosoftTranslate.Destroy;
begin
  FreeAndNil(FRESTClient);
  FreeAndNil(FRESTResponse);
  FreeAndNil(FRESTRequest);

  inherited;
end;

function TMicrosoftTranslate.FromLanguages: TObjectList<TLanguageInfo>;
var
  LResponseJson: TJSONObject;
  LLanguagesArray: TJSONObject;
  LLanguageObj : TJSONObject;
  LLangCode: string;
  I: Integer;
  LApiVersion : string;
  LLangInfo: TLanguageInfo;
begin
  LApiVersion := '3.0';
  FRESTRequest.Method := rmGET;
  FRESTRequest.ResetToDefaults;
  FRESTRequest.Resource := '/languages?api-version={version}&scope={scope}';
  FRESTRequest.AddParameter('version', LApiVersion, TRESTRequestParameterKind.pkURLSEGMENT);
  FRESTRequest.AddParameter('scope', 'translation', TRESTRequestParameterKind.pkURLSEGMENT);
  FRESTRequest.Response := FRESTResponse;

  FRESTRequest.Execute;

  LResponseJson := FRESTResponse.JSONValue as TJSONObject; //TJSONObject.ParseJSONValue(ResponseText) as TJSONObject;

  LLanguagesArray := LResponseJson.Values['translation'] as TJSONObject;
  FFromLanguages.Clear;
  for I := 0 to LLanguagesArray.Count - 1 do
  begin
    LLanguageObj := LLanguagesArray.Pairs[i].JsonValue as TJSONObject;
    LLangCode := LLanguagesArray.Pairs[i].JsonString.Value;
    LLangInfo := TLanguageInfo.Create;
    LLangInfo.LanguageName := LLanguageObj.Values['name'].Value;
    LLangInfo.LanguageCode := LLangCode;
    FFromLanguages.Add(LLangInfo);
  end;
  Result := FFromLanguages;
end;

function TMicrosoftTranslate.ToLanguages: TObjectList<TLanguageInfo>;
var
  LResponseJson: TJSONObject;
  LLanguagesArray: TJSONObject;
  LLanguageObj : TJSONObject;
  LLangCode: string;
  I: Integer;
  LApiVersion : string;
  LLangInfo : TLanguageInfo;
begin
  LApiVersion := '3.0';
  FRESTRequest.Method := rmGET;
  FRESTRequest.ResetToDefaults;
  FRESTRequest.Resource := '/languages?api-version={version}&scope={scope}';
  FRESTRequest.AddParameter('version', LApiVersion, TRESTRequestParameterKind.pkURLSEGMENT);
  FRESTRequest.AddParameter('scope', 'translation', TRESTRequestParameterKind.pkURLSEGMENT);
  FRESTRequest.Response := FRESTResponse;

  FRESTRequest.Execute;

  LResponseJson := FRESTResponse.JSONValue as TJSONObject; //TJSONObject.ParseJSONValue(ResponseText) as TJSONObject;

  LLanguagesArray := LResponseJson.Values['translation'] as TJSONObject;
  FToLanguages.Clear;

  LLangInfo := TLanguageInfo.Create;
  LLangInfo.LanguageName := 'auto';
  LLangInfo.LanguageCode := 'auto';
  FFromLanguages.Add(LLangInfo);

  for I := 0 to LLanguagesArray.Count - 1 do
  begin
    LLanguageObj := LLanguagesArray.Pairs[i].JsonValue as TJSONObject;
    LLangCode := LLanguagesArray.Pairs[i].JsonString.Value;
    LLangInfo := TLanguageInfo.Create;
    LLangInfo.LanguageName := LLanguageObj.Values['name'].Value;
    LLangInfo.LanguageCode := LLangCode;
    FToLanguages.Add(LLangInfo);
  end;
  Result := FToLanguages;
end;

procedure TMicrosoftTranslate.GetAccessToken;
var
  LRESTClient: TRESTClient;
  LRESTRequest: TRESTRequest;
  LRESTResponse: TRESTResponse;
begin
  LRESTClient := nil;
  LRESTRequest := nil;
  LRESTResponse := nil;
  try
    LRESTClient := TRESTClient.Create('https://api.cognitive.microsoft.com');
    LRESTRequest := TRESTRequest.Create(nil);
    LRESTResponse := TRESTResponse.Create(nil);
    LRESTRequest.Method := TRESTRequestMethod.rmPOST;
    LRESTRequest.Resource := '/sts/v1.0/issueToken';

    LRESTRequest.Client := LRESTClient;
    LRESTRequest.Response := LRESTResponse;
    LRESTRequest.AddParameter('Ocp-Apim-Subscription-Key', FSubscriptionKey, TRESTRequestParameterKind.pkHTTPHEADER);
    LRESTRequest.AddParameter('Content-Type', 'application/x-www-form-urlencoded', TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
    LRESTRequest.Execute;
    FAccessToken := TEncoding.UTF8.GetString(LRESTResponse.RawBytes);
    FExpiryTime := IncMinute(FExpiryTime, 8);
  finally
    LRESTResponse.Free;
    LRESTRequest.Free;
    LRESTClient.Free;
  end;
end;

function TMicrosoftTranslate.Translate(const SourceText: string; const toLang: string; const fromLang: string): string;
var
  LRequestBody: TJSONObject;
  LJsonArray : TJSONArray;
  LTranslations: TJSONArray;
  LTranslationsArray: TJSONArray;
  LApiVersion : string;
begin
  if SourceText.Trim.IsEmpty or toLang.Trim.IsEmpty then
    Exit('');

  if (Now > FExpiryTime) then
    GetAccessToken;

  LApiVersion := '3.0';
  FRESTRequest.ResetToDefaults;
  LRequestBody := nil;
  try
  // Build the request body JSON using a TJSONObject
  LRequestBody := TJSONObject.Create;
    try
      LRequestBody.AddPair('text', SourceText);

      // Set the request body and execute the REST request
     // FRESTRequest.AddParameter('application/json', RequestBody.ToJSON, TRESTRequestParameterKind.pkREQUESTBODY);
      LJsonArray := TJSONArray.Create(LRequestBody);
      FRESTRequest.AddBody(LJsonArray.ToJSON, ctAPPLICATION_JSON);
      // Set the target language and URL for the Microsoft Translate API
      FRESTRequest.AddParameter('to', toLang, TRESTRequestParameterKind.pkQUERY);
      FRESTRequest.AddParameter('api-version', LApiVersion, TRESTRequestParameterKind.pkQUERY);
      FRESTRequest.AddParameter('Authorization', 'Bearer ' + FAccessToken, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
      FRESTRequest.Method := rmPOST;
      FRESTRequest.Resource := 'translate';

      FRESTRequest.Execute;

      if FRESTResponse.StatusCode <> 200 then
        Exit('');

      if FRESTResponse.ContentType <> 'application/json' then
        Exit('');

      // Get the translations array from the REST response
      LTranslations := FRESTResponse.JSONValue as TJSONArray;
      if not Assigned(LTranslations) or (LTranslations.Count = 0) then
        Exit('');

      // Get the translated text from the first element of the translations array
      LTranslationsArray := LTranslations.Items[0].GetValue<TJSONArray>('translations');
      if not Assigned(LTranslationsArray) or (LTranslationsArray.Count = 0) then
        Exit('');

      Result := (LTranslationsArray[0] as TJSONObject).GetValue('text').Value;
    except
      on E: Exception do
        Result := '';
    end;
  finally
    FreeAndNil(LRequestBody);
  end;
end;
end.

