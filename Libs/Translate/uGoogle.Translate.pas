unit uGoogle.Translate;


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
  TGoogleTranslate = class(TBaseTranslate)
  private
    FRESTClient: TRESTClient;
    FRESTRequest: TRESTRequest;
    FRESTResponse: TRESTResponse;
    FAPIKey: string;
    FSourceLang: string;
    FTargetLang: string;
  public
    constructor Create(const APIKey: string; const SourceLang, TargetLang: string);
    function Translate(const SourceText: string): string; override;
    function EngineName: string; override;
    function FromLanguages: TArray<string>; override;
    function ToLanguages: TArray<string>; override;
  end;

implementation

constructor TGoogleTranslate.Create(const APIKey: string; const SourceLang, TargetLang: string);
begin
  FAPIKey := APIKey;
  FSourceLang := SourceLang;
  FTargetLang := TargetLang;

  // Create a new REST client and set the base URL to the Google Translate API endpoint
  FRESTClient := TRESTClient.Create(nil);
  FRESTClient.BaseURL := 'https://translation.googleapis.com/language/translate/v2';

  // Create a new REST request and set its parameters
  FRESTRequest := TRESTRequest.Create(nil);
  FRESTRequest.Method := rmPOST;
  FRESTRequest.Client := FRESTClient;
  FRESTRequest.AddParameter('source', FSourceLang);
  FRESTRequest.AddParameter('target', FTargetLang);
  FRESTRequest.AddParameter('key', FAPIKey);

  // Create a new REST response adapter
  FRESTResponse := TRESTResponse.Create(nil);
  FRESTResponse.ContentType := 'application/json';
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

  FRESTRequest.Client := FRESTClient;
  FRESTRequest.Execute;

  ResponseJson := FRESTResponse.JSONValue  as TJSONObject;

  LanguagesArray := (ResponseJson.GetValue('data') as TJSONObject).GetValue('languages') as TJSONArray;
  SetLength(Result, LanguagesArray.Count);

  for I := 0 to LanguagesArray.Count - 1 do
    Result[I] := LanguagesArray.Items[I].GetValue<string>('language');
end;


function TGoogleTranslate.ToLanguages: TArray<string>;
begin

end;

function TGoogleTranslate.Translate(const SourceText: string): string;
begin
  // Set the source text parameter and execute the REST request
  FRESTRequest.AddParameter('q', SourceText);
  FRESTRequest.Execute;
  FRESTRequest.Response := FRESTResponse;

  // Get the translated text from the REST response
  Result := FRESTResponse.Content;// FRESTResponse.JSONValue
//    .GetValue<string>(data.translations[0].translatedText);
end;

end.
