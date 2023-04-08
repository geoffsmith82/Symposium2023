unit OpenAI;

interface

uses
  System.JSON,
  System.SysUtils,
  System.Generics.Collections,
  REST.Client,
  REST.Types,
  uDALLe2.DTO
  ;

type
  TDALLESize = (DALLE256, DALLE512, DALLE1024);

  TOpenAI = class
  public
    class function CallDALL_E(const prompt: string; n: Integer; size: TDALLESize): TGeneratedImagesClass;
    class function AskChatGPT(const AQuestion: string; const AModel: string): string;
  end;

implementation

{$I APIKEY.INC}

class function TOpenAI.CallDALL_E(const prompt: string; n: Integer; size: TDALLESize): TGeneratedImagesClass;
var
  LClient: TRESTClient;
  LRequest: TRESTRequest;
  LResponse: TRESTResponse;
  url: string;
  json: TJSONObject;
begin
  LClient := TRESTClient.Create(nil);
  LClient.ReadTimeout := 60000;
  try
    LRequest := TRESTRequest.Create(nil);
    try
      url := 'https://api.openai.com/v1/images/generations';
      LClient.BaseURL := url;
      LRequest.Client := LClient;
      LRequest.Method := rmPOST;
      LRequest.AddAuthParameter('Authorization', 'Bearer ' + CHATGPT_APIKEY, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);

      json := TJSONObject.Create;
      try
        json.AddPair('prompt', TJSONString.Create(prompt));
        json.AddPair('n', TJSONNumber.Create(n));
        case size of
          DALLE256: json.AddPair('size', '256x256');
          DALLE512: json.AddPair('size', '512x512');
          DALLE1024: json.AddPair('size', '1024x1024');
        end;
        LRequest.AddBody(json.ToString, ctAPPLICATION_JSON);
      finally
        FreeAndNil(json);
      end;

      LResponse := TRESTResponse.Create(nil);
      LResponse.ContentType := 'application/json';
      LRequest.Response := LResponse;
      LRequest.Execute;
      Result := TGeneratedImagesClass.FromJsonString(LResponse.Content);
    finally
      LRequest.Free;
    end;
  finally
    LClient.Free;
  end;
end;

class function TOpenAI.AskChatGPT(const AQuestion: string; const AModel: string): string;
var
  LClient : TRESTClient;
  LRequest : TRESTRequest;
  LResponse : TRESTResponse;
  LJsonPostData : TJSONObject;
  LJsonValue: TJsonValue;
  LJsonArray: TJsonArray;
  LJSonString: TJsonString;
begin
  Result := '';
  LJsonPostData := nil;
  LClient := nil;
  LRequest := nil;
  LResponse := nil;

  try
    LJsonPostData := TJSONObject.Create;
    LJsonPostData.AddPair('model', AModel);
    LJsonPostData.AddPair('prompt', AQuestion);
    LJsonPostData.AddPair('max_tokens', TJSONNumber.Create(2048));
    LJsonPostData.AddPair('temperature', TJSONNumber.Create(0));

    LClient := TRESTClient.Create(nil);
    LRequest := TRESTRequest.Create(nil);
    LResponse := TRESTResponse.Create(nil);
    LRequest.Client := LClient;
    LRequest.Response := LResponse;

    LClient.ReadTimeout := 180000;

    // Use JSON for the REST API calls and set API KEY via Authorization header
    LRequest.AddAuthParameter('Authorization', 'Bearer ' + CHATGPT_APIKEY, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
    LRequest.Accept := '*/*';

    // Select HTTPS POST method, set POST data and specify endpoint URL
    LRequest.Method := rmPOST;
    LRequest.AddBody(LJsonPostData);
    LClient.BaseURL := 'https://api.openai.com';
    LRequest.Resource := 'v1/completions';

    // Execute the HTTPS POST request synchronously (last param Async = false)
    LRequest.Execute;
    // Process returned JSON when request was successful
    if LRequest.Response.StatusCode = 200 then
    begin
      LJsonValue := LResponse.JSONValue;
      LJsonValue := LJsonValue.GetValue<TJSonValue>('choices');
      if LJsonValue is TJSonArray then
      begin
        LJSonArray := LJsonValue as TJSonArray;
        LJSonString := LJSonArray.Items[0].GetValue<TJSONString>('text');
        Result := LJSonString.Value;
      end;
    end
    else
      raise Exception.Create('HTTP response code: ' + LResponse.StatusCode.ToString);
  finally
    FreeAndNil(LResponse);
    FreeAndNil(LRequest);
    FreeAndNil(LClient);
    FreeAndNil(LJsonPostData);
  end;
end;

end.
