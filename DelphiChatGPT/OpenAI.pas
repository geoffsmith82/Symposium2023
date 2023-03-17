unit OpenAI;

interface


uses
  System.JSON,
  System.SysUtils,
  System.Generics.Collections,
  REST.Client,
  REST.Types
  ;

type
  TOpenAI = class
  public
    class function CallDALL_E(prompt, model: string): string;
    class function AskChatGPT(AQuestion, AModel: string): string;
  end;

implementation

{$I APIKEY.INC}

class function TOpenAI.CallDALL_E(prompt: string; model: string): string;
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
      json.AddPair('prompt', TJSONString.Create(prompt));
      json.AddPair('model', TJSONString.Create(model));
      LRequest.AddBody(json.ToString, ctAPPLICATION_JSON);

      LResponse := TRESTResponse.Create(nil);
      LResponse.ContentType := 'application/json';
      LRequest.Response := LResponse;
      LRequest.Execute;

      Result := LResponse.Content;
    finally
      LRequest.Free;
    end;
  finally
    LClient.Free;
  end;
end;

class function TOpenAI.AskChatGPT(AQuestion: string; AModel: string): string;
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
      end
      else
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
