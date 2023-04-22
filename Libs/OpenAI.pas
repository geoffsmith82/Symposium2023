unit OpenAI;

interface

uses
  System.Classes,
  System.JSON,
  System.SysUtils,
  System.Generics.Collections,
  REST.Client,
  REST.Types,
  uDALLe2.DTO
  ;

type
  TDALLESize = (DALLE256, DALLE512, DALLE1024);

  TChatMessage = class
    Role: string;
    Content: string;
  end;

  TChatResponse = record
    Content : string;
    Completion_Tokens : Cardinal;
    Prompt_Tokens : Cardinal;
    Total_Tokens : Cardinal;
  end;

  TOpenAI = class
  public
    class procedure ListOpenAIModels(out AModelList: TStringList); static;
    class function SendChatMessagesToOpenAI(const APIKey: string; Messages: TObjectList<TChatMessage>): TChatResponse; static;
    class function CallDALL_E(const prompt: string; n: Integer; size: TDALLESize): TGeneratedImagesClass;
    class function AskChatGPT(const AQuestion: string; const AModel: string): string;
    class function Embeddings(const Texts: TArray<string>): TArray<TArray<Double>>; static;
  end;

implementation

{$I APIKEY.INC}

const
  API_URL = 'https://api.openai.com/v1/embeddings';

class function TOpenAI.SendChatMessagesToOpenAI(const APIKey: string; Messages: TObjectList<TChatMessage>): TChatResponse;
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
  JSONBody: TJSONObject;
  JSONMessages: TJSONArray;
  JSONMsg : TJSONObject;
  Message: TChatMessage;
begin
  Result.Content := '';
  Result.Completion_Tokens := 0;
  Result.Prompt_Tokens := 0;
  Result.Total_Tokens := 0;
  RESTClient := TRESTClient.Create(nil);
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);
  try
    RESTClient.BaseURL := 'https://api.openai.com/v1/chat/completions';
    RESTClient.Accept := 'application/json';
    RESTClient.AcceptCharset := 'UTF-8';
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Method := TRESTRequestMethod.rmPOST;
    RESTRequest.Timeout := 60000; // Set the timeout as needed
    RESTRequest.Resource := '';
    RESTRequest.Params.AddItem('Authorization', 'Bearer ' + APIKey, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
    RESTRequest.Params.AddItem('Content-Type', 'application/json', TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
    JSONBody := TJSONObject.Create;
    JSONMessages := TJSONArray.Create;
    try
      for Message in Messages do
      begin
        JSONMsg := TJSONObject.Create;
        JSONMsg.AddPair('role', Message.Role.ToLower);
        JSONMsg.AddPair('content', Message.Content);
        JSONMessages.AddElement(JSONMsg);
      end;


      JSONBody.AddPair('model', 'gpt-3.5-turbo');
      JSONBody.AddPair('messages', JSONMessages);
//      JSONBody.AddPair('max_tokens', TJSONNumber.Create(50)); // Adjust the number of tokens as needed
//      JSONBody.AddPair('n', TJSONNumber.Create(1));
      RESTRequest.AddBody(JSONBody.ToString, TRESTContentType.ctAPPLICATION_JSON);
      RESTRequest.Execute;
      if RESTResponse.StatusCode = 200 then
      begin
        var jsonResponse := TJSONObject.ParseJSONValue(RESTResponse.Content) as TJSONObject;
        try
          var choices := jsonResponse.GetValue<TJSONArray>('choices');
          var usage := jsonResponse.GetValue<TJSONObject>('usage');
          usage.TryGetValue('completion_tokens', Result.Completion_Tokens);
          usage.TryGetValue('prompt_tokens', Result.Prompt_Tokens);
          usage.TryGetValue('total_tokens', Result.Total_Tokens);
          var choice := choices.Items[0] as TJSONObject;
          Result.Content := choice.GetValue('message').GetValue<string>('content');
        finally
          FreeAndNil(jsonResponse);
        end;
      end
      else
      begin
        raise Exception.CreateFmt('Error: %d - %s', [RESTResponse.StatusCode, RESTResponse.StatusText]);
      end;
    finally
      FreeAndNil(JSONBody);
    end;
  finally
    FreeAndNil(RESTClient);
    FreeAndNil(RESTRequest);
    FreeAndNil(RESTResponse);
  end;
end;

class function TOpenAI.CallDALL_E(const prompt: string; n: Integer; size: TDALLESize): TGeneratedImagesClass;
var
  LClient: TRESTClient;
  LRequest: TRESTRequest;
  LResponse: TRESTResponse;
  url: string;
  json: TJSONObject;
begin
  LClient := nil;
  LRequest := nil;
  LResponse := nil;
  json := nil;
  LClient := TRESTClient.Create(nil);
  LClient.ReadTimeout := 60000;
  try
    LRequest := TRESTRequest.Create(nil);
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
    FreeAndNil(LRequest);
    FreeAndNil(LResponse);
    FreeAndNil(LClient);
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

class function TOpenAI.Embeddings(const Texts: TArray<string>): TArray<TArray<Double>>;
var
  LRestClient: TRESTClient;
  LRestRequest: TRESTRequest;
  LRestResponse: TRESTResponse;
  LJsonRequest: TJSONArray;
  LDataArray, LEmbeddingArray: TJSONArray;
  LJsonResponse : TJSONObject;
  LJson: TJSONObject;
  I, J: Integer;
begin
  LRestClient := TRESTClient.Create(API_URL);
  LRestRequest := TRESTRequest.Create(nil);
  LRestResponse := TRESTResponse.Create(nil);

  try
    LRestRequest.Client := LRestClient;
    LRestRequest.Response := LRestResponse;
    LRestRequest.Method := TRESTRequestMethod.rmPOST;

    LJsonRequest := TJSONArray.Create;
    for I := 0 to High(Texts) do
      LJsonRequest.AddElement(TJSONString.Create(Texts[I]));

    LJson := TJSONObject.Create;
    LJson.AddPair('input', LJsonRequest);

    LJson.AddPair('model', 'text-embedding-ada-002');

    LRestRequest.AddBody(LJson.ToString, TRESTContentType.ctAPPLICATION_JSON);
    LRestRequest.AddAuthParameter('Authorization', 'Bearer ' + CHATGPT_APIKEY, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
    LRestRequest.Execute;

    if LRestResponse.StatusCode = 200 then
    begin
      LJsonResponse := TJSONObject.ParseJSONValue(LRestResponse.Content) as TJSONObject;
      LDataArray := LJsonResponse.GetValue<TJSONArray>('data');
      SetLength(Result, LDataArray.Count);

      for I := 0 to LDataArray.Count - 1 do
      begin
        LEmbeddingArray := LDataArray.Items[I].GetValue<TJSONArray>('embedding');
        SetLength(Result[I], LEmbeddingArray.Count);
        for J := 0 to LEmbeddingArray.Count - 1 do
          Result[I][J] := (LEmbeddingArray.Items[J] as TJSONNumber).AsDouble;
      end;

      FreeAndNil(LJsonResponse);
    end
    else
      raise Exception.CreateFmt('Error: %d - %s', [LRestResponse.StatusCode, LRestResponse.StatusText]);


  finally
    FreeAndNil(LJson);
    FreeAndNil(LRestRequest);
    FreeAndNil(LRestClient);
  end;
end;

class procedure TOpenAI.ListOpenAIModels(out AModelList: TStringList);
var
  LRESTClient: TRESTClient;
  LRESTRequest: TRESTRequest;
  LRESTResponse: TRESTResponse;
  LJSONValue: TJSONValue;
  LJSONArray: TJSONArray;
  LJSONModel: TJSONObject;
  LBaseJSONObject: TJSONObject;
  i: Integer;
begin
  LRESTClient := TRESTClient.Create('https://api.openai.com');
  try
    LRESTRequest := TRESTRequest.Create(nil);
    try
      LRESTResponse := TRESTResponse.Create(nil);
      try
        LRESTRequest.Client := LRESTClient;
        LRESTRequest.Resource := '/v1/models';
        LRESTRequest.Method := rmGET;
        LRESTRequest.Response := LRESTResponse;

        // Add your API key to the request header
        LRESTRequest.Params.AddItem('Authorization', 'Bearer ' + CHATGPT_APIKEY, pkHTTPHEADER, [poDoNotEncode]);

        LRESTRequest.Execute;

        if LRESTResponse.StatusCode = 200 then
        begin
          LBaseJSONObject := TJSONObject.ParseJSONValue(LRESTResponse.JSONText) as TJSONObject;
          try
            if LBaseJSONObject.TryGetValue<TJSONArray>('data', LJSONArray) then
            begin
              for i := 0 to LJSONArray.Count - 1 do
              begin
                LJSONModel := LJSONArray.Items[i] as TJSONObject;
                AModelList.Add(LJSONModel.GetValue<string>('id'));
              end;
            end;
          finally
            FreeAndNil(LBaseJSONObject);
          end;
        end;
      finally
        FreeAndNil(LRESTResponse);
      end;
    finally
      FreeAndNil(LRESTRequest);
    end;
  finally
    FreeAndNil(LRESTClient);
  end;
end;

end.
