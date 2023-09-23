unit uAzureGPT;

interface
uses
  System.Generics.Collections,
  REST.Client,
  REST.Types,
  REST.Response.Adapter,
  System.JSON,
  System.SysUtils,
  uLLM
  ;
type
  TMicrosoftOpenAI = class(TBaseLLM)
  strict private
    FAPIKey : string;
  public
    constructor Create(APIKey: string);
    function ChatCompletion(ChatConfig: TChatSettings; AMessages: TObjectList<TChatMessage>): TChatResponse; override;
    function Completion(const AQuestion: string; const AModel: string): string; override;
    function Embeddings(const Texts: TArray<string>): TEmbeddings;
  end;

implementation
{$I APIKEY.INC}
{ TMicrosoftOpenAI }

constructor TMicrosoftOpenAI.Create(APIKey: string);
begin
  inherited Create(APIKey);
end;

function TMicrosoftOpenAI.ChatCompletion(ChatConfig: TChatSettings;
  AMessages: TObjectList<TChatMessage>): TChatResponse;
const
  API_Version = '2023-03-15-preview';
var
  LRESTClient: TRESTClient;
  LRESTRequest: TRESTRequest;
  LRESTResponse: TRESTResponse;
  LJSONObj: TJSONObject;
  LJSONArray: TJSONArray;
  LJSONMessage: TJSONObject;
  LChatMessage: TChatMessage;
  LResponseJSON: TJSONObject;
  LChoicesArray: TJSONArray;
  LUsage: TJSONObject;
  LChoice: TJSONObject;
begin
  LRESTClient := TRESTClient.Create(AzureOpenAIEndpoint);
  try
    LRESTRequest := TRESTRequest.Create(nil);
    LRESTResponse := TRESTResponse.Create(nil);

    try
      LRESTRequest.Client := LRESTClient;
      LRESTRequest.Response := LRESTResponse;
      LRESTRequest.Resource := 'openai/deployments/gpt-35-turbo/chat/completions';
      LRESTRequest.Method := TRESTRequestMethod.rmPOST;
      LRESTRequest.Params.AddItem('api-version', API_Version, TRESTRequestParameterKind.pkQUERY);

      LRESTRequest.AddParameter('Content-Type', 'application/json', TRESTRequestParameterKind.pkHTTPHEADER,[poDoNotEncode]);
      LRESTRequest.AddParameter('api-key', FAPIKey, TRESTRequestParameterKind.pkHTTPHEADER);

      LJSONObj := TJSONObject.Create;
      try
        LJSONArray := TJSONArray.Create;
        for LChatMessage in AMessages do
        begin
          LJSONMessage := TJSONObject.Create;
          LJSONMessage.AddPair('role', LChatMessage.Role.ToLower);
          LJSONMessage.AddPair('content', LChatMessage.Content);
          LJSONArray.AddElement(LJSONMessage);
        end;

        LJSONObj.AddPair('messages', LJSONArray);
        LRESTRequest.AddBody(LJSONObj.ToString, TRESTContentType.ctAPPLICATION_JSON);

        LRESTRequest.Execute;

        LResponseJSON := TJSONObject.ParseJSONValue(LRESTResponse.Content) as TJSONObject;
        try
          LChoicesArray := LResponseJSON.GetValue<TJSONArray>('choices');
          LUsage := LResponseJSON.GetValue<TJSONObject>('usage');
          LUsage.TryGetValue('completion_tokens', Result.Completion_Tokens);
          LUsage.TryGetValue('prompt_tokens', Result.Prompt_Tokens);
          LUsage.TryGetValue('total_tokens', Result.Total_Tokens);
          LChoice := LChoicesArray.Items[0] as TJSONObject;
          Result.Content := LChoice.GetValue('message').GetValue<string>('content');
        finally
          FreeAndNil(LResponseJSON);
        end;
      finally
        FreeAndNil(LJSONObj);
      end;
    finally
      FreeAndNil(LRESTRequest);
      FreeAndNil(LRESTResponse);
    end;
  finally
    FreeAndNil(LRESTClient);
  end;
end;


function TMicrosoftOpenAI.Completion(const AQuestion, AModel: string): string;
begin
  raise Exception.Create('Not Implemented');
end;

function TMicrosoftOpenAI.Embeddings(const Texts: TArray<string>): TEmbeddings;
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
  LRestClient := TRESTClient.Create(AzureOpenAIEndpoint);
  LRestRequest := TRESTRequest.Create(nil);
  LRestResponse := TRESTResponse.Create(nil);

  try
    LRestRequest.Client := LRestClient;
    LRestRequest.Response := LRestResponse;
    LRestRequest.Method := TRESTRequestMethod.rmPOST;
    LRestRequest.Resource := '/openai/deployments/{deployment-id}/embeddings?api-version={api-version}';

    LRestRequest.AddParameter('deployment-id', 'text-embedding-ada-002', pkURLSEGMENT);
    LRestRequest.AddParameter('api-version', '2023-03-15-preview', pkQUERY);


    LJsonRequest := TJSONArray.Create;
    for I := 0 to High(Texts) do
      LJsonRequest.AddElement(TJSONString.Create(Texts[I]));

    LJson := TJSONObject.Create;
    LJson.AddPair('input', LJsonRequest);

    LJson.AddPair('model', 'text-embedding-ada-002');

    LRestRequest.AddBody(LJson.ToString, TRESTContentType.ctAPPLICATION_JSON);
    LRestRequest.AddAuthParameter('Authorization', 'Bearer ' + FAPIKey, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
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

end.
