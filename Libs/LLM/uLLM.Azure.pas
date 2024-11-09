unit uLLM.Azure;

interface
uses
  System.Classes,
  System.JSON,
  System.SysUtils,
  windows,
  System.Generics.Collections,
  REST.Client,
  REST.Types,
  REST.Response.Adapter,
  REST.Json,
  uLLM,
  uLLM.Functions
  ;
type
  TCapabilities = class
    fine_tune: Boolean;
    inference: Boolean;
    completion: Boolean;
    chat_completion: Boolean;
    embeddings: Boolean;
  end;

  TDeprecation = class
    fine_tune: Int64;
    inference: Int64;
  end;

  TModelData = class
    capabilities: TCapabilities;
    lifecycle_status: string;
    deprecation: TDeprecation;
    id: string;
    status: string;
    created_at: Int64;
    updated_at: Int64;
    object_type: string;
  end;

  TModelsResponse = class
    data: TArray<TModelData>;
  end;


  TMicrosoftOpenAI = class(TBaseLLM)
  private
    FEndpoint : string;
    FDeploymentId: string;
    function GetAzureModels: TModelsResponse;
  protected
    function GetModelInfo: TObjectList<TBaseModelInfo>; override;
  private
    FFunctions : TFunctionRegistry;
    procedure HandleErrorResponse(AResponse: TRESTResponse);
  public
    constructor Create(const APIKey: string; const Endpoint: string; const DeploymentId: string);
    destructor Destroy; override;
    function ChatCompletion(ChatConfig: TChatSettings; AMessages: TObjectList<TChatMessage>): TChatResponse; override;
    function Completion(const AQuestion: string; const AModel: string): string; override;
    property Functions: TFunctionRegistry read FFunctions write FFunctions;
  end;

implementation
{ TMicrosoftOpenAI }


function TMicrosoftOpenAI.ChatCompletion(ChatConfig: TChatSettings; AMessages: TObjectList<TChatMessage>): TChatResponse;
const
  API_Version = '2024-10-21';
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
  Result := Default(TChatResponse);
  LRESTClient := nil;
  LRESTRequest := nil;
  LRESTResponse := nil;
  try
    LRESTClient := TRESTClient.Create(FEndpoint);
    LRESTRequest := TRESTRequest.Create(nil);
    LRESTResponse := TRESTResponse.Create(nil);

    try
      LRESTRequest.Client := LRESTClient;
      LRESTRequest.Response := LRESTResponse;
      LRESTRequest.Resource := 'openai/deployments/{deploymentId}/chat/completions';
      LRESTRequest.Method := TRESTRequestMethod.rmPOST;
      LRESTRequest.Params.AddItem('api-version', API_Version, TRESTRequestParameterKind.pkQUERY);
      LRESTRequest.Params.AddItem('deploymentId', FDeploymentId, TRESTRequestParameterKind.pkURLSEGMENT);

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

procedure TMicrosoftOpenAI.HandleErrorResponse(AResponse: TRESTResponse);
var
  LJSONResponse: TJSONObject;
  LJSONMsg: TJSONObject;
begin
  LJSONResponse := TJSONObject.ParseJSONValue(AResponse.Content) as TJSONObject;
  if Assigned(LJSONResponse) then
  try
    if LJSONResponse.TryGetValue<TJSONObject>('error', LJSONMsg) then
    begin
      raise Exception.CreateFmt(
        'Error: %s - %s. Param: %s',
        [LJSONMsg.GetValue<string>('type'),
         LJSONMsg.GetValue<string>('message'),
         LJSONMsg.GetValue<string>('param')])
    end
    else
      raise Exception.CreateFmt('Error: %d - %s', [AResponse.StatusCode, AResponse.StatusText]);
  finally
    LJSONResponse.Free;
  end
  else
    raise Exception.CreateFmt('Error: %d - %s', [AResponse.StatusCode, AResponse.StatusText]);
end;


constructor TMicrosoftOpenAI.Create(const APIKey: string; const Endpoint: string; const DeploymentId: string);
begin
  inherited Create(APIKey);
  FEndpoint := Endpoint;
  FDeploymentId := DeploymentId;
  FFunctions := TFunctionRegistry.Create;  
end;


destructor TMicrosoftOpenAI.Destroy;
begin
  FreeAndNil(FFunctions);
  inherited;
end;
function TMicrosoftOpenAI.Completion(const AQuestion, AModel: string): string;
begin
  raise Exception.Create('Not Implemented');
end;

function TMicrosoftOpenAI.GetAzureModels: TModelsResponse;
var
  LRestClient: TRESTClient;
  LRestRequest: TRESTRequest;
  LRestResponse: TRESTResponse;
  LJsonResponse: TJSONObject;
  LJSONValue: TJSONValue;
begin
  Result := nil;
  LRestClient := TRESTClient.Create(nil);
  LRestRequest := TRESTRequest.Create(nil);
  LRestResponse := TRESTResponse.Create(nil);

  try
    LRestClient.BaseURL := FEndpoint;
    LRestClient.Accept := 'application/json';
    LRestClient.AcceptCharset := 'UTF-8';

    LRestRequest.Client := LRestClient;
    LRestRequest.Response := LRestResponse;
    LRestRequest.Method := rmGET;
    LRestRequest.Resource := '/openai/models?api-version=2023-05-15';
    LRestRequest.AddParameter('api-key', FAPIKey, TRESTRequestParameterKind.pkHTTPHEADER);

    LRestRequest.Execute;

    if LRestResponse.StatusCode = 200 then
    begin
      LJSONValue := TJSONObject.ParseJSONValue(LRestResponse.Content);
      try
        if LJSONValue is TJSONObject then
        begin
          LJsonResponse := LJSONValue as TJSONObject;
          Result := TJSON.JsonToObject<TModelsResponse>(LJsonResponse);
        end;
      finally
        LJSONValue.Free;
      end;
    end
    else
      raise Exception.CreateFmt('Error: %s (%d)', [LRestResponse.StatusText, LRestResponse.StatusCode]);
  finally
    LRestClient.Free;
    LRestRequest.Free;
    LRestResponse.Free;
  end;
end;


function TMicrosoftOpenAI.GetModelInfo: TObjectList<TBaseModelInfo>;
var
  LModelList : TModelsResponse;
  LModelObj: TBaseModelInfo;
  LModelData: TModelData;
begin
  FModelInfo.Clear;

  LModelList := GetAzureModels;
  try
    for LModelData in LModelList.data do
    begin
      LModelObj := TBaseModelInfo.Create;
      LModelObj.modelName := LModelData.id;
      FModelInfo.Add(LModelObj);
    end;
  finally
    FreeAndNil(LModelList);
  end;
  Result := FModelInfo;
end;

end.
