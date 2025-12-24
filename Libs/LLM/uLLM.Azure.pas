unit uLLM.Azure;

interface
uses
  System.Classes,
  System.JSON,
  System.SysUtils,
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
    constructor Create;
    destructor Destroy; override;
  end;

  TModelsResponse = class
    data: TArray<TModelData>;
    destructor Destroy; override;
  end;


  TMicrosoftOpenAI = class(TBaseLLM)
  private
    FEndpoint : string;
    FDeploymentId: string;
    function GetAzureModels: TModelsResponse;
  protected
    function GetModelInfo: TObjectList<TBaseModelInfo>; override;
  private
    procedure CreateRESTClientAndRequest(out AClient: TRESTClient; out ARequest: TRESTRequest; out AResponse: TRESTResponse);
    procedure BuildJSONRequestBody(ARequest: TRESTRequest; ChatConfig: TChatSettings; AMessages: TObjectList<TChatMessage>);
    procedure ProcessResponse(LJSONResponse: TJSONObject; var AResponse: TChatResponse; out FunctionReturnValue: string; AMessages : TObjectList<TChatMessage>);
    procedure HandleErrorResponse(AResponse: TRESTResponse);
  public
    constructor Create(const APIKey: string; const Endpoint: string; const DeploymentId: string);
    destructor Destroy; override;
    function ChatCompletion(ChatConfig: TChatSettings; AMessages: TObjectList<TChatMessage>): TChatResponse; override;
    function Completion(const AQuestion: string; const AModel: string): string; override;
  end;

implementation
{ TMicrosoftOpenAI }

uses
  FMX.Types
  ;


function TMicrosoftOpenAI.ChatCompletion(ChatConfig: TChatSettings; AMessages: TObjectList<TChatMessage>): TChatResponse;
const
  API_Version = '2024-10-21';
var
  LRESTClient: TRESTClient;
  LRESTRequest: TRESTRequest;
  LRESTResponse: TRESTResponse;
  LJSONResponse: TJSONObject;
  FunctionReturnValue: string;
begin
  Result := Default(TChatResponse);
  Result.Content := '';
  Result.Completion_Tokens := 0;
  Result.Prompt_Tokens := 0;
  Result.Total_Tokens := 0;
  CreateRESTClientAndRequest(LRESTClient, LRESTRequest, LRESTResponse);
  try
    BuildJSONRequestBody(LRESTRequest, ChatConfig, AMessages);
    LRESTRequest.Execute;
    if LRESTResponse.StatusCode = 200 then
    begin
      LJSONResponse := LRESTResponse.JSONValue as TJSONObject;
      ProcessResponse(LJSONResponse, Result, FunctionReturnValue, AMessages);
      if Result.Finish_Reason = 'tool_calls' then
        Result := ChatCompletion(ChatConfig, AMessages);
    end
    else
    begin
      HandleErrorResponse(LRESTResponse);
    end;
  finally
    FreeAndNil(LRESTClient);
    FreeAndNil(LRESTRequest);
    FreeAndNil(LRESTResponse);
  end;
end;

procedure TMicrosoftOpenAI.CreateRESTClientAndRequest(out AClient: TRESTClient; out ARequest: TRESTRequest; out AResponse: TRESTResponse);
const
  API_Version = '2024-10-21';
begin
  AClient := TRESTClient.Create(nil);
  ARequest := TRESTRequest.Create(nil);
  AResponse := TRESTResponse.Create(nil);

  AClient.BaseURL := FEndPoint;
  AClient.Accept := 'application/json';
  AClient.AcceptCharset := 'UTF-8';

  ARequest.Client := AClient;
  ARequest.Response := AResponse;
  ARequest.Method := TRESTRequestMethod.rmPOST;
  ARequest.Timeout := 80000; // Set the timeout as needed
  ARequest.Resource := 'openai/deployments/{deploymentId}/chat/completions';
  ARequest.Params.AddItem('Authorization', 'Bearer ' + FAPIKey, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
  ARequest.Params.AddItem('Content-Type', 'application/json', TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
  ARequest.Params.AddItem('api-version', API_Version, TRESTRequestParameterKind.pkQUERY);
  ARequest.Params.AddItem('deploymentId', FDeploymentId, TRESTRequestParameterKind.pkURLSEGMENT);
  ARequest.AddParameter('api-key', FAPIKey, TRESTRequestParameterKind.pkHTTPHEADER);  
end;

procedure TMicrosoftOpenAI.BuildJSONRequestBody(ARequest: TRESTRequest; ChatConfig: TChatSettings; AMessages: TObjectList<TChatMessage>);
var
  LJSONBody: TJSONObject;
  LJSONMessages: TJSONArray;
  LJSONMessage: TJSONObject;
  LMessage: TChatMessage;
  LJSONFunctions: TJSONArray;
begin
  LJSONBody := nil;
  LJSONMessages := nil;
  LJSONMessage := nil;
  LMessage := nil;
  LJSONFunctions := nil;

  try
    LJSONBody := TJSONObject.Create;
    LJSONMessages := TJSONArray.Create;
    for LMessage in AMessages do
    begin
      LJSONMessages.AddElement(LMessage.AsJSON);
    end;

    if ChatConfig.model.IsEmpty then
      ChatConfig.model := 'gpt-4o';

    LJSONBody.AddPair('model', ChatConfig.model);
    LJSONBody.AddPair('messages', LJSONMessages);
    if ChatConfig.max_tokens > 0 then
      LJSONBody.AddPair('max_tokens', ChatConfig.max_tokens);
    if ChatConfig.user.Length > 0 then
      LJSONBody.AddPair('user', ChatConfig.user);
    if ChatConfig.n > 0 then
      LJSONBody.AddPair('n', ChatConfig.n);
    if ChatConfig.seed > 0 then
      LJSONBody.AddPair('seed', ChatConfig.seed);
    if ChatConfig.json_mode then
    begin
      LJSONMessage := TJSONObject.Create;
      LJSONMessage.AddPair('type', 'json_object');
      LJSONBody.AddPair('response_format', LJSONMessage);
    end;

    // Include available functions in the request
    if Functions.Count > 0 then
    begin
      LJSONFunctions := Functions.GetAvailableFunctionsJSON;
      LJSONBody.AddPair('tools', LJSONFunctions as TJSONArray);
      LJSONBody.AddPair('tool_choice', 'auto');
    end;
    Log.d(LJSONBody.ToJSON);
    ARequest.AddBody(LJSONBody.ToJSON, TRESTContentType.ctAPPLICATION_JSON);
  finally
    FreeAndNil(LJSONBody);
  end;
end;

procedure TMicrosoftOpenAI.ProcessResponse(LJSONResponse: TJSONObject; var AResponse: TChatResponse; out FunctionReturnValue: string; AMessages : TObjectList<TChatMessage>);
var
  LChoices: TJSONArray;
  LChoice: TJSONObject;
  LUsage: TJSONObject;
  LMessageJSON: TJSONObject;
  ToolCallsArray: TJSONArray;
  ToolValue : TJSONValue;
  ToolCall: TJSONObject;
  FunctionCallObj: TJSONObject;
  FunctionId: string;
  FunctionName: string;
  content : string;
begin
  LChoices := LJSONResponse.GetValue<TJSONArray>('choices');
  if Assigned(LJSONResponse.GetValue('model')) then
    AResponse.Model := LJSONResponse.GetValue('model').Value;

  if Assigned(LJSONResponse.GetValue('id')) then
    AResponse.Log_Id := LJSONResponse.GetValue('id').Value;

  LUsage := LJSONResponse.GetValue<TJSONObject>('usage');
  LUsage.TryGetValue('completion_tokens', AResponse.Completion_Tokens);
  LUsage.TryGetValue('prompt_tokens', AResponse.Prompt_Tokens);
  LUsage.TryGetValue('total_tokens', AResponse.Total_Tokens);
  LChoice := LChoices.Items[0] as TJSONObject;
  LMessageJSON := LChoice.GetValue('message') as TJSONObject;
  Content := '';
  LMessageJSON.TryGetValue<string>('content', Content);
  AResponse.Content := Content;
  AResponse.Finish_Reason := LChoice.GetValue('finish_reason').Value;

  // Handle function calls
  if Assigned(LMessageJSON) then
  begin

    if LMessageJSON.TryGetValue<TJSONArray>('tool_calls', ToolCallsArray) then
    begin
      var funcCall := TFunctionCallMessage.Create(ToolCallsArray.Clone as TJSONArray);
      AMessages.Add(funcCall);

      for ToolValue in ToolCallsArray do
      begin
        ToolCall := ToolValue as TJSONObject;
        FunctionCallObj := ToolCall.GetValue<TJSONObject>('function');
        FunctionId := ToolCall.GetValue<string>('id');
        FunctionName := FunctionCallObj.GetValue<string>('name');
        // Invoke the function with the arguments
        Functions.InvokeFunction(FunctionCallObj, FunctionReturnValue);
        var funcMsg := TFunctionMessage.Create;
        funcMsg.function_name := FunctionName;
        funcMsg.Content := FunctionReturnValue;
        funcMsg.Id := FunctionId;
        AMessages.Add(funcMsg);
      end;
    end;
  end;

  if Assigned(LJSONResponse.GetValue('system_fingerprint')) then
    AResponse.System_Fingerprint := LJSONResponse.GetValue('system_fingerprint').Value;
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
        'Error: %s - %s',
        [LJSONMsg.GetValue<string>('code'),
         LJSONMsg.GetValue<string>('message')])
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
    LRestRequest.Resource := '/openai/models?api-version=2024-08-01-preview';
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
  if Assigned(FModelInfo) and (FModelInfo.Count <> 0) then
  begin
    Result := FModelInfo;
    Exit;
  end;

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

{ TModelData }

constructor TModelData.Create;
begin

end;

destructor TModelData.Destroy;
begin
  FreeAndNil(capabilities);
  FreeAndNil(deprecation);
end;

{ TModelsResponse }

destructor TModelsResponse.Destroy;
var
  i : Integer;
begin
  for i := 0 to High(data) do
  begin
    FreeAndNil(data[i]);
  end;
end;

end.
