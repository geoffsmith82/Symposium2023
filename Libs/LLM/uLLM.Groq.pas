unit uLLM.Groq;

interface

uses
  System.Classes,
  System.JSON,
  System.SysUtils,
  windows,
  System.Generics.Collections,
  REST.Client,
  REST.Types,
  uLLM,
  uLLM.Functions
  ;

type
  TGroqLLM = class(TBaseLLM)
  protected
    function GetModelInfo: TObjectList<TBaseModelInfo>; override;
  private
    FFunctions : TFunctionRegistry;
    procedure ListOpenAIModels(out AModelList: TStringList);
    procedure CreateRESTClientAndRequest(out AClient: TRESTClient; out ARequest: TRESTRequest; out AResponse: TRESTResponse);
    procedure BuildJSONRequestBody(ARequest: TRESTRequest; ChatConfig: TChatSettings; AMessages: TObjectList<TChatMessage>);
    procedure ProcessResponse(LJSONResponse: TJSONObject; var AResponse: TChatResponse; out FunctionReturnValue: string; AMessages : TObjectList<TChatMessage>);
    procedure HandleErrorResponse(AResponse: TRESTResponse);
  public
    constructor Create(const APIKey: string);
    destructor Destroy; override;
    function ChatCompletion(ChatConfig: TChatSettings; AMessages: TObjectList<TChatMessage>): TChatResponse; override;
    function Completion(const AQuestion: string; const AModel: string): string; override;
    property Functions: TFunctionRegistry read FFunctions;
  end;

implementation

{ TGroqLLM }


function TGroqLLM.ChatCompletion(ChatConfig: TChatSettings; AMessages: TObjectList<TChatMessage>): TChatResponse;
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

procedure TGroqLLM.CreateRESTClientAndRequest(out AClient: TRESTClient; out ARequest: TRESTRequest; out AResponse: TRESTResponse);
begin
  AClient := TRESTClient.Create(nil);
  ARequest := TRESTRequest.Create(nil);
  AResponse := TRESTResponse.Create(nil);
  AClient.BaseURL := 'https://api.groq.com/openai/';
  AClient.Accept := 'application/json';
  AClient.AcceptCharset := 'UTF-8';
  ARequest.Client := AClient;
  ARequest.Response := AResponse;
  ARequest.Method := TRESTRequestMethod.rmPOST;
  ARequest.Timeout := 80000; // Set the timeout as needed
  ARequest.Resource := '/v1/chat/completions';
  ARequest.Params.AddItem('Authorization', 'Bearer ' + FAPIKey, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
  ARequest.Params.AddItem('Content-Type', 'application/json', TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
end;

procedure TGroqLLM.BuildJSONRequestBody(ARequest: TRESTRequest; ChatConfig: TChatSettings; AMessages: TObjectList<TChatMessage>);
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
      ChatConfig.model := 'llama-3.2-90b-text-preview';

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
    OutputDebugString(PChar(LJSONBody.ToJSON));
    ARequest.AddBody(LJSONBody.ToJSON, TRESTContentType.ctAPPLICATION_JSON);
  finally
    FreeAndNil(LJSONBody);
  end;
end;

procedure TGroqLLM.ProcessResponse(LJSONResponse: TJSONObject; var AResponse: TChatResponse; out FunctionReturnValue: string; AMessages : TObjectList<TChatMessage>);
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

procedure TGroqLLM.HandleErrorResponse(AResponse: TRESTResponse);
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

constructor TGroqLLM.Create(const APIKey: string);
begin
  inherited Create(APIKey);
  FFunctions := TFunctionRegistry.Create;
end;

destructor TGroqLLM.Destroy;
begin
  FreeAndNil(FFunctions);
  inherited;
end;

function TGroqLLM.Completion(const AQuestion, AModel: string): string;
begin
  raise Exception.Create('Completion Not implemented. Use ChatCompletion instead');
end;

function TGroqLLM.GetModelInfo: TObjectList<TBaseModelInfo>;
var
  LModelList : TStringList;
  LModel : string;
  LModelObj : TBaseModelInfo;
begin
  LModelList := TStringList.Create;
  try
    ListOpenAIModels(LModelList);
    FModelInfo.Clear;
    for LModel in LModelList do
    begin
      LModelObj := TBaseModelInfo.Create;
      LModelObj.modelName := LModel;
      FModelInfo.Add(LModelObj);
    end;
  finally
    FreeandNil(LModelList);
  end;
  Result := FModelInfo;
end;

procedure TGroqLLM.ListOpenAIModels(out AModelList: TStringList);
var
  LRESTClient: TRESTClient;
  LRESTRequest: TRESTRequest;
  LRESTResponse: TRESTResponse;
  LJSONArray: TJSONArray;
  LJSONModel: TJSONObject;
  LBaseJSONObject: TJSONObject;
  i: Integer;
begin
  LRESTClient := nil;
  LRESTRequest := nil;
  LRESTResponse := nil;
  LBaseJSONObject := nil;

  try
    LRESTClient := TRESTClient.Create('https://api.groq.com/openai/');
    LRESTRequest := TRESTRequest.Create(nil);
    LRESTResponse := TRESTResponse.Create(nil);
    LRESTRequest.Client := LRESTClient;
    LRESTRequest.Resource := '/v1/models';
    LRESTRequest.Method := rmGET;
    LRESTRequest.Response := LRESTResponse;

    // Add your API key to the request header
    LRESTRequest.Params.AddItem('Authorization', 'Bearer ' + FAPIKey, pkHTTPHEADER, [poDoNotEncode]);

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
    FreeAndNil(LRESTRequest);
    FreeAndNil(LRESTClient);
  end;
end;

end.
