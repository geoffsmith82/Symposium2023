unit uLLM.Anthropic;

interface

uses
  System.Classes,


  REST.Response.Adapter,
  System.JSON,
  System.SysUtils,
  FMX.Types,
  windows,
  System.Generics.Collections,
  REST.Client,
  REST.Types,
  uLLM,
  uLLM.Functions
  ;

type
  EAnthropicError = class(ELLMException)
  private
    FErrorType: string;
  public
    constructor Create(const AErrorType, AMessage: string);
    property ErrorType: string read FErrorType;
  end;

  TAnthropic = class(TBaseLLM)
  protected
    function GetModelInfo: TObjectList<TBaseModelInfo>; override;
  private
    FFunctions : TFunctionRegistry;
    FOnLog: TOnLog;
    procedure CreateRESTClientAndRequest(out AClient: TRESTClient; out ARequest: TRESTRequest; out AResponse: TRESTResponse);
    procedure BuildJSONRequestBody(ARequest: TRESTRequest; ChatConfig: TChatSettings; AMessages: TObjectList<TChatMessage>);
    procedure ProcessResponse(LJSONResponse: TJSONObject; var AResponse: TChatResponse; out FunctionReturnValue: string; AMessages : TObjectList<TChatMessage>);
    procedure HandleErrorResponse(AResponse: TRESTResponse);
    procedure DoOnLog(inLog: string);
  public
    constructor Create(const APIKey: string);
    destructor Destroy; override;
    function ChatCompletion(ChatConfig: TChatSettings; AMessages: TObjectList<TChatMessage>): TChatResponse; override;
    function Completion(const AQuestion: string; const AModel: string): string; override;
    property Functions: TFunctionRegistry read FFunctions write FFunctions;
    property OnLog: TOnLog read FOnLog write FOnLog;
  end;

implementation

constructor EAnthropicError.Create(const AErrorType, AMessage: string);
begin
  inherited Create(AMessage);
  FErrorType := AErrorType;
end;

{ TAnthropic }

function TAnthropic.ChatCompletion(ChatConfig: TChatSettings; AMessages: TObjectList<TChatMessage>): TChatResponse;
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
      if Result.Finish_Reason = 'tool_use' then
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

procedure TAnthropic.CreateRESTClientAndRequest(out AClient: TRESTClient; out ARequest: TRESTRequest; out AResponse: TRESTResponse);
begin
  AClient := TRESTClient.Create(nil);
  ARequest := TRESTRequest.Create(nil);
  AResponse := TRESTResponse.Create(nil);

  AClient.BaseURL := 'https://api.anthropic.com/';
  AClient.Accept := 'application/json';
  AClient.AcceptCharset := 'UTF-8';

  ARequest.Client := AClient;
  ARequest.Response := AResponse;
  ARequest.Method := TRESTRequestMethod.rmPOST;
  ARequest.Timeout := 80000; // Set the timeout as needed
  ARequest.Resource := '/v1/messages';
  ARequest.Params.AddItem('Authorization', 'Bearer ' + FAPIKey, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
  ARequest.Params.AddItem('Content-Type', 'application/json', TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);


  ARequest.Params.AddItem('anthropic-version', '2023-06-01', TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
  ARequest.Params.AddItem('content-type', 'application/json', TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
  ARequest.Params.AddItem('x-api-key', FAPIKey, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
end;

procedure TAnthropic.BuildJSONRequestBody(ARequest: TRESTRequest; ChatConfig: TChatSettings; AMessages: TObjectList<TChatMessage>);
var
  LJSONBody: TJSONObject;
  LJSONMessages: TJSONArray;
  LJSONMessage: TJSONObject;
  LMessage: TChatMessage;
  LJSONFunctions: TJSONArray;
  LMsgNo: Integer;
begin
  LJSONBody := nil;
  LJSONMessages := nil;
  LJSONMessage := nil;
  LMessage := nil;
  LJSONFunctions := nil;

  try
    LJSONBody := TJSONObject.Create;
    LJSONMessages := TJSONArray.Create;
    LMsgNo := 0;
    for LMessage in AMessages do
    begin
      if (LMsgNo = 0) and (LMessage.Role.ToLower = 'system') then
      begin
        LJSONBody.AddPair('system', LMessage.Content);
      end
      else
        LJSONMessages.AddElement(LMessage.AsJSON);
      Inc(LMsgNo);
    end;

//    if ChatConfig.model.IsEmpty then
//      ChatConfig.model := 'gpt-4o';

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
      LJSONFunctions := Functions.GetAvailableFunctionsClaudeJSON(False);
      LJSONBody.AddPair('tools', LJSONFunctions as TJSONArray);
    end;
    OutputDebugString(PChar(LJSONBody.ToJSON));
    ARequest.AddBody(LJSONBody.ToJSON, TRESTContentType.ctAPPLICATION_JSON);
  finally
    FreeAndNil(LJSONBody);
  end;
end;

procedure TAnthropic.ProcessResponse(LJSONResponse: TJSONObject; var AResponse: TChatResponse; out FunctionReturnValue: string; AMessages : TObjectList<TChatMessage>);
var
  LChoices: TJSONArray;
  LChoice: TJSONValue;
  LUsage: TJSONObject;
  FunctionId: string;
  FunctionName: string;
begin
  LChoices := LJSONResponse.GetValue('content') as TJSONArray;
  if Assigned(LJSONResponse.GetValue('model')) then
      AResponse.Model := LJSONResponse.GetValue('model').Value;
  Log.d('Choices  ' + LChoices.ToJSON);

  DoOnLog('');
  DoOnLog('');
  DoOnLog(LJSONResponse.ToJSON);

  if Assigned(LJSONResponse.GetValue('id')) then
    AResponse.Log_Id := LJSONResponse.GetValue('id').Value;

  LUsage := LJSONResponse.GetValue<TJSONObject>('usage');
  LUsage.TryGetValue('completion_tokens', AResponse.Completion_Tokens);
  LUsage.TryGetValue('prompt_tokens', AResponse.Prompt_Tokens);
  LUsage.TryGetValue('total_tokens', AResponse.Total_Tokens);
  if LChoices.Count > 0 then      
  begin
    AResponse.Content := (LChoices.Items[0] as TJSONObject).GetValue('text').Value;
    LChoice := LChoices.Items[0] as TJSONObject;
  end;

  AResponse.Finish_Reason := LJSONResponse.GetValue('stop_reason').Value;
  for LChoice in LChoices do
  begin
      var msgType : string;
      if LChoice.TryGetValue<string>('type', msgType) then
      begin
        if msgType = 'tool_use' then
        begin
          var msg := TClaudeJSONFunctionMessage.Create(LChoice as TJSONObject);
          AMessages.Add(msg);
          FunctionId := LChoice.GetValue<string>('id');
          FunctionName := LChoice.GetValue<string>('name');
          // Invoke the function with the arguments
          Functions.InvokeClaudeFunction(LChoice as TJSONObject, FunctionReturnValue);
          var funcMsg := TClaudeFunctionMessage.Create;
          funcMsg.Content := FunctionReturnValue;
          funcMsg.Id := FunctionId;
          AMessages.Add(funcMsg);
        end;
      end;
  end;
end;

procedure TAnthropic.HandleErrorResponse(AResponse: TRESTResponse);
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

constructor TAnthropic.Create(const APIKey: string);
begin
  inherited Create(APIKey);
  FFunctions := TFunctionRegistry.Create;
end;

destructor TAnthropic.Destroy;
begin
  FreeAndNil(FFunctions);
  inherited;
end;

procedure TAnthropic.DoOnLog(inLog: string);
begin
  if Assigned(FOnLog) then
    FOnLog(inLog);
end;

function TAnthropic.Completion(const AQuestion, AModel: string): string;
var
  LChatConfig: TChatSettings;
  LMessages: TObjectList<TChatMessage>;
  LMsg: TChatMessage;
begin
  LChatConfig.model := AModel;
  LMessages := nil;
  LMsg := nil;
  try
    LMessages := TObjectList<TChatMessage>.Create;
    LMsg := TChatMessage.Create;
    LMsg.Role := 'Human';
    LMsg.Content := AQuestion;
    LMessages.Add(LMsg);
    Result := ChatCompletion(LChatConfig, LMessages).Content;
  finally
    FreeAndNil(LMessages);
  end;
end;

function TAnthropic.GetModelInfo: TObjectList<TBaseModelInfo>;
var
  LRESTClient: TRESTClient;
  LRESTRequest: TRESTRequest;
  LRESTResponse: TRESTResponse;
  LJSONArray: TJSONArray;
  LJSONModel: TJSONObject;
  LBaseJSONObject: TJSONObject;
  i: Integer;
  LModelObj : TBaseModelInfo;
begin
  Result := nil;
  if FModelInfo.Count > 0 then
  begin
    Result := FModelInfo;
    Exit;
  end;
  
  LRESTClient := nil;
  LRESTRequest := nil;
  LRESTResponse := nil;
  LBaseJSONObject := nil;

  try
    CreateRESTClientAndRequest(LRESTClient, LRESTRequest, LRESTResponse);
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
            LModelObj := TBaseModelInfo.Create;
            LModelObj.modelName := LJSONModel.GetValue<string>('id');
            LModelObj.version := LJSONModel.GetValue<string>('created_at');
            FModelInfo.Add(LModelObj);
          end;
          Result := FModelInfo;
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
