unit uLLM.Mistral;

interface

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.Generics.Collections,
  REST.Client,
  REST.Types,
  REST.Response.Adapter,
  uLLM,
  uLLM.Functions
  ;

type
  EMistralError = class(ELLMException);

  TMistralFunctionRegistry = class(TFunctionRegistry)
  public
    procedure InvokeFunction(const JSONObject: TJSONObject; out ReturnValue: string); override;
  end;

  TMistralFunctionCallMessage = class(TChatMessage)
  private
    FToolCalls: TJSONArray;
  public
    constructor Create(const ToolCalls: TJSONArray);
    destructor Destroy; override;
    function AsJSON: TJSONObject; override;
  end;

  TMistralFunctionResponseMessage = class(TChatMessage)
  private
    FToolCallID: string;
  public
    constructor Create(const ToolCallID, Content: string);
    function AsJSON: TJSONObject; override;
  end;

  TMistral = class(TBaseLLM)
  private
    FOnLog: TOnLog;
    procedure DoOnLog(const Msg: string);
    procedure CreateRESTClient(out AClient: TRESTClient; out ARequest: TRESTRequest; out AResponse: TRESTResponse);
    procedure BuildJSONBody(ChatConfig: TChatSettings; AMessages: TObjectList<TChatMessage>; out JSONBody: TJSONObject);
    procedure ProcessResponse(const AResponseJSON: TJSONObject; var AChatResponse: TChatResponse; AMessages: TObjectList<TChatMessage>; out FunctionReturnValue: string);
  protected
    function GetModelInfo: TObjectList<TBaseModelInfo>; override;
  public
    constructor Create(const APIKey: string);
    destructor Destroy; override;
    function ChatCompletion(ChatConfig: TChatSettings; AMessages: TObjectList<TChatMessage>): TChatResponse; override;
    function Completion(const AQuestion, AModel: string): string; override;

    property OnLog: TOnLog read FOnLog write FOnLog;
  end;

implementation

const
  BASE_URL = 'https://api.mistral.ai/v1';

{ TMistralFunctionRegistry }

procedure TMistralFunctionRegistry.InvokeFunction(const JSONObject: TJSONObject; out ReturnValue: string);
var
  FuncName: string;
  ArgsStr: string;
  InputObj: TJSONObject;
  WrappedJSON: TJSONObject;
begin
  FuncName := JSONObject.GetValue<string>('name');
  ArgsStr := JSONObject.GetValue<string>('arguments');

  if ArgsStr.IsEmpty then
    raise Exception.Create('Missing "arguments" for function call');

  InputObj := TJSONObject.ParseJSONValue(ArgsStr) as TJSONObject;
  if not Assigned(InputObj) then
    raise Exception.Create('Invalid arguments JSON string');

  WrappedJSON := nil;
  try
    WrappedJSON := TJSONObject.Create;
    WrappedJSON.AddPair('name', FuncName);
    WrappedJSON.AddPair('arguments', InputObj.ToJSON);
    inherited InvokeFunction(WrappedJSON, ReturnValue);
  finally
    FreeAndNil(InputObj);
    FreeAndNil(WrappedJSON);
  end;
end;

{ TMistralFunctionCallMessage }

constructor TMistralFunctionCallMessage.Create(const ToolCalls: TJSONArray);
begin
  inherited Create;
  Role := 'assistant';
  FToolCalls := ToolCalls.Clone as TJSONArray;
  Content := '';
end;

destructor TMistralFunctionCallMessage.Destroy;
begin
  FreeAndNil(FToolCalls);
  inherited;
end;

function TMistralFunctionCallMessage.AsJSON: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('role', Role);
  Result.AddPair('content', Content);
  Result.AddPair('tool_calls', FToolCalls.Clone as TJSONArray);
end;

{ TMistralFunctionResponseMessage }

constructor TMistralFunctionResponseMessage.Create(const ToolCallID, Content: string);
begin
  inherited Create;
  Role := 'tool';
  Self.Content := Content;
  FToolCallID := ToolCallID;
end;

function TMistralFunctionResponseMessage.AsJSON: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('role', 'tool');
  Result.AddPair('content', Content);
  Result.AddPair('tool_call_id', FToolCallID);
end;

{ TMistral }

constructor TMistral.Create(const APIKey: string);
begin
  inherited Create(APIKey);
  FFunctions := TMistralFunctionRegistry.Create;
end;

destructor TMistral.Destroy;
begin
  FreeAndNil(FFunctions);
  inherited;
end;

procedure TMistral.CreateRESTClient(out AClient: TRESTClient; out ARequest: TRESTRequest; out AResponse: TRESTResponse);
begin
  AClient := TRESTClient.Create(nil);
  ARequest := TRESTRequest.Create(nil);
  AResponse := TRESTResponse.Create(nil);

  AClient.BaseURL := BASE_URL;
  ARequest.Client := AClient;
  ARequest.Response := AResponse;
  ARequest.Method := rmPOST;
  ARequest.Resource := 'chat/completions';
  ARequest.Timeout := 60000;

  ARequest.Params.AddItem('Authorization', 'Bearer ' + FAPIKey, pkHTTPHEADER, [poDoNotEncode]);
  ARequest.Params.AddItem('Content-Type', 'application/json', pkHTTPHEADER, [poDoNotEncode]);
end;

function TMistral.ChatCompletion(ChatConfig: TChatSettings; AMessages: TObjectList<TChatMessage>): TChatResponse;
var
  Client: TRESTClient;
  Request: TRESTRequest;
  Response: TRESTResponse;
  JSONBody, JSONResult: TJSONObject;
  FuncResult: string;
begin
  Result := Default(TChatResponse);
  Result.ChatConfig := ChatConfig;
  CreateRESTClient(Client, Request, Response);
  try
    BuildJSONBody(ChatConfig, AMessages, JSONBody);
    try
      Request.AddBody(JSONBody.ToJSON, ctAPPLICATION_JSON);
      Request.Execute;

      if Response.StatusCode <> 200 then
        raise EMistralError.CreateFmt('Mistral Error: %s', [Response.StatusText]);

      JSONResult := TJSONObject.ParseJSONValue(Response.Content) as TJSONObject;
      try
        ProcessResponse(JSONResult, Result, AMessages, FuncResult);
      finally
        FreeAndNil(JSONResult);
      end;
    finally
      FreeAndNil(JSONBody);
    end;
  finally
    FreeAndNil(Client);
    FreeAndNil(Request);
    FreeAndNil(Response);
  end;
end;

procedure TMistral.BuildJSONBody(ChatConfig: TChatSettings; AMessages: TObjectList<TChatMessage>; out JSONBody: TJSONObject);
var
  JSONMessages: TJSONArray;
  Msg: TChatMessage;
begin
  JSONBody := TJSONObject.Create;
  JSONMessages := TJSONArray.Create;

  for Msg in AMessages do
    JSONMessages.AddElement(Msg.AsJSON);

  JSONBody.AddPair('model', ChatConfig.model);
  JSONBody.AddPair('messages', JSONMessages);
  JSONBody.AddPair('temperature', TJSONNumber.Create(ChatConfig.temperature));
  if ChatConfig.max_tokens > 0 then
    JSONBody.AddPair('max_tokens', TJSONNumber.Create(ChatConfig.max_tokens));

  if FFunctions.Count > 0 then
    JSONBody.AddPair('tools', FFunctions.GetAvailableFunctionsJSON);
end;

procedure TMistral.ProcessResponse(const AResponseJSON: TJSONObject; var AChatResponse: TChatResponse; AMessages: TObjectList<TChatMessage>; out FunctionReturnValue: string);
var
  Choices: TJSONArray;
  Choice: TJSONObject;
  ToolCalls: TJSONArray;
  ToolCall: TJSONObject;
  FuncId, FuncName: string;
  usage: TJSONObject;
begin
  if AResponseJSON.TryGetValue<TJSONArray>('choices', Choices) and (Choices.Count > 0) then
  begin
    Choice := Choices.Items[0] as TJSONObject;
    AChatResponse.Model := AResponseJSON.GetValue<string>('model');
    AChatResponse.Finish_Reason := Choice.GetValue<string>('finish_reason');

    if AChatResponse.Finish_Reason = 'stop' then
    begin
      AChatResponse.Content := Choice.GetValue<TJSONObject>('message').GetValue<string>('content');
      AChatResponse.Role := Choice.GetValue<TJSONObject>('message').GetValue<string>('role');

    end
    else if AChatResponse.Finish_Reason = 'tool_calls' then
    begin
      ToolCalls := Choice.GetValue<TJSONObject>('message').GetValue<TJSONArray>('tool_calls');
      for var i := 0 to ToolCalls.Count - 1 do
      begin
        ToolCall := ToolCalls[i] as TJSONObject;
        FuncId := ToolCall.GetValue<string>('id');
        FuncName := ToolCall.GetValue<TJSONObject>('function').GetValue<string>('name');

        FFunctions.InvokeFunction(ToolCall.GetValue<TJSONObject>('function'), FunctionReturnValue);

        AMessages.Add(TMistralFunctionCallMessage.Create(ToolCalls));
        AMessages.Add(TMistralFunctionResponseMessage.Create(FuncId, FunctionReturnValue));
        Sleep(2000);
      end;
      AChatResponse := ChatCompletion(AChatResponse.ChatConfig, AMessages);
    end;

    if AResponseJSON.TryGetValue<TJSONObject>('usage', usage) then
    begin
      AChatResponse.Prompt_Tokens := usage.GetValue<Integer>('prompt_tokens');
      AChatResponse.Completion_Tokens := usage.GetValue<Integer>('completion_tokens');
      AChatResponse.Total_Tokens := usage.GetValue<Integer>('total_tokens');
    end;
  end;
end;

function TMistral.Completion(const AQuestion, AModel: string): string;
var
  Config: TChatSettings;
  Messages: TObjectList<TChatMessage>;
  Msg: TChatMessage;
begin
  Config.model := AModel;
  Config.temperature := 0.7;

  Messages := TObjectList<TChatMessage>.Create;
  try
    Msg := TChatMessage.Create;
    Msg.Role := 'user';
    Msg.Content := AQuestion;
    Messages.Add(Msg);
    Result := ChatCompletion(Config, Messages).Content;
  finally
    FreeAndNil(Messages);
  end;
end;

procedure TMistral.DoOnLog(const Msg: string);
begin
  if Assigned(FOnLog) then
    FOnLog(Msg);
end;

function TMistral.GetModelInfo: TObjectList<TBaseModelInfo>;
var
  Client: TRESTClient;
  Request: TRESTRequest;
  Response: TRESTResponse;
  Data: TJSONArray;
  ModelJSON: TJSONObject;
  i: Integer;
  Info: TBaseModelInfo;
  Root: TJSONObject;
begin
  if FModelInfo.Count > 0 then
    Exit(FModelInfo);

  Client := TRESTClient.Create(BASE_URL + '/models');
  Request := TRESTRequest.Create(nil);
  Response := TRESTResponse.Create(nil);
  try
    Request.Client := Client;
    Request.Response := Response;
    Request.Method := rmGET;
    Request.Params.AddItem('Authorization', 'Bearer ' + FAPIKey, pkHTTPHEADER, [poDoNotEncode]);

    Request.Execute;

    if Response.StatusCode = 200 then
    begin
      Root := TJSONObject.ParseJSONValue(Response.Content) as TJSONObject;
      try
        if Root.TryGetValue<TJSONArray>('data', Data) then
        begin
          for i := 0 to Data.Count - 1 do
          begin
            ModelJSON := Data.Items[i] as TJSONObject;
            Info := TBaseModelInfo.Create;
            Info.modelName := ModelJSON.GetValue<string>('id');
            Info.version := '';
            FModelInfo.Add(Info);
          end;
        end;
        Result := FModelInfo;
      finally
        FreeAndNil(Root);
      end;
    end
    else
      raise EMistralError.CreateFmt('Failed to fetch models: %s', [Response.StatusText]);
  finally
    FreeAndNil(Client);
    FreeAndNil(Request);
    FreeAndNil(Response);
  end;
end;

end.

