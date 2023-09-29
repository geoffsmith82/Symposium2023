unit uGoogle.PaLM;

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
  EGooglePaLMError = class(ELLMException)
  private
    FErrorType: string;
  public
    constructor Create(const AErrorType, AMessage: string);
    property ErrorType: string read FErrorType;
  end;

  TGenerativeModelResponse = record
    name: string;
    baseModelId: string;
    version: string;
    displayName: string;
    description: string;
    inputTokenLimit: Integer;
    outputTokenLimit: Integer;
    supportedGenerationMethods: TArray<string>;
    temperature: Double;
    topP: Double;
    topK: Integer;
  end;

  TGooglePaLM = class(TBaseLLM)
  private
    function GetGenerativeLanguageModels: string;
    function ParseModelsResponse(const AJsonStr: string): TArray<TGenerativeModelResponse>;
  protected
    function GetModelInfo: TObjectList<TBaseModelInfo>; override;
  public
    function ChatCompletion(ChatConfig: TChatSettings; AMessages: TObjectList<TChatMessage>): TChatResponse; override;
    function Completion(const AQuestion: string; const AModel: string): string; override;
    function Embeddings(const Texts: TArray<string>): TEmbeddings;
  end;


implementation

{ TGooglePaLM }

function TGooglePaLM.ChatCompletion(ChatConfig: TChatSettings;
  AMessages: TObjectList<TChatMessage>): TChatResponse;
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
  Params: TJSONObject;
  JSONCandidates: TJSONArray;
  JSONCandidate: TJSONObject;
  ErrorJSON: TJSONObject;
  ErrorMsg: string;
  ErrorCode: string;
  ChatPrompt: TJSONArray;
  Msgs : TJSONObject;
  I: Integer;
  ResponseJSON: TJSONObject;
  msg : TJSONObject;
begin
  // Build the chat prompt
  ChatPrompt := TJSONArray.Create;
  for I := 0 to AMessages.Count - 1 do
  begin
    msg := TJSONObject.Create;
    msg.AddPair('author', AMessages[I].Role);
    msg.AddPair('content', AMessages[I].Content);
    ChatPrompt.Add(msg);
  end;

  // Create REST client
  RESTClient := TRESTClient.Create(nil);
  try
    RESTClient.BaseURL := 'https://generativelanguage.googleapis.com';

    RESTRequest := TRESTRequest.Create(nil);
    RESTResponse := TRESTResponse.Create(nil);
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Method := rmPOST;
    RESTRequest.Resource := '/v1beta2/' + ChatConfig.model + ':generateMessage?key=' + FAPIKey;

    // Populate request parameters
    Params := TJSONObject.Create;

    Msgs := TJSONObject.Create;

    Msgs.AddPair('messages', ChatPrompt);

    Params.AddPair('prompt', Msgs);
    if (ChatConfig.temperature > 0.000001) then
      Params.AddPair('temperature', ChatConfig.temperature);
    if (ChatConfig.top_p > 0.000001) then
    Params.AddPair('top_p', ChatConfig.top_p);
    // etc

    // Set request body to JSON
    RESTRequest.Body.ClearBody;
    RESTRequest.AddBody(Params);

    RESTRequest.Execute;

    // Parse JSON response
    if RESTRequest.Response.StatusCode = 200 then
    begin
      ResponseJSON := RESTRequest.Response.JSONValue as TJSONObject;
      if Assigned(ResponseJSON.GetValue('candidates')) then
      begin
        JSONCandidates := ResponseJSON.GetValue('candidates') as TJSONArray;
        JSONCandidate := JSONCandidates[0] as TJSONObject;
        Result.Content := JSONCandidate.GetValue('content').Value;
      end;
    end
    else
    begin
      // Parse error message
      ErrorJSON := (RESTResponse.JSONValue as TJSONObject).GetValue('error') as TJSONObject;
      ErrorMsg := ErrorJSON.GetValue('message').Value;
      ErrorCode := ErrorJSON.GetValue('code').Value;
      // Raise exception
      raise EGooglePaLMError.Create(ErrorCode, ErrorMsg);
    end;
  finally
    FreeAndNil(RESTResponse);
    FreeAndNil(RESTClient);
    FreeAndNil(RESTRequest);
    FreeAndNil(Params);
  end;
end;

function TGooglePaLM.Completion(const AQuestion, AModel: string): string;
begin
  raise Exception.Create('Not Implemented Yet');
end;

function TGooglePaLM.Embeddings(const Texts: TArray<string>): TEmbeddings;
begin
  raise Exception.Create('Not Implemented Yet');
end;

function TGooglePaLM.ParseModelsResponse(const AJsonStr: string): TArray<TGenerativeModelResponse>;
var
  JsonObj, ModelObj: TJSONObject;
  ModelsArr: TJSONArray;
  I, J: Integer;
  GenMethodsArr: TJSONArray;
begin
  JsonObj := TJSONObject.ParseJSONValue(AJsonStr) as TJSONObject;
  try
    ModelsArr := JsonObj.GetValue<TJSONArray>('models');
    SetLength(Result, ModelsArr.Count);

    for I := 0 to ModelsArr.Count - 1 do
    begin
      ModelObj := ModelsArr.Items[I] as TJSONObject;
      with Result[I] do
      begin
        name := ModelObj.GetValue<string>('name');
        version := ModelObj.GetValue<string>('version');
        displayName := ModelObj.GetValue<string>('displayName');
        description := ModelObj.GetValue<string>('description');
        inputTokenLimit := ModelObj.GetValue<Integer>('inputTokenLimit');
        outputTokenLimit := ModelObj.GetValue<Integer>('outputTokenLimit');

        if ModelObj.TryGetValue<TJSONArray>('supportedGenerationMethods', GenMethodsArr) then
        begin
          SetLength(supportedGenerationMethods, GenMethodsArr.Count);
          for J := 0 to GenMethodsArr.Count - 1 do
            supportedGenerationMethods[J] := GenMethodsArr.Items[J].Value;
        end;
        if Assigned(ModelObj.GetValue('temperature')) then
          temperature := ModelObj.GetValue<Double>('temperature');
        if Assigned(ModelObj.GetValue('topP')) then
          topP := ModelObj.GetValue<Double>('topP');
        if Assigned(ModelObj.GetValue('topK')) then
          topK := ModelObj.GetValue<Integer>('topK');
      end;
    end;
  finally
    JsonObj.Free;
  end;
end;


function TGooglePaLM.GetGenerativeLanguageModels: string;
var
  RestClient: TRESTClient;
  RestRequest: TRESTRequest;
  RestResponse: TRESTResponse;
begin
  Result := '';
  RestClient := TRESTClient.Create(nil);
  RestRequest := TRESTRequest.Create(nil);
  RestResponse := TRESTResponse.Create(nil);
  try
    RestClient.BaseURL := 'https://generativelanguage.googleapis.com';
    RestRequest.Client := RestClient;
    RestRequest.Response := RestResponse;
    RestRequest.Resource := Format('/v1beta2/models?key=%s', [FAPIKey]);
    RestRequest.Execute;
    Result := RestResponse.Content;
  finally
    RestClient.Free;
    RestRequest.Free;
    RestResponse.Free;
  end;
end;


function TGooglePaLM.GetModelInfo: TObjectList<TBaseModelInfo>;
var
  JsonResponse: string;
  ParsedResponses: TArray<TGenerativeModelResponse>;
  SingleResponse: TGenerativeModelResponse;
  i: Integer;
  modelObj: TBaseModelInfo;
begin
  FModelInfo.Clear;
  JsonResponse := GetGenerativeLanguageModels;
  ParsedResponses := ParseModelsResponse(JsonResponse);

  // Example of accessing each model:
  for i := 0 to Length(ParsedResponses) - 1 do
  begin
    SingleResponse := ParsedResponses[i];
    modelObj := TBaseModelInfo.Create;
    // Now you can use fields of SingleResponse like SingleResponse.name, SingleResponse.version, etc.
    modelObj.modelName := SingleResponse.name;
    FModelInfo.Add(modelObj);
  end;

  Result := FModelInfo;
end;

{ EGooglePaLMError }

constructor EGooglePaLMError.Create(const AErrorType, AMessage: string);
begin
  inherited Create(AMessage);
  FErrorType := AErrorType;
end;

end.
