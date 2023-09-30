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

function TGooglePaLM.ChatCompletion(ChatConfig: TChatSettings; AMessages: TObjectList<TChatMessage>): TChatResponse;
var
  LRESTClient: TRESTClient;
  LRESTRequest: TRESTRequest;
  LRESTResponse: TRESTResponse;
  LParams: TJSONObject;
  LJSONCandidates: TJSONArray;
  LJSONCandidate: TJSONObject;
  LErrorJSON: TJSONObject;
  LErrorMsg: string;
  LErrorCode: string;
  LChatPrompt: TJSONArray;
  LMsgs : TJSONObject;
  I: Integer;
  LResponseJSON: TJSONObject;
  LMsg : TJSONObject;
begin
  Result := Default(TChatResponse);
  LChatPrompt := nil;
  LRESTClient := nil;
  LRESTRequest := nil;
  LRESTResponse := nil;
  LParams := nil;
  // Create REST client
  try
    // Build the chat prompt
    LChatPrompt := TJSONArray.Create;
    for I := 0 to AMessages.Count - 1 do
    begin
      LMsg := TJSONObject.Create;
      LMsg.AddPair('author', AMessages[I].Role);
      LMsg.AddPair('content', AMessages[I].Content);
      LChatPrompt.Add(LMsg);
    end;


    LRESTClient := TRESTClient.Create(nil);
    LRESTClient.BaseURL := 'https://generativelanguage.googleapis.com';

    LRESTRequest := TRESTRequest.Create(nil);
    LRESTResponse := TRESTResponse.Create(nil);
    LRESTRequest.Client := LRESTClient;
    LRESTRequest.Response := LRESTResponse;
    LRESTRequest.Method := rmPOST;
    LRESTRequest.Resource := '/v1beta2/' + ChatConfig.model + ':generateMessage?key=' + FAPIKey;

    // Populate request parameters
    LParams := TJSONObject.Create;

    LMsgs := TJSONObject.Create;

    LMsgs.AddPair('messages', LChatPrompt);

    LParams.AddPair('prompt', LMsgs);
    if (ChatConfig.temperature > 0.000001) then
      LParams.AddPair('temperature', ChatConfig.temperature);
    if (ChatConfig.top_p > 0.000001) then
    LParams.AddPair('top_p', ChatConfig.top_p);
    // etc

    // Set request body to JSON
    LRESTRequest.Body.ClearBody;
    LRESTRequest.AddBody(LParams);

    LRESTRequest.Execute;

    // Parse JSON response
    if LRESTRequest.Response.StatusCode = 200 then
    begin
      LResponseJSON := LRESTRequest.Response.JSONValue as TJSONObject;
      if Assigned(LResponseJSON.GetValue('candidates')) then
      begin
        LJSONCandidates := LResponseJSON.GetValue('candidates') as TJSONArray;
        LJSONCandidate := LJSONCandidates[0] as TJSONObject;
        Result.Content := LJSONCandidate.GetValue('content').Value;
      end;
    end
    else
    begin
      // Parse error message
      LErrorJSON := (LRESTResponse.JSONValue as TJSONObject).GetValue('error') as TJSONObject;
      LErrorMsg := LErrorJSON.GetValue('message').Value;
      LErrorCode := LErrorJSON.GetValue('code').Value;
      // Raise exception
      raise EGooglePaLMError.Create(LErrorCode, LErrorMsg);
    end;
  finally
    FreeAndNil(LRESTResponse);
    FreeAndNil(LRESTClient);
    FreeAndNil(LRESTRequest);
    FreeAndNil(LParams);
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
  LJsonObj, LModelObj: TJSONObject;
  LModelsArr: TJSONArray;
  I, J: Integer;
  LGenMethodsArr: TJSONArray;
begin
  LJsonObj := TJSONObject.ParseJSONValue(AJsonStr) as TJSONObject;
  try
    LModelsArr := LJsonObj.GetValue<TJSONArray>('models');
    SetLength(Result, LModelsArr.Count);

    for I := 0 to LModelsArr.Count - 1 do
    begin
      LModelObj := LModelsArr.Items[I] as TJSONObject;
      with Result[I] do
      begin
        name := LModelObj.GetValue<string>('name');
        version := LModelObj.GetValue<string>('version');
        displayName := LModelObj.GetValue<string>('displayName');
        description := LModelObj.GetValue<string>('description');
        inputTokenLimit := LModelObj.GetValue<Integer>('inputTokenLimit');
        outputTokenLimit := LModelObj.GetValue<Integer>('outputTokenLimit');

        if LModelObj.TryGetValue<TJSONArray>('supportedGenerationMethods', LGenMethodsArr) then
        begin
          SetLength(supportedGenerationMethods, LGenMethodsArr.Count);
          for J := 0 to LGenMethodsArr.Count - 1 do
            supportedGenerationMethods[J] := LGenMethodsArr.Items[J].Value;
        end;
        if Assigned(LModelObj.GetValue('temperature')) then
          temperature := LModelObj.GetValue<Double>('temperature');
        if Assigned(LModelObj.GetValue('topP')) then
          topP := LModelObj.GetValue<Double>('topP');
        if Assigned(LModelObj.GetValue('topK')) then
          topK := LModelObj.GetValue<Integer>('topK');
      end;
    end;
  finally
    LJsonObj.Free;
  end;
end;


function TGooglePaLM.GetGenerativeLanguageModels: string;
var
  LRestClient: TRESTClient;
  LRestRequest: TRESTRequest;
  LRestResponse: TRESTResponse;
begin
  Result := '';
  LRestClient := TRESTClient.Create(nil);
  LRestRequest := TRESTRequest.Create(nil);
  LRestResponse := TRESTResponse.Create(nil);
  try
    LRestClient.BaseURL := 'https://generativelanguage.googleapis.com';
    LRestRequest.Client := LRestClient;
    LRestRequest.Response := LRestResponse;
    LRestRequest.Resource := Format('/v1beta2/models?key=%s', [FAPIKey]);
    LRestRequest.Execute;
    Result := LRestResponse.Content;
  finally
    LRestClient.Free;
    LRestRequest.Free;
    LRestResponse.Free;
  end;
end;


function TGooglePaLM.GetModelInfo: TObjectList<TBaseModelInfo>;
var
  LJsonResponse: string;
  LParsedResponses: TArray<TGenerativeModelResponse>;
  LSingleResponse: TGenerativeModelResponse;
  i: Integer;
  LModelObj: TBaseModelInfo;
begin
  FModelInfo.Clear;
  LJsonResponse := GetGenerativeLanguageModels;
  LParsedResponses := ParseModelsResponse(LJsonResponse);

  // Example of accessing each model:
  for i := 0 to Length(LParsedResponses) - 1 do
  begin
    LSingleResponse := LParsedResponses[i];
    LModelObj := TBaseModelInfo.Create;
    // Now you can use fields of SingleResponse like SingleResponse.name, SingleResponse.version, etc.
    LModelObj.modelName := LSingleResponse.name;
    FModelInfo.Add(LModelObj);
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
