unit uLLM.Google.Gemini;

interface

uses
  System.Classes,
  System.SysUtils,
  System.JSON,
  System.Generics.Collections,
  System.StrUtils,
  REST.Client,
  REST.Types,
  NetEncoding,
  uLLM,
  uLLM.Functions,
  uAttributes;

type
  // Helper record to map Gemini response structure
  TGeminiUsageMetadata = record
    PromptTokenCount: Cardinal;
    CandidatesTokenCount: Cardinal;
    TotalTokenCount: Cardinal;
  end;

  // Helper record for Gemini parts
  TGeminiPart = record
    Text: string;
    // Add fields for functionCall, functionResponse, inlineData etc. as needed
    // For inlineData (images):
    MimeType: string;
    Data: string; // Assumed to be Base64 encoded data provided by TChatAttachment
  end;

  // Helper record for Gemini content
  TGeminiContent = record
    Parts: TArray<TGeminiPart>;
    Role: string; // 'user' or 'model'
  end;

  // Helper record for Gemini candidates
  TGeminiCandidate = record
    Content: TGeminiContent;
    FinishReason: string; // e.g., "STOP", "MAX_TOKENS", "SAFETY", "RECITATION", "FUNCTION_CALL"
    // Add SafetyRatings etc. if needed
    TokenCount: Cardinal;
  end;

  TGemini = class(TBaseLLM)
  private
    FFunctions: TFunctionRegistry; // Add function registry if needed
    FBaseURL: string;
    FAPIVersion: string;

    procedure CreateRESTClientAndRequest(out AClient: TRESTClient; out ARequest: TRESTRequest; out AResponse: TRESTResponse; const AModel: string; AStream: Boolean = False);
    function BuildJSONRequestBody(ChatConfig: TChatSettings; AMessages: TObjectList<TChatMessage>): TJSONObject;
    procedure ProcessResponse(LJSONResponse: TJSONObject; var AResponse: TChatResponse; AStream: Boolean = False); // Modify if streaming is implemented
    procedure HandleErrorResponse(AResponse: TRESTResponse);
    function ConvertMessagesToGeminiContent(AMessages: TObjectList<TChatMessage>): TJSONArray;
    function ParseGeminiResponse(LJSONResponse: TJSONObject): TChatResponse; // Helper to parse Gemini specific structure

  protected
    function GetModelInfo: TObjectList<TBaseModelInfo>; override;

  public
    constructor Create(const APIKey: string);
    destructor Destroy; override;

    function ChatCompletion(ChatConfig: TChatSettings; AMessages: TObjectList<TChatMessage>): TChatResponse; override;
    // function Completion(const AQuestion: string; const AModel: string): string; override; // Optional: Implement if needed, might just wrap ChatCompletion

    property Functions: TFunctionRegistry read FFunctions;
  end;

implementation

{ TGemini }

constructor TGemini.Create(const APIKey: string);
begin
  inherited Create(APIKey);
  FFunctions := TFunctionRegistry.Create; // Create if function calling is used
  FBaseURL := 'https://generativelanguage.googleapis.com';
  FAPIVersion := 'v1beta'; // Or 'v1' depending on features needed
end;

destructor TGemini.Destroy;
begin
  FreeAndNil(FFunctions); // Free if created
  inherited;
end;

procedure TGemini.CreateRESTClientAndRequest(out AClient: TRESTClient; out ARequest: TRESTRequest; out AResponse: TRESTResponse; const AModel: string; AStream: Boolean = False);
var
  LResource: string;
begin
  AClient := TRESTClient.Create(nil);
  ARequest := TRESTRequest.Create(nil);
  AResponse := TRESTResponse.Create(nil);

  AClient.BaseURL := FBaseURL;
  AClient.Accept := 'application/json';
  AClient.AcceptCharset := 'UTF-8';

  ARequest.Client := AClient;
  ARequest.Response := AResponse;
  ARequest.Method := TRESTRequestMethod.rmPOST;
  ARequest.Timeout := 80000; // Set timeout as needed

  LResource := Format('/%s/models/%s:%sgenerateContent?key=%s',
                      [FAPIVersion, AModel, IfThen(AStream, 'stream', ''), FAPIKey]);
  ARequest.Resource := LResource;
end;

function TGemini.ConvertMessagesToGeminiContent(AMessages: TObjectList<TChatMessage>): TJSONArray;
var
  LGeminiContents: TJSONArray;
  LMessage: TChatMessage;
  LGeminiContent: TJSONObject;
  LGeminiParts: TJSONArray;
  LGeminiPart: TJSONObject;
  LImageAttachment: TChatAttachment;
  LVisionMessage: TChatVisionMessage; // Assuming TChatVisionMessage exists
begin
  LGeminiContents := TJSONArray.Create;
  for LMessage in AMessages do
  begin
    LGeminiContent := TJSONObject.Create;
    LGeminiParts := TJSONArray.Create;

    if SameText(LMessage.Role, 'assistant') then
      LGeminiContent.AddPair('role', 'model')
    else if SameText(LMessage.Role, 'tool') then
      Continue // Skip tool messages for now in this basic conversion
    else
      LGeminiContent.AddPair('role', 'user');

    // --- Handle Content and Images ---
    LGeminiPart := TJSONObject.Create;
    if LMessage is TChatVisionMessage then // Handle vision messages
    begin
       LVisionMessage := LMessage as TChatVisionMessage;
       if not LVisionMessage.Content.IsEmpty then
       begin
          LGeminiPart.AddPair('text', LVisionMessage.Content);
          LGeminiParts.AddElement(LGeminiPart);
          LGeminiPart := TJSONObject.Create;
       end;

       for LImageAttachment in LVisionMessage.Attachments do
       begin
         LGeminiPart := TJSONObject.Create;
         if not LImageAttachment.data.IsEmpty then
         begin
            var LInlineData := TJSONObject.Create;
            LInlineData.AddPair('mime_type', LImageAttachment.mimeType);
            // Directly use the data field, assuming it's already Base64 encoded
            // The encoding should happen where TChatAttachment.data is populated.
            LInlineData.AddPair('data', LImageAttachment.data);
            LGeminiPart.AddPair('inline_data', LInlineData);
            LGeminiParts.AddElement(LGeminiPart);
         end
         else if not LImageAttachment.url.IsEmpty then
         begin
            var LTextPart := TJSONObject.Create;
            LTextPart.AddPair('text', ' [Image URL Skipped: ' + LImageAttachment.url + ']');
            LGeminiParts.AddElement(LTextPart);
         end;
       end;
    end
    else // Handle standard text messages
    begin
        if not LMessage.Content.IsEmpty then
        begin
           LGeminiPart.AddPair('text', LMessage.Content);
           LGeminiParts.AddElement(LGeminiPart);
        end;
    end;

    if LGeminiParts.Count > 0 then
    begin
      LGeminiContent.AddPair('parts', LGeminiParts);
      LGeminiContents.AddElement(LGeminiContent);
    end
    else
      FreeAndNil(LGeminiContent);

  end;
  Result := LGeminiContents;
end;

function TGemini.BuildJSONRequestBody(ChatConfig: TChatSettings; AMessages: TObjectList<TChatMessage>): TJSONObject;
var
  LJSONBody: TJSONObject;
  LGeminiContents: TJSONArray;
  LGenerationConfig: TJSONObject;
  LToolsArray: TJSONArray;
  LToolConfig: TJSONObject;
begin
  LJSONBody := TJSONObject.Create;
  try
    LGeminiContents := ConvertMessagesToGeminiContent(AMessages);
    LJSONBody.AddPair('contents', LGeminiContents);

    LGenerationConfig := TJSONObject.Create;
    if ChatConfig.temperature > 0 then
      LGenerationConfig.AddPair('temperature', TJSONNumber.Create(ChatConfig.temperature));
    if ChatConfig.top_p > 0 then
      LGenerationConfig.AddPair('topP', TJSONNumber.Create(ChatConfig.top_p));
    if ChatConfig.max_tokens > 0 then
      LGenerationConfig.AddPair('maxOutputTokens', TJSONNumber.Create(ChatConfig.max_tokens));
    if not ChatConfig.stop.IsEmpty then
    begin
      var LStopSequences := TJSONArray.Create;
      LStopSequences.Add(ChatConfig.stop);
      LGenerationConfig.AddPair('stopSequences', LStopSequences);
    end;
    if ChatConfig.json_mode then
       LGenerationConfig.AddPair('responseMimeType', 'application/json');

    if LGenerationConfig.Count > 0 then
      LJSONBody.AddPair('generationConfig', LGenerationConfig)
    else
      FreeAndNil(LGenerationConfig);

    // --- Function Calling Placeholder ---
    // if Assigned(FFunctions) and (FFunctions.Count > 0) then ...

    Result := LJSONBody;
  except
    FreeAndNil(LJSONBody);
    raise;
  end;
end;

function TGemini.ParseGeminiResponse(LJSONResponse: TJSONObject): TChatResponse;
var
  LCandidates: TJSONArray;
  LCandidate: TJSONObject;
  LContent: TJSONObject;
  LParts: TJSONArray;
  LPart: TJSONObject;
  LUsage: TJSONObject;
  LTextContent: TStringBuilder;
  LFunctionCall: TJSONObject;
  LToolCallRec: TChatToolCall;
  LToolCallsList: TList<TChatToolCall>;
  LPromptFeedback : TJSONObject;
  LBlockReason: string;
begin
  Result := Default(TChatResponse);
  LTextContent := TStringBuilder.Create;
  LToolCallsList := TList<TChatToolCall>.Create;
  try
    if LJSONResponse.TryGetValue<TJSONArray>('candidates', LCandidates) and (LCandidates.Count > 0) then
    begin
      LCandidate := LCandidates.Items[0] as TJSONObject;
      LCandidate.TryGetValue<string>('finishReason', Result.Finish_Reason);

      if LCandidate.TryGetValue<TJSONObject>('content', LContent) then
      begin
        if LContent.TryGetValue<TJSONArray>('parts', LParts) then
        begin
          for var i := 0 to LParts.Count - 1 do
          begin
            LPart := LParts.Items[i] as TJSONObject;
            var LTextValue: string;
            if LPart.TryGetValue<string>('text', LTextValue) then
              LTextContent.Append(LTextValue);

            // --- Function Call Parsing Placeholder ---
            // if LPart.TryGetValue<TJSONObject>('functionCall', LFunctionCall) then ...

          end;
        end;
      end;
      Result.Content := LTextContent.ToString;
      if LToolCallsList.Count > 0 then
        Result.Tool_Calls := LToolCallsList.ToArray;
    end
    else if LJSONResponse.TryGetValue<TJSONObject>('promptFeedback', LPromptFeedback) then
    begin
       if LPromptFeedback.TryGetValue<string>('blockReason', LBlockReason) then
         raise ELLMException.CreateFmt('Prompt blocked due to: %s', [LBlockReason]);
    end;

    if LJSONResponse.TryGetValue<TJSONObject>('usageMetadata', LUsage) then
    begin
      LUsage.TryGetValue<Cardinal>('promptTokenCount', Result.Prompt_Tokens);
      LUsage.TryGetValue<Cardinal>('candidatesTokenCount', Result.Completion_Tokens);
      LUsage.TryGetValue<Cardinal>('totalTokenCount', Result.Total_Tokens);
    end;

  finally
    FreeAndNil(LTextContent);
    FreeAndNil(LToolCallsList);
  end;
end;

procedure TGemini.ProcessResponse(LJSONResponse: TJSONObject; var AResponse: TChatResponse; AStream: Boolean = False);
begin
  if Assigned(LJSONResponse) then
  begin
      AResponse := ParseGeminiResponse(LJSONResponse);
      // --- Function Calls Post-Processing Placeholder ---
      // if (AResponse.Finish_Reason = 'tool_calls') and Assigned(FFunctions) and (Length(AResponse.Tool_Calls) > 0) then ...
  end
  else
  begin
    raise ELLMException.Create('Received empty response from Gemini API.');
  end;
end;

procedure TGemini.HandleErrorResponse(AResponse: TRESTResponse);
var
  LJSONResponse: TJSONObject;
  LErrorObj: TJSONObject;
  LMessage: string;
  LStatus: string;
  LCode: Integer;
begin
  LMessage := AResponse.StatusText;
  LCode := AResponse.StatusCode;
  LStatus := '';

  try
    LJSONResponse := TJSONObject.ParseJSONValue(AResponse.Content) as TJSONObject;
    if Assigned(LJSONResponse) then
    try
      if LJSONResponse.TryGetValue<TJSONObject>('error', LErrorObj) then
      begin
        LErrorObj.TryGetValue<string>('message', LMessage);
        LErrorObj.TryGetValue<Integer>('code', LCode);
        LErrorObj.TryGetValue<string>('status', LStatus);
      end;
    finally
      FreeAndNil(LJSONResponse);
    end;
  except
    // Ignore JSON parsing errors
  end;

  raise ELLMException.CreateFmt('Gemini API Error: (%d %s) %s', [LCode, LStatus, LMessage]);
end;

function TGemini.ChatCompletion(ChatConfig: TChatSettings; AMessages: TObjectList<TChatMessage>): TChatResponse;
var
  LRESTClient: TRESTClient;
  LRESTRequest: TRESTRequest;
  LRESTResponse: TRESTResponse;
  LJSONRequestBody: TJSONObject;
  LJSONResponseBody: TJSONObject;
begin
  Result := Default(TChatResponse);
  Result.Content := '';
  Result.Completion_Tokens := 0;
  Result.Prompt_Tokens := 0;
  Result.Total_Tokens := 0;
  Result.Tool_Calls := [];

  if ChatConfig.model.IsEmpty then
    ChatConfig.model := 'gemini-1.5-flash';

  CreateRESTClientAndRequest(LRESTClient, LRESTRequest, LRESTResponse, ChatConfig.model);
  try
    LJSONRequestBody := BuildJSONRequestBody(ChatConfig, AMessages);
    try
      LRESTRequest.AddBody(LJSONRequestBody.ToJSON, TRESTContentType.ctAPPLICATION_JSON);
    finally
       // Assume AddBody takes ownership or manage manually if needed
       // FreeAndNil(LJSONRequestBody);
    end;

    LRESTRequest.Execute;

    if LRESTResponse.StatusCode = 200 then
    begin
      LJSONResponseBody := LRESTResponse.JSONValue as TJSONObject;
      if LJSONResponseBody = nil then
         raise ELLMException.Create('Failed to parse Gemini JSON response.');

      ProcessResponse(LJSONResponseBody, Result);

      // --- Recursive call placeholder for tool usage ---
      // if Result.Finish_Reason = 'tool_calls' then ...
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

function TGemini.GetModelInfo: TObjectList<TBaseModelInfo>;
var
  LRESTClient: TRESTClient;
  LRESTRequest: TRESTRequest;
  LRESTResponse: TRESTResponse;
  LBaseJSONObject: TJSONObject;
  LJSONArray: TJSONArray;
  LJSONModel: TJSONObject;
  i: Integer;
  LModelObj: TBaseModelInfo;
  LResource : string;
begin
  if FModelInfo = nil then
     FModelInfo := TObjectList<TBaseModelInfo>.Create(True);
  FModelInfo.Clear;

  LRESTClient := nil;
  LRESTRequest := nil;
  LRESTResponse := nil;
  LBaseJSONObject := nil;
  try
    LRESTClient := TRESTClient.Create(FBaseURL);
    LRESTRequest := TRESTRequest.Create(nil);
    LRESTResponse := TRESTResponse.Create(nil);

    LRESTRequest.Client := LRESTClient;
    LRESTRequest.Response := LRESTResponse;
    LRESTRequest.Method := rmGET;

    LResource := Format('/%s/models?key=%s', [FAPIVersion, FAPIKey]);
    LRESTRequest.Resource := LResource;

    LRESTRequest.Execute;

    if LRESTResponse.StatusCode = 200 then
    begin
      LBaseJSONObject := LRESTResponse.JSONValue as TJSONObject;
      if Assigned(LBaseJSONObject) then
      begin
        if LBaseJSONObject.TryGetValue<TJSONArray>('models', LJSONArray) then
        begin
          for i := 0 to LJSONArray.Count - 1 do
          begin
            LJSONModel := LJSONArray.Items[i] as TJSONObject;
            LModelObj := TBaseModelInfo.Create;
            LModelObj.modelName := LJSONModel.GetValue<string>('name');
            LModelObj.version := LJSONModel.GetValue<string>('version');
            FModelInfo.Add(LModelObj);
          end;
        end;
      end
      // else Log failure
    end
    // else Log error

  finally
    FreeAndNil(LRESTClient);
    FreeAndNil(LRESTRequest);
    FreeAndNil(LRESTResponse);
  end;
  Result := FModelInfo;
end;


end.
