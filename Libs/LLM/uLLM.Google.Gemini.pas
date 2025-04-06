unit uLLM.Google.Gemini;

interface

uses
  System.Classes,
  System.SysUtils,
  System.JSON,
  System.Generics.Collections,
  System.StrUtils,
  System.Rtti,
  FMX.Types,
  REST.Client,
  REST.Types,
  NetEncoding,
  uLLM,
  uLLM.Functions,
  uAttributes;

type
  // --- Forward Declarations ---
  TGeminiFunctionRegistry = class;

  // --- Gemini Specific Error ---
  EGeminiError = class(ELLMException);

  // --- Helper Records for Gemini API Structures ---


  // Represents the function call request from the model
  TGeminiFunctionCall = record
    Name: string;
    Args: TJSONObject; // Arguments are provided as a JSON object
  end;

  // --- Gemini Function Call Message (Model calls function) ---
  TGeminiFunctionCallMessage = class(TChatMessage)
  private
    FFunctionCall: TGeminiFunctionCall;
  public
    constructor Create(const ACall: TGeminiFunctionCall);
    function AsJSON: TJSONObject; override;
    property FunctionCall: TGeminiFunctionCall read FFunctionCall;
  end;

  TGeminiPart = record
    Text: string;
    FunctionCall: TGeminiFunctionCall; // Populated when the model requests a function call
    // For inlineData (images):
    MimeType: string;
    Data: string; // Assumed to be Base64 encoded data provided by TChatAttachment
    class function FromText(AText: string): TGeminiPart; static;
    class function FromAttachment(AAttachment: TChatAttachment): TGeminiPart; static; // Helper for images
  end;

  TGeminiContent = record
    Parts: TArray<TGeminiPart>;
    Role: string; // 'user', 'model', or 'tool'
  end;

  TGeminiCandidate = record
    Content: TGeminiContent;
    FinishReason: string; // e.g., "STOP", "MAX_TOKENS", "SAFETY", "RECITATION", "FUNCTION_CALL"
    SafetyRatings: TJSONArray; // Placeholder, structure depends on API details
    TokenCount: Cardinal; // Often found in usageMetadata instead
    UsageMetadata: TGeminiUsageMetadata; // Added based on API structure
  end;

  // --- Gemini Specific Chat Message Types ---

  // Represents a message containing the result of a tool/function call
  TGeminiFunctionResultMessage = class(TChatMessage)
  private
    FFunctionName: string;
    FResultJson: string; // Store the raw JSON result string
  public
    constructor Create(AFunctionName: string; AResultJson: string);
    function AsJSON: TJSONObject; override; // Not used directly for Gemini, see ConvertMessagesToGeminiContent
    property FunctionName: string read FFunctionName;
    property ResultJson: string read FResultJson;
  end;

  // --- Gemini Function Registry ---
  TGeminiFunctionRegistry = class(TFunctionRegistry)
  private
  public
    constructor Create;
    destructor Destroy; override;
    // Override to generate Gemini-specific function declaration JSON
    function GetAvailableFunctionsJSON(UseStrict: Boolean = False): TJSONArray; override;
    function GenerateParameterJSON(Method: TRttiMethod): TJSONObject; override;
  end;

  // --- Main Gemini LLM Class ---
  TGemini = class(TBaseLLM)
  private
    FFunctions: TGeminiFunctionRegistry; // Use the specific registry
    FBaseURL: string;
    FAPIVersion: string;
    FAPIKey: string;
    FModelInfo: TObjectList<TBaseModelInfo>;
    procedure FetchModelInfo;
    function ConvertMessagesToGeminiContent(const AMessages: TObjectList<TChatMessage>): TJSONArray;
    procedure ParseGeminiResponse(const AJsonResponse: TJSONObject; out AChatResponse: TChatResponse; out APendingCall: TGeminiFunctionCall; out ATextContent: string);
    procedure HandleErrorResponse(AResponse: TRESTResponse);
    procedure CreateRESTClientAndRequest(out AClient: TRESTClient; out ARequest: TRESTRequest; out AResponse: TRESTResponse; const AModel: string);
  protected
    function BuildJSONRequestBody(const AChatConfig: TChatSettings; const AMessages: TObjectList<TChatMessage>): TJSONObject; virtual;
    function ChatCompletion(ChatConfig: TChatSettings; AMessages: TObjectList<TChatMessage>): TChatResponse; override;
    function Completion(const AQuestion, AModel: string): string; override;
    function GetModelInfo: TObjectList<TBaseModelInfo>; override;
    property APIKey: string read FAPIKey write FAPIKey;
  public
    constructor Create(const AAPIKey: string);
    destructor Destroy; override;
  published
    property BaseURL: string read FBaseURL write FBaseURL;
    property APIVersion: string read FAPIVersion write FAPIVersion;
    property Functions: TGeminiFunctionRegistry read FFunctions;
  end;

implementation

uses
  System.Net.HttpClient, // Needed for TNetEncoding
  System.IOUtils,
  System.Net.Mime; // Needed for TNetEncoding

procedure Log(msg: string);
begin
  FMX.Types.Log.d(msg);
end;

// --- TGeminiFunctionCall ---
// (No methods needed for this simple record)

// --- TGeminiPart ---
class function TGeminiPart.FromText(AText: string): TGeminiPart;
begin
  Result := Default(TGeminiPart); // Initialize all fields
  Result.Text := AText;
end;

class function TGeminiPart.FromAttachment(AAttachment: TChatAttachment): TGeminiPart;
var
  LBytes: TBytes;
  LMimeType: string;
  k : TMimeTypes.TKind;
begin
   Result := Default(TGeminiPart);
   if not AAttachment.data.IsEmpty then // Prefer direct data
   begin
     Result.MimeType := AAttachment.mimeType;
     Result.Data := AAttachment.data; // Assume already Base64
   end
   else if not AAttachment.url.IsEmpty then // Handle URL if data not present (requires fetch/convert)
   begin
     // Basic example: Assuming URL is a local file path
     // In production, handle http URLs, download, check mime type, base64 encode
     if TFile.Exists(AAttachment.url) then
     begin
        Result.MimeType := AAttachment.mimeType;
        if Result.MimeType.IsEmpty then // Try to guess mime type
           TMimeTypes.Default.GetFileInfo(AAttachment.url, LMimeType, k);
        LBytes := TFile.ReadAllBytes(AAttachment.url);
        Result.Data := TNetEncoding.Base64.EncodeBytesToString(LBytes);
     end;
   end;
end;

// --- TGeminiFunctionResultMessage ---
constructor TGeminiFunctionResultMessage.Create(AFunctionName: string; AResultJson: string);
begin
  inherited Create;
  Role := 'tool'; // Gemini uses 'tool' role for function results
  FFunctionName := AFunctionName;
  FResultJson := AResultJson;
  // Content field is not directly used, info goes into FunctionResponse part
  Content := Format('[Function Result for %s]', [FFunctionName]);
end;

function TGeminiFunctionResultMessage.AsJSON: TJSONObject;
var
  LFunctionResponseObj: TJSONObject;
  LParsedResult: TJSONValue;
begin
  Result := TJSONObject.Create;
  LFunctionResponseObj := TJSONObject.Create;

  LFunctionResponseObj.AddPair('name', TJSONString.Create(FFunctionName));

  LParsedResult := TJSONObject.ParseJSONValue(FResultJson);
  if LParsedResult is TJSONObject then
    LFunctionResponseObj.AddPair('response', LParsedResult as TJSONObject)
  else if (LParsedResult <> nil) then
  begin
    var Wrapper := TJSONObject.Create;
    Wrapper.AddPair('result', LParsedResult); // may be string/number/bool
    LFunctionResponseObj.AddPair('response', Wrapper);
  end
  else
  begin
  // On parse failure, fallback to raw string
  var Wrapper := TJSONObject.Create;
  Wrapper.AddPair('result', TJSONString.Create(FResultJson));
  LFunctionResponseObj.AddPair('response', Wrapper);
  end;

  Result.AddPair('functionResponse', LFunctionResponseObj);
end;


// --- TGeminiFunctionRegistry ---
constructor TGeminiFunctionRegistry.Create;
begin
  inherited Create;
end;

destructor TGeminiFunctionRegistry.Destroy;
begin
  inherited;
end;

// Replace the existing GenerateParameterJSON function in uLLM.Functions.pas
// with this corrected version:
function TGeminiFunctionRegistry.GenerateParameterJSON(Method: TRttiMethod): TJSONObject;
var
  Params: TJSONObject;
  Properties: TJSONObject;
  RequiredArray: TJSONArray;
  Param: TRttiParameter;
  Attr: TCustomAttribute;
  ParamDesc: string;
  ParamObj: TJSONObject;
  ParamType: string;
begin
  Properties := TJSONObject.Create;
  RequiredArray := TJSONArray.Create;
  for Param in Method.GetParameters do
  begin
    ParamDesc := '';
    for Attr in Param.GetAttributes do
    begin
      if Attr is ParamDescriptionAttribute then
      begin
        ParamDesc := ParamDescriptionAttribute(Attr).Description;
        Break;
      end;
    end;

    ParamType := GetJSONTypeFromRTTI(Param.ParamType);

    ParamObj := TJSONObject.Create;
    ParamObj.AddPair('type', ParamType);
    ParamObj.AddPair('description', ParamDesc);
    Properties.AddPair(Param.Name, ParamObj);

    // Add parameter name to required array
    RequiredArray.AddElement(TJSONString.Create(Param.Name));
  end;

  Params := TJSONObject.Create;
  Params.AddPair('type', 'object');
  Params.AddPair('properties', Properties);
  Params.AddPair('required', RequiredArray);
//  Params.AddPair('additionalProperties', TJSONBool.Create(false));
  Result := Params;
end;

function TGeminiFunctionRegistry.GetAvailableFunctionsJSON(UseStrict: Boolean): TJSONArray;
var
  FuncDeclarationsArray: TJSONArray;
  FuncDesc: TFunctionDescription;
  FunctionDeclObject: TJSONObject;
  functionName: string;
  functionArray: TArray<string>;
begin
  // Gemini expects an array containing one object, which has a "functionDeclarations" array.
  Result := TJSONArray.Create;
  FuncDeclarationsArray := TJSONArray.Create;

  functionArray := FMethods.Keys.ToArray;

  for functionName in functionArray do
  begin
    if FMethods.TryGetValue(functionName, FuncDesc) then
    begin
      FunctionDeclObject := TJSONObject.Create;
      try
        FunctionDeclObject.AddPair('name', FuncDesc.Name);
        FunctionDeclObject.AddPair('description', FuncDesc.Description);
        // Gemini expects parameters directly in the 'parameters' field using OpenAPI schema
        FunctionDeclObject.AddPair('parameters', FuncDesc.Parameters.Clone as TJSONObject);

        FuncDeclarationsArray.AddElement(FunctionDeclObject);
      except
        FreeAndNil(FunctionDeclObject);
        raise;
      end;
    end;
  end;

  // Wrap the declarations array in the final structure
  if FuncDeclarationsArray.Count > 0 then
  begin
    var ToolsWrapperObject := TJSONObject.Create;
    ToolsWrapperObject.AddPair('functionDeclarations', FuncDeclarationsArray);
    Result.AddElement(ToolsWrapperObject);
  end
  else
    FreeAndNil(FuncDeclarationsArray); // Nothing to add

end;


// --- TGemini ---
constructor TGemini.Create(const AAPIKey: string);
begin
  FBaseURL := 'https://generativelanguage.googleapis.com';
  FAPIVersion := 'v1beta';
  FAPIKey := AAPIKey;
  FModelInfo := nil; // Lazy loaded
  FFunctions := TGeminiFunctionRegistry.Create; // Create specific registry
end;

destructor TGemini.Destroy;
begin
  FreeAndNil(FModelInfo);
  FreeAndNil(FFunctions);
  inherited;
end;

procedure TGemini.FetchModelInfo;
var
  LRESTClient: TRESTClient;
  LRESTRequest: TRESTRequest;
  LRESTResponse: TRESTResponse;
  LBaseJSONObject: TJSONObject;
  LJSONArray: TJSONArray;
  LJSONModel: TJSONObject;
  LModelObj: TBaseModelInfo;
  i: Integer;
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
    CreateRESTClientAndRequest(LRESTClient, LRESTRequest, LRESTResponse, ''); // Model irrelevant for listing

    LResource := Format('/%s/models?key=%s', [FAPIVersion, FAPIKey]);
    LRESTRequest.Resource := LResource;
    LRESTRequest.Method := rmGET;

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
            // Model names might be prefixed with "models/"
            LModelObj.modelName := LJSONModel.GetValue<string>('name');
            LModelObj.version := LJSONModel.GetValue<string>('version'); // Might not always be present
//            LModelObj.Description := LJSONModel.GetValue<string>('description');
//            LModelObj.InputTokenLimit := LJSONModel.GetValue<Integer>('inputTokenLimit');
//            LModelObj.OutputTokenLimit := LJSONModel.GetValue<Integer>('outputTokenLimit');
            FModelInfo.Add(LModelObj);
          end;
        end;
      end
    end
    else
      HandleErrorResponse(LRESTResponse);

  finally
    FreeAndNil(LRESTClient);
    FreeAndNil(LRESTRequest);
    FreeAndNil(LRESTResponse);
  end;
end;

function TGemini.GetModelInfo: TObjectList<TBaseModelInfo>;
begin
  if FModelInfo = nil then
    FetchModelInfo;
  Result := FModelInfo;
end;

procedure TGemini.CreateRESTClientAndRequest(out AClient: TRESTClient; out ARequest: TRESTRequest; out AResponse: TRESTResponse; const AModel: string);
begin
  AClient := TRESTClient.Create(nil); // Base URL set later if needed
  ARequest := TRESTRequest.Create(nil);
  AResponse := TRESTResponse.Create(nil);

  AClient.BaseURL := Self.FBaseURL; // Set base URL
  ARequest.Client := AClient;
  ARequest.Response := AResponse;

  // Common headers can be added here if necessary
  // ARequest.Params.AddHeader('Content-Type', 'application/json'); // Added automatically by AddBody(TJSONObject)
end;


procedure TGemini.HandleErrorResponse(AResponse: TRESTResponse);
var
  LErrorJson: TJSONObject;
  LErrorMessage: string;
  LErrorType: string;
begin
  LErrorMessage := AResponse.Content;
  LErrorType := 'HTTP Error ' + AResponse.StatusCode.ToString;
  try
    LErrorJson := AResponse.JSONValue as TJSONObject;
    if Assigned(LErrorJson) and LErrorJson.TryGetValue<TJSONObject>('error', LErrorJson) then
    begin
       LErrorMessage := LErrorJson.GetValue<string>('message', LErrorMessage);
       LErrorType := LErrorJson.GetValue<string>('status', LErrorType);
    end;
  except
    // Ignore JSON parsing errors, use raw content
  end;
  Log('Error: ' + LErrorType + ' - ' + LErrorMessage);
  raise EGeminiError.Create(LErrorType + ': ' + LErrorMessage);
end;

function TGemini.ConvertMessagesToGeminiContent(const AMessages: TObjectList<TChatMessage>): TJSONArray;
var
  LMessage: TChatMessage;
  LContentObj: TJSONObject;
  LPartsArray: TJSONArray;
  LPartObj: TJSONObject;
  LGeminiRole: string;
  LPrevRole: string;
  LVisionMsg: TChatVisionMessage;
  LFuncResultMsg: TGeminiFunctionResultMessage;
  LPartRec: TGeminiPart;
  LAttachment : TChatAttachment;
  LFuncResponsePart: TJSONObject; // Declare here for logging access
begin
  Result := TJSONArray.Create;
  LPrevRole := '';

  for LMessage in AMessages do
  begin
    // Determine Gemini Role ('user', 'model', 'tool')
    if LMessage is TGeminiFunctionResultMessage then
      LGeminiRole := 'tool'
    else if SameText(LMessage.Role, 'assistant') or SameText(LMessage.Role, 'model') then
      LGeminiRole := 'model'
    else // Default to 'user' for system, user, etc.
      LGeminiRole := 'user';

    // Gemini requires alternating roles (user/model or user/tool/model)
    if SameText(LGeminiRole, LPrevRole) and (LGeminiRole <> 'tool') then
    begin
       Log('Warning: Consecutive messages with the same role ('+LGeminiRole+') sent to Gemini. API might reject.');
       // Consider injecting an empty message of the opposite role if strict alternation is needed
    end;

    // --- ADDED: Initialize LPartObj to nil for safety ---
    LPartObj := nil;
    LFuncResponsePart := nil;
    // ----------------------------------------------------

    LContentObj := TJSONObject.Create;
    LPartsArray := TJSONArray.Create;

    // --- Create Parts based on Message Type ---
    if LMessage is TGeminiFunctionResultMessage then
    begin
      LPartObj := LMessage.AsJSON;
      LPartsArray.AddElement(LPartObj);
    end
    else if LMessage is TGeminiFunctionCallMessage then
    begin
      LPartObj := LMessage.AsJSON;
      LPartsArray.AddElement(LPartObj);
    end
    else if LMessage is TChatVisionMessage then
    begin
      LVisionMsg := LMessage as TChatVisionMessage;
      // Add text part if present
      if not LVisionMsg.Content.IsEmpty then
      begin
        LPartRec := TGeminiPart.FromText(LVisionMsg.Content);
        LPartObj := TJSONObject.Create;
        LPartObj.AddPair('text', TJSONString.Create(LPartRec.Text));
        LPartsArray.AddElement(LPartObj);
        LPartObj := nil; // Reset after adding
      end;
      // Add image parts
      for LAttachment in LVisionMsg.Attachments do
      begin
          LPartRec := TGeminiPart.FromAttachment(LAttachment);
          if not LPartRec.Data.IsEmpty then
          begin
             LPartObj := TJSONObject.Create;
             var LInlineData := TJSONObject.Create;
             LInlineData.AddPair('mimeType', TJSONString.Create(LPartRec.MimeType));
             LInlineData.AddPair('data', TJSONString.Create(LPartRec.Data));
             LPartObj.AddPair('inlineData', LInlineData);
             LPartsArray.AddElement(LPartObj);
             LPartObj := nil; // Reset after adding
          end
          else
             Log('Skipping attachment due to missing data: ' + LAttachment.url);
      end;
    end
    else // Default TChatMessage (treat as text)
    begin
      if not LMessage.Content.IsEmpty then // Only add if content exists
      begin
        LPartRec := TGeminiPart.FromText(LMessage.Content);
        LPartObj := TJSONObject.Create;
        LPartObj.AddPair('text', TJSONString.Create(LPartRec.Text));
        LPartsArray.AddElement(LPartObj);
        LPartObj := nil; // Reset after adding
      end
      else
        Continue; // Skip empty messages unless they are needed for role alternation logic
    end;
    // ----------------------------------------

    // --- Only add content object if it has parts ---
    if LPartsArray.Count > 0 then
    begin
      LContentObj.AddPair('role', LGeminiRole);
      LContentObj.AddPair('parts', LPartsArray); // AddPair takes ownership of LPartsArray
      Result.AddElement(LContentObj); // AddElement takes ownership of LContentObj
      LPrevRole := LGeminiRole;
    end
    else // Clean up if no parts were added
    begin
       // LPartObj should be nil here already if parts were added or if loop continued
       FreeAndNil(LPartObj);
       FreeAndNil(LContentObj);
       FreeAndNil(LPartsArray);
    end;
  end; // end for LMessage in AMessages
end;


function TGemini.BuildJSONRequestBody(const AChatConfig: TChatSettings; const AMessages: TObjectList<TChatMessage>): TJSONObject;
var
  LGenerationConfig: TJSONObject;
  LContentsArray: TJSONArray;
  LToolsArray: TJSONArray;
begin
  Result := TJSONObject.Create;

  // 1. Convert Messages to Gemini 'contents' format
  LContentsArray := ConvertMessagesToGeminiContent(AMessages);
  Result.AddPair('contents', LContentsArray);

  // 2. Add Tools (Function Declarations) if functions are registered
  if FFunctions.Count > 0 then
  begin
    LToolsArray := FFunctions.GetAvailableFunctionsJSON; // Use overridden version
    if LToolsArray.Count > 0 then
      Result.AddPair('tools', LToolsArray);
    // Optional: Add tool_config if needed
    // var LToolConfig := TJSONObject.Create;
    // var LFuncCallingConfig := TJSONObject.Create;
    // LFuncCallingConfig.AddPair('mode', 'AUTO'); // Or 'ANY', 'NONE'
    // LToolConfig.AddPair('functionCallingConfig', LFuncCallingConfig);
    // Result.AddPair('toolConfig', LToolConfig);
  end;


  // 3. Add Generation Config
  LGenerationConfig := TJSONObject.Create;
  if AChatConfig.temperature > 0 then // Gemini default is 0.9 if unset
    LGenerationConfig.AddPair('temperature', TJSONNumber.Create(AChatConfig.temperature));
  if AChatConfig.top_p > 0 then
    LGenerationConfig.AddPair('topP', TJSONNumber.Create(AChatConfig.top_p));
  if AChatConfig.top_k > 0 then // Gemini specific
     LGenerationConfig.AddPair('topK', TJSONNumber.Create(AChatConfig.top_k));
  if AChatConfig.max_tokens > 0 then
    LGenerationConfig.AddPair('maxOutputTokens', TJSONNumber.Create(AChatConfig.max_tokens));


//  if Assigned(AChatConfig.stop_sequences) and (AChatConfig.stop_sequences.Count > 0) then
//  begin
//     var LStopSequences := TJSONArray.Create;
//     for var stopSeq in AChatConfig.stop_sequences do
//        LStopSequences.Add(stopSeq);
//     LGenerationConfig.AddPair('stopSequences', LStopSequences);
//  end;


  // Add other generationConfig fields as needed (candidateCount, etc.)

  Result.AddPair('generationConfig', LGenerationConfig);

  // 4. Add Safety Settings (Optional)
  // var LSafetySettings := TJSONArray.Create;
  // var LSetting := TJSONObject.Create;
  // LSetting.AddPair('category', 'HARM_CATEGORY_SEXUALLY_EXPLICIT');
  // LSetting.AddPair('threshold', 'BLOCK_NONE');
  // LSafetySettings.AddElement(LSetting);
  // Result.AddPair('safetySettings', LSafetySettings);
  Log(Result.ToJSON);
end;

procedure TGemini.ParseGeminiResponse(const AJsonResponse: TJSONObject; out AChatResponse: TChatResponse; out APendingCall: TGeminiFunctionCall; out ATextContent: string);
var
  LCandidatesArray: TJSONArray;
  LCandidateObj: TJSONObject;
  LContentObj: TJSONObject;
  LPartsArray: TJSONArray;
  LPartObj, LFuncCallObj, LArgsObj: TJSONObject;
  LPartText: string;
  LUsage: TGeminiUsageMetadata;
  LPromptFeedback: TJSONObject;
  LUsageObj: TJSONObject;
begin
  ATextContent := '';
  APendingCall := Default(TGeminiFunctionCall); // Initialize
  AChatResponse := Default(TChatResponse); // Initialize

  // Extract Usage Metadata first (often outside 'candidates')
  if AJsonResponse.TryGetValue<TJSONObject>('usageMetadata', LUsageObj) then
  begin
    LUsage.PromptTokenCount := LUsageObj.GetValue<Integer>('promptTokenCount');
    LUsage.CandidatesTokenCount := LUsageObj.GetValue<Integer>('candidatesTokenCount'); // Sum if multiple candidates
    LUsage.TotalTokenCount := LUsageObj.GetValue<Integer>('totalTokenCount');
    AChatResponse.Usage := LUsage; // Assign parsed usage
  end;


  if not AJsonResponse.TryGetValue<TJSONArray>('candidates', LCandidatesArray) or (LCandidatesArray.Count = 0) then
  begin
     // Check for promptFeedback (blocked prompt)
     if AJsonResponse.TryGetValue<TJSONObject>('promptFeedback', LPromptFeedback) then
     begin
        AChatResponse.Finish_Reason := LPromptFeedback.GetValue<string>('blockReason', 'BLOCKED_BY_API');
        AChatResponse.Content := 'Prompt blocked: ' + LPromptFeedback.ToString;
     end
     else
        AChatResponse.Finish_Reason := 'NO_CANDIDATES';
     Exit; // No candidates to process
  end;

  // Process the first candidate
  LCandidateObj := LCandidatesArray.Items[0] as TJSONObject;

  AChatResponse.Finish_Reason := LCandidateObj.GetValue<string>('finishReason', 'UNKNOWN');
  // Sometimes usage is per-candidate
  AChatResponse.Usage.CandidatesTokenCount := LCandidateObj.GetValue<Integer>('tokenCount', AChatResponse.Usage.CandidatesTokenCount);
   // Update total if needed, though top-level usageMetadata is more common
  if (AChatResponse.Usage.TotalTokenCount = 0) and (AChatResponse.Usage.PromptTokenCount > 0) and (AChatResponse.Usage.CandidatesTokenCount > 0) then
     AChatResponse.Usage.TotalTokenCount := AChatResponse.Usage.PromptTokenCount + AChatResponse.Usage.CandidatesTokenCount;


  if not LCandidateObj.TryGetValue<TJSONObject>('content', LContentObj) then
    Exit; // No content in candidate

  if not LContentObj.TryGetValue<TJSONArray>('parts', LPartsArray) then
    Exit; // No parts in content


  // Iterate through parts to find text or function call
  for var i := 0 to LPartsArray.Count - 1 do
  begin
    LPartObj := LPartsArray.Items[i] as TJSONObject;

    if LPartObj.TryGetValue<string>('text', LPartText) then
    begin
      ATextContent := ATextContent + LPartText; // Concatenate text parts
    end
    else if LPartObj.TryGetValue<TJSONObject>('functionCall', LFuncCallObj) then
    begin
      APendingCall.Name := LFuncCallObj.GetValue<string>('name');
      if LFuncCallObj.TryGetValue<TJSONObject>('args', LArgsObj) then
      begin
         APendingCall.Args := LArgsObj.Clone as TJSONObject; // Clone args object
      end else
         APendingCall.Args := TJSONObject.Create; // Create empty if missing

      // Function call found, stop processing parts for this candidate
      Break;
    end;
  end;

  AChatResponse.Content := ATextContent; // Assign concatenated text content

end;

function TGemini.Completion(const AQuestion, AModel: string): string;
begin
  raise Exception.Create('Completion Not implemented. Use ChatCompletion instead');
end;


function TGemini.ChatCompletion(ChatConfig: TChatSettings; AMessages: TObjectList<TChatMessage>): TChatResponse;
var
  LRESTClient: TRESTClient;
  LRESTRequest: TRESTRequest;
  LRESTResponse: TRESTResponse;
  LJSONRequestBody: TJSONObject;
  LJSONResponse: TJSONObject;
  LResource: string;
  LPendingCall: TGeminiFunctionCall;
  LFunctionResultString: string;
  LTextContent: string;
  LLoopIteration: Integer;
  LCurrentMessages: TObjectList<TChatMessage>;
  LInvokeArgsWrapper: TJSONObject;
  FuncCallMsg: TGeminiFunctionCallMessage;
  ResultMsg: TGeminiFunctionResultMessage;
begin
  Result := Default(TChatResponse);
  LLoopIteration := 0;
  LPendingCall := Default(TGeminiFunctionCall);
  LCurrentMessages := TObjectList<TChatMessage>.Create(False);

  try
    for var msg in AMessages do
      LCurrentMessages.Add(msg);

    repeat
      Inc(LLoopIteration);
      if LLoopIteration > 5 then
      begin
        Result.Finish_Reason := 'FUNCTION_LOOP_LIMIT';
        Result.Content := 'Exceeded function call limit.';
        Log('Function call loop limit reached.');
        Exit;
      end;

      // --- If a function call is pending from previous iteration ---
      if not LPendingCall.Name.IsEmpty then
      begin
        try
          Log('Invoking function: ' + LPendingCall.Name);

          LInvokeArgsWrapper := TJSONObject.Create;
          try
            LInvokeArgsWrapper.AddPair('name', TJSONString.Create(LPendingCall.Name));
            if Assigned(LPendingCall.Args) then
              LInvokeArgsWrapper.AddPair('input', LPendingCall.Args.Clone as TJSONObject);

            FFunctions.InvokeFunction(LInvokeArgsWrapper, LFunctionResultString);
            Log('Function result: ' + LFunctionResultString);

            // Insert model functionCall message BEFORE the tool response
            FuncCallMsg := TGeminiFunctionCallMessage.Create(LPendingCall);
            LCurrentMessages.Add(FuncCallMsg);

            ResultMsg := TGeminiFunctionResultMessage.Create(LPendingCall.Name, LFunctionResultString);
            LCurrentMessages.Add(ResultMsg);

          finally
            FreeAndNil(LInvokeArgsWrapper);
           // FreeAndNil(LPendingCall.Args);
            LPendingCall := Default(TGeminiFunctionCall);
          end;
        except
          on E: Exception do
          begin
            Log('Error invoking function ' + LPendingCall.Name + ': ' + E.Message);
            Result.Finish_Reason := 'FUNCTION_INVOCATION_ERROR';
            Result.Content := 'Error invoking function "' + LPendingCall.Name + '": ' + E.Message;
            FreeAndNil(LPendingCall.Args);
            Exit;
          end;
        end;
      end;

      // --- Call Gemini API ---
      LRESTClient := nil;
      LRESTRequest := nil;
      LRESTResponse := nil;
      LJSONRequestBody := nil;
      LJSONResponse := nil;
      try
        CreateRESTClientAndRequest(LRESTClient, LRESTRequest, LRESTResponse, ChatConfig.model);

        LResource := Format('/%s/models/%s:generateContent?key=%s', [FAPIVersion, ChatConfig.model, FAPIKey]);
        if ChatConfig.model.StartsWith('models/') then
          LResource := Format('/%s/%s:generateContent?key=%s', [FAPIVersion, ChatConfig.model, FAPIKey]);

        LRESTRequest.Resource := LResource;
        LRESTRequest.Method := rmPOST;

        LJSONRequestBody := BuildJSONRequestBody(ChatConfig, LCurrentMessages);
        Log('Gemini Request: ' + LJSONRequestBody.ToString);
        LRESTRequest.AddBody(LJSONRequestBody, TRESTObjectOwnership.ooApp);

        LRESTRequest.Execute;
        Log('Gemini Response Code: ' + LRESTResponse.StatusCode.ToString);
        Log('Gemini Response: ' + LRESTResponse.Content);

        if LRESTResponse.StatusCode = 200 then
        begin
          LJSONResponse := LRESTResponse.JSONValue as TJSONObject;
          if Assigned(LJSONResponse) then
          begin
            ParseGeminiResponse(LJSONResponse, Result, LPendingCall, LTextContent);
            Result.Content := LTextContent;
          end
          else
          begin
            Result.Finish_Reason := 'INVALID_JSON_RESPONSE';
            Result.Content := 'Failed to parse JSON response: ' + LRESTResponse.Content;
            Log(Result.Content);
            Break;
          end;
        end
        else
        begin
          HandleErrorResponse(LRESTResponse);
          Break;
        end;

      finally
        FreeAndNil(LRESTClient);
        FreeAndNil(LRESTRequest);
        FreeAndNil(LRESTResponse);
      end;

    until LPendingCall.Name.IsEmpty;

  finally
    FreeAndNil(LCurrentMessages);
  end;
end;

{ TGeminiFunctionCallMessage }

function TGeminiFunctionCallMessage.AsJSON: TJSONObject;
var
  LFunctionCallObj: TJSONObject;
begin
  Result := TJSONObject.Create;
  LFunctionCallObj := TJSONObject.Create;

  LFunctionCallObj.AddPair('name', FFunctionCall.Name);
  if Assigned(FFunctionCall.Args) then
    LFunctionCallObj.AddPair('args', FFunctionCall.Args.Clone as TJSONObject)
  else
    LFunctionCallObj.AddPair('args', TJSONObject.Create);

  Result.AddPair('functionCall', LFunctionCallObj);
end;

constructor TGeminiFunctionCallMessage.Create(const ACall: TGeminiFunctionCall);
begin
  inherited Create;
  Role := 'model';
  Content := ''; // Not used
  FFunctionCall := ACall;
end;

end.
