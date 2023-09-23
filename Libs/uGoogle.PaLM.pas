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
  EGooglePaLMError = class(Exception)
  private
    FErrorType: string;
  public
    constructor Create(const AErrorType, AMessage: string);
    property ErrorType: string read FErrorType;
  end;

  TGooglePaLM = class(TBaseLLM)
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

{ EGooglePaLMError }

constructor EGooglePaLMError.Create(const AErrorType, AMessage: string);
begin
  inherited Create(AMessage);
  FErrorType := AErrorType;
end;

end.
