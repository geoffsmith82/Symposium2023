unit uAnthropic;

interface

uses
  System.Generics.Collections,
  REST.Client,
  REST.Types,
  REST.Response.Adapter,
  System.JSON,
  System.SysUtils,
  uGPT
  ;

type
  EAnthropicError = class(Exception)
  private
    FErrorType: string;
  public
    constructor Create(const AErrorType, AMessage: string);
    property ErrorType: string read FErrorType;
  end;


  TAnthropic = class(TBaseGPT)
  public
    function ChatCompletion(ChatConfig: TChatSettings; AMessages: TObjectList<TChatMessage>): TChatResponse; override;
    function Completion(const AQuestion: string; const AModel: string): string; override;
    function Embeddings(const Texts: TArray<string>): TEmbeddings;
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
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
  JSONBody: TJSONObject;
  Msg: TChatMessage;
  JSONResponse: TJSONObject;
  PromptText: string;
begin
  RESTClient := TRESTClient.Create('https://api.anthropic.com/v1/complete');
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);

  try
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Method := rmPOST;

    RESTRequest.Params.AddItem('anthropic-version', '2023-06-01', TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
    RESTRequest.Params.AddItem('content-type', 'application/json', TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
    RESTRequest.Params.AddItem('x-api-key', FAPIKey, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);

    JSONBody := TJSONObject.Create;
    try
      // Add the chat settings to the JSON body...
      JSONBody.AddPair('model', ChatConfig.model);
      // ... (add other chat settings properties here)
      JSONBody.AddPair('max_tokens_to_sample', ChatConfig.max_tokens);

      // Constructing the prompt text based on the messages list.
      PromptText := '';
      for Msg in AMessages do
      begin
        PromptText := PromptText + Chr(10)+ Chr(10) + Msg.Role + ': ' + Msg.Content;
      end;
      JSONBody.AddPair('prompt', PromptText);

      RESTRequest.AddBody(JSONBody.ToString, ctAPPLICATION_JSON);

      RESTRequest.Execute;

      // Parse the response...
      JSONResponse := TJSONObject.ParseJSONValue(RESTResponse.Content) as TJSONObject;
      try
        if Assigned(JSONResponse.GetValue('completion')) then
        begin
          Result.Content := JSONResponse.GetValue('completion').Value;
          // ... (parse other fields if necessary)
          if Assigned(JSONResponse.GetValue('log_id')) then
              Result.Log_Id := JSONResponse.GetValue('log_id').Value;
          if Assigned(JSONResponse.GetValue('model')) then
              Result.Model := JSONResponse.GetValue('model').Value;
        end
        else if Assigned(JSONResponse.GetValue('error')) then
        begin
          raise EAnthropicError.Create(
            JSONResponse.GetValue<TJSONObject>('error').GetValue('type').Value,
            JSONResponse.GetValue<TJSONObject>('error').GetValue('message').Value
          );
        end;
      finally
        JSONResponse.Free;
      end;

    finally
      JSONBody.Free;
    end;

  finally
    RESTRequest.Free;
    RESTClient.Free;
    RESTResponse.Free;
  end;
end;

function TAnthropic.Completion(const AQuestion, AModel: string): string;
var
  ChatConfig: TChatSettings;
  AMessages: TObjectList<TChatMessage>;
  msg: TChatMessage;
begin
  ChatConfig.model := AModel;
  AMessages := nil;
  msg := nil;
  try
    AMessages := TObjectList<TChatMessage>.Create;
    msg := TChatMessage.Create;
    msg.Role := 'Human';
    msg.Content := AQuestion;
    AMessages.Add(msg);
    Result := ChatCompletion(ChatConfig, AMessages).Content;
  finally
    FreeAndNil(AMessages);
  end;
end;

function TAnthropic.Embeddings(const Texts: TArray<string>): TEmbeddings;
begin
  raise Exception.Create('Not Available');
end;

end.
