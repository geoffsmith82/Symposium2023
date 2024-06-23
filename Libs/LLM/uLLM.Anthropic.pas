unit uLLM.Anthropic;

interface

uses
  System.Generics.Collections,
  REST.Client,
  REST.Types,
  REST.Response.Adapter,
  System.JSON,
  System.SysUtils,
  windows,
  uLLM
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
  public
    function ChatCompletion(ChatConfig: TChatSettings; AMessages: TObjectList<TChatMessage>): TChatResponse; override;
    function Completion(const AQuestion: string; const AModel: string): string; override;
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
  LJSONBody: TJSONObject;
  LMsg: TChatMessage;
  LJSONContentArray : TJSONArray;
  LJSONResponse: TJSONObject;
  LJSONMsgArr : TJSONArray;
  LJSONMsg : TJSONObject;
begin
  Result := Default(TChatResponse);
  LRESTClient := nil;
  LRESTRequest := nil;
  LRESTResponse := nil;

  try
    LRESTClient := TRESTClient.Create('https://api.anthropic.com/v1/messages');
    LRESTRequest := TRESTRequest.Create(nil);
    LRESTResponse := TRESTResponse.Create(nil);
    LRESTRequest.Client := LRESTClient;
    LRESTRequest.Response := LRESTResponse;
    LRESTRequest.Method := rmPOST;

    LRESTRequest.Params.AddItem('anthropic-version', '2023-06-01', TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
    LRESTRequest.Params.AddItem('content-type', 'application/json', TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
    LRESTRequest.Params.AddItem('x-api-key', FAPIKey, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);

    LJSONBody := TJSONObject.Create;
    try
      // Add the chat settings to the JSON body...
      LJSONBody.AddPair('model', ChatConfig.model);
      // ... (add other chat settings properties here)
      LJSONBody.AddPair('max_tokens', ChatConfig.max_tokens);
      LJSONMsgArr := TJSONArray.Create;
      // Constructing the prompt text based on the messages list.
      for LMsg in AMessages do
      begin
        LJSONMsgArr.AddElement(LMsg.AsJSON);
        OutputDebugString(PChar(LMsg.AsJSON));
      end;

      LJSONBody.AddPair('messages', LJSONMsgArr);

      OutputDebugString(PChar(LJSONBody.ToJSON));

      LRESTRequest.AddBody(LJSONBody, TRESTObjectOwnership.ooApp);

      LRESTRequest.Execute;

      // Parse the response...
      LJSONResponse := LRESTResponse.JSONValue as TJSONObject;
      try
        if Assigned(LJSONResponse.GetValue('content')) then
        begin
          LJSONContentArray := LJSONResponse.GetValue('content') as TJSONArray;
          Result.Content := (LJSONContentArray.Items[0] as TJSONObject).GetValue('text').Value;
          // ... (parse other fields if necessary)
          if Assigned(LJSONResponse.GetValue('id')) then
              Result.Log_Id := LJSONResponse.GetValue('id').Value;
          if Assigned(LJSONResponse.GetValue('model')) then
              Result.Model := LJSONResponse.GetValue('model').Value;
        end
        else if Assigned(LJSONResponse.GetValue('error')) then
        begin
          raise EAnthropicError.Create(
            LJSONResponse.GetValue<TJSONObject>('error').GetValue('type').Value,
            LJSONResponse.GetValue<TJSONObject>('error').GetValue('message').Value
          );
        end;
      finally
      //  LJSONResponse.Free;
      end;

    finally
      LJSONBody.Free;
    end;

  finally
    LRESTRequest.Free;
    LRESTClient.Free;
    LRESTResponse.Free;
  end;
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
  LModelObj : TBaseModelInfo;
begin
  FModelInfo.Clear;

  LModelObj := TBaseModelInfo.Create;
  LModelObj.modelName := 'claude-3-opus-20240229';
  FModelInfo.Add(LModelObj);

  LModelObj := TBaseModelInfo.Create;
  LModelObj.modelName := 'claude-3-5-sonnet-20240620';
  FModelInfo.Add(LModelObj);

  LModelObj := TBaseModelInfo.Create;
  LModelObj.modelName := 'claude-3-sonnet-20240229';
  FModelInfo.Add(LModelObj);

  LModelObj := TBaseModelInfo.Create;
  LModelObj.modelName := 'claude-3-haiku-20240307';
  FModelInfo.Add(LModelObj);

  Result := FModelInfo;
end;

end.
