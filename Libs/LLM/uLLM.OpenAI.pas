unit uLLM.OpenAI;

interface

uses
  System.Classes,
  System.JSON,
  System.SysUtils,
  System.Generics.Collections,
  REST.Client,
  REST.Types,
  uLLM
  ;

type
  TOpenAI = class(TBaseLLM)
  protected
    function GetModelInfo: TObjectList<TBaseModelInfo>; override;
  private
    procedure ListOpenAIModels(out AModelList: TStringList);
  public
    constructor Create(const APIKey: string);
    function ChatCompletion(ChatConfig: TChatSettings; AMessages: TObjectList<TChatMessage>): TChatResponse; override;
    function Completion(const AQuestion: string; const AModel: string): string; override;
  end;



implementation


function TOpenAI.ChatCompletion(ChatConfig: TChatSettings; AMessages: TObjectList<TChatMessage>): TChatResponse;
var
  LRESTClient: TRESTClient;
  LRESTRequest: TRESTRequest;
  LRESTResponse: TRESTResponse;
  LJSONBody: TJSONObject;
  LJSONMessages: TJSONArray;
  LJSONMsg : TJSONObject;
  LJSONReponseFormat : TJSONObject;
  LMessage: TChatMessage;
  LJSONResponse: TJSONObject;
  LChoices: TJSONArray;
  LUsage: TJSONObject;
  LChoice: TJSONObject;
begin
  Result := Default(TChatResponse);
  Result.Content := '';
  Result.Completion_Tokens := 0;
  Result.Prompt_Tokens := 0;
  Result.Total_Tokens := 0;
  LRESTClient := nil;
  LRESTRequest := nil;
  LRESTResponse := nil;
  try
    LRESTClient := TRESTClient.Create(nil);
    LRESTRequest := TRESTRequest.Create(nil);
    LRESTResponse := TRESTResponse.Create(nil);
    LRESTClient.BaseURL := 'https://api.openai.com';
    LRESTClient.Accept := 'application/json';
    LRESTClient.AcceptCharset := 'UTF-8';
    LRESTRequest.Client := LRESTClient;
    LRESTRequest.Response := LRESTResponse;
    LRESTRequest.Method := TRESTRequestMethod.rmPOST;
    LRESTRequest.Timeout := 80000; // Set the timeout as needed
    LRESTRequest.Resource := '/v1/chat/completions';
    LRESTRequest.Params.AddItem('Authorization', 'Bearer ' + FAPIKey, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
    LRESTRequest.Params.AddItem('Content-Type', 'application/json', TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
    LJSONBody := TJSONObject.Create;
    LJSONMessages := TJSONArray.Create;
    try
      for LMessage in AMessages do
      begin
        LJSONMsg := TJSONObject.Create;
        LJSONMsg.AddPair('role', LMessage.Role.ToLower);
        LJSONMsg.AddPair('content', LMessage.Content);
        LJSONMessages.AddElement(LJSONMsg);
      end;

      if ChatConfig.model.IsEmpty then
        ChatConfig.model := 'gpt-3.5-turbo';

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
        LJSONReponseFormat := TJSONObject.Create;
        LJSONReponseFormat.AddPair('type', 'json_object');
        LJSONBody.AddPair('response_format', LJSONReponseFormat);
      end;

      LRESTRequest.AddBody(LJSONBody.ToString, TRESTContentType.ctAPPLICATION_JSON);
      LRESTRequest.Execute;
      if LRESTResponse.StatusCode = 200 then
      begin
        LJSONResponse := TJSONObject.ParseJSONValue(LRESTResponse.Content) as TJSONObject;
        try
          LChoices := LJSONResponse.GetValue<TJSONArray>('choices');
          if Assigned(LJSONResponse.GetValue('model')) then
            Result.Model := LJSONResponse.GetValue('model').Value;

          if Assigned(LJSONResponse.GetValue('id')) then
            Result.Log_Id := LJSONResponse.GetValue('id').Value;

          LUsage := LJSONResponse.GetValue<TJSONObject>('usage');
          LUsage.TryGetValue('completion_tokens', Result.Completion_Tokens);
          LUsage.TryGetValue('prompt_tokens', Result.Prompt_Tokens);
          LUsage.TryGetValue('total_tokens', Result.Total_Tokens);
          LChoice := LChoices.Items[0] as TJSONObject;
          Result.Content := LChoice.GetValue('message').GetValue<string>('content');
          Result.Finish_Reason := LChoice.GetValue('finish_reason').Value;
          if Assigned(LJSONResponse.GetValue('system_fingerprint')) then
            Result.System_Fingerprint := LJSONResponse.GetValue('system_fingerprint').Value;
        finally
          FreeAndNil(LJSONResponse);
        end;
      end
      else
      begin
        // Parse the error message
        LJSONResponse := TJSONObject.ParseJSONValue(LRESTResponse.Content) as TJSONObject;
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
            raise Exception.CreateFmt('Error: %d - %s', [LRESTResponse.StatusCode, LRESTResponse.StatusText]);
        finally
          FreeAndNil(LJSONResponse);
        end
        else
          raise Exception.CreateFmt('Error: %d - %s', [LRESTResponse.StatusCode, LRESTResponse.StatusText]);
      end;
    finally
      FreeAndNil(LJSONBody);
    end;
  finally
    FreeAndNil(LRESTClient);
    FreeAndNil(LRESTRequest);
    FreeAndNil(LRESTResponse);
  end;
end;

constructor TOpenAI.Create(const APIKey: string);
begin
  inherited Create(APIKey);
end;

function TOpenAI.Completion(const AQuestion: string; const AModel: string): string;
var
  LClient : TRESTClient;
  LRequest : TRESTRequest;
  LResponse : TRESTResponse;
  LJsonPostData : TJSONObject;
  LJsonValue: TJsonValue;
  LJsonArray: TJsonArray;
  LJSonString: TJsonString;
begin
  Result := '';
  LJsonPostData := nil;
  LClient := nil;
  LRequest := nil;
  LResponse := nil;

  try
    LJsonPostData := TJSONObject.Create;
    LJsonPostData.AddPair('model', AModel);
    LJsonPostData.AddPair('prompt', AQuestion);
    LJsonPostData.AddPair('max_tokens', TJSONNumber.Create(2048));
    LJsonPostData.AddPair('temperature', TJSONNumber.Create(0));

    LClient := TRESTClient.Create(nil);
    LRequest := TRESTRequest.Create(nil);
    LResponse := TRESTResponse.Create(nil);
    LRequest.Client := LClient;
    LRequest.Response := LResponse;

    LClient.ReadTimeout := 180000;

    // Use JSON for the REST API calls and set API KEY via Authorization header
    LRequest.AddAuthParameter('Authorization', 'Bearer ' + FAPIKey, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
    LRequest.Accept := '*/*';

    // Select HTTPS POST method, set POST data and specify endpoint URL
    LRequest.Method := rmPOST;
    LRequest.AddBody(LJsonPostData);
    LClient.BaseURL := 'https://api.openai.com';
    LRequest.Resource := '/v1/completions';

    // Execute the HTTPS POST request synchronously (last param Async = false)
    LRequest.Execute;
    // Process returned JSON when request was successful
    if LRequest.Response.StatusCode = 200 then
    begin
      LJsonValue := LResponse.JSONValue;
      LJsonValue := LJsonValue.GetValue<TJSonValue>('choices');
      if LJsonValue is TJSonArray then
      begin
        LJSonArray := LJsonValue as TJSonArray;
        LJSonString := LJSonArray.Items[0].GetValue<TJSONString>('text');
        Result := LJSonString.Value;
      end;
    end
    else
      raise Exception.Create('HTTP response code: ' + LResponse.StatusCode.ToString);
  finally
    FreeAndNil(LResponse);
    FreeAndNil(LRequest);
    FreeAndNil(LClient);
    FreeAndNil(LJsonPostData);
  end;
end;

function TOpenAI.GetModelInfo: TObjectList<TBaseModelInfo>;
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

procedure TOpenAI.ListOpenAIModels(out AModelList: TStringList);
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
    LRESTClient := TRESTClient.Create('https://api.openai.com');
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
