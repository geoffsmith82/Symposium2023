unit uReplicate.LLM;

interface

uses
  System.Generics.Collections,
  REST.Client,
  REST.Types,
  System.JSON,
  System.SysUtils,
  uLLM
  ;

type
  TUrls = record
    cancel: string;
    get: string;
  end;
  TPredictionInfo = class
  private
    FId: string;
    FVersion: string;
    FPrompt: string;  // This is part of the 'input' field in the JSON
    FLogs: string;
    FError: Variant;  // or you can use another type if you know more details about possible values
    FStatus: string;
    FCreatedAt: TDateTime; // Note: you might need to convert the JSON date string to TDateTime
  public
    Urls: TUrls;
    property Id: string read FId write FId;
    property Version: string read FVersion write FVersion;
    property Prompt: string read FPrompt write FPrompt;
    property Logs: string read FLogs write FLogs;
    property Error: Variant read FError write FError;
    property Status: string read FStatus write FStatus;
    property CreatedAt: TDateTime read FCreatedAt write FCreatedAt;
  //  property Urls: TUrls read FUrls write FUrls;
  end;



  TReplicateLLM = class(TBaseLLM)
  private
    function DeserializePredictionInfo(const AJsonStr: string): TPredictionInfo;
    function GetPredictionDetails(const Prediction: TPredictionInfo; out IsCompleted: Boolean): string;
  protected
    function GetModelInfo: TObjectList<TBaseModelInfo>; override;
  public
    function ChatCompletion(ChatConfig: TChatSettings; AMessages: TObjectList<TChatMessage>): TChatResponse; override;
    function Completion(const AQuestion: string; const AModel: string): string; override;
  end;

implementation

uses
  System.DateUtils,
  System.Diagnostics
  ;

{ TAnthropic }

function TReplicateLLM.ChatCompletion(ChatConfig: TChatSettings; AMessages: TObjectList<TChatMessage>): TChatResponse;
begin
  raise Exception.Create('Not Implemented Yet!');
end;

function TReplicateLLM.GetPredictionDetails(const Prediction: TPredictionInfo; out IsCompleted: Boolean): string;
var
  LRestClient: TRESTClient;
  LRestRequest: TRESTRequest;
  LRestResponse: TRESTResponse;
  LJSONValue: TJSONValue;
  LJSONObj: TJSONObject;
begin
  Result := '';
  IsCompleted := False;

  if not Assigned(Prediction) then
    raise Exception.Create('Prediction object is not assigned');

  LRestClient := TRESTClient.Create('https://api.replicate.com');
  try
    LRestRequest := TRESTRequest.Create(nil);
    LRestResponse := TRESTResponse.Create(nil);
    try
      LRestRequest.Client := LRestClient;
      LRestRequest.Response := LRestResponse;

      LRestRequest.Resource := '/v1/predictions/{predictionId}';
      LRestRequest.AddParameter('predictionId', Prediction.Id, TRESTRequestParameterKind.pkURLSEGMENT);

      LRestRequest.Method := rmGET;

      // Adding the Authorization header
      LRestRequest.AddParameter('Authorization', 'Token ' + FAPIKey, TRESTRequestParameterKind.pkHTTPHEADER, [TRESTRequestParameterOption.poDoNotEncode]);

      // Execute the request
      LRestRequest.Execute;

      if LRestResponse.StatusCode = 200 then
      begin
        LJSONValue := TJSONObject.ParseJSONValue(LRestResponse.Content);
        try
          if Assigned(LJSONValue) and (LJSONValue is TJSONObject) then
          begin
            LJSONObj := LJSONValue as TJSONObject;
            if LJSONObj.GetValue<string>('status') = 'succeeded' then
            begin
              IsCompleted := True;
              Result := LRestResponse.Content;
            end;
          end;
        finally
          LJSONValue.Free;
        end;
      end
      else
      begin
        raise Exception.CreateFmt('Error fetching prediction details: %s', [LRestResponse.Content]);
      end;

    finally
      LRestRequest.Free;
      LRestResponse.Free;
    end;

  finally
    LRestClient.Free;
  end;
end;

function TReplicateLLM.DeserializePredictionInfo(const AJsonStr: string): TPredictionInfo;
var
  LJsonValue: TJSONValue;
  LJsonObject: TJSONObject;
  LInputObj: TJSONObject;
  LUrlsObj: TJSONObject;
begin
  Result := TPredictionInfo.Create;

  LJsonValue := TJSONObject.ParseJSONValue(AJsonStr);
  if not Assigned(LJsonValue) then
    raise Exception.Create('Invalid JSON string');

  try
    if LJsonValue is TJSONObject then
    begin
      LJsonObject := LJsonValue as TJSONObject;

      Result.Id := LJsonObject.GetValue<string>('id');
      Result.Version := LJsonObject.GetValue<string>('version');

      // Extracting input.prompt
      LInputObj := LJsonObject.GetValue<TJSONObject>('input');
      if Assigned(LInputObj) then
        Result.Prompt := LInputObj.GetValue<string>('prompt');

      Result.Logs := LJsonObject.GetValue<string>('logs');
   //   Result.Error := JsonObject.GetValue<Variant>('error');
      Result.Status := LJsonObject.GetValue<string>('status');

      // Convert JSON date string to TDateTime
      Result.CreatedAt := ISO8601ToDate(LJsonObject.GetValue<string>('created_at'));

      // Extracting urls object
      LUrlsObj := LJsonObject.GetValue<TJSONObject>('urls');
      if Assigned(LUrlsObj) then
      begin
        Result.Urls.cancel := LUrlsObj.GetValue<string>('cancel');
        Result.Urls.get := LUrlsObj.GetValue<string>('get');
      end;
    end;
  finally
    LJsonValue.Free;
  end;
end;

function TReplicateLLM.Completion(const AQuestion, AModel: string): string;
var
  LRestClient: TRESTClient;
  LRestRequest: TRESTRequest;
  LRestResponse: TRESTResponse;
  LJSONRoot, LJSONInput: TJSONObject;
  LVersion: string;
  i: Integer;
  LPrediction: TPredictionInfo;
  LStopwatch: TStopwatch;
  LOperationCompleted: Boolean;
begin
  if ModelInfo.Count = 0 then
  begin
    GetModelInfo;
  end;
  LVersion := '';
  for i := 0 to FModelInfo.Count - 1 do
  begin
    if FModelInfo[i].modelName = AModel then
    begin
      LVersion := FModelInfo[i].version;
      Break;
    end;
  end;
  if LVersion.IsEmpty then
    raise Exception.Create('Could not find model ' + AModel);
  // Create and setup REST client, request and response
  LRestClient := TRESTClient.Create('https://api.replicate.com');
  try
    LRestRequest := TRESTRequest.Create(nil);
    LRestResponse := TRESTResponse.Create(nil);
    try
      LRestRequest.Client := LRestClient;
      LRestRequest.Response := LRestResponse;
      LRestRequest.Resource := '/v1/predictions';
      LRestRequest.Method := rmPOST;
      // Adding headers
      LRestRequest.AddParameter('Authorization', 'Token ' + FAPIKey, TRESTRequestParameterKind.pkHTTPHEADER, [TRESTRequestParameterOption.poDoNotEncode]);
      // Create JSON body for POST request
      LJSONRoot := TJSONObject.Create;
      try
        LJSONRoot.AddPair('version', LVersion);
        LJSONInput := TJSONObject.Create;
        LJSONInput.AddPair('prompt', AQuestion);
        LJSONRoot.AddPair('input', LJSONInput);
        LRestRequest.AddBody(LJSONRoot.ToString, TRESTContentType.ctAPPLICATION_JSON);
        // Execute the request
        LRestRequest.Execute;
        if (LRestResponse.StatusCode = 200) or (LRestResponse.StatusCode = 201) then
        begin
          // Assuming the response contains a field called 'completion' with the answer
         // JSONBody := TJSONObject.ParseJSONValue(RestResponse.Content) as TJSONObject;
          LPrediction := DeserializePredictionInfo(LRestResponse.Content);
          try
            LStopwatch := TStopwatch.StartNew;
            LOperationCompleted := False;
            repeat
              Result := GetPredictionDetails(LPrediction, LOperationCompleted);
            until (LStopwatch.ElapsedMilliseconds > 30000) or LOperationCompleted;
          finally
            FreeAndNil(LPrediction);
          end;
        end
        else
        begin
          // Handle errors or exceptions if needed
          raise Exception.CreateFmt('Error making prediction: %s', [LRestResponse.Content]);
        end;
      finally
        LJSONRoot.Free;
      end;
    finally
      LRestRequest.Free;
      LRestResponse.Free;
    end;
  finally
    LRestClient.Free;
  end;
end;

function TReplicateLLM.GetModelInfo: TObjectList<TBaseModelInfo>;
var
  LRestClient: TRESTClient;
  LRestRequest: TRESTRequest;
  LRestResponse: TRESTResponse;
  LJSONValue: TJSONObject;
  LJSONVersion: TJSONObject;
  LJSONArray: TJSONArray;
  I: Integer;
  LModelInfo: TBaseModelInfo;
begin
  FModelInfo.Clear;
  LRestClient := nil;
  LRestRequest := nil;
  LRestResponse := nil;
  try
    LRestClient := TRESTClient.Create('https://api.replicate.com');
    LRestRequest := TRESTRequest.Create(nil);
    LRestResponse := TRESTResponse.Create(nil);
    try
      LRestRequest.Client := LRestClient;
      LRestRequest.Response := LRestResponse;
      LRestRequest.Resource := '/v1/collections/streaming-language-models';

      LRestRequest.Method := rmGET;

      LRestRequest.AddParameter('Authorization', 'Token ' + FAPIKey, TRESTRequestParameterKind.pkHTTPHEADER, [TRESTRequestParameterOption.poDoNotEncode]);

      LRestRequest.Execute;

      if LRestResponse.StatusCode = 200 then
      begin
        LJSONValue := TJSONObject.ParseJSONValue(LRestResponse.Content) as TJSONObject;

        if Assigned(LJSONValue) and Assigned(LJSONValue.GetValue('models')) then
        begin
          LJSONArray := LJSONValue.GetValue('models') as TJSONArray;
          for I := 0 to LJSONArray.Count - 1 do
          begin
            LModelInfo := TBaseModelInfo.Create;
            LModelInfo.modelName := LJSONArray.Items[I].GetValue<string>('name');
            LJSONVersion := LJSONArray.Items[I].GetValue<TJSONObject>('latest_version');
            if Assigned(LJSONVersion.GetValue('id')) then
              LModelInfo.version := LJSONVersion.GetValue<string>('id');
            FModelInfo.Add(LModelInfo);
          end;
        end;

        FreeAndNil(LJSONValue);
      end
      else
      begin
        // Handle errors or exceptions if needed
      end;

    finally
      LRestRequest.Free;
      LRestResponse.Free;
    end;

  finally
    LRestClient.Free;
  end;
  Result := FModelInfo;
end;

end.
