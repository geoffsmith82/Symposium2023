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
  RestClient: TRESTClient;
  RestRequest: TRESTRequest;
  RestResponse: TRESTResponse;
  JSONValue: TJSONValue;
  JSONObj: TJSONObject;
begin
  Result := '';
  IsCompleted := False;

  if not Assigned(Prediction) then
    raise Exception.Create('Prediction object is not assigned');

  RestClient := TRESTClient.Create('https://api.replicate.com');
  try
    RestRequest := TRESTRequest.Create(nil);
    RestResponse := TRESTResponse.Create(nil);
    try
      RestRequest.Client := RestClient;
      RestRequest.Response := RestResponse;

      RestRequest.Resource := '/v1/predictions/{predictionId}';
      RestRequest.AddParameter('predictionId', Prediction.Id, TRESTRequestParameterKind.pkURLSEGMENT);

      RestRequest.Method := rmGET;

      // Adding the Authorization header
      RestRequest.AddParameter('Authorization', 'Token ' + FAPIKey, TRESTRequestParameterKind.pkHTTPHEADER, [TRESTRequestParameterOption.poDoNotEncode]);

      // Execute the request
      RestRequest.Execute;

      if RestResponse.StatusCode = 200 then
      begin
        JSONValue := TJSONObject.ParseJSONValue(RestResponse.Content);
        try
          if Assigned(JSONValue) and (JSONValue is TJSONObject) then
          begin
            JSONObj := JSONValue as TJSONObject;
            if JSONObj.GetValue<string>('status') = 'succeeded' then
            begin
              IsCompleted := True;
              Result := RestResponse.Content;
            end;
          end;
        finally
          JSONValue.Free;
        end;
      end
      else
      begin
        raise Exception.CreateFmt('Error fetching prediction details: %s', [RestResponse.Content]);
      end;

    finally
      RestRequest.Free;
      RestResponse.Free;
    end;

  finally
    RestClient.Free;
  end;
end;

function TReplicateLLM.DeserializePredictionInfo(const AJsonStr: string): TPredictionInfo;
var
  JsonValue: TJSONValue;
  JsonObject: TJSONObject;
  InputObj: TJSONObject;
  UrlsObj: TJSONObject;
begin
  Result := TPredictionInfo.Create;

  JsonValue := TJSONObject.ParseJSONValue(AJsonStr);
  if not Assigned(JsonValue) then
    raise Exception.Create('Invalid JSON string');

  try
    if JsonValue is TJSONObject then
    begin
      JsonObject := JsonValue as TJSONObject;

      Result.Id := JsonObject.GetValue<string>('id');
      Result.Version := JsonObject.GetValue<string>('version');

      // Extracting input.prompt
      InputObj := JsonObject.GetValue<TJSONObject>('input');
      if Assigned(InputObj) then
        Result.Prompt := InputObj.GetValue<string>('prompt');

      Result.Logs := JsonObject.GetValue<string>('logs');
   //   Result.Error := JsonObject.GetValue<Variant>('error');
      Result.Status := JsonObject.GetValue<string>('status');

      // Convert JSON date string to TDateTime
      Result.CreatedAt := ISO8601ToDate(JsonObject.GetValue<string>('created_at'));

      // Extracting urls object
      UrlsObj := JsonObject.GetValue<TJSONObject>('urls');
      if Assigned(UrlsObj) then
      begin
        Result.Urls.cancel := UrlsObj.GetValue<string>('cancel');
        Result.Urls.get := UrlsObj.GetValue<string>('get');
      end;
    end;
  finally
    JsonValue.Free;
  end;
end;

function TReplicateLLM.Completion(const AQuestion, AModel: string): string;
var
  RestClient: TRESTClient;
  RestRequest: TRESTRequest;
  RestResponse: TRESTResponse;
  JSONRoot, JSONInput: TJSONObject;
  version: string;
  i: Integer;
  prediction: TPredictionInfo;
  Stopwatch: TStopwatch;
  OperationCompleted: Boolean;
begin
  if ModelInfo.Count = 0 then
  begin
    GetModelInfo;
  end;
  version := '';
  for i := 0 to FModelInfo.Count - 1 do
  begin
    if FModelInfo[i].modelName = AModel then
    begin
      version := FModelInfo[i].version;
      Break;
    end;
  end;
  if version.IsEmpty then
    raise Exception.Create('Could not find model ' + AModel);
  // Create and setup REST client, request and response
  RestClient := TRESTClient.Create('https://api.replicate.com');
  try
    RestRequest := TRESTRequest.Create(nil);
    RestResponse := TRESTResponse.Create(nil);
    try
      RestRequest.Client := RestClient;
      RestRequest.Response := RestResponse;
      RestRequest.Resource := '/v1/predictions';
      RestRequest.Method := rmPOST;
      // Adding headers
      RestRequest.AddParameter('Authorization', 'Token ' + FAPIKey, TRESTRequestParameterKind.pkHTTPHEADER, [TRESTRequestParameterOption.poDoNotEncode]);
      // Create JSON body for POST request
      JSONRoot := TJSONObject.Create;
      try
        JSONRoot.AddPair('version', version);
        JSONInput := TJSONObject.Create;
        JSONInput.AddPair('prompt', AQuestion);
        JSONRoot.AddPair('input', JSONInput);
        RestRequest.AddBody(JSONRoot.ToString, TRESTContentType.ctAPPLICATION_JSON);
        // Execute the request
        RestRequest.Execute;
        if (RestResponse.StatusCode = 200) or (RestResponse.StatusCode = 201) then
        begin
          // Assuming the response contains a field called 'completion' with the answer
         // JSONBody := TJSONObject.ParseJSONValue(RestResponse.Content) as TJSONObject;
          prediction := DeserializePredictionInfo(RestResponse.Content);
          Stopwatch := TStopwatch.StartNew;
          OperationCompleted := False;
          repeat
          Result := GetPredictionDetails(prediction, OperationCompleted);
          until (Stopwatch.ElapsedMilliseconds > 30000) or OperationCompleted;
        end
        else
        begin
          // Handle errors or exceptions if needed
          raise Exception.CreateFmt('Error making prediction: %s', [RestResponse.Content]);
        end;
      finally
        JSONRoot.Free;
      end;
    finally
      RestRequest.Free;
      RestResponse.Free;
    end;
  finally
    RestClient.Free;
  end;
end;

function TReplicateLLM.GetModelInfo: TObjectList<TBaseModelInfo>;
var
  RestClient: TRESTClient;
  RestRequest: TRESTRequest;
  RestResponse: TRESTResponse;
  JSONValue: TJSONObject;
  JSONVersion: TJSONObject;
  JSONArray: TJSONArray;
  I: Integer;
  ModelInfo: TBaseModelInfo;
begin
  FModelInfo.Clear;

  RestClient := TRESTClient.Create('https://api.replicate.com');
  try
    RestRequest := TRESTRequest.Create(nil);
    RestResponse := TRESTResponse.Create(nil);
    try
      RestRequest.Client := RestClient;
      RestRequest.Response := RestResponse;
      RestRequest.Resource := '/v1/collections/streaming-language-models';

      RestRequest.Method := rmGET;

      RestRequest.AddParameter('Authorization', 'Token ' + FAPIKey, TRESTRequestParameterKind.pkHTTPHEADER, [TRESTRequestParameterOption.poDoNotEncode]);

      RestRequest.Execute;

      if RestResponse.StatusCode = 200 then
      begin
        JSONValue := TJSONObject.ParseJSONValue(RestResponse.Content) as TJSONObject;

        if Assigned(JSONValue) and Assigned(JSONValue.GetValue('models')) then
        begin
          JSONArray := JSONValue.GetValue('models') as TJSONArray;
          for I := 0 to JSONArray.Count - 1 do
          begin
            ModelInfo := TBaseModelInfo.Create;
            ModelInfo.modelName := JSONArray.Items[I].GetValue<string>('name');
            JSONVersion := JSONArray.Items[I].GetValue<TJSONObject>('latest_version');
            if Assigned(JSONVersion.GetValue('id')) then
              ModelInfo.version := JSONVersion.GetValue<string>('id');
            FModelInfo.Add(ModelInfo);
          end;
        end;

        FreeAndNil(JSONValue);
      end
      else
      begin
        // Handle errors or exceptions if needed
      end;

    finally
      RestRequest.Free;
      RestResponse.Free;
    end;

  finally
    RestClient.Free;
  end;
  Result := FModelInfo;
end;

end.
