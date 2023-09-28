unit uImageGeneration.Replicate;

interface

uses
  System.Generics.Collections,
  REST.Client,
  REST.Types,
  System.JSON,
  System.SysUtils,
  uImageGeneration,
  uDALLe2.DTO
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


  TInput = class
  private
    FPrompt: string;
  public
    property Prompt: string read FPrompt write FPrompt;
  end;

  TUrl2s = class
  private
    FCancel: string;
    FGet: string;
  public
    property Cancel: string read FCancel write FCancel;
    property Get: string read FGet write FGet;
  end;

  TMetrics = class
  private
    FPredictTime: Double;
  public
    property PredictTime: Double read FPredictTime write FPredictTime;
  end;

  TJsonRepresentation = class
  private
    FId: string;
    FVersion: string;
    FInput: TInput;
    FLogs: string;
    FOutput: TArray<string>;
    FError: Variant;  // Using Variant to handle potential null value
    FStatus: string;
    FCreatedAt: TDateTime;
    FStartedAt: TDateTime;
    FCompletedAt: TDateTime;
    FMetrics: TMetrics;
    FUrls: TUrl2s;
    procedure ParseJsonToObj(const AJsonStr: string);
  public
    property Id: string read FId write FId;
    property Version: string read FVersion write FVersion;
    property Input: TInput read FInput write FInput;
    property Logs: string read FLogs write FLogs;
    property Output: TArray<string> read FOutput write FOutput;
    property Error: Variant read FError write FError;
    property Status: string read FStatus write FStatus;
    property CreatedAt: TDateTime read FCreatedAt write FCreatedAt;
    property StartedAt: TDateTime read FStartedAt write FStartedAt;
    property CompletedAt: TDateTime read FCompletedAt write FCompletedAt;
    property Metrics: TMetrics read FMetrics write FMetrics;
    property Urls: TUrl2s read FUrls write FUrls;

    constructor Create;
    destructor Destroy; override;
  end;



  TImageGenerationReplicate = class(TBaseImageGeneration)
  private
    function DeserializePredictionInfo(const AJsonStr: string): TPredictionInfo;
    function GetPredictionDetails(const Prediction: TPredictionInfo;
      out IsCompleted: Boolean): string;
  protected
    function GetModelInfo: TObjectList<TImageModelInfo>; override;
  public
    function Generate(const prompt: string; n: Integer; size: TDALLESize; const modelVersion: string): TGeneratedImagesClass; override;
  end;

implementation

uses
  System.DateUtils,
  System.Diagnostics
  ;

constructor TJsonRepresentation.Create;
begin
  inherited;
  FInput := TInput.Create;
  FMetrics := TMetrics.Create;
  FUrls := TUrl2s.Create;
end;

destructor TJsonRepresentation.Destroy;
begin
  FInput.Free;
  FMetrics.Free;
  FUrls.Free;
  inherited;
end;

{ TImageGenerationReplicate }


function TImageGenerationReplicate.DeserializePredictionInfo(const AJsonStr: string): TPredictionInfo;
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

function TImageGenerationReplicate.GetPredictionDetails(const Prediction: TPredictionInfo; out IsCompleted: Boolean): string;
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

procedure TJsonRepresentation.ParseJsonToObj(const AJsonStr: string);
var
  LJsonObj: TJSONObject;
  LJsonValue: TJSONValue;
  LJsonArray: TJSONArray;
  I: Integer;
begin

  LJsonObj := TJSONObject.ParseJSONValue(AJsonStr) as TJSONObject;
  try
    Id := LJsonObj.GetValue<string>('id');
    Version := LJsonObj.GetValue<string>('version');

    LJsonValue := LJsonObj.GetValue('input');
    if Assigned(LJsonValue) and (LJsonValue is TJSONObject) then
      Input.Prompt := TJSONObject(LJsonValue).GetValue<string>('prompt');

    Logs := LJsonObj.GetValue<string>('logs');

    LJsonValue := LJsonObj.GetValue('output');
    if Assigned(LJsonValue) and (LJsonValue is TJSONArray) then
    begin
      LJsonArray := LJsonValue as TJSONArray;
      SetLength(FOutput, LJsonArray.Count);
      for I := 0 to LJsonArray.Count - 1 do
        Output[I] := LJsonArray.Items[I].Value;
    end;

    Error := LJsonObj.GetValue('error').Value;
    Status := LJsonObj.GetValue<string>('status');

    CreatedAt := ISO8601ToDate(LJsonObj.GetValue<string>('created_at'));
    StartedAt := ISO8601ToDate(LJsonObj.GetValue<string>('started_at'));
    CompletedAt := ISO8601ToDate(LJsonObj.GetValue<string>('completed_at'));

    LJsonValue := LJsonObj.GetValue('metrics');
    if Assigned(LJsonValue) and (LJsonValue is TJSONObject) then
      Metrics.PredictTime := TJSONObject(LJsonValue).GetValue<Double>('predict_time');

    LJsonValue := LJsonObj.GetValue('urls');
    if Assigned(LJsonValue) and (LJsonValue is TJSONObject) then
    begin
      Urls.Cancel := TJSONObject(LJsonValue).GetValue<string>('cancel');
      Urls.Get := TJSONObject(LJsonValue).GetValue<string>('get');
    end;
  finally
    LJsonObj.Free;
  end;
end;

function TImageGenerationReplicate.Generate(const prompt: string; n: Integer; size: TDALLESize; const modelVersion: string): TGeneratedImagesClass;
var
  RestClient: TRESTClient;
  RestRequest: TRESTRequest;
  RestResponse: TRESTResponse;
  JSONBody: TJSONObject;
  InputJSON: TJSONObject;
  ResponseString: String;
  prediction: TPredictionInfo;
  Stopwatch: TStopwatch;
  OperationCompleted: Boolean;
  version : string;
  i : Integer;
  data: TArray<TDataClass>;
  rep : TJsonRepresentation;
begin
  version := '';
  Result := nil;
  for i := 0 to FModelInfo.Count - 1 do
  begin
    if FModelInfo[i].modelName = modelVersion then
    begin
      version := FModelInfo[i].version;
      Break;
    end;
  end;

  if version.IsEmpty then
    raise Exception.Create('Could not find model ' + modelVersion);

  RestClient := TRESTClient.Create('https://api.replicate.com');
  try
    RestRequest := TRESTRequest.Create(nil);
    RestResponse := TRESTResponse.Create(nil);

    try
      RestRequest.Client := RestClient;
      RestRequest.Response := RestResponse;
      RestRequest.Resource := '/v1/predictions';
      RestRequest.Method := rmPOST;

      // Set headers using AddParameter
      RestRequest.AddParameter('Content-Type', 'application/json', TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
      RestRequest.AddParameter('Authorization', 'Token ' + FAPIKey, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);

      // Set JSON body
      JSONBody := TJSONObject.Create;
      try
        JSONBody.AddPair('version', version);
        InputJSON := TJSONObject.Create;
        InputJSON.AddPair('prompt', prompt);
        // If you have other parameters like 'n' or 'size' to send with the request, add them to the InputJSON here.
        JSONBody.AddPair('input', InputJSON);
        RestRequest.AddBody(JSONBody.ToString, ctAPPLICATION_JSON);
      finally
        JSONBody.Free;
      end;

      RestRequest.Execute;

      // Handle response
      if (RestResponse.StatusCode = 200) or (RestResponse.StatusCode = 201) then
      begin
        //ResponseString := RestResponse.Content;  // Return the response content. Modify as necessary.
        prediction := DeserializePredictionInfo(RestResponse.Content);
        try
          Stopwatch := TStopwatch.StartNew;
          OperationCompleted := False;
          repeat
            ResponseString := GetPredictionDetails(prediction, OperationCompleted);
            Sleep(500);
          until (Stopwatch.ElapsedMilliseconds > 30000) or OperationCompleted;
        finally
          FreeAndNil(prediction);
        end;

        rep := TJsonRepresentation.Create;
        try
          rep.ParseJsonToObj(ResponseString);

          Result := TGeneratedImagesClass.Create;

          SetLength(data, Length(rep.FOutput));
          Result.data := data;
          for i := 0 to Length(rep.Output) - 1 do
          begin
            Result.data[i] := TDataClass.Create;
            Result.data[i].url := rep.Output[i];
          end;
          Result.created := rep.CreatedAt;
        finally
          FreeAndNil(rep);
        end;
      end
      else
      begin
        raise Exception.CreateFmt('Error %d: %s', [RestResponse.StatusCode, RestResponse.StatusText]);
      end;

    finally
      RestRequest.Free;
      RestResponse.Free;
    end;

  finally
    RestClient.Free;
  end;
end;

function TImageGenerationReplicate.GetModelInfo: TObjectList<TImageModelInfo>;
var
  RestClient: TRESTClient;
  RestRequest: TRESTRequest;
  RestResponse: TRESTResponse;
  JSONValue: TJSONObject;
  JSONVersion: TJSONObject;
  JSONArray: TJSONArray;
  I: Integer;
  ModelInfo: TImageModelInfo;
begin
  FModelInfo.Clear;

  RestClient := TRESTClient.Create('https://api.replicate.com');
  try
    RestRequest := TRESTRequest.Create(nil);
    RestResponse := TRESTResponse.Create(nil);
    try
      RestRequest.Client := RestClient;
      RestRequest.Response := RestResponse;
      RestRequest.Resource := '/v1/collections/text-to-image';

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
            ModelInfo := TImageModelInfo.Create;
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
