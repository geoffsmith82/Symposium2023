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
  TUrls = class
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
    FUrls: TUrls;
  public
    property Id: string read FId write FId;
    property Version: string read FVersion write FVersion;
    property Prompt: string read FPrompt write FPrompt;
    property Logs: string read FLogs write FLogs;
    property Error: Variant read FError write FError;
    property Status: string read FStatus write FStatus;
    property CreatedAt: TDateTime read FCreatedAt write FCreatedAt;
    property Urls: TUrls read FUrls write FUrls;

    constructor Create;
    destructor Destroy; override;
  end;

  TInput = class
  private
    FPrompt: string;
  public
    property Prompt: string read FPrompt write FPrompt;
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
    FUrls: TUrls;
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
    property Urls: TUrls read FUrls write FUrls;

    constructor Create;
    destructor Destroy; override;
  end;



  TImageGenerationReplicate = class(TBaseImageGeneration)
  private
    function DeserializePredictionInfo(const AJsonStr: string): TPredictionInfo;
    function GetPredictionDetails(const Prediction: TPredictionInfo; out IsCompleted: Boolean): string;
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
  FUrls := TUrls.Create;
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

function TImageGenerationReplicate.GetPredictionDetails(const Prediction: TPredictionInfo; out IsCompleted: Boolean): string;
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
  LRestClient: TRESTClient;
  LRestRequest: TRESTRequest;
  LRestResponse: TRESTResponse;
  LJSONBody: TJSONObject;
  LInputJSON: TJSONObject;
  LResponseString: String;
  LPrediction: TPredictionInfo;
  LStopwatch: TStopwatch;
  LOperationCompleted: Boolean;
  LVersion : string;
  i : Integer;
  LData: TArray<TDataClass>;
  LRepresentation : TJsonRepresentation;
begin
  LVersion := '';
  Result := nil;
  for i := 0 to FModelInfo.Count - 1 do
  begin
    if FModelInfo[i].modelName = modelVersion then
    begin
      LVersion := FModelInfo[i].version;
      Break;
    end;
  end;

  if LVersion.IsEmpty then
    raise Exception.Create('Could not find model ' + modelVersion);

  LRestClient := TRESTClient.Create('https://api.replicate.com');
  try
    LRestRequest := TRESTRequest.Create(nil);
    LRestResponse := TRESTResponse.Create(nil);

    try
      LRestRequest.Client := LRestClient;
      LRestRequest.Response := LRestResponse;
      LRestRequest.Resource := '/v1/predictions';
      LRestRequest.Method := rmPOST;

      // Set headers using AddParameter
      LRestRequest.AddParameter('Content-Type', 'application/json', TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
      LRestRequest.AddParameter('Authorization', 'Token ' + FAPIKey, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);

      // Set JSON body
      LJSONBody := TJSONObject.Create;
      try
        LJSONBody.AddPair('version', LVersion);
        LInputJSON := TJSONObject.Create;
        LInputJSON.AddPair('prompt', prompt);
        // If you have other parameters like 'n' or 'size' to send with the request, add them to the InputJSON here.
        LJSONBody.AddPair('input', LInputJSON);
        LRestRequest.AddBody(LJSONBody.ToString, ctAPPLICATION_JSON);
      finally
        LJSONBody.Free;
      end;

      LRestRequest.Execute;

      // Handle response
      if (LRestResponse.StatusCode = 200) or (LRestResponse.StatusCode = 201) then
      begin
        //ResponseString := RestResponse.Content;  // Return the response content. Modify as necessary.
        LPrediction := DeserializePredictionInfo(LRestResponse.Content);
        try
          LStopwatch := TStopwatch.StartNew;
          LOperationCompleted := False;
          repeat
            LResponseString := GetPredictionDetails(LPrediction, LOperationCompleted);
            Sleep(500);
          until (LStopwatch.ElapsedMilliseconds > 30000) or LOperationCompleted;
        finally
          FreeAndNil(LPrediction);
        end;

        LRepresentation := TJsonRepresentation.Create;
        try
          LRepresentation.ParseJsonToObj(LResponseString);

          Result := TGeneratedImagesClass.Create;

          SetLength(LData, Length(LRepresentation.FOutput));
          Result.data := LData;
          for i := 0 to Length(LRepresentation.Output) - 1 do
          begin
            Result.data[i] := TDataClass.Create;
            Result.data[i].url := LRepresentation.Output[i];
          end;
          Result.created := LRepresentation.CreatedAt;
        finally
          FreeAndNil(LRepresentation);
        end;
      end
      else
      begin
        raise Exception.CreateFmt('Error %d: %s', [LRestResponse.StatusCode, LRestResponse.StatusText]);
      end;

    finally
      LRestRequest.Free;
      LRestResponse.Free;
    end;

  finally
    LRestClient.Free;
  end;
end;

function TImageGenerationReplicate.GetModelInfo: TObjectList<TImageModelInfo>;
var
  LRestClient: TRESTClient;
  LRestRequest: TRESTRequest;
  LRestResponse: TRESTResponse;
  LJSONValue: TJSONObject;
  LJSONVersion: TJSONObject;
  LJSONArray: TJSONArray;
  I: Integer;
  LModelInfo: TImageModelInfo;
begin
  FModelInfo.Clear;

  LRestClient := TRESTClient.Create('https://api.replicate.com');
  try
    LRestRequest := TRESTRequest.Create(nil);
    LRestResponse := TRESTResponse.Create(nil);
    try
      LRestRequest.Client := LRestClient;
      LRestRequest.Response := LRestResponse;
      LRestRequest.Resource := '/v1/collections/text-to-image';

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
            LModelInfo := TImageModelInfo.Create;
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

{ TPredictionInfo }

constructor TPredictionInfo.Create;
begin
  FUrls := TUrls.Create;
end;

destructor TPredictionInfo.Destroy;
begin
  FreeAndNil(FUrls);
  inherited;
end;

end.
