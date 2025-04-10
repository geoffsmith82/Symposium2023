unit uImageGeneration.XAI;

interface

uses
  System.Generics.Collections,
  REST.Client,
  REST.Types,
  System.JSON,
  System.SysUtils,
  uImageGeneration,
  uDALLe2.DTO;

type
  TXAIModel = class
  private
    FId: string;
    FFingerprint: string;
    FMaxPromptLength: Integer;
    FCreated: Int64;  // Unix timestamp
    FObject: string;
    FOwnedBy: string;
    FVersion: string;
    FPromptTextTokenPrice: Integer;
    FPromptImageTokenPrice: Integer;
    FGeneratedImageTokenPrice: Integer;
    FAliases: TArray<string>;
  public
    property Id: string read FId write FId;
    property Fingerprint: string read FFingerprint write FFingerprint;
    property MaxPromptLength: Integer read FMaxPromptLength write FMaxPromptLength;
    property Created: Int64 read FCreated write FCreated;
    property &Object: string read FObject write FObject;
    property OwnedBy: string read FOwnedBy write FOwnedBy;
    property Version: string read FVersion write FVersion;
    property PromptTextTokenPrice: Integer read FPromptTextTokenPrice write FPromptTextTokenPrice;
    property PromptImageTokenPrice: Integer read FPromptImageTokenPrice write FPromptImageTokenPrice;
    property GeneratedImageTokenPrice: Integer read FGeneratedImageTokenPrice write FGeneratedImageTokenPrice;
    property Aliases: TArray<string> read FAliases write FAliases;
  end;

  TXAIResponse = class(TGeneratedImagesClass)
  private
    procedure ParseJsonToObj(const AJsonStr: string);
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TImageGenerationXAI = class(TBaseImageGeneration)
  private
    FEndpoint: string;
    function DeserializeResponse(const AJsonStr: string): TXAIResponse;
    function DeserializeModels(const AJsonStr: string): TObjectList<TXAIModel>;
  protected
    function GetModelInfo: TObjectList<TImageModelInfo>; override;
  public
    constructor Create(const APIKey: string; const endpoint: string = 'https://api.x.ai/v1');
    function Generate(const prompt: string; n: Integer; size: TDALLESize; const modelVersion: string): TGeneratedImagesClass; override;
    function GetAvailableModels: TObjectList<TXAIModel>;
  end;

implementation

uses
  System.DateUtils;

{ TXAIModel }

{ TXAIResponse }

constructor TXAIResponse.Create;
begin
  inherited Create;
end;

destructor TXAIResponse.Destroy;
begin
  inherited Destroy;
end;

procedure TXAIResponse.ParseJsonToObj(const AJsonStr: string);
var
  LJsonObj: TJSONObject;
  LJsonArray: TJSONArray;
  I: Integer;
begin
  LJsonObj := TJSONObject.ParseJSONValue(AJsonStr) as TJSONObject;
  try
    created := Now;

    LJsonArray := LJsonObj.GetValue<TJSONArray>('data');
    if Assigned(LJsonArray) then
    begin
      SetLength(Fdata, LJsonArray.Count);
      for I := 0 to LJsonArray.Count - 1 do
      begin
        data[I] := TDataClass.Create;
        data[I].url := LJsonArray.Items[I].GetValue<string>('url');
      end;
    end;
  finally
    LJsonObj.Free;
  end;
end;

{ TImageGenerationXAI }

constructor TImageGenerationXAI.Create(const APIKey: string; const endpoint: string);
begin
  inherited Create(APIKey);
  FEndpoint := endpoint;
end;

function TImageGenerationXAI.DeserializeResponse(const AJsonStr: string): TXAIResponse;
begin
  Result := TXAIResponse.Create;
  try
    Result.ParseJsonToObj(AJsonStr);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TImageGenerationXAI.DeserializeModels(const AJsonStr: string): TObjectList<TXAIModel>;
var
  LJsonObj: TJSONObject;
  LJsonArray: TJSONArray;
  LModel: TXAIModel;
  I, J: Integer;
begin
  Result := TObjectList<TXAIModel>.Create(True); // Owns objects
  LJsonObj := TJSONObject.ParseJSONValue(AJsonStr) as TJSONObject;
  try
    LJsonArray := LJsonObj.GetValue<TJSONArray>('models');
    if Assigned(LJsonArray) then
    begin
      for I := 0 to LJsonArray.Count - 1 do
      begin
        LModel := TXAIModel.Create;
        with LJsonArray.Items[I] as TJSONObject do
        begin
          LModel.Id := GetValue<string>('id', '');
          LModel.Fingerprint := GetValue<string>('fingerprint', '');
          LModel.MaxPromptLength := GetValue<Integer>('max_prompt_length', 0);
          LModel.Created := GetValue<Int64>('created', 0);
          LModel.&Object := GetValue<string>('object', '');
          LModel.OwnedBy := GetValue<string>('owned_by', '');
          LModel.Version := GetValue<string>('version', '');
          LModel.PromptTextTokenPrice := GetValue<Integer>('prompt_text_token_price', 0);
          LModel.PromptImageTokenPrice := GetValue<Integer>('prompt_image_token_price', 0);
          LModel.GeneratedImageTokenPrice := GetValue<Integer>('generated_image_token_price', 0);

          LJsonArray := GetValue<TJSONArray>('aliases');
          if Assigned(LJsonArray) then
          begin
            SetLength(LModel.FAliases, LJsonArray.Count);
            for J := 0 to LJsonArray.Count - 1 do
              LModel.Aliases[J] := LJsonArray.Items[J].Value;
          end;
        end;
        Result.Add(LModel);
      end;
    end;
  finally
    LJsonObj.Free;
  end;
end;

function TImageGenerationXAI.GetAvailableModels: TObjectList<TXAIModel>;
var
  LRestClient: TRESTClient;
  LRestRequest: TRESTRequest;
  LRestResponse: TRESTResponse;
begin
  LRestClient := TRESTClient.Create(FEndpoint);
  try
    LRestRequest := TRESTRequest.Create(nil);
    LRestResponse := TRESTResponse.Create(nil);
    try
      LRestRequest.Client := LRestClient;
      LRestRequest.Response := LRestResponse;
      LRestRequest.Resource := '/image-generation-models';
      LRestRequest.Method := rmGET;

      LRestRequest.AddParameter('accept', 'application/json', TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
      LRestRequest.AddParameter('Authorization', 'Bearer ' + FAPIKey, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);

      LRestRequest.Execute;

      if LRestResponse.StatusCode = 200 then
        Result := DeserializeModels(LRestResponse.Content)
      else
        raise Exception.CreateFmt('Error %d: %s - %s',
          [LRestResponse.StatusCode, LRestResponse.StatusText, LRestResponse.Content]);

    finally
      LRestRequest.Free;
      LRestResponse.Free;
    end;
  finally
    LRestClient.Free;
  end;
end;

function TImageGenerationXAI.Generate(const prompt: string; n: Integer; size: TDALLESize; const modelVersion: string): TGeneratedImagesClass;
var
  LRestClient: TRESTClient;
  LRestRequest: TRESTRequest;
  LRestResponse: TRESTResponse;
  LJSONBody: TJSONObject;
  LXAIResponse: TXAIResponse;
  LSizeStr: string;
begin
  case size of
    DALLE256: LSizeStr := '256x256';
    DALLE512: LSizeStr := '512x512';
    DALLE1024: LSizeStr := '1024x1024';
  else
    LSizeStr := '1024x1024'; // Default size
  end;

  LRestClient := TRESTClient.Create(FEndpoint);
  try
    LRestRequest := TRESTRequest.Create(nil);
    LRestResponse := TRESTResponse.Create(nil);
    try
      LRestRequest.Client := LRestClient;
      LRestRequest.Response := LRestResponse;
      LRestRequest.Resource := '/images/generations';
      LRestRequest.Method := rmPOST;

      LRestRequest.AddParameter('accept', 'application/json', TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
      LRestRequest.AddParameter('Authorization', 'Bearer ' + FAPIKey, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
      LRestRequest.AddParameter('Content-Type', 'application/json', TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);

      LJSONBody := TJSONObject.Create;
      try
        LJSONBody.AddPair('model', modelVersion);
        LJSONBody.AddPair('prompt', prompt);
        LJSONBody.AddPair('n', TJSONNumber.Create(n));
//        LJSONBody.AddPair('size', LSizeStr);
        LRestRequest.AddBody(LJSONBody.ToString, ctAPPLICATION_JSON);
      finally
        LJSONBody.Free;
      end;

      LRestRequest.Execute;

      if LRestResponse.StatusCode = 200 then
      begin
        LXAIResponse := DeserializeResponse(LRestResponse.Content);
        Result := LXAIResponse;
      end
      else
        raise Exception.CreateFmt('Error %d: %s - %s',
          [LRestResponse.StatusCode, LRestResponse.StatusText, LRestResponse.Content]);

    finally
      LRestRequest.Free;
      LRestResponse.Free;
    end;
  finally
    LRestClient.Free;
  end;
end;

function TImageGenerationXAI.GetModelInfo: TObjectList<TImageModelInfo>;
var
  LModels: TObjectList<TXAIModel>;
  LModel: TXAIModel;
  LModelInfo: TImageModelInfo;
  I: Integer;
begin
  // Clear existing model info
  if FModelInfo.Count > 0 then
  begin
    Result := FModelInfo;
    Exit;
  end;


  // Get the latest models from the API
  LModels := GetAvailableModels;
  try
    for I := 0 to LModels.Count - 1 do
    begin
      LModel := LModels[I];
      LModelInfo := TImageModelInfo.Create;
      LModelInfo.modelName := LModel.Id;
      LModelInfo.version := LModel.Version;
      FModelInfo.Add(LModelInfo);
    end;
  finally
    LModels.Free;
  end;

  Result := FModelInfo;
end;

end.
