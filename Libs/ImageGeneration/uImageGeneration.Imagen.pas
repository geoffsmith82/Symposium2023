unit uImageGeneration.Imagen;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.JSON,
  REST.Client,
  REST.Types,
  uImageGeneration,
  uDALLe2.DTO;

type
  TGeneratedImagesImagen = class(TGeneratedImagesClass)
  public
    // Optionally, here we could override or extend functionality
    // like converting base64 to image file or memory stream
    function SaveImagesToFiles(const Directory: string; const FilePrefix: string = 'img'): TArray<string>;
  end;

  TImageGenerationImagen = class(TBaseImageGeneration)
  private
    FEndpoint: string;
    FProjectID: string;
    FLocation: string;
    function GetAccessToken: string;
    function MakeImagenRequest(const Prompt: string; Size: TDALLESize; const ModelVersion: string): string;
  protected
    function GetModelInfo: TObjectList<TImageModelInfo>; override;
  public
    function Generate(const prompt: string; n: Integer; size: TDALLESize; const modelVersion: string): TGeneratedImagesClass; override;
    constructor Create(const APIKey, ProjectID, Location: string; const endpoint: string = 'https://us-central1-aiplatform.googleapis.com');
  end;

implementation

uses
  System.NetEncoding,
  System.IOUtils,
  System.DateUtils;

constructor TImageGenerationImagen.Create(const APIKey, ProjectID, Location: string; const endpoint: string);
begin
  inherited Create(APIKey);
  FProjectID := ProjectID;
  FLocation := Location;
  FEndpoint := endpoint;
end;

function TImageGenerationImagen.GetAccessToken: string;
begin
  // For real implementation, replace with OAuth2 token retrieval
  Result := FAPIKey;
end;

function TImageGenerationImagen.GetModelInfo: TObjectList<TImageModelInfo>;
var
  Info: TImageModelInfo;
begin
  if FModelInfo.Count = 0 then
  begin
    Info := TImageModelInfo.Create;
    Info.modelName := 'imagen';
    Info.version := 'latest'; // or specific model version identifier
    FModelInfo.Add(Info);
  end;
  Result := FModelInfo;
end;

function TImageGenerationImagen.MakeImagenRequest(const Prompt: string; Size: TDALLESize; const ModelVersion: string): string;
var
  Client: TRESTClient;
  Request: TRESTRequest;
  Response: TRESTResponse;
  JsonBody, Instance, Parameters: TJSONObject;
  ImageSize: string;
  ResourcePath: string;
begin
  case Size of
    DALLE256: ImageSize := '256x256';
    DALLE512: ImageSize := '512x512';
    DALLE1024: ImageSize := '1024x1024';
  end;

  ResourcePath := Format('/v1/projects/%s/locations/%s/publishers/google/models/%s:predict', [
    FProjectID, FLocation, ModelVersion
  ]);

  Client := TRESTClient.Create(FEndpoint);
  try
    Request := TRESTRequest.Create(nil);
    Response := TRESTResponse.Create(nil);
    try
      Request.Client := Client;
      Request.Response := Response;
      Request.Resource := ResourcePath;
      Request.Method := rmPOST;

      Request.AddParameter('Authorization', 'Bearer ' + GetAccessToken, pkHTTPHEADER, [poDoNotEncode]);
      Request.AddParameter('Content-Type', 'application/json', pkHTTPHEADER, [poDoNotEncode]);

      Instance := TJSONObject.Create;
      Instance.AddPair('prompt', Prompt);

      Parameters := TJSONObject.Create;
      Parameters.AddPair('sampleCount', '1');
      Parameters.AddPair('imageSize', ImageSize);

      JsonBody := TJSONObject.Create;
      JsonBody.AddPair('instances', TJSONArray.Create(Instance));
      JsonBody.AddPair('parameters', Parameters);

      Request.AddBody(JsonBody.ToString, ctAPPLICATION_JSON);
      Request.Execute;

      if not (Response.StatusCode in [200, 201]) then
        raise Exception.CreateFmt('Error: %s', [Response.Content]);

      Result := Response.Content;
    finally
      Request.Free;
      Response.Free;
    end;
  finally
    Client.Free;
  end;
end;

function TImageGenerationImagen.Generate(const prompt: string; n: Integer; size: TDALLESize; const modelVersion: string): TGeneratedImagesClass;
var
  Json: TJSONObject;
  Images: TJSONArray;
  ImgResult: TGeneratedImagesImagen;
  i: Integer;
begin
  Result := nil;
  Json := TJSONObject.ParseJSONValue(MakeImagenRequest(prompt, size, modelVersion)) as TJSONObject;
  try
    Images := Json.GetValue<TJSONArray>('predictions');

    ImgResult := TGeneratedImagesImagen.Create;
    SetLength(ImgResult.Fdata, Images.Count);
    for i := 0 to Images.Count - 1 do
    begin
      ImgResult.data[i] := TDataClass.Create;
      ImgResult.data[i].url := Images.Items[i].GetValue<string>('bytesBase64Encoded');
    end;
    ImgResult.created := Now;
    Result := ImgResult;
  finally
    Json.Free;
  end;
end;

function TGeneratedImagesImagen.SaveImagesToFiles(const Directory, FilePrefix: string): TArray<string>;
var
  i: Integer;
  ImageBytes: TBytes;
  FilePath: string;
  Decoder: TBase64Encoding;
begin
  SetLength(Result, Length(Self.data));
  Decoder := TBase64Encoding.Create;
  try
    for i := 0 to High(Self.data) do
    begin
      ImageBytes := Decoder.DecodeStringToBytes(Self.data[i].url);
      FilePath := TPath.Combine(Directory, Format('%s_%d.png', [FilePrefix, i + 1]));
      TFile.WriteAllBytes(FilePath, ImageBytes);
      Result[i] := FilePath;
    end;
  finally
    Decoder.Free;
  end;
end;

end.

