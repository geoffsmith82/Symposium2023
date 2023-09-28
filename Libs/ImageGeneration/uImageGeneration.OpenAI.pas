unit uImageGeneration.OpenAI;

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
  TImageGenerationOpenAI = class(TBaseImageGeneration)
  protected
    function GetModelInfo: TObjectList<TImageModelInfo>; override;
  public
    function Generate(const prompt: string; n: Integer; size: TDALLESize): TGeneratedImagesClass; override;
  end;

implementation

{ TImageGenerationOpenAI }

function TImageGenerationOpenAI.Generate(const prompt: string; n: Integer; size: TDALLESize): TGeneratedImagesClass;
var
  LClient: TRESTClient;
  LRequest: TRESTRequest;
  LResponse: TRESTResponse;
  json: TJSONObject;
begin
  LClient := nil;
  LRequest := nil;
  LResponse := nil;
  json := nil;
  try
    LClient := TRESTClient.Create(nil);
    LClient.ReadTimeout := 60000;
    LRequest := TRESTRequest.Create(nil);
    LClient.BaseURL := 'https://api.openai.com';
    LRequest.Client := LClient;
    LRequest.Method := rmPOST;
    LRequest.Resource := '/v1/images/generations';
    LRequest.AddAuthParameter('Authorization', 'Bearer ' + FAPIKey, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);

    json := TJSONObject.Create;
    try
      json.AddPair('prompt', TJSONString.Create(prompt));
      json.AddPair('n', TJSONNumber.Create(n));
      case size of
        DALLE256: json.AddPair('size', '256x256');
        DALLE512: json.AddPair('size', '512x512');
        DALLE1024: json.AddPair('size', '1024x1024');
      end;
      LRequest.AddBody(json.ToString, ctAPPLICATION_JSON);
    finally
      FreeAndNil(json);
    end;

    LResponse := TRESTResponse.Create(nil);
    LResponse.ContentType := 'application/json';
    LRequest.Response := LResponse;
    LRequest.Execute;
    Result := TGeneratedImagesClass.FromJsonString(LResponse.Content);
  finally
    FreeAndNil(LRequest);
    FreeAndNil(LResponse);
    FreeAndNil(LClient);
  end;
end;

function TImageGenerationOpenAI.GetModelInfo: TObjectList<TImageModelInfo>;
var
  model : TImageModelInfo;
begin
  FModelInfo.Clear;
  model := TImageModelInfo.Create;
  model.ModelName := 'DALLE-2';
  model.Version := 'DALLE-2';
  FModelInfo.Add(model);
  Result := FModelInfo;
end;

end.
