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
    function Generate(const prompt: string; n: Integer; size: TDALLESize; const modelVersion: string = 'dall-e-2'): TGeneratedImagesClass; override;
  end;

implementation

{ TImageGenerationOpenAI }

function TImageGenerationOpenAI.Generate(const prompt: string; n: Integer; size: TDALLESize; const modelVersion: string): TGeneratedImagesClass;
var
  LClient: TRESTClient;
  LRequest: TRESTRequest;
  LResponse: TRESTResponse;
  LJson: TJSONObject;
begin
  LClient := nil;
  LRequest := nil;
  LResponse := nil;
  LJson := nil;
  try
    LClient := TRESTClient.Create(nil);
    LClient.ReadTimeout := 60000;
    LRequest := TRESTRequest.Create(nil);
    LClient.BaseURL := 'https://api.openai.com';
    LRequest.Client := LClient;
    LRequest.Method := rmPOST;
    LRequest.Resource := '/v1/images/generations';
    LRequest.AddAuthParameter('Authorization', 'Bearer ' + FAPIKey, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);

    LJson := TJSONObject.Create;
    try
      LJson.AddPair('prompt', TJSONString.Create(prompt));
      LJson.AddPair('n', TJSONNumber.Create(n));
      LJson.AddPair('model', modelVersion);
      case size of
        DALLE256: LJson.AddPair('size', '256x256');
        DALLE512: LJson.AddPair('size', '512x512');
        DALLE1024: LJson.AddPair('size', '1024x1024');
      end;
      LRequest.AddBody(LJson.ToString, ctAPPLICATION_JSON);
    finally
      FreeAndNil(LJson);
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
  LModel : TImageModelInfo;
begin
  FModelInfo.Clear;
  LModel := TImageModelInfo.Create;
  LModel.ModelName := 'dall-e-3';
  LModel.Version := 'dall-e-3';
  FModelInfo.Add(LModel);

  LModel := TImageModelInfo.Create;
  LModel.ModelName := 'dall-e-2';
  LModel.Version := 'dall-e-2';
  FModelInfo.Add(LModel);
  Result := FModelInfo;
end;

end.
