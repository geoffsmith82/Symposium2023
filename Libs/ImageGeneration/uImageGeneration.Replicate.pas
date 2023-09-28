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
  TImageGenerationReplicate = class(TBaseImageGeneration)
  protected
    function GetModelInfo: TObjectList<TImageModelInfo>; override;
  public
    function Generate(const prompt: string; n: Integer; size: TDALLESize): TGeneratedImagesClass; override;
  end;

implementation

{ TImageGenerationReplicate }

function TImageGenerationReplicate.Generate(const prompt: string; n: Integer; size: TDALLESize): TGeneratedImagesClass;
begin

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
