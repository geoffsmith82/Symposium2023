unit uEmbeddings.Ollama;

interface

uses
  System.Classes,
  System.JSON,
  System.SysUtils,
  System.Generics.Collections,
  REST.Client,
  REST.Types,
  uEmbeddings
  ;

type
  TEmbeddingsOllama = class (TEmbeddingService)
  protected
    function GetModelInfo: TObjectList<TBaseModelInfo>; override;
  public
    function Embeddings(const Texts: TArray<string>): TEmbeddings; override;
  end;

implementation

function TEmbeddingsOllama.Embeddings(const Texts: TArray<string>): TEmbeddings;
var
  LRestClient: TRESTClient;
  LRestRequest: TRESTRequest;
  LRestResponse: TRESTResponse;
  LJsonRequest: TJSONArray;
  LDataArray, LEmbeddingArray: TJSONArray;
  LJsonResponse : TJSONObject;
  LJson: TJSONObject;
  I, J: Integer;
begin
  LRestClient := nil;
  LRestRequest := nil;
  LRestResponse := nil;
  LJson := nil;

  try
    LRestClient := TRESTClient.Create(nil);
    LRestRequest := TRESTRequest.Create(nil);
    LRestResponse := TRESTResponse.Create(nil);

    LRestRequest.Client := LRestClient;
    LRestClient.BaseURL := 'http://localhost:11434';
    LRestRequest.Resource := '/api/embed';
    LRestRequest.Response := LRestResponse;
    LRestRequest.Method := TRESTRequestMethod.rmPOST;

    LJsonRequest := TJSONArray.Create;
    for I := 0 to High(Texts) do
      LJsonRequest.AddElement(TJSONString.Create(Texts[I]));

    LJson := TJSONObject.Create;
    LJson.AddPair('input', LJsonRequest);

    LJson.AddPair('model', 'mxbai-embed-large:latest');

    LRestRequest.AddBody(LJson.ToString, TRESTContentType.ctAPPLICATION_JSON);
    LRestRequest.AddAuthParameter('Authorization', 'Bearer ' + FAPIKey, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
    LRestRequest.Execute;

    if LRestResponse.StatusCode = 200 then
    begin
      LJsonResponse := TJSONObject.ParseJSONValue(LRestResponse.Content) as TJSONObject;
      LDataArray := LJsonResponse.GetValue<TJSONArray>('data');
      SetLength(Result, LDataArray.Count);

      for I := 0 to LDataArray.Count - 1 do
      begin
        LEmbeddingArray := LDataArray.Items[I].GetValue<TJSONArray>('embedding');
        SetLength(Result[I], LEmbeddingArray.Count);
        for J := 0 to LEmbeddingArray.Count - 1 do
          Result[I][J] := (LEmbeddingArray.Items[J] as TJSONNumber).AsDouble;
      end;

      FreeAndNil(LJsonResponse);
    end
    else
      raise Exception.CreateFmt('Error: %d - %s', [LRestResponse.StatusCode, LRestResponse.StatusText]);

  finally
    FreeAndNil(LJson);
    FreeAndNil(LRestRequest);
    FreeAndNil(LRestClient);
  end;
end;

function TEmbeddingsOllama.GetModelInfo: TObjectList<TBaseModelInfo>;
var
  modelInfo : TBaseModelInfo;
begin
  if FModelInfo.Count = 0 then
  begin
    modelInfo := TBaseModelInfo.Create;
    modelInfo.modelName := 'mxbai-embed-large:latest';
    modelInfo.version := 'mxbai-embed-large:latest';
    FModelInfo.Add(modelInfo);
  end;
  Result := FModelInfo;
end;

end.
