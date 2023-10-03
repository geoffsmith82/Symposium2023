unit uEmbeddings.Microsoft.OpenAI;

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
  TEmbeddingMicrosoftOpenAI = class(TEmbeddingService)
  protected
    FEndPoint : string;
  public
    function Embeddings(const Texts: TArray<string>): TEmbeddings; override;
    constructor Create(const APIKey: string; const Endpoint: string);
  end;

implementation

constructor TEmbeddingMicrosoftOpenAI.Create(const APIKey, Endpoint: string);
begin
  inherited Create(APIKey);
  FEndPoint := Endpoint;
end;

function TEmbeddingMicrosoftOpenAI.Embeddings(const Texts: TArray<string>): TEmbeddings;
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
  LJson := nil;
  LRestClient := nil;
  LRestRequest := nil;
  LRestResponse := nil;
  try
    LRestClient := TRESTClient.Create(FEndpoint);
    LRestRequest := TRESTRequest.Create(nil);
    LRestResponse := TRESTResponse.Create(nil);
    LRestRequest.Client := LRestClient;
    LRestRequest.Response := LRestResponse;
    LRestRequest.Method := TRESTRequestMethod.rmPOST;
    LRestRequest.Resource := '/openai/deployments/{deployment-id}/embeddings?api-version={api-version}';

    LRestRequest.AddParameter('deployment-id', 'text-embedding-ada-002', pkURLSEGMENT);
    LRestRequest.AddParameter('api-version', '2023-03-15-preview', pkQUERY);


    LJsonRequest := TJSONArray.Create;
    for I := 0 to High(Texts) do
      LJsonRequest.AddElement(TJSONString.Create(Texts[I]));

    LJson := TJSONObject.Create;
    LJson.AddPair('input', LJsonRequest);

    LJson.AddPair('model', 'text-embedding-ada-002');

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

end.
