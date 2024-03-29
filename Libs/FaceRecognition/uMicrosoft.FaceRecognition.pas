unit uMicrosoft.FaceRecognition;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Net.URLClient,
  REST.Client,
  REST.Types,
  System.JSON,
  uBaseFaceRecognition
  ;

type
  TMicrosoftFaceRecognition = class(TBaseFaceRecognition)
  public
    function DetectFacesFromURL(imageUrl: string): string; override;
    function DetectFacesFromStream(imageStream: TStream): string; override;
    function DetectFacesFromFile(imageFilename: string): string; override;
    constructor Create(const APIKey: string; const AHost: string);
  end;

implementation

{ TMicrosoftFaceRecognition }

constructor TMicrosoftFaceRecognition.Create(const APIKey: string; const AHost: string);
begin
  inherited Create(APIKey, AHost);
end;

function TMicrosoftFaceRecognition.DetectFacesFromFile(imageFilename: string): string;
var
  fs : TFileStream;
begin
  fs := TFileStream.Create(imageFilename, fmOpenRead);
  try
    Result := DetectFacesFromStream(fs);
  finally
    FreeAndNil(fs);
  end;
end;

function TMicrosoftFaceRecognition.DetectFacesFromStream(imageStream: TStream): string;
var
  restClient: TRESTClient;
  restRequest: TRESTRequest;
  restResponse: TRESTResponse;
  serviceUrl: string;
  uri : TURI;
  response : TJSONArray;
begin
  restClient := nil;
  restRequest := nil;
  restResponse := nil;
  try
    restClient := TRESTClient.Create(nil);
    restRequest := TRESTRequest.Create(nil);
    restResponse := TRESTResponse.Create(nil);
    // Construct the API endpoint URL
  //  url := 'https://<your region>.api.cognitive.microsoft.com/face/v1.0/detect';
    serviceUrl := FHost;
    uri := TURI.Create(serviceUrl);
    uri.AddParameter('returnFaceId', 'false');
    uri.AddParameter('returnFaceLandmarks', 'true');

    // Set the necessary REST client properties
    restClient.BaseURL := uri.ToString;
    restClient.ContentType := 'application/json';
    restClient.HandleRedirects := True;
    restClient.Authenticator := nil; // no authentication required for the Face API

    // Set the necessary REST request properties
    restRequest.Method := TRESTRequestMethod.rmPOST;
    restRequest.AddBody(imageStream, TRESTContentType.ctAPPLICATION_OCTET_STREAM);
    restRequest.Client := restClient;
    restRequest.Response := restResponse;

    // Set the necessary REST request headers
    restRequest.Params.AddHeader('Ocp-Apim-Subscription-Key', FResourceKey);

    // Execute the REST request and get the response
    restRequest.Execute;

    // Parse the JSON response and return the array of detected faces
    response := (TJSONObject.ParseJSONValue(restResponse.Content) as TJSONArray);
    try
      Result := response.ToJSON;
    finally
      FreeAndNil(response);
    end;
  finally
    FreeAndNil(restResponse);
    FreeAndNil(restRequest);
    FreeAndNil(restClient);
  end;
end;

function TMicrosoftFaceRecognition.DetectFacesFromURL(imageUrl: string): string;
var
  restClient: TRESTClient;
  restRequest: TRESTRequest;
  restResponse: TRESTResponse;
  serviceUrl: string;
  uri : TURI;
  request: TJSONObject;
  response : TJSONArray;
begin
  restClient := nil;
  restRequest := nil;
  restResponse := nil;
  request := nil;
  try
    restClient := TRESTClient.Create(nil);
    restRequest := TRESTRequest.Create(nil);
    restResponse := TRESTResponse.Create(nil);
    // Construct the API endpoint URL
  //  url := 'https://<your region>.api.cognitive.microsoft.com/face/v1.0/detect';
    serviceUrl := FHost;//'https://adugfaces.cognitiveservices.azure.com/face/v1.0/detect';
    uri := TURI.Create(serviceUrl);
    uri.AddParameter('returnFaceId', 'false');
    uri.AddParameter('returnFaceLandmarks', 'true');

    // Create a JSON request with the image URL
    request := TJSONObject.Create;
    request.AddPair('url', imageUrl);

    // Set the necessary REST client properties
    restClient.BaseURL := uri.ToString;
    restClient.ContentType := 'application/json';
    restClient.HandleRedirects := True;
    restClient.Authenticator := nil; // no authentication required for the Face API

    // Set the necessary REST request properties
    restRequest.Method := TRESTRequestMethod.rmPOST;
    restRequest.AddBody(request.ToJSON, TRESTContentType.ctAPPLICATION_JSON);
    restRequest.Client := restClient;
    restRequest.Response := restResponse;

    // Set the necessary REST request headers
    restRequest.Params.AddHeader('Ocp-Apim-Subscription-Key', FResourceKey);

    // Execute the REST request and get the response
    restRequest.Execute;

    // Parse the JSON response and return the array of detected faces
    response := (TJSONObject.ParseJSONValue(restResponse.Content) as TJSONArray);
    try
      Result := response.ToJSON;
    finally
      FreeAndNil(response);
    end;
  finally
    FreeAndNil(request);
    FreeAndNil(restResponse);
    FreeAndNil(restRequest);
    FreeAndNil(restClient);
  end;
end;


end.
