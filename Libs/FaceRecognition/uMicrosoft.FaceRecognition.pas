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
  strict private
    FAPIKey : string;
  public
    function DetectFacesFromURL(imageUrl: string): string; override;
    function DetectFacesFromStream(imageStream: TStream): string; override;
    function DetectFacesFromFile(imageFilename: string): string; override;
    constructor Create(const APIKey: string);
  end;

implementation

{ TMicrosoftFaceRecognition }

constructor TMicrosoftFaceRecognition.Create(const APIKey: string);
begin
  inherited Create(APIKey, '', '');
  FAPIKey := APIKey;
end;

function TMicrosoftFaceRecognition.DetectFacesFromFile(imageFilename: string): string;
begin

end;

function TMicrosoftFaceRecognition.DetectFacesFromStream(imageStream: TStream): string;
begin

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
    serviceUrl := 'https://adugfaces.cognitiveservices.azure.com/face/v1.0/detect';
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
    restRequest.Params.AddHeader('Ocp-Apim-Subscription-Key', FAPIKey);

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
