unit uCodeProject.FaceRecognition;

interface

uses
  System.Classes,
  System.SysUtils,
  System.JSON,
  Net.HttpClient,
  Net.HttpClientComponent,
  REST.Client,
  REST.Types,
  uBaseFaceRecognition
  ;

type
  TCodeProjectFaceRecognition = class(TBaseFaceRecognition)
  private
    FBaseURL: string;
    FMinConfidence : Single;
  public
    function DetectFacesFromURL(imageUrl: string): string; override;
    function DetectFacesFromStream(imageStream: TStream): string; override;
    function DetectFacesFromFile(imageFilename: string): string; override;
    constructor Create(const ABaseURL: string);
  end;

implementation

{ TCodeProjectFaceRecognition }

constructor TCodeProjectFaceRecognition.Create(const ABaseURL: string);
begin
  inherited Create('', ABaseURL);
  FMinConfidence := 0.4;
end;

function TCodeProjectFaceRecognition.DetectFacesFromFile(imageFilename: string): string;
var
  ImageFileStream: TFileStream;
begin
  ImageFileStream := TFileStream.Create(imageFilename, fmOpenRead or fmShareDenyWrite);
  try
    Result := DetectFacesFromStream(ImageFileStream);
  finally
    FreeAndNil(ImageFileStream);
  end;
end;

function TCodeProjectFaceRecognition.DetectFacesFromStream(imageStream: TStream): string;
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
  fileParam: TRESTRequestParameter;
  jsonError : TJSONObject;
  errorMsg : string;
begin
  RESTClient := nil;
  RESTRequest := nil;
  RESTResponse := nil;
  jsonError := nil;
  try
    RESTClient := TRESTClient.Create(FBaseURL);
    RESTRequest := TRESTRequest.Create(nil);
    RESTResponse := TRESTResponse.Create(nil);
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Resource := '/v1/vision/face';
    RESTRequest.Method := TRESTRequestMethod.rmPOST;

    RESTRequest.Params.Clear;
    fileParam := RESTRequest.Params.AddItem;
    fileParam.Name := 'image';
    fileParam.Kind := TRESTRequestParameterKind.pkFILE;
    fileParam.SetStream(imageStream);
    RESTRequest.Params.AddItem('min_confidence', FMinConfidence.ToString, TRESTRequestParameterKind.pkGETorPOST);

    RESTRequest.Execute;
    Result := RESTResponse.Content;
    jsonError := TJSONObject.ParseJSONValue(Result) as TJSONObject;
    if jsonError.TryGetValue('error', errorMsg) then
      raise Exception.Create(errorMsg);
  finally
    FreeAndNil(jsonError);
    FreeAndNil(RESTResponse);
    FreeAndNil(RESTRequest);
    FreeAndNil(RESTClient);
  end;
end;

function TCodeProjectFaceRecognition.DetectFacesFromURL(imageUrl: string): string;
var
  HTTPClient: TNetHTTPClient;
  ImageStream: TMemoryStream;
  Response: IHTTPResponse;
begin
  HTTPClient := nil;
  ImageStream := nil;
  try
    HTTPClient := TNetHTTPClient.Create(nil);
    ImageStream := TMemoryStream.Create;
    Response := HTTPClient.Get(imageUrl, ImageStream);
    if Response.StatusCode = 200 then
    begin
      ImageStream.Position := 0;
      Result := DetectFacesFromStream(ImageStream);
    end
    else
    begin
      Result := 'Failed to fetch image from the URL.';
    end;
  finally
    FreeAndNil(ImageStream);
    FreeAndNil(HTTPClient);
  end;
end;

end.
