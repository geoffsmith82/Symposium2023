unit uGoogle.FaceRecognition;

interface

uses
  System.Classes,
  System.SysUtils,
  System.JSON,
  System.NetEncoding,
  System.Net.URLClient,
  System.IniFiles,
  REST.Client,
  REST.Types,
  uBaseFaceRecognition,
  REST.Authenticator.EnhancedOAuth,
  IdHTTPServer,
  IdCustomHTTPServer,
  IdHTTPHeaderInfo,
  Winapi.ShellAPI,
  IdContext
  ;

type
  TGoogleFaceRecognition = class(TBaseFaceRecognition)
  strict private
    function Base64EncodedFile(const filename:string): string;
    function Base64EncodedStream(stream: TStream): string;
  strict private
    FOAuth2 : TEnhancedOAuth2Authenticator;
    FHTTPServer : TIdHttpServer;
    FSettings : TIniFile;
    FSecretKey : string;
    procedure IdHTTPServer1CommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
  private
    function GetFaceDetectionRequestBody(const ImageUrl: string): TJSONObject;
  public
    function DetectFacesFromURL(imageUrl: string): string; override;
    function DetectFacesFromStream(imageStream: TStream): string; override;
    function DetectFacesFromFile(imageFilename: string): string; override;
    procedure Authenticate;
    constructor Create(const AClientID, AClientSecret: string; AHost: string; ASettings : TIniFile);
  end;

implementation



{ TGoogleFaceRecognition }

constructor TGoogleFaceRecognition.Create(const AClientID, AClientSecret: string; AHost: string; ASettings : TIniFile);
begin
  inherited Create(AClientID, AHost);
  FSecretKey := AClientSecret;
  FSettings := ASettings;
  FOAuth2 := TEnhancedOAuth2Authenticator.Create(nil);
  FOAuth2.Scope := 'https://www.googleapis.com/auth/cloud-platform';
  FOAuth2.AuthorizationEndpoint := 'https://accounts.google.com/o/oauth2/auth?access_type=offline';
  FOAuth2.AccessTokenEndpoint := 'https://accounts.google.com/o/oauth2/token';
  FOAuth2.RedirectionEndpoint := 'http://localhost:7777/';
  FOAuth2.ClientID := FResourceKey;
  FOAuth2.ClientSecret := FSecretKey;
  FHTTPServer := TIdHttpServer.Create;
  FHTTPServer.DefaultPort := 7777;
  FHTTPServer.OnCommandGet := IdHTTPServer1CommandGet;
end;

procedure TGoogleFaceRecognition.IdHTTPServer1CommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  LCode: string;
  LURL : TURI;
begin
  if ARequestInfo.QueryParams = '' then
    Exit;
  LURL := TURI.Create('https://localhost/?' + ARequestInfo.QueryParams);
  try
    LCode := LURL.ParameterByName['code'];
  except
    Exit;
  end;
  FOAuth2.AuthCode := LCode;
  FOAuth2.ChangeAuthCodeToAccesToken;

  FSettings.WriteString('GoogleAuthentication', 'RefreshToken', FOAuth2.RefreshToken);
end;

procedure TGoogleFaceRecognition.Authenticate;
begin
  FHTTPServer.Active := True;
  ShellExecute(0, 'OPEN', PChar(FOAuth2.AuthorizationRequestURI), nil, nil, 0);
end;

function TGoogleFaceRecognition.Base64EncodedFile(const filename:string):string;
var
  fs : TFileStream;
  mem : TStringStream;
begin
  fs := nil;
  mem := nil;
  Result := '';
  try
    fs := TFileStream.Create(filename, fmOpenRead);
    mem := TStringStream.Create;
    if TNetEncoding.Base64.Encode(fs, mem) > 0 then
    begin
      Result := mem.DataString;
    end;
  finally
    FreeAndNil(fs);
    FreeAndNil(mem)
  end;
end;

function TGoogleFaceRecognition.Base64EncodedStream(stream: TStream):string;
var
  mem : TStringStream;
begin
  mem := nil;
  Result := '';
  try
    mem := TStringStream.Create;
    if TNetEncoding.Base64.Encode(stream, mem) > 0 then
    begin
      Result := mem.DataString;
    end;
  finally
    FreeAndNil(mem);
  end;
end;

function TGoogleFaceRecognition.DetectFacesFromFile(imageFilename: string): string;
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

function TGoogleFaceRecognition.DetectFacesFromStream(imageStream: TStream): string;
var
  Client: TRESTClient;
  Request: TRESTRequest;
  Response: TRESTResponse;
  Image: TJSONObject;
  Features: TJSONArray;
  RequestBody: TJSONObject;
begin
  Client := nil;
  Request := nil;
  Response := nil;
  Image := nil;
  Features := nil;
  RequestBody := nil;
  try
    // Create the REST components
    Client := TRESTClient.Create(nil);
    Request := TRESTRequest.Create(nil);
    Response := TRESTResponse.Create(nil);
    // Configure the REST components
    Client.BaseURL := 'https://vision.googleapis.com/v1/images:annotate';
    Request.Client := Client;
    Request.Response := Response;
    Request.Method := rmPOST;
    Request.Resource := '';

    // Define the input data for the face recognition request
    Image := TJSONObject.Create;
    Image.AddPair('content', Base64EncodedStream(imageStream));

    Features := TJSONArray.Create;
    Features.Add(TJSONObject.Create.AddPair('type', 'FACE_DETECTION'));

    RequestBody := TJSONObject.Create;
    RequestBody.AddPair('image', Image);
    RequestBody.AddPair('features', Features);

    // Send the face recognition request
    Request.AddParameter('Content-Type', 'application/json');
    Request.AddBody(RequestBody);
    Request.Execute;

    // Handle the response from the Vision API
    Result := (TJSONObject.ParseJSONValue(Response.Content) as TJSONArray).ToJSON;
  finally
    // Clean up the REST components
    FreeAndNil(RequestBody);
    FreeAndNil(Features);
    FreeAndNil(Image);
    FreeAndNil(Response);
    FreeAndNil(Request);
    FreeAndNil(Client);
  end;
end;

function TGoogleFaceRecognition.GetFaceDetectionRequestBody(const ImageUrl: string): TJSONObject;
var
  RequestObject: TJSONObject;
  ImageObject: TJSONObject;
  SourceObject: TJSONObject;
  FeaturesObject: TJSONObject;
begin
  // Create the JSON objects for the request body
  RequestObject := TJSONObject.Create;
  ImageObject := TJSONObject.Create;
  SourceObject := TJSONObject.Create;
  FeaturesObject := TJSONObject.Create;

  try
    // Build the request body as a JSON object
    SourceObject.AddPair('imageUri', ImageUrl);
    ImageObject.AddPair('source', SourceObject);
    FeaturesObject.AddPair('type', 'FACE_DETECTION');
    FeaturesObject.AddPair('maxResults', '10');
    RequestObject.AddPair('image', ImageObject);
    RequestObject.AddPair('features', TJSONArray.Create(FeaturesObject));

    // Return the JSON object for the request body
    Result := RequestObject;
  except
    RequestObject.Free;
    ImageObject.Free;
    SourceObject.Free;
    FeaturesObject.Free;
    raise;
  end;
end;


function TGoogleFaceRecognition.DetectFacesFromURL(imageUrl: string): string;
var
  RestClient: TRESTClient;
  Request: TRESTRequest;
  Response: TRESTResponse;
  RequestBody: TJSONObject;
begin
  RestClient := TRESTClient.Create(nil);
  Request := TRESTRequest.Create(nil);
  Response := TRESTResponse.Create(nil);

  try
    RequestBody := GetFaceDetectionRequestBody(imageUrl);
    // Set up the REST components
    RestClient.BaseURL := 'https://vision.googleapis.com/v1/images:annotate';
    RestClient.Authenticator := FOAuth2;
    Request.Client := RestClient;
    Request.Response := Response;
    Request.Method := rmPOST;

    // Set up the request body with the image URL
    Request.AddBody(RequestBody.ToJSON, TRESTContentType.ctAPPLICATION_JSON);

    // Send the request and handle the response
    Request.Execute;
    if Response.StatusCode = 200 then
      Result := Response.Content
    else
      Result := 'Error: ' + Response.StatusText;
  finally
    FreeAndNil(RestClient);
    FreeAndNil(Request);
    FreeAndNil(Response);
    FreeAndNil(RequestBody);
  end;
end;

end.
