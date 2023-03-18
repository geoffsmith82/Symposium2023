unit uGoogle.FaceRecognition;

interface

uses
  System.Classes,
  System.SysUtils,
  System.JSON,
  System.NetEncoding,
  REST.Client,
  REST.Types,
  uBaseFaceRecognition
  ;

type
  TGoogleFaceRecognition = class(TBaseFaceRecognition)
  private
    function Base64EncodedFile(filename: string): string;
    function Base64EncodedStream(stream: TStream): string;

  public
    function DetectFacesFromURL(imageUrl: string): string; override;
    function DetectFacesFromStream(imageStream: TStream): string; override;
    function DetectFacesFromFile(imageFilename: string): string; override;
  end;

implementation

{ TGoogleFaceRecognition }

function TGoogleFaceRecognition.Base64EncodedFile(filename:string):string;
var
  fs : TFileStream;
  mem : TStringStream;
begin
  fs := nil;
  mem := nil;
  try
    fs := TFileStream.Create(filename, fmOpenRead);
    mem := TStringStream.Create;
    if TNetEncoding.Base64.Encode(fs, mem) > 0 then
    begin
      Result := mem.DataString;
    end;
  finally
    FreeAndNil(fs);
  end;
end;

function TGoogleFaceRecognition.Base64EncodedStream(stream:TStream):string;
var
  mem : TStringStream;
begin
  mem := nil;
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
  //  Request.AddParameter('key', APIKey);

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


function TGoogleFaceRecognition.DetectFacesFromURL(imageUrl: string): string;
begin

end;

end.
