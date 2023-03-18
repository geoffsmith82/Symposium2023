unit uGoogle.SpeechToText;

interface

uses uBaseSpeechToText,
  System.Classes,
  System.SysUtils,
  System.JSON,
  REST.Client,
  REST.Types,
  System.NetEncoding;

type
  TGoogleSpeechToText = class(TBaseSpeechToText)
  strict private
    FAccessToken : string;
    function Base64EncodedFile(filename: string): string;
    function CreateRequestJSON(const FilePath, ModelName: string): TJSONObject;
  public
    function TranscribeAudio(const FilePath, ModelName: string): string; override;

  end;

implementation

{ TGoogleSpeechToText }

function TGoogleSpeechToText.Base64EncodedFile(filename:string):string;
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

function TGoogleSpeechToText.CreateRequestJSON(const FilePath, ModelName: string): TJSONObject;
var
  ConfigObj, AudioObj: TJSONObject;
  ConfigPair, EncodingPair, SampleRatePair, LanguageCodePair, ModelPair, AudioPair, ContentPair: TJSONPair;
begin
  // Create the JSON objects and pairs
  ConfigObj := TJSONObject.Create;
  EncodingPair := TJSONPair.Create('encoding', 'FLAC');
  SampleRatePair := TJSONPair.Create('sampleRateHertz', TJSONNumber.Create(16000));
  LanguageCodePair := TJSONPair.Create('languageCode', 'en-US');
  ModelPair := TJSONPair.Create('model', ModelName);
  ConfigObj.AddPair(EncodingPair);
  ConfigObj.AddPair(SampleRatePair);
  ConfigObj.AddPair(LanguageCodePair);
  ConfigObj.AddPair(ModelPair);

  AudioObj := TJSONObject.Create;
  ContentPair := TJSONPair.Create('content', Base64EncodedFile(FilePath));
  AudioObj.AddPair(ContentPair);

  Result := TJSONObject.Create;
  ConfigPair := TJSONPair.Create('config', ConfigObj);
  AudioPair := TJSONPair.Create('audio', AudioObj);
  Result.AddPair(ConfigPair);
  Result.AddPair(AudioPair);
end;

function TGoogleSpeechToText.TranscribeAudio(const FilePath, ModelName: string): string;
var
  RestClient: TRESTClient;
  Request: TRESTRequest;
  Response: TRESTResponse;
  jsonBody: TJSONObject;
//  AccessToken: string;
begin
  // 1. Get authentication credentials
 // AccessToken := 'YOUR_ACCESS_TOKEN';

  // 2. Install a REST client library
  RestClient := TRESTClient.Create('https://speech.googleapis.com');

  // 3. Prepare the audio file
  // ...

  // 4. Send a POST request to the Speech-to-Text API
  Request := TRESTRequest.Create(RestClient);
  Request.Resource := '/v1/speech:recognize';
  Request.Method := rmPOST;
 // Request.Params.AddItem('key', 'YOUR_API_KEY');
  Request.Params.AddItem('access_token', FAccessToken);
  Request.Params.AddItem('Content-Type', 'application/json', TRESTRequestParameterKind.pkHTTPHEADER);

  jsonBody := CreateRequestJSON(FilePath, ModelName);
  try
    Request.AddBody(jsonBody);
    //AddBody('{"config": {"encoding": "FLAC", "sampleRateHertz": 16000, "languageCode": "en-US", "model": "' + ModelName + '"}, "audio": {"content": "' + Base64EncodedFile(FilePath) + '"}}', TRESTContentType.ctAPPLICATION_JSON);
  finally
    FreeAndNil(jsonBody);
  end;
  Response := TRESTResponse.Create(Request);
  try
    Request.Execute;
    Result := Response.Content;
  finally
    Response.Free;
    Request.Free;
    RestClient.Free;
  end;

  // 5. Parse the response
  // ...
end;


end.
