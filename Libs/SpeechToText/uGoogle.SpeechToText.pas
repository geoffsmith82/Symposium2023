unit uGoogle.SpeechToText;

interface

uses uBaseSpeechToText,
  System.Classes,
  System.SysUtils,
  System.JSON,
  System.IniFiles,
  System.IOUtils,
  System.Net.URLClient,
  Winapi.ShellAPI,
  REST.Client,
  REST.Types,
  System.NetEncoding,
  REST.Authenticator.EnhancedOAuth,
  IdHTTPServer,
  IdCustomHTTPServer,
  IdHTTPHeaderInfo,
  IdContext,
  uGoogle.SpeechToText.DTO
  ;

type
  TGoogleSpeechToText = class(TBaseSpeechToText)
  strict private
    FOAuth2 : TEnhancedOAuth2Authenticator;
    FHTTPServer : TIdHttpServer;
    FSettings : TIniFile;
    FSecretKey : string;
    procedure IdHTTPServer1CommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    function Base64EncodedFile(const filename:string): string;
    function CreateRequestJSON(const FilePath, ModelName: string): TJSONObject;
  public
    function SupportedFormats: TArray<string>; override;
    function TranscribeAudio(const FilePath, ModelName: string): string; override;
    procedure Authenticate;
    constructor Create(const AResourceKey: string; const ASecretKey: string; const AApplicationName: string; AHost: string; Settings: TIniFile);
    destructor Destroy; override;
  end;

implementation

{ TGoogleSpeechToText }

constructor TGoogleSpeechToText.Create(const AResourceKey: string; const ASecretKey: string;const AApplicationName: string; AHost: string; Settings: TIniFile);
begin
  inherited Create(AResourceKey, AApplicationName, AHost);
  FSecretKey := ASecretKey;
  FOAuth2 := TEnhancedOAuth2Authenticator.Create(nil);
  FOAuth2.Scope := 'https://www.googleapis.com/auth/cloud-platform';
  FOAuth2.AuthorizationEndpoint := 'https://accounts.google.com/o/oauth2/auth?access_type=offline';
  FOAuth2.AccessTokenEndpoint := 'https://accounts.google.com/o/oauth2/token';
  FOAuth2.RedirectionEndpoint := 'http://localhost:7777/';
  FOAuth2.ClientID := FResourceKey;
  FOAuth2.ClientSecret := FSecretKey;
  FSettings := Settings;
  FOAuth2.RefreshToken := FSettings.ReadString('GoogleAuthentication', 'RefreshToken', '');
  FHTTPServer := TIdHttpServer.Create;
  FHTTPServer.DefaultPort := 7777;
  FHTTPServer.OnCommandGet := IdHTTPServer1CommandGet;
end;

procedure TGoogleSpeechToText.IdHTTPServer1CommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
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

procedure TGoogleSpeechToText.Authenticate;
begin
  FHTTPServer.Active := True;
  ShellExecute(0, 'OPEN', PChar(FOAuth2.AuthorizationRequestURI), nil, nil, 0);
end;


function TGoogleSpeechToText.Base64EncodedFile(const filename:string): string;
var
  Lfs : TFileStream;
  LMem : TStringStream;
begin
  Lfs := nil;
  LMem := nil;
  Result := '';
  try
    Lfs := TFileStream.Create(filename, fmOpenRead);
    LMem := TStringStream.Create;
    if TNetEncoding.Base64String.Encode(Lfs, LMem) > 0 then
    begin
      Result := LMem.DataString;
    end;
  finally
    FreeAndNil(Lfs);
    FreeAndNil(LMem);
  end;
end;

function TGoogleSpeechToText.SupportedFormats(): TArray<string>;
begin
  SetLength(Result, 5);
  Result[0] := 'flac'; {TODO: Add rest of supported types}
  Result[1] := 'mp3';
  Result[2] := 'ogg';
  Result[3] := 'wav';
  Result[4] := 'spx';
end;

function TGoogleSpeechToText.CreateRequestJSON(const FilePath, ModelName: string): TJSONObject;
var
  LConfigObj, LAudioObj: TJSONObject;
  LConfigPair, LEncodingPair, LSampleRatePair, LLanguageCodePair, LModelPair, LAudioPair, LContentPair: TJSONPair;
begin
  // Create the JSON objects and pairs
  LConfigObj := TJSONObject.Create;
  LEncodingPair := TJSONPair.Create('encoding', 'FLAC');
 // LSampleRatePair := TJSONPair.Create('sampleRateHertz', TJSONNumber.Create(22050));
  LLanguageCodePair := TJSONPair.Create('languageCode', 'en-US');
  LModelPair := TJSONPair.Create('model', 'default');// ModelName);
  LConfigObj.AddPair(LEncodingPair);
//  ConfigObj.AddPair(LSampleRatePair);
  LConfigObj.AddPair(LLanguageCodePair);
  LConfigObj.AddPair(LModelPair);

  LAudioObj := TJSONObject.Create;
  LContentPair := TJSONPair.Create('content', Base64EncodedFile(FilePath));
  LAudioObj.AddPair(LContentPair);

  Result := TJSONObject.Create;
  LConfigPair := TJSONPair.Create('config', LConfigObj);
  LAudioPair := TJSONPair.Create('audio', LAudioObj);
  Result.AddPair(LConfigPair);
  Result.AddPair(LAudioPair);
end;

destructor TGoogleSpeechToText.Destroy;
begin
  FreeAndNil(FOAuth2);
  FHTTPServer.Active := False;
  FreeAndNil(FHTTPServer);
  inherited;
end;

function TGoogleSpeechToText.TranscribeAudio(const FilePath, ModelName: string): string;
var
  LRestClient: TRESTClient;
  LRequest: TRESTRequest;
  LResponse: TRESTResponse;
  LJsonBody: TJSONObject;
  LGoogleResults : TTGoogleSpeechToTextResultsClass;
begin
  if not IsFileSupported(FilePath) then
  begin
    raise Exception.Create('Unsupported file format');
  end;


  // 1. Get authentication credentials
 // AccessToken := 'YOUR_ACCESS_TOKEN';
  // 2. Install a REST client library
  LRestClient := TRESTClient.Create('https://speech.googleapis.com');
  // 3. Prepare the audio file
  // ...
  // 4. Send a POST request to the Speech-to-Text API
  LRequest := TRESTRequest.Create(LRestClient);
  LRequest.Resource := '/v1/speech:recognize';
  FOAuth2.RefreshAccessTokenIfRequired;
  LRestClient.Authenticator := FOAuth2;

  LRequest.Method := rmPOST;
  LRequest.Params.AddItem('Content-Type', 'application/json', TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
  LJsonBody := CreateRequestJSON(FilePath, ModelName);
  try
    LRequest.AddBody(LJsonBody);
    //AddBody('{"config": {"encoding": "FLAC", "sampleRateHertz": 16000, "languageCode": "en-US", "model": "' + ModelName + '"}, "audio": {"content": "' + Base64EncodedFile(FilePath) + '"}}', TRESTContentType.ctAPPLICATION_JSON);
  finally
    FreeAndNil(LJsonBody);
  end;
  LResponse := TRESTResponse.Create(LRequest);
  LRequest.Response := LResponse;
  try
    LRequest.Execute;
    LGoogleResults := TTGoogleSpeechToTextResultsClass.FromJsonString(LResponse.Content);
    Result := LGoogleResults.results[0].alternatives[0].transcript;
  finally
    LResponse.Free;
    LRequest.Free;
    LRestClient.Free;
  end;
  // 5. Parse the response
  // ...
end;


end.
