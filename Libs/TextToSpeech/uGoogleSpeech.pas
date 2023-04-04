unit uGoogleSpeech;

interface

uses
  REST.Client,
  REST.Types,
  REST.Authenticator.EnhancedOAuth,
  IdHTTPServer,
  IdContext,
  IdHTTPHeaderInfo,
  IdCustomHTTPServer,
  System.Classes,
  System.SysUtils,
  System.Net.URLClient,
  System.NetEncoding,
  System.JSON,
  uBaseSpeech,
  uGoogleSpeech.DTO,
  System.IniFiles
  ;

type
  TGoogleSpeechService = class(TBaseSpeech)
  private
    FOAuth2 : TEnhancedOAuth2Authenticator;
    FHTTPServer : TIdHttpServer;
    FSettings : TIniFile;
    procedure IdHTTPServer1CommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
  public
    constructor Create(const AResourceKey: string; const AApplicationName: string; const AHost: string; Settings : TIniFile);
    destructor Destroy; override;
    function TextToSpeech(text: string; VoiceName: string = ''): TMemoryStream; override;
    function SpeechEngineName: string; override;
    function GetVoiceList: TGoogleVoicesListClass;
    procedure Authenticate;
  end;

implementation

uses
  Winapi.ShellAPI
  ;

{$I ..\apikey.inc}

{ TGoogleSpeechService }

procedure TGoogleSpeechService.Authenticate;
begin
  ShellExecute(0, 'OPEN', PChar(FOAuth2.AuthorizationRequestURI), nil,nil,0);
end;

constructor TGoogleSpeechService.Create(const AResourceKey: string; const AApplicationName: string; const AHost: string; Settings : TIniFile);
begin
  inherited Create(AResourceKey, AApplicationName, AHost);
  FOAuth2 := TEnhancedOAuth2Authenticator.Create(nil);
  FOAuth2.Scope := 'https://www.googleapis.com/auth/cloud-platform';
  FOAuth2.AuthorizationEndpoint := 'https://accounts.google.com/o/oauth2/auth?access_type=offline';
  FOAuth2.AccessTokenEndpoint := 'https://accounts.google.com/o/oauth2/token';
  FOAuth2.RedirectionEndpoint := 'http://localhost:7777/';
  FOAuth2.ClientID := google_clientid;
  FOAuth2.ClientSecret := google_clientsecret;
  FHTTPServer := TIdHttpServer.Create;
  FHTTPServer.DefaultPort := 7777;
  FHTTPServer.OnCommandGet := IdHTTPServer1CommandGet;
  FHTTPServer.Active := True;
  FSettings := Settings;
  FOAuth2.RefreshToken := FSettings.ReadString('GoogleAuthentication', 'RefreshToken', '');
end;

procedure TGoogleSpeechService.IdHTTPServer1CommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
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


destructor TGoogleSpeechService.Destroy;
begin
  FreeAndNil(FOAuth2);
  FreeAndNil(FHTTPServer);
  inherited;
end;

function TGoogleSpeechService.GetVoiceList: TGoogleVoicesListClass;
var
  RESTClient1: TRESTClient;
  RESTRequest1: TRESTRequest;
  RESTResponse1: TRESTResponse;
  googleResponseString : TStringStream;
begin
  RESTClient1 := TRESTClient.Create(nil);
  RESTRequest1 := TRESTRequest.Create(RESTClient1);
  RESTResponse1 := TRESTResponse.Create(RESTClient1);
  try
    RESTClient1.BaseURL := 'https://texttospeech.googleapis.com/v1/voices';
    RESTRequest1.Client := RESTClient1;
    RESTRequest1.Method := rmGET;
    RESTRequest1.Resource := '';
    RESTRequest1.Response := RESTResponse1;
    FOAuth2.RefreshAccessTokenIfRequired;
    RESTClient1.Authenticator := FOAuth2;
    RESTRequest1.AddParameter('Content-Type', 'application/json',
      TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);

    RESTRequest1.Execute;

    googleResponseString := TStringStream.Create;
    try
      googleResponseString.Write(RESTResponse1.RawBytes, Length(RESTResponse1.RawBytes));
      Result := TGoogleVoicesListClass.FromJsonString(googleResponseString.DataString);
    finally
      FreeAndNil(googleResponseString);
    end;
  finally
    FreeAndNil(RESTRequest1);
    FreeAndNil(RESTResponse1);
    FreeAndNil(RESTClient1);
  end;

end;

function TGoogleSpeechService.SpeechEngineName: string;
begin
  Result := 'GoogleVoice';
end;

function TGoogleSpeechService.TextToSpeech(text, VoiceName: string): TMemoryStream;
var
  RESTClient1: TRESTClient;
  RESTRequest1: TRESTRequest;
  RESTResponse1: TRESTResponse;
  JsonBody : TJSONObject;
  JsonInput: TJSONObject;
  JsonVoice: TJSONObject;
  JsonAudioConfig: TJSONObject;
  googleResponseString : TStringStream;
  googleResponse : TGoogleTextToSpeechResponseClass;
  audioBytes : TBytes;
begin
  RESTClient1 := TRESTClient.Create(nil);
  RESTRequest1 := TRESTRequest.Create(RESTClient1);
  RESTResponse1 := TRESTResponse.Create(RESTClient1);
  try
    RESTClient1.BaseURL := 'https://texttospeech.googleapis.com/v1beta1/text:synthesize';
    RESTRequest1.Client := RESTClient1;
    RESTRequest1.Method := rmPOST;
    RESTRequest1.Resource := '';
    RESTRequest1.Response := RESTResponse1;
    RESTClient1.Authenticator := FOAuth2;
    FOAuth2.RefreshAccessTokenIfRequired;
    RESTRequest1.AddParameter('Content-Type', 'application/json',
      TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);

    JsonBody := nil;
    JsonInput := nil;
    JsonVoice := nil;
    try
      JsonInput := TJSONObject.Create;
      JsonVoice := TJSONObject.Create;
      JsonAudioConfig := TJSONObject.Create;
      JsonBody := TJSONObject.Create;
      JsonBody.AddPair('input', JsonInput);
      JsonBody.AddPair('voice', JsonVoice);
      JsonBody.AddPair('audioConfig', JsonAudioConfig);
      JsonInput.AddPair('text', Text);
      JsonVoice.AddPair('languageCode', 'en-US');
      JsonVoice.AddPair('name', 'en-US-Wavenet-C');
      JsonAudioConfig.AddPair('audioEncoding', 'MP3');
      RESTRequest1.AddBody(JsonBody.ToJSON, ctAPPLICATION_JSON);
    finally
      FreeAndNil(JsonBody);
    end;
    RESTRequest1.Execute;
    // Extract the audio data from the response and return it as a TMemoryStream
    Result := TMemoryStream.Create;
    googleResponseString := TStringStream.Create;
    try
      googleResponseString.Write(RESTResponse1.RawBytes, Length(RESTResponse1.RawBytes));
      googleResponse := TGoogleTextToSpeechResponseClass.FromJsonString(googleResponseString.DataString);
      audioBytes := TBase64Encoding.Base64.Decode(TEncoding.UTF8.GetBytes(googleResponse.audioContent));
    finally
      FreeAndNil(googleResponseString);
      FreeAndNil(googleResponse);
    end;
    Result.Write(audioBytes, length(audiobytes));
  finally
    RESTRequest1.Free;
    RESTResponse1.Free;
    RESTClient1.Free;
  end;
end;

end.
