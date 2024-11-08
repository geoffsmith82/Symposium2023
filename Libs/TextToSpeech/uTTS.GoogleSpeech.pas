unit uTTS.GoogleSpeech;

interface

uses
  REST.Client,
  REST.Types,
  Vcl.Controls,
  REST.Authenticator.EnhancedOAuth,
  IdHTTPServer,
  IdContext,
  IdHTTPHeaderInfo,
  IdCustomHTTPServer,
  System.Generics.Collections,
  System.Classes,
  System.SysUtils,
  System.Net.URLClient,
  System.NetEncoding,
  System.JSON,
  uTTS,
  uTTS.GoogleSpeech.DTO,
  System.IniFiles
  ;

type
  TGoogleSpeechService = class(TBaseTextToSpeech)
  private
    FOAuth2 : TEnhancedOAuth2Authenticator;
    FHTTPServer : TIdHttpServer;
    FSecretKey : string;
    FSettings : TIniFile;
    procedure IdHTTPServer1CommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
  protected
    function GetVoices: TObjectList<TVoiceInfo>; override;
    function GetVoiceList: TGoogleVoicesListClass;
  public
    constructor Create(const AResourceKey: string; const ASecretKey: string; const AApplicationName: string; const AHost: string; Settings : TIniFile);
    destructor Destroy; override;
    function TextToSpeech(text: string; VoiceName: string = ''): TMemoryStream; override;
    procedure Authenticate;
  end;

implementation

uses
  Winapi.ShellAPI
  ;

{ TGoogleSpeechService }

procedure TGoogleSpeechService.Authenticate;
begin
  FHTTPServer.Active := True;
  ShellExecute(0, 'OPEN', PChar(FOAuth2.AuthorizationRequestURI), nil, nil, 0);
end;

constructor TGoogleSpeechService.Create(const AResourceKey: string; const ASecretKey: string; const AApplicationName: string; const AHost: string; Settings : TIniFile);
begin
  inherited Create(AResourceKey, AHost);
  FSecretKey := ASecretKey;
  FOAuth2 := TEnhancedOAuth2Authenticator.Create(nil);
  FOAuth2.Scope := 'https://www.googleapis.com/auth/cloud-platform';
  FOAuth2.AuthorizationEndpoint := 'https://accounts.google.com/o/oauth2/auth?access_type=offline';
  FOAuth2.AccessTokenEndpoint := 'https://accounts.google.com/o/oauth2/token';
  FOAuth2.RedirectionEndpoint := 'http://localhost:7779/';
  FOAuth2.ClientID := FResourceKey;
  FOAuth2.ClientSecret := FSecretKey;
  FHTTPServer := TIdHttpServer.Create;
  FHTTPServer.DefaultPort := 7779;
  //FHTTPServer.
  FHTTPServer.OnCommandGet := IdHTTPServer1CommandGet;
 // FHTTPServer.On
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

function TGoogleSpeechService.GetVoices: TObjectList<TVoiceInfo>;
var
  LGoogleVoiceList : TGoogleVoicesListClass;
  LGoogleVoice : TGoogleVoiceClass;
  LVoice : TVoiceInfo;
begin
  if FVoicesInfo.count = 0 then
  begin
    LGoogleVoiceList := GetVoiceList;
    try
      for LGoogleVoice in LGoogleVoiceList.voices do
      begin
        LVoice := TVoiceInfo.Create;
        LVoice.VoiceName := LGoogleVoice.name;
        LVoice.VoiceId := LGoogleVoice.name;
        FVoicesInfo.Add(LVoice);
      end;
    finally
      FreeAndNil(LGoogleVoiceList);
    end;
  end;
  Result := FVoicesInfo;
end;

function TGoogleSpeechService.GetVoiceList: TGoogleVoicesListClass;
var
  LRESTClient: TRESTClient;
  LRESTRequest: TRESTRequest;
  LRESTResponse: TRESTResponse;
  LGoogleResponseString : TStringStream;
begin
  LRESTClient := TRESTClient.Create(nil);
  LRESTRequest := TRESTRequest.Create(LRESTClient);
  LRESTResponse := TRESTResponse.Create(LRESTClient);
  try
    LRESTClient.BaseURL := 'https://texttospeech.googleapis.com';
    LRESTRequest.Client := LRESTClient;
    LRESTRequest.Method := rmGET;
    LRESTRequest.Resource := '/v1/voices';
    LRESTRequest.Response := LRESTResponse;
    FOAuth2.RefreshAccessTokenIfRequired;
    LRESTClient.Authenticator := FOAuth2;
    LRESTRequest.AddParameter('Content-Type', 'application/json',
      TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);

    LRESTRequest.Execute;

    LGoogleResponseString := TStringStream.Create;
    try
      LGoogleResponseString.Write(LRESTResponse.RawBytes, Length(LRESTResponse.RawBytes));
      Result := TGoogleVoicesListClass.FromJsonString(LGoogleResponseString.DataString);
    finally
      FreeAndNil(LGoogleResponseString);
    end;
  finally
    FreeAndNil(LRESTRequest);
    FreeAndNil(LRESTResponse);
    FreeAndNil(LRESTClient);
  end;
end;

function TGoogleSpeechService.TextToSpeech(text, VoiceName: string): TMemoryStream;
var
  LRESTClient: TRESTClient;
  LRESTRequest: TRESTRequest;
  LRESTResponse: TRESTResponse;
  LJsonBody : TJSONObject;
  LJsonInput: TJSONObject;
  LJsonVoice: TJSONObject;
  LJsonAudioConfig: TJSONObject;
  LGoogleResponseString : TStringStream;
  LGoogleResponse : TGoogleTextToSpeechResponseClass;
  LAudioBytes : TBytes;
begin
  LRESTClient := nil;
  LRESTRequest := nil;
  LRESTResponse := nil;
  LJsonBody := nil;
  LJsonInput := nil;
  LJsonVoice := nil;
  try
    LRESTClient := TRESTClient.Create(nil);
    LRESTRequest := TRESTRequest.Create(LRESTClient);
    LRESTResponse := TRESTResponse.Create(LRESTClient);
    LRESTClient.BaseURL := 'https://texttospeech.googleapis.com/v1beta1/text:synthesize';
    LRESTRequest.Client := LRESTClient;
    LRESTRequest.Method := rmPOST;
    LRESTRequest.Resource := '';
    LRESTRequest.Response := LRESTResponse;
    LRESTClient.Authenticator := FOAuth2;
    FOAuth2.RefreshAccessTokenIfRequired;
    LRESTRequest.AddParameter('Content-Type', 'application/json',
      TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);

    try
      LJsonInput := TJSONObject.Create;
      LJsonVoice := TJSONObject.Create;
      LJsonAudioConfig := TJSONObject.Create;
      LJsonBody := TJSONObject.Create;
      LJsonBody.AddPair('input', LJsonInput);
      LJsonBody.AddPair('voice', LJsonVoice);
      LJsonBody.AddPair('audioConfig', LJsonAudioConfig);
      LJsonInput.AddPair('text', Text);
      LJsonVoice.AddPair('languageCode', 'en-US');
      LJsonVoice.AddPair('name', 'en-US-Wavenet-C');
      LJsonAudioConfig.AddPair('audioEncoding', 'MP3');
      LRESTRequest.AddBody(LJsonBody.ToJSON, ctAPPLICATION_JSON);
    finally
      FreeAndNil(LJsonBody);
    end;
    LRESTRequest.Execute;
    // Extract the audio data from the response and return it as a TMemoryStream
    Result := TMemoryStream.Create;
    LGoogleResponseString := TStringStream.Create;
    try
      LGoogleResponseString.Write(LRESTResponse.RawBytes, Length(LRESTResponse.RawBytes));
      LGoogleResponse := TGoogleTextToSpeechResponseClass.FromJsonString(LGoogleResponseString.DataString);
      LAudioBytes := TBase64Encoding.Base64.Decode(TEncoding.UTF8.GetBytes(LGoogleResponse.audioContent));
    finally
      FreeAndNil(LGoogleResponseString);
      FreeAndNil(LGoogleResponse);
    end;
    Result.Write(LAudioBytes, length(LAudioBytes));
  finally
    FreeAndNil(LRESTRequest);
    FreeAndNil(LRESTResponse);
    FreeAndNil(LRESTClient);
  end;
end;

end.
