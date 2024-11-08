unit uTTS.Microsoft.Cognitive;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  Vcl.Controls,
  REST.Client,
  REST.Types,
  uTTS,
  uTTS.Microsoft.Cognitive.Voices.DTO;

type
  TMicrosoftCognitiveService = class(TBaseTextToSpeech)
  private
    FAccessToken: string;
    FExpiryTime: TDateTime;
    FOutputFormat: string;
    function GetVoiceList: TMicrosoftCognitiveVoicesClass;
  strict protected
    function GetVoices: TObjectList<TVoiceInfo>; override;
  public
    procedure GetAccessToken;
    constructor Create(const AResourceKey: string; const AHost: string);
    function TextToSpeech(text: string; VoiceName: string = ''): TMemoryStream; override;
  end;

implementation

uses
  System.DateUtils;

constructor TMicrosoftCognitiveService.Create(const AResourceKey: string; const AHost: string);
begin
  inherited Create(AResourceKey, AHost);
  FOutputFormat := 'audio-24khz-48kbitrate-mono-mp3';
  FExpiryTime := 0;
end;

procedure TMicrosoftCognitiveService.GetAccessToken;
var
  LRESTClient: TRESTClient;
  LRESTRequest: TRESTRequest;
  LRESTResponse: TRESTResponse;
begin
  LRESTClient := TRESTClient.Create('https://australiaeast.api.cognitive.microsoft.com');
  LRESTRequest := TRESTRequest.Create(LRESTClient);
  LRESTResponse := TRESTResponse.Create(LRESTClient);
  try
    LRESTRequest.Method := TRESTRequestMethod.rmPOST;
    LRESTRequest.Resource := '/sts/v1.0/issueToken';
    LRESTRequest.Response := LRESTResponse;
    LRESTRequest.AddParameter('Ocp-Apim-Subscription-Key', FResourceKey, TRESTRequestParameterKind.pkHTTPHEADER);
    LRESTRequest.AddParameter('Content-Type', 'application/x-www-form-urlencoded', TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
    LRESTRequest.Execute;
    FAccessToken := LRESTResponse.Content;
    FExpiryTime := IncMinute(FExpiryTime, 8);
  finally
    LRESTResponse.Free;
    LRESTRequest.Free;
    LRESTClient.Free;
  end;
end;

function TMicrosoftCognitiveService.GetVoiceList: TMicrosoftCognitiveVoicesClass;
var
  LRESTClient: TRESTClient;
  LRESTRequest: TRESTRequest;
  LRESTResponse: TRESTResponse;
  LResultString: string;
begin
  LRESTClient := TRESTClient.Create('https://australiaeast.tts.speech.microsoft.com');
  LRESTRequest := TRESTRequest.Create(LRESTClient);
  LRESTResponse := TRESTResponse.Create(LRESTClient);
  try
    LRESTRequest.Method := TRESTRequestMethod.rmGET;
    LRESTRequest.Resource := '/cognitiveservices/voices/list';
    LRESTRequest.Response := LRESTResponse;
    LRESTRequest.AddParameter('Ocp-Apim-Subscription-Key', FResourceKey, TRESTRequestParameterKind.pkHTTPHEADER);
    LRESTRequest.Execute;
    LResultString := '{ "items": ' + LRESTResponse.Content + '}';
    Result := TMicrosoftCognitiveVoicesClass.FromJsonString(LResultString);
  finally
    LRESTResponse.Free;
    LRESTRequest.Free;
    LRESTClient.Free;
  end;
end;

function TMicrosoftCognitiveService.GetVoices: TObjectList<TVoiceInfo>;
var
  LMicrosoftVoice : TItemClass;
  LVoice : TVoiceInfo;
  LVoices : TMicrosoftCognitiveVoicesClass;
begin
  if FVoicesInfo.Count = 0 then
  begin
    LVoices := nil;
    try
      LVoices := GetVoiceList;
      for LMicrosoftVoice in LVoices.Items do
      begin
        LVoice := TVoiceInfo.Create;
        LVoice.VoiceName := LMicrosoftVoice.DisplayName;
        LVoice.VoiceGender := LMicrosoftVoice.Gender;
        LVoice.VoiceId := LMicrosoftVoice.ShortName;
        FVoicesInfo.Add(LVoice);
      end;
    finally
      FreeAndNil(LVoices);
    end;
  end;
  Result := FVoicesInfo;
end;

function TMicrosoftCognitiveService.TextToSpeech(text: string; VoiceName: string = ''): TMemoryStream;
var
  LRESTClient: TRESTClient;
  LRESTRequest: TRESTRequest;
  LRESTResponse: TRESTResponse;
  LssmlText : string;
  LParam : TRESTRequestParameter;
begin
  if (Now > FExpiryTime) then
    GetAccessToken;

  if VoiceName.IsEmpty then
    VoiceName := 'en-US-ChristopherNeural';

  LRESTClient := TRESTClient.Create('https://' + FHost);
  LRESTRequest := TRESTRequest.Create(LRESTClient);
  LRESTResponse := TRESTResponse.Create(LRESTClient);
  try
    LRESTRequest.Method := TRESTRequestMethod.rmPOST;
    LRESTRequest.Resource := '/cognitiveservices/v1';
    LRESTRequest.Response := LRESTResponse;
    LRESTRequest.AddParameter('X-Microsoft-OutputFormat', FOutputFormat, TRESTRequestParameterKind.pkHTTPHEADER);
    LRESTRequest.AddParameter('Content-Type', 'application/ssml+xml', TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
    LRESTRequest.AddParameter('Authorization', 'Bearer ' + FAccessToken, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
    LRESTRequest.AddParameter('User-Agent', FApplicationName, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);

    LssmlText := '<speak version=''1.0'' xml:lang=''en-US''><voice xml:lang=''en-US'' xml:gender=''Male'' ' +
      'name=''' + VoiceName +  '''>' + text + '</voice></speak>';
//    RESTRequest.AddBody(ssmlText);
    LParam := LRESTRequest.Params.AddItem;
    LParam.Kind := pkREQUESTBODY;
    LParam.Options := [poDoNotEncode];
    LParam.Value := LssmlText;
    LRESTRequest.Execute;

    Result := TMemoryStream.Create;
    Result.Write(LRESTResponse.RawBytes, Length(LRESTResponse.RawBytes));
  finally
    LRESTResponse.Free;
    LRESTRequest.Free;
    LRESTClient.Free;
  end;
end;

end.
