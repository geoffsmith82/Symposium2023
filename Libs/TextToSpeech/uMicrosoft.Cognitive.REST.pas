unit uMicrosoft.Cognitive.REST;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  Vcl.Controls,
  REST.Client,
  REST.Types,
  uBaseSpeech,
  uMicrosoft.Cognitive.Voices.DTO;

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
    constructor Create(Sender: TWinControl; const AResourceKey: string; const AHost: string);
    function TextToSpeech(text: string; VoiceName: string = ''): TMemoryStream; override;
  end;

implementation

uses
  System.DateUtils;

constructor TMicrosoftCognitiveService.Create(Sender: TWinControl; const AResourceKey: string; const AHost: string);
begin
  inherited Create(Sender, AResourceKey, AHost);
  FOutputFormat := 'audio-24khz-48kbitrate-mono-mp3';
  FExpiryTime := 0;
end;

procedure TMicrosoftCognitiveService.GetAccessToken;
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
begin
  RESTClient := TRESTClient.Create('https://australiaeast.api.cognitive.microsoft.com');
  RESTRequest := TRESTRequest.Create(RESTClient);
  RESTResponse := TRESTResponse.Create(RESTClient);
  try
    RESTRequest.Method := TRESTRequestMethod.rmPOST;
    RESTRequest.Resource := '/sts/v1.0/issueToken';
    RESTRequest.Response := RESTResponse;
    RESTRequest.AddParameter('Ocp-Apim-Subscription-Key', FResourceKey, TRESTRequestParameterKind.pkHTTPHEADER);
    RESTRequest.AddParameter('Content-Type', 'application/x-www-form-urlencoded', TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
    RESTRequest.Execute;
    FAccessToken := RESTResponse.Content;
    FExpiryTime := IncMinute(FExpiryTime, 8);
  finally
    RESTResponse.Free;
    RESTRequest.Free;
    RESTClient.Free;
  end;
end;

function TMicrosoftCognitiveService.GetVoiceList: TMicrosoftCognitiveVoicesClass;
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
  ResultString: string;
begin
  RESTClient := TRESTClient.Create('https://australiaeast.tts.speech.microsoft.com');
  RESTRequest := TRESTRequest.Create(RESTClient);
  RESTResponse := TRESTResponse.Create(RESTClient);
  try
    RESTRequest.Method := TRESTRequestMethod.rmGET;
    RESTRequest.Resource := '/cognitiveservices/voices/list';
    RESTRequest.Response := RESTResponse;
    RESTRequest.AddParameter('Ocp-Apim-Subscription-Key', FResourceKey, TRESTRequestParameterKind.pkHTTPHEADER);
    RESTRequest.Execute;
    ResultString := '{ "items": ' + RESTResponse.Content + '}';
    Result := TMicrosoftCognitiveVoicesClass.FromJsonString(ResultString);
  finally
    RESTResponse.Free;
    RESTRequest.Free;
    RESTClient.Free;
  end;
end;

function TMicrosoftCognitiveService.GetVoices: TObjectList<TVoiceInfo>;
var
  microsoftVoice : TItemClass;
  voice : TVoiceInfo;
  voices : TMicrosoftCognitiveVoicesClass;
begin
  FVoicesInfo.Clear;
  voices := nil;
  try
    voices := GetVoiceList;
    for microsoftVoice in voices.Items do
    begin
      voice := TVoiceInfo.Create;
      voice.VoiceName := microsoftVoice.DisplayName;
      voice.VoiceGender := microsoftVoice.Gender;
      voice.VoiceId := microsoftVoice.Name;
      FVoicesInfo.Add(voice);
    end;
  finally
    FreeAndNil(voices);
    Result := FVoicesInfo;
  end;
end;

function TMicrosoftCognitiveService.TextToSpeech(text: string; VoiceName: string = ''): TMemoryStream;
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
  ssmlText : string;
  param : TRESTRequestParameter;
begin
  if (Now > FExpiryTime) then
    GetAccessToken;

  if VoiceName.IsEmpty then
    VoiceName := 'en-US-ChristopherNeural';

  RESTClient := TRESTClient.Create('https://' + FHost);
  RESTRequest := TRESTRequest.Create(RESTClient);
  RESTResponse := TRESTResponse.Create(RESTClient);
  try
    RESTRequest.Method := TRESTRequestMethod.rmPOST;
    RESTRequest.Resource := '/cognitiveservices/v1';
    RESTRequest.Response := RESTResponse;
    RESTRequest.AddParameter('X-Microsoft-OutputFormat', FOutputFormat, TRESTRequestParameterKind.pkHTTPHEADER);
    RESTRequest.AddParameter('Content-Type', 'application/ssml+xml', TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
    RESTRequest.AddParameter('Authorization', 'Bearer ' + FAccessToken, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
    RESTRequest.AddParameter('User-Agent', FApplicationName, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);

    ssmlText := '<speak version=''1.0'' xml:lang=''en-US''><voice xml:lang=''en-US'' xml:gender=''Male'' ' +
      'name=''' + VoiceName +  '''>' + text + '</voice></speak>';
//    RESTRequest.AddBody(ssmlText);
    param := RESTRequest.Params.AddItem;
    param.Kind := pkREQUESTBODY;
    param.Options := [poDoNotEncode];
    param.Value := ssmlText;
    RESTRequest.Execute;

    Result := TMemoryStream.Create;
    Result.Write(RESTResponse.RawBytes, Length(RESTResponse.RawBytes));
  finally
    RESTResponse.Free;
    RESTRequest.Free;
    RESTClient.Free;
  end;
end;

end.
