unit uMicrosoft.Cognitive.REST;

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Controls,
  REST.Client,
  REST.Types,
  uBaseSpeech,
  uMicrosoft.Cognitive.Voices.DTO;

type
  TMicrosoftCognitiveService = class(TBaseSpeech)
  private
    FAccessToken: string;
    FExpiryTime: TDateTime;
    FOutputFormat: string;
  public
    procedure GetAccessToken;
    constructor Create(Sender: TWinControl; const AResourceKey: string; const AApplicationName:string; const AHost: string);
    function TextToSpeech(text: string; VoiceName: string = ''): TMemoryStream; override;
    function GetVoiceList: TMicrosoftCognitiveVoicesClass;
    function SpeechEngineName: string; override;
  end;

implementation

uses
  System.DateUtils;

constructor TMicrosoftCognitiveService.Create(Sender: TWinControl; const AResourceKey: string; const AApplicationName:string; const AHost: string);
begin
  inherited Create(Sender, AResourceKey, AApplicationName, AHost);
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
    RESTRequest.AddParameter('Ocp-Apim-Subscription-Key', FResourceKey, TRESTRequestParameterKind.pkHTTPHEADER);
    RESTRequest.Execute;
    ResultString := RESTResponse.Content;
    Result := TMicrosoftCognitiveVoicesClass.FromJsonString(ResultString);
  finally
    RESTResponse.Free;
    RESTRequest.Free;
    RESTClient.Free;
  end;
end;

function TMicrosoftCognitiveService.SpeechEngineName: string;
begin
  Result := 'MicrosoftCognitive';
end;

function TMicrosoftCognitiveService.TextToSpeech(text: string; VoiceName: string = ''): TMemoryStream;
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
  ssmlText : string;
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
    RESTRequest.AddBody(ssmlText);
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
