unit uTTS.Amazon.Polly;

interface

uses
  System.Classes,
  System.SysUtils,
  System.JSON,
  System.Generics.Collections,
  System.Net.URLClient,
  System.Net.HttpClient,
  System.NetEncoding,
  System.Hash,
  Vcl.Controls,
  Data.Cloud.AmazonAPI,
  Data.Cloud.CloudAPI,
  AWS.Polly,
  AWS.Runtime.ClientConfig,
  AWS.Runtime.Credentials,
  AWS.RegionEndpoints,
  AWS.RegionEndpoint,
  uTTS
  ;

type
  TAmazonPollyService = class(TBaseTextToSpeech)
  private
    FAccessKey : string;
    FSecretKey: string;
    FRegion: IRegionEndpointEx;
  protected
    function GetVoices: TObjectList<TVoiceInfo>; override;
  public
    constructor Create(const AccountName:string; const AccountKey: string; const Region: string);
    destructor Destroy; override;
    function TextToSpeech(text: string; VoiceName: string = ''): TMemoryStream; override;
  end;

implementation

constructor TAmazonPollyService.Create(const AccountName:string; const AccountKey: string; const Region: string);
begin
  inherited Create('', '');
  FAccessKey := AccountName;
  FSecretKey := AccountKey;

  FRegion := TRegionEndpoint.GetEndpoint(Region, '');
end;

destructor TAmazonPollyService.Destroy;
begin
  inherited;
end;

function TAmazonPollyService.GetVoices: TObjectList<TVoiceInfo>;
var
  LPolly : TAmazonPollyClient;
  LRequest : IDescribeVoicesRequest;
  LResponse : IDescribeVoicesResponse;
  LConfig : IClientConfig;
  LCredentials : IAWSCredentials;
  LPollyVoice : TVoice;
  i : Integer;
  LVoice : TVoiceInfo;
begin
  if FVoicesInfo.Count = 0 then
  begin
    LCredentials := TBasicAWSCredentials.Create(FAccessKey, FSecretKey);
    LConfig := TAmazonPollyConfig.Create;
    LConfig.RegionEndpoint := FRegion;

    LPolly := TAmazonPollyClient.Create(LCredentials, LConfig);
    try
      LRequest := TDescribeVoicesRequest.Create;
      LResponse := LPolly.DescribeVoices(LRequest);
      for i  := 0 to LResponse.Voices.Count - 1 do
      begin
        LPollyVoice := LResponse.Voices[i];
        LVoice := TVoiceInfo.Create;
        LVoice.VoiceName := LPollyVoice.Name;
        LVoice.VoiceId := LPollyVoice.Id.Value;
        LVoice.VoiceGender := LPollyVoice.Gender.Value;
        FVoicesInfo.Add(LVoice);
      end;
    finally
      LResponse := nil;
      FreeAndNil(LPolly);
    end;
  end;
  Result := FVoicesInfo;
end;

function TAmazonPollyService.TextToSpeech(text, VoiceName: string): TMemoryStream;
var
  LPolly : TAmazonPollyClient;
  LRequest : ISynthesizeSpeechRequest;
  LResponse : ISynthesizeSpeechResponse;
  LConfig : IClientConfig;
  LCredentials : IAWSCredentials;
begin
  LCredentials := TBasicAWSCredentials.Create(FAccessKey, FSecretKey);
  LConfig := TAmazonPollyConfig.Create;
  LConfig.RegionEndpoint := FRegion;

  LPolly := TAmazonPollyClient.Create(LCredentials, LConfig);
  try
    LRequest := TSynthesizeSpeechRequest.Create;
    LRequest.Text := text;
    LRequest.OutputFormat := 'mp3';
    LRequest.VoiceId := VoiceName;
    if VoiceName.IsEmpty then
      Exception.Create('Missing Voice for Text to Speech');
    
    LRequest.SetEngine('neural');
    LResponse := LPolly.SynthesizeSpeech(LRequest);
    Result := TMemoryStream.Create;
    Result.CopyFrom(LResponse.AudioStream, LResponse.AudioStream.Size);
  finally
    FreeAndNil(LPolly);
  end;
end;

end.
