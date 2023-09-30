unit uAmazon.Polly;

interface

uses
  System.Classes,
  System.SysUtils,
  System.JSON,
  Vcl.Controls,
  Data.Cloud.AmazonAPI,
  Data.Cloud.CloudAPI,
  System.Generics.Collections,
  System.Net.URLClient,
  System.Net.HttpClient,
  System.NetEncoding,
  System.Hash,
  uBaseSpeech
{$IFNDEF NOPOLLY}
  ,AWS.Polly,  // need to install AWS SDK For Delphi Preview
  AWS.Core    // Need Delphi Enterprise 11 or above and install from
{$ENDIF}              // GetIt
  ;

type
  TAmazonPollyService = class(TBaseTextToSpeech)
  private
    FAccountName : string;
    FAccountKey: string;
    FRegion: string;
  protected
    function GetVoices: TObjectList<TVoiceInfo>; override;
  public
    constructor Create(Sender: TWinControl; const AccountName:string; const AccountKey: string; const Region: string);
    destructor Destroy; override;
    function TextToSpeech(text: string; VoiceName: string = ''): TMemoryStream; override;
  end;

implementation

constructor TAmazonPollyService.Create(Sender: TWinControl; const AccountName:string; const AccountKey: string; const Region: string);
begin
  inherited Create(Sender, '', '');
  FAccountName := AccountName;
  FAccountKey := AccountKey;
  FRegion := Region;
end;

destructor TAmazonPollyService.Destroy;
begin
  inherited;
end;

function TAmazonPollyService.GetVoices: TObjectList<TVoiceInfo>;
{$IFNDEF NOPOLLY}
var
  LPolly : TPollyClient;
  LRequest : IPollyDescribeVoicesRequest;
  LResponse : IPollyDescribeVoicesResponse;
  LOptions : IAWSOptions;
  LPollyVoice : IPollyVoice;
  i : Integer;
  LVoice : TVoiceInfo;
{$ENDIF}
begin
  FVoicesInfo.Clear;
{$IFNDEF NOPOLLY}
  LOptions := TAWSOptions.Create;
  LOptions.AccessKeyId := FAccountName;
  LOptions.SecretAccessKey := FAccountKey;
  LOptions.Region := FRegion;
  LPolly := TPollyClient.Create(LOptions);
  try
    LRequest := TPollyDescribeVoicesRequest.Create;
    LResponse := LPolly.DescribeVoices(LRequest);
    for i  := 0 to LResponse.Voices.Count - 1 do
    begin
      LPollyVoice := LResponse.Voices[i];
      LVoice := TVoiceInfo.Create;
      LVoice.VoiceName := LPollyVoice.Name;
      LVoice.VoiceId := LPollyVoice.Id;
      LVoice.VoiceGender := LPollyVoice.Gender;
      FVoicesInfo.Add(LVoice);
    end;
  finally
    LResponse := nil;
    FreeAndNil(LPolly);
  end;
{$ENDIF}
{$IFDEF NOPOLLY}
  raise Exception.Create('Polly Not Available/Not Compiled in');
{$ENDIF}
  Result := FVoicesInfo;
end;

function TAmazonPollyService.TextToSpeech(text, VoiceName: string): TMemoryStream;
{$IFNDEF NOPOLLY}
var
  LPolly : TPollyClient;
  LRequest : IPollySynthesizeSpeechRequest;
  LResponse : IPollySynthesizeSpeechResponse;
  LOptions : IAWSOptions;
{$ENDIF}
begin
{$IFNDEF NOPOLLY}
  LOptions := TAWSOptions.Create;
  LOptions.AccessKeyId := FAccountName;
  LOptions.SecretAccessKey := FAccountKey;
  LOptions.Region := FRegion;
  LPolly := TPollyClient.Create(LOptions);
  try
    LRequest := TPollySynthesizeSpeechRequest.Create;
    LRequest.Text := text;
    LRequest.OutputFormat := 'mp3';
    LRequest.VoiceId := VoiceName;
    LRequest.SetEngine('neural');
    LResponse := LPolly.SynthesizeSpeech(LRequest);
    Result := TMemoryStream.Create;
    Result.CopyFrom(LResponse.AudioStream, LResponse.AudioStream.Size);
  finally
    FreeAndNil(LPolly);
  end;
{$ENDIF}
{$IFDEF NOPOLLY}
  raise Exception.Create('Polly Not Available/Not Compiled in');
{$ENDIF}
end;

end.
