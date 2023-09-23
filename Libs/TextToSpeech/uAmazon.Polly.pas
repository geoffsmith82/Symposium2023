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
  private
    function GetVoiceInfo: TObjectList<TVoiceInfo>; override;
  public
    constructor Create(Sender: TWinControl; const AccountName:string; const AccountKey: string);
    destructor Destroy; override;
    function TextToSpeech(text: string; VoiceName: string = ''): TMemoryStream; override;
  end;

implementation

constructor TAmazonPollyService.Create(Sender: TWinControl; const AccountName:string; const AccountKey: string);
begin
  inherited Create(Sender, '', '');
  FAccountName := AccountName;
  FAccountKey := AccountKey;
end;

destructor TAmazonPollyService.Destroy;
begin
  inherited;
end;

function TAmazonPollyService.GetVoiceInfo: TObjectList<TVoiceInfo>;
{$IFNDEF NOPOLLY}
var
  polly : TPollyClient;
  request : TPollyDescribeVoicesRequest;
  response : IPollyDescribeVoicesResponse;
  options : IAWSOptions;
  pollyVoice : IPollyVoice;
  i : Integer;
  voice : TVoiceInfo;
{$ENDIF}
begin
  FVoicesInfo.Clear;
{$IFNDEF NOPOLLY}
  options := TAWSOptions.Create;
  options.AccessKeyId := FAccountName;
  options.SecretAccessKey := FAccountKey;
  options.Region := 'ap-southeast-2';
  polly := TPollyClient.Create(options);
  try
    request := TPollyDescribeVoicesRequest.Create;
    response := polly.DescribeVoices(request);
    for i  := 0 to response.Voices.Count - 1 do
    begin
      pollyVoice := response.Voices[i];
      voice := TVoiceInfo.Create;
      voice.VoiceName := pollyVoice.Name;
      voice.VoiceId := pollyVoice.Id;
      voice.VoiceGender := pollyVoice.Gender;
      FVoicesInfo.Add(voice);
    end;
  finally
    FreeAndNil(polly);
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
  polly : TPollyClient;
  request : TPollySynthesizeSpeechRequest;
  response : IPollySynthesizeSpeechResponse;
  options : IAWSOptions;
{$ENDIF}
begin
{$IFNDEF NOPOLLY}
  options := TAWSOptions.Create;
  options.AccessKeyId := FAccountName;
  options.SecretAccessKey := FAccountKey;
  options.Region := 'ap-southeast-2';
  polly := TPollyClient.Create(options);
  try
    request := TPollySynthesizeSpeechRequest.Create;
    request.Text := text;
    request.OutputFormat := 'mp3';
    request.VoiceId := 'Olivia';
    request.SetEngine('neural');
    response := polly.SynthesizeSpeech(request);
    Result := TMemoryStream.Create;
    Result.CopyFrom(response.AudioStream, response.AudioStream.Size);
  finally
    FreeAndNil(polly);
  end;
{$ENDIF}
{$IFDEF NOPOLLY}
  raise Exception.Create('Polly Not Available/Not Compiled in');
{$ENDIF}
end;

end.
