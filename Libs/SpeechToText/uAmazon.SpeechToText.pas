unit uAmazon.SpeechToText;

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Controls,
  uBaseSpeechToText
  ;

type
  TAmazonSpeechToText = class(TBaseSpeechToText)
  strict private
    FAccessKey: string;
    FSecretKey: string;
    FRegion: string;
    FBucket: string;
  public
    constructor Create(Sender: TWinControl; const AccessKey:string; const SecretKey: string; const Region: string; bucket: string);
    function SupportedFormats: TArray<string>; override;
    function TranscribeAudio(const FilePath, ModelName: string): string; override;
  end;

implementation

{$IFNDEF NOPOLLY}
uses
  AWS.Transcribe,  // need to install AWS SDK For Delphi Preview
  AWS.S3,
  AWS.Core;    // Need Delphi Enterprise or above and install from
{$ENDIF}              // GetIt

{ TAmazonSpeechToText }

constructor TAmazonSpeechToText.Create(Sender: TWinControl; const AccessKey:string; const SecretKey: string; const Region: string; bucket: string);
begin
  inherited Create('','', '');
  FAccessKey := AccessKey;
  FSecretKey := SecretKey;
  FRegion := Region;
  FBucket := Bucket;
end;

function TAmazonSpeechToText.SupportedFormats: TArray<string>;
begin
  SetLength(Result, 1);
  Result[0] := 'wav';
end;

function TAmazonSpeechToText.TranscribeAudio(const FilePath, ModelName: string): string;
var
  transcribeClient : TTranscribeClient;
  s3Client : IS3Client;
  request : TTranscribeStartTranscriptionJobRequest;
  response : ITranscribeResponse;
  response2 : ITranscribeGetTranscriptionJobResponse;
  putObject : TS3PutObjectRequest;
  getObject : TS3GetObjectRequest;
  putObjectResponse: IS3PutObjectResponse;
  getObjectResponse: IS3GetObjectResponse;
  media : ITranscribeMedia;
  options : IAWSOptions;
  stream : TFileStream;
  key : string;
begin

end;

end.
