unit uSTT.Amazon;

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Controls,
  uSTT
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
  options := TAWSOptions.Create;
  options.AccessKeyId := FAccessKey;
  options.SecretAccessKey := FSecretKey;
  options.Region := FRegion;
  transcribeClient := TTranscribeClient.Create(options);
  s3Client := TS3Client.Create(options);
  stream := nil;
  try
    stream := TFileStream.Create(FilePath, fmOpenRead);
    putObject := TS3PutObjectRequest.Create(FBucket, Key, stream);
    putObjectResponse := s3Client.PutObject(putObject);
//    getObject := TS3GetObjectRequest.Create(FBucket, Key);
//    getObjectResponse := s3Client.GetObject(getObject);
//    s3Client.

    media := TTranscribeMedia.Transcribe('s3uri');
    request := TTranscribeStartTranscriptionJobRequest.Create('Job', media);
    response := transcribeClient.StartTranscriptionJob(request);

    response2 := transcribeClient.GetTranscriptionJob('Job');

 //   response2.TranscriptionJob.Transcript.TranscriptFileUri

  finally
    FreeAndNil(transcribeClient);
  end;
end;

end.
