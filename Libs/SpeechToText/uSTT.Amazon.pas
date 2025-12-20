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

uses
  AWS.Transcribe,
  AWS.S3;

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
  transcribeClient : TAmazonTranscribeServiceClient;
  s3Client : TAmazonS3Client;
  request : IStartTranscriptionJobRequest;
  request2 : IGetTranscriptionJobRequest;
  response : IStartTranscriptionJobResponse;
  response2 : IGetTranscriptionJobResponse;
  putObject : IPutObjectRequest;
  getObject : TGetObjectRequest;
  putObjectResponse: IPutObjectResponse;
  getObjectResponse: TGetObjectResponse;
  media : TMedia;
  stream : TFileStream;
  key : string;
begin
  transcribeClient := TAmazonTranscribeServiceClient.Create(FAccessKey, FSecretKey, FRegion);
  s3Client := TAmazonS3Client.Create(FAccessKey, FSecretKey, FRegion);
  stream := nil;
  try
    stream := TFileStream.Create(FilePath, fmOpenRead);
    putObject := TPutObjectRequest.Create;
    //(FBucket, Key, stream);
    putObjectResponse := s3Client.PutObject(putObject);
//    getObject := TS3GetObjectRequest.Create(FBucket, Key);
//    getObjectResponse := s3Client.GetObject(getObject);
//    s3Client.

    media := TMedia.Create;
//    media.MediaFileUri :=
    request := TStartTranscriptionJobRequest.Create;//('Job', media);
    response := transcribeClient.StartTranscriptionJob(request);

    response2 := transcribeClient.GetTranscriptionJob(request2);

 //   response2.TranscriptionJob.Transcript.TranscriptFileUri

  finally
    FreeAndNil(transcribeClient);
  end;
end;

end.
