unit uSTT.OpenAI.Whisper.Online;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Net.HttpClient,
  System.Net.HttpClientComponent,
  System.Net.Mime,
  System.IOUtils,
  uSTT;

type
  TOpenAiWhisperOnline = class(TBaseSpeechToText)
  public
    function SupportedFormats(): TArray<string>; override;
    function TranscribeAudio(const FilePath, ModelName: string): string; override;
  end;

implementation

function TOpenAiWhisperOnline.SupportedFormats: TArray<string>;
begin
  SetLength(Result, 7);
  Result[0] := 'mp3';
  Result[1] := 'mp4';
  Result[2] := 'mpeg';
  Result[3] := 'mpga';
  Result[4] := 'm4a';
  Result[5] := 'wav';
  Result[6] := 'webm';
end;

function TOpenAiWhisperOnline.TranscribeAudio(const FilePath, ModelName: string): string;
var
  LHTTPClient: TNetHttpClient;
  LRequest: TNetHttpRequest;
  LResponse: IHttpResponse;
  LFormData: TMultipartFormData;
  LUrl : string;
  LMime : TMimeTypes;
  LMimeType : string;
  LKind : TMimeTypes.TKind;
begin
  if not IsFileSupported(FilePath) then
  begin
    raise Exception.Create('Unsupported file format');
  end;

  LHTTPClient := TNetHttpClient.Create(nil);
  LRequest := TNetHttpRequest.Create(nil);
  LResponse := nil;
  LFormData := TMultipartFormData.Create;

  try
    LHTTPClient.CustomHeaders['Authorization'] := 'Bearer ' + FResourceKey;

    LMime := TMimeTypes.Default;
    LMime.GetExtInfo(TPath.GetExtension(FilePath), LMimeType, LKind);
    LFormData.AddFile('file', FilePath, LMimeType);
    LFormData.AddField('model', ModelName);
    LUrl := 'https://api.openai.com/v1/audio/transcriptions';
    LResponse := LHTTPClient.Post(LUrl, LFormData);
    Result := LResponse.ContentAsString();
  finally
    LHTTPClient.Free;
    LRequest.Free;
    LFormData.Free;
  end;
end;


end.
