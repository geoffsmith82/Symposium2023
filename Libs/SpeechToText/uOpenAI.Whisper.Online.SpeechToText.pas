unit uOpenAI.Whisper.Online.SpeechToText;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Net.HttpClient,
  System.Net.HttpClientComponent,
  System.Net.Mime,
  System.IOUtils,
  uBaseSpeechToText;

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
  HTTPClient: TNetHttpClient;
  Request: TNetHttpRequest;
  Response: IHttpResponse;
  FormData: TMultipartFormData;
  url : string;
  mime : TMimeTypes;
  mimeType : string;
  kind : TMimeTypes.TKind;
begin
  if not IsFileSupported(FilePath) then
  begin
    raise Exception.Create('Unsupported file format');
  end;

  HTTPClient := TNetHttpClient.Create(nil);
  Request := TNetHttpRequest.Create(nil);
  Response := nil;
  FormData := TMultipartFormData.Create;

  try
    HTTPClient.CustomHeaders['Authorization'] := 'Bearer ' + FResourceKey;

    mime := TMimeTypes.Default;
    mime.GetExtInfo(TPath.GetExtension(FilePath), mimeType, kind);
    FormData.AddFile('file', FilePath, mimeType);
    FormData.AddField('model', ModelName);
    url := 'https://api.openai.com/v1/audio/transcriptions';
    Response := HTTPClient.Post(Url, FormData);
    Result := Response.ContentAsString();
  finally
    HTTPClient.Free;
    Request.Free;
    FormData.Free;
  end;
end;


end.
