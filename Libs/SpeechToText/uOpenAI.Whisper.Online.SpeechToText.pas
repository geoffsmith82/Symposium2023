unit uOpenAI.Whisper.Online.SpeechToText;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Net.HttpClient,
  System.Net.Mime,
  uBaseSpeechToText;

type
  TOpenAiWhisperOnline = class(TBaseSpeechToText)
  public
    function TranscribeAudio(const FilePath, ModelName: string): string; override;
    function SpeechEngineName: string; override;
  end;

implementation

{$i ..\apikey.inc}

function TOpenAiWhisperOnline.SpeechEngineName: string;
begin
  Result := 'OpenAiWhisperOnlineSpeech';
end;

function TOpenAiWhisperOnline.TranscribeAudio(const FilePath, ModelName: string): string;
var
  HTTPClient: THttpClient;
  Request: THttpRequest;
  Response: IHttpResponse;
  FormData: TMultipartFormData;
  url : string;
begin
  HTTPClient := THttpClient.Create;
  Request := THttpRequest.Create;
  Response := nil;
  FormData := TMultipartFormData.Create;

  try
    HTTPClient.CustomHeaders['Authorization'] := 'Bearer ' + CHATGPT_APIKEY;

    FormData.AddFile('file', FilePath, 'audio/mp3');
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
