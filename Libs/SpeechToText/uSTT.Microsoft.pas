unit uSTT.Microsoft;

interface

uses
  uSTT,
  System.Classes,
  REST.Client,
  REST.Types,
  System.JSON,
  System.Net.Mime,
  System.SysUtils
  ;

type
  TMicrosoftSpeechToText = class(TBaseSpeechToText)
  strict private
    FSubscriptionKey : string;
  public
    function SupportedFormats(): TArray<string>; override;
    function TranscribeAudio(const FilePath, ModelName: string): string; override;
  end;

implementation

uses
  System.IOUtils
  ;

{ TMicrosoftSpeechToText }

function TMicrosoftSpeechToText.SupportedFormats: TArray<string>;
begin
  SetLength(Result, 2);
  Result[0] := 'wav';
  Result[1] := 'ogg';
end;

function TMicrosoftSpeechToText.TranscribeAudio(const FilePath, ModelName: string): string;
var
  LRestClient: TRESTClient;
  LRestRequest: TRESTRequest;
  LRestResponse: TRESTResponse;
  LAccessToken: string;
  LSpeechToTextEndpoint: string;
  LFileStream : TFileStream;
  LMime : TMimeTypes;
  LMimeType : string;
  LKind : TMimeTypes.TKind;
begin
  Result := '';

  if not IsFileSupported(FilePath) then
  begin
    raise Exception.Create('Unsupported file format');
  end;

  // Get access token
  LRestClient := TRESTClient.Create(nil);
  try
    LRestClient.BaseURL := 'https://westus.api.cognitive.microsoft.com/sts/v1.0';
    LRestRequest := TRESTRequest.Create(nil);
    LRestRequest.Client := LRestClient;
    LRestRequest.Method := rmPOST;
    LRestRequest.Resource := '/issueToken';
    LRestRequest.Params.AddItem('Subscription-Key', FSubscriptionKey, TRESTRequestParameterKind.pkHTTPHEADER);
    LRestResponse := TRESTResponse.Create(nil);
    try
      LRestRequest.Execute;
      LAccessToken := LRestResponse.Content;
    finally
      LRestResponse.Free;
    end;
  finally
    LRestClient.Free;
  end;

  // Convert speech to text
  LRestClient := TRESTClient.Create(nil);
  try
    LSpeechToTextEndpoint := 'https://westus.stt.speech.microsoft.com/speech/recognition/conversation/cognitiveservices/v1?language=en-US';
    LRestClient.BaseURL := LSpeechToTextEndpoint;
    LRestRequest := TRESTRequest.Create(nil);
    LRestRequest.Client := LRestClient;
    LRestRequest.Method := rmPOST;

    LMime := TMimeTypes.Default;
    LMime.GetExtInfo(TPath.GetExtension(FilePath), LMimeType, LKind);
    LRestRequest.Params.AddItem('Authorization', 'Bearer ' + LAccessToken, TRESTRequestParameterKind.pkHTTPHEADER);
    LRestRequest.Params.AddItem('Content-Type', LMimeType, TRESTRequestParameterKind.pkHTTPHEADER);
    LFileStream := TFileStream.Create(filepath, fmOpenRead);
    try
      LRestRequest.AddBody(LFileStream);
    finally
      FreeAndNil(LFileStream);
    end;

    LRestResponse := TRESTResponse.Create(nil);
    try
      LRestRequest.Execute;
      Result := LRestResponse.Content;
    finally
      FreeAndNil(LRestResponse);
    end;
  finally
    FreeAndNil(LRestClient);
  end;
end;

end.
