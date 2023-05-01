unit uMicrosoft.SpeechToText;

interface

uses
  uBaseSpeechToText,
  System.Classes,
  REST.Client,
  REST.Types,
  System.JSON,
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
  SetLength(Result, 1);
  Result[0] := 'wav';
end;

function TMicrosoftSpeechToText.TranscribeAudio(const FilePath, ModelName: string): string;
var
  RestClient: TRESTClient;
  RestRequest: TRESTRequest;
  RestResponse: TRESTResponse;
  AccessToken: string;
  SpeechToTextEndpoint: string;
  fileStream : TFileStream;
begin
  Result := '';

  // Get access token
  RestClient := TRESTClient.Create(nil);
  try
    RestClient.BaseURL := 'https://westus.api.cognitive.microsoft.com/sts/v1.0';
    RestRequest := TRESTRequest.Create(nil);
    RestRequest.Client := RestClient;
    RestRequest.Method := rmPOST;
    RestRequest.Resource := '/issueToken';
    RestRequest.Params.AddItem('Subscription-Key', FSubscriptionKey, TRESTRequestParameterKind.pkHTTPHEADER);
    RestResponse := TRESTResponse.Create(nil);
    try
      RestRequest.Execute;
      AccessToken := RestResponse.Content;
    finally
      RestResponse.Free;
    end;
  finally
    RestClient.Free;
  end;

  // Convert speech to text
  RestClient := TRESTClient.Create(nil);
  try
    SpeechToTextEndpoint := 'https://westus.stt.speech.microsoft.com/speech/recognition/conversation/cognitiveservices/v1?language=en-US';
    RestClient.BaseURL := SpeechToTextEndpoint;
    RestRequest := TRESTRequest.Create(nil);
    RestRequest.Client := RestClient;
    RestRequest.Method := rmPOST;
    RestRequest.Params.AddItem('Authorization', 'Bearer ' + AccessToken, TRESTRequestParameterKind.pkHTTPHEADER);
    RestRequest.Params.AddItem('Content-Type', 'audio/wav', TRESTRequestParameterKind.pkHTTPHEADER);
    fileStream := TFileStream.Create(filepath, fmOpenRead);
    try
      RestRequest.AddBody(fileStream);
    finally
      FreeAndNil(fileStream);
    end;

    RestResponse := TRESTResponse.Create(nil);
    try
      RestRequest.Execute;
      Result := RestResponse.Content;
    finally
      FreeAndNil(RestResponse);
    end;
  finally
    FreeAndNil(RestClient);
  end;
end;

end.
