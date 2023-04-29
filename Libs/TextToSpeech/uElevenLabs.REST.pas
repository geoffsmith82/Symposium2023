unit uElevenLabs.REST;

interface

uses
  REST.Client,
  REST.Types,
  Vcl.Controls,
  System.SysUtils,
  System.Classes,
  System.JSON,
  uBaseSpeech
  ;

type
  TElevenLabsService = class(TBaseTextToSpeech)
  public
    constructor Create(Sender: TWinControl; const AResourceKey: string);
    procedure SendTextToSpeechRequest(const apiKey: string; const voice: string; const text: string; out responseStream: TMemoryStream);
    function TextToSpeech(text: string; VoiceName: string = ''): TMemoryStream; override;
  end;

implementation

constructor TElevenLabsService.Create(Sender: TWinControl; const AResourceKey: string);
begin
  inherited Create(Sender, AResourceKey, '');
end;

procedure TElevenLabsService.SendTextToSpeechRequest(const apiKey: string; const voice: string; const text: string; out responseStream: TMemoryStream);
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
  size : Integer;
  jsonBody : TJSONObject;
begin
  RESTClient := TRESTClient.Create(Format('https://api.elevenlabs.io/v1/text-to-speech/%s', [voice]));
  RESTRequest := TRESTRequest.Create(RESTClient);
  RESTResponse := TRESTResponse.Create(RESTClient);
  responseStream := TMemoryStream.Create;
  try
    RESTRequest.Method := rmPOST;
    RESTRequest.AddParameter('xi-api-key', apiKey, pkHTTPHEADER, [poDoNotEncode]);
    RESTRequest.AddParameter('Content-Type', 'application/json', pkHTTPHEADER, [poDoNotEncode]);
    RESTRequest.AddParameter('accept', 'audio/mpeg', pkHTTPHEADER, [poDoNotEncode]);
    jsonBody := TJSONObject.Create;
    try
      jsonBody.AddPair('text', text);
      RESTRequest.AddBody(jsonBody.ToJson, ctAPPLICATION_JSON);
    finally
      FreeAndNil(jsonBody);
    end;
    RESTRequest.Response := RESTResponse;

    RESTRequest.Execute;
    size := Length(RESTResponse.RawBytes);
    responseStream.Write(RESTResponse.RawBytes, size)
  finally
    RESTRequest.Free;
    RESTResponse.Free;
    RESTClient.Free;
  end;
end;

function TElevenLabsService.TextToSpeech(text: string; VoiceName: string = ''): TMemoryStream;
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
  jsonBody : TJSONObject;
  responseStream : TMemoryStream;
begin
  if VoiceName.IsEmpty then
    VoiceName := '21m00Tcm4TlvDq8ikWAM';
  RESTClient := TRESTClient.Create(Format('https://api.elevenlabs.io/v1/text-to-speech/%s', [VoiceName]));
  RESTRequest := TRESTRequest.Create(RESTClient);
  RESTResponse := TRESTResponse.Create(RESTClient);
  responseStream := TMemoryStream.Create;
  try
    RESTRequest.Method := rmPOST;
    RESTRequest.AddParameter('xi-api-key', FResourceKey, pkHTTPHEADER, [poDoNotEncode]);
    RESTRequest.AddParameter('Content-Type', 'application/json', pkHTTPHEADER, [poDoNotEncode]);
    RESTRequest.AddParameter('accept', 'audio/mpeg', pkHTTPHEADER, [poDoNotEncode]);
    jsonBody := TJSONObject.Create;
    try
      jsonBody.AddPair('text', text);
      RESTRequest.AddBody(jsonBody.ToJson, ctAPPLICATION_JSON);
    finally
      FreeAndNil(jsonBody);
    end;
    RESTRequest.Response := RESTResponse;

    RESTRequest.Execute;
    responseStream.Write(RESTResponse.RawBytes, Length(RESTResponse.RawBytes));
    Result := responseStream;
  finally
    RESTRequest.Free;
    RESTResponse.Free;
    RESTClient.Free;
  end;
end;

end.
