unit uElevenLabs.REST;

interface

uses
  REST.Client,
  REST.Types,
  Vcl.Controls,
  System.SysUtils,
  System.Generics.Collections,
  System.Classes,
  System.JSON,
  uBaseSpeech
  ;

type
  TElevenLabsService = class(TBaseTextToSpeech)
  protected
    function GetVoices: TObjectList<TVoiceInfo>; override;
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

function TElevenLabsService.GetVoices: TObjectList<TVoiceInfo>;
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
  JSONValue: TJSONValue;
  JSONValue2: TJSONValue;
  JSONLabels: TJSONObject;
  voice : TVoiceInfo;
begin
  FVoicesInfo.Clear;

  RESTClient := nil;
  RESTRequest := nil;
  RESTResponse := nil;

  try
    RESTClient := TRESTClient.Create('https://api.elevenlabs.io');
    RESTRequest := TRESTRequest.Create(nil);
    RESTResponse := TRESTResponse.Create(nil);

    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Method := TRESTRequestMethod.rmGET;
    RESTRequest.Resource := '/v1/voices';
    RESTRequest.Params.AddHeader('accept', 'application/json');
    RESTRequest.AddParameter('xi-api-key', FResourceKey, pkHTTPHEADER, [poDoNotEncode]);

    RESTRequest.Execute;

    if RESTResponse.StatusCode = 200 then
    begin
      JSONValue := TJSONObject.ParseJSONValue(RESTResponse.Content);
      try
        if JSONValue is TJSONObject then
        begin
          // Parse the JSON response and populate the VoiceList
          // assuming that "voices" is an array of voice objects
          for JSONValue2 in ((JSONValue as TJSONObject).Values['voices'] as TJSONArray) do
          begin
            // Parse the individual voice object
            // Modify this part according to your JSON structure
            with JSONValue2 as TJSONObject do
            begin
              voice := TVoiceInfo.Create;
              if Assigned(GetValue('labels')) then
              begin
                JSONLabels := GetValue('labels') as TJSONObject;
                if Assigned(JSONLabels.GetValue('gender')) then
                  voice.VoiceGender := JSONLabels.GetValue<string>('gender');
              end;
              voice.VoiceName := GetValue('name').Value;

              voice.VoiceId := GetValue('voice_id').Value;
              FVoicesInfo.Add(voice);
            end;
          end;
        end;
      finally
        FreeAndNil(JSONValue);
      end;
    end
    else
    begin
      // Handle the error response if needed
      raise Exception.Create('Failed to fetch voice data: ' + RESTResponse.StatusText);
    end;
  finally
    RESTClient.Free;
    RESTRequest.Free;
    RESTResponse.Free;
  end;

  Result := FVoicesInfo;
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
