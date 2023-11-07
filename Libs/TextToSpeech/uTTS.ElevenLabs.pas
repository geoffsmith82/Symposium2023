unit uTTS.ElevenLabs;

interface

uses
  REST.Client,
  REST.Types,
  Vcl.Controls,
  System.SysUtils,
  System.Generics.Collections,
  System.Classes,
  System.JSON,
  uTTS
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
  LRESTClient: TRESTClient;
  LRESTRequest: TRESTRequest;
  LRESTResponse: TRESTResponse;
  LJSONValue: TJSONValue;
  LJSONValue2: TJSONValue;
  LJSONLabels: TJSONObject;
  LVoice : TVoiceInfo;
begin
  FVoicesInfo.Clear;

  LRESTClient := nil;
  LRESTRequest := nil;
  LRESTResponse := nil;

  try
    LRESTClient := TRESTClient.Create('https://api.elevenlabs.io');
    LRESTRequest := TRESTRequest.Create(nil);
    LRESTResponse := TRESTResponse.Create(nil);

    LRESTRequest.Client := LRESTClient;
    LRESTRequest.Response := LRESTResponse;
    LRESTRequest.Method := TRESTRequestMethod.rmGET;
    LRESTRequest.Resource := '/v1/voices';
    LRESTRequest.Params.AddHeader('accept', 'application/json');
    LRESTRequest.AddParameter('xi-api-key', FResourceKey, pkHTTPHEADER, [poDoNotEncode]);

    LRESTRequest.Execute;

    if LRESTResponse.StatusCode = 200 then
    begin
      LJSONValue := TJSONObject.ParseJSONValue(LRESTResponse.Content);
      try
        if LJSONValue is TJSONObject then
        begin
          // Parse the JSON response and populate the VoiceList
          // assuming that "voices" is an array of voice objects
          for LJSONValue2 in ((LJSONValue as TJSONObject).Values['voices'] as TJSONArray) do
          begin
            // Parse the individual voice object
            // Modify this part according to your JSON structure
            with LJSONValue2 as TJSONObject do
            begin
              LVoice := TVoiceInfo.Create;
              if Assigned(GetValue('labels')) then
              begin
                LJSONLabels := GetValue('labels') as TJSONObject;
                if Assigned(LJSONLabels.GetValue('gender')) then
                  LVoice.VoiceGender := LJSONLabels.GetValue<string>('gender');
              end;
              LVoice.VoiceName := GetValue('name').Value;

              LVoice.VoiceId := GetValue('voice_id').Value;
              FVoicesInfo.Add(LVoice);
            end;
          end;
        end;
      finally
        FreeAndNil(LJSONValue);
      end;
    end
    else
    begin
      // Handle the error response if needed
      raise Exception.Create('Failed to fetch voice data: ' + LRESTResponse.StatusText);
    end;
  finally
    LRESTClient.Free;
    LRESTRequest.Free;
    LRESTResponse.Free;
  end;

  Result := FVoicesInfo;
end;

procedure TElevenLabsService.SendTextToSpeechRequest(const apiKey: string; const voice: string; const text: string; out responseStream: TMemoryStream);
var
  LRESTClient: TRESTClient;
  LRESTRequest: TRESTRequest;
  LRESTResponse: TRESTResponse;
  LSize : Integer;
  LJsonBody : TJSONObject;
begin
  LRESTClient := TRESTClient.Create(Format('https://api.elevenlabs.io/v1/text-to-speech/%s', [voice]));
  LRESTRequest := TRESTRequest.Create(LRESTClient);
  LRESTResponse := TRESTResponse.Create(LRESTClient);
  responseStream := TMemoryStream.Create;
  try
    LRESTRequest.Method := rmPOST;
    LRESTRequest.AddParameter('xi-api-key', apiKey, pkHTTPHEADER, [poDoNotEncode]);
    LRESTRequest.AddParameter('Content-Type', 'application/json', pkHTTPHEADER, [poDoNotEncode]);
    LRESTRequest.AddParameter('accept', 'audio/mpeg', pkHTTPHEADER, [poDoNotEncode]);
    LJsonBody := TJSONObject.Create;
    try
      LJsonBody.AddPair('text', text);
      LRESTRequest.AddBody(LJsonBody.ToJson, ctAPPLICATION_JSON);
    finally
      FreeAndNil(LJsonBody);
    end;
    LRESTRequest.Response := LRESTResponse;

    LRESTRequest.Execute;
    LSize := Length(LRESTResponse.RawBytes);
    responseStream.Write(LRESTResponse.RawBytes, LSize)
  finally
    LRESTRequest.Free;
    LRESTResponse.Free;
    LRESTClient.Free;
  end;
end;

function TElevenLabsService.TextToSpeech(text: string; VoiceName: string = ''): TMemoryStream;
var
  LRESTClient: TRESTClient;
  LRESTRequest: TRESTRequest;
  LRESTResponse: TRESTResponse;
  LJsonBody : TJSONObject;
  LResponseStream : TMemoryStream;
begin
  if VoiceName.IsEmpty then
    VoiceName := '21m00Tcm4TlvDq8ikWAM';
  LRESTClient := TRESTClient.Create(Format('https://api.elevenlabs.io/v1/text-to-speech/%s', [VoiceName]));
  LRESTRequest := TRESTRequest.Create(LRESTClient);
  LRESTResponse := TRESTResponse.Create(LRESTClient);
  LResponseStream := TMemoryStream.Create;
  try
    LRESTRequest.Method := rmPOST;
    LRESTRequest.AddParameter('xi-api-key', FResourceKey, pkHTTPHEADER, [poDoNotEncode]);
    LRESTRequest.AddParameter('Content-Type', 'application/json', pkHTTPHEADER, [poDoNotEncode]);
    LRESTRequest.AddParameter('accept', 'audio/mpeg', pkHTTPHEADER, [poDoNotEncode]);
    LJsonBody := TJSONObject.Create;
    try
      LJsonBody.AddPair('text', text);
      LRESTRequest.AddBody(LJsonBody.ToJson, ctAPPLICATION_JSON);
    finally
      FreeAndNil(LJsonBody);
    end;
    LRESTRequest.Response := LRESTResponse;

    LRESTRequest.Execute;
    LResponseStream.Write(LRESTResponse.RawBytes, Length(LRESTResponse.RawBytes));
    Result := LResponseStream;
  finally
    LRESTRequest.Free;
    LRESTResponse.Free;
    LRESTClient.Free;
  end;
end;

end.
