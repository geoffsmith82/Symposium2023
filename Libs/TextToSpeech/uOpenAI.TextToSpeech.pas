unit uOpenAI.TextToSpeech;

interface

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.Generics.Collections,
  Vcl.Controls,
  REST.Client,
  REST.Types,
  uTTS
  ;


type
  TOpenAITextToSpeech = class(TBaseTextToSpeech)
  strict protected
    function GetVoices: TObjectList<TVoiceInfo>; override;
  public
    function TextToSpeech(text: string; VoiceName: string = ''): TMemoryStream; override;
    constructor Create(Sender: TWinControl; const AResourceKey: string);
  end;

implementation

{ TOpenAITextToSpeech }

constructor TOpenAITextToSpeech.Create(Sender: TWinControl; const AResourceKey: string);
begin
  inherited Create(Sender, AResourceKey, '');
end;

function TOpenAITextToSpeech.GetVoices: TObjectList<TVoiceInfo>;
var
  voice : TVoiceInfo;
begin
  FVoicesInfo.Clear;
  voice := TVoiceInfo.Create;
  voice.VoiceName := 'Alloy';
  voice.VoiceId := 'alloy';
  FVoicesInfo.Add(voice);

  voice := TVoiceInfo.Create;
  voice.VoiceName := 'Echo';
  voice.VoiceId := 'echo';
  FVoicesInfo.Add(voice);

  voice := TVoiceInfo.Create;
  voice.VoiceName := 'Fable';
  voice.VoiceId := 'fable';
  FVoicesInfo.Add(voice);

  voice := TVoiceInfo.Create;
  voice.VoiceName := 'Onyx';
  voice.VoiceId := 'onyx';
  FVoicesInfo.Add(voice);

  voice := TVoiceInfo.Create;
  voice.VoiceName := 'Nova';
  voice.VoiceId := 'nova';
  FVoicesInfo.Add(voice);

  voice := TVoiceInfo.Create;
  voice.VoiceName := 'Shimmer';
  voice.VoiceId := 'shimmer';
  FVoicesInfo.Add(voice);

  Result := FVoicesInfo;
end;

function TOpenAITextToSpeech.TextToSpeech(text: string; VoiceName: string = ''): TMemoryStream;
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
  JSONPayload: TJSONObject;
begin
  Result := TMemoryStream.Create;

  // Initialize REST components
  RESTClient := TRESTClient.Create(nil);
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);

  try
    // Configure RESTClient
    RESTClient.BaseURL := 'https://api.openai.com/v1/audio/speech';

    // Configure RESTRequest
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Method := rmPOST;

    // Add Authorization header using AddAuthParameter
    RESTRequest.AddAuthParameter('Authorization', 'Bearer ' + FResourceKey, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);

    // Create JSON payload
    JSONPayload := TJSONObject.Create;
    try
      JSONPayload.AddPair('model', 'tts-1');
      JSONPayload.AddPair('input', text);
      if VoiceName <> '' then
        JSONPayload.AddPair('voice', VoiceName);

      RESTRequest.Body.Add(JSONPayload);

      // Execute the request
      RESTRequest.Execute;

      // The response content should be the binary of the mp3
      if RESTResponse.StatusCode = 200 then
      begin
        Result.WriteBuffer(RESTResponse.RawBytes[0], Length(RESTResponse.RawBytes));
        Result.Position := 0; // Reset the position for reading
      end
      else
      begin
        Result.Free; // Important: Free the memory stream if we're not returning it
        raise Exception.CreateFmt('Error from OpenAI: %s', [RESTResponse.Content]);
      end;

    finally
      JSONPayload.Free;
    end;

  finally
    // Clean up the REST components
    RESTClient.Free;
    RESTRequest.Free;
    RESTResponse.Free;
  end;
end;

end.
