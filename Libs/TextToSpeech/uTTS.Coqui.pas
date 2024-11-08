unit uTTS.Coqui;

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
  TCoquiTTSService = class(TBaseTextToSpeech)
  protected
    function GetVoices: TObjectList<TVoiceInfo>; override;
  public
    constructor Create(const AResourceKey: string; const AHost: string);
    function TextToSpeech(text: string; VoiceName: string = ''): TMemoryStream; override;
  end;

implementation

{ TCoquiTTSService }

constructor TCoquiTTSService.Create(const AResourceKey: string; const AHost: string);
begin
  inherited Create(AResourceKey, AHost);
end;

function TCoquiTTSService.GetVoices: TObjectList<TVoiceInfo>;
var
  voice : TVoiceInfo;
begin
  voice := TVoiceInfo.Create;
  voice.VoiceId := 'Id';
  Voice.VoiceName := 'VoiceName';
  Voice.VoiceGender := 'Gender';
  FVoicesInfo.Add(voice);
  Result := FVoicesInfo;
end;

function TCoquiTTSService.TextToSpeech(text, VoiceName: string): TMemoryStream;
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
begin
  // Create instances of the required components
  RESTClient := TRESTClient.Create(nil);
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);

  try
    // Set up the REST client
    RESTClient.BaseURL := FHost;

    // Assign components to each other
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;

    // Set up the REST request
    RESTRequest.Resource := '/api/tts';
    RESTRequest.Method := TRESTRequestMethod.rmGET;

    // Add parameters to the request
    RESTRequest.AddParameter('text', text, TRESTRequestParameterKind.pkGETorPOST);
    if not VoiceName.IsEmpty then
      RESTRequest.AddParameter('speaker_id', VoiceName, TRESTRequestParameterKind.pkGETorPOST);
    // If there are any other parameters required for style or language, you can add them similarly

    // Execute the request
    RESTRequest.Execute;

    // If we have a successful response, copy it to the result TMemoryStream
    if RESTResponse.StatusCode = 200 then
    begin
      Result := TBytesStream.Create;
      Result.Write(RESTResponse.RawBytes, Length(RESTResponse.RawBytes));
      Result.Position := 0;
    end
    else
    begin
      Result := nil;
      // Handle any errors or log them if necessary
    end;
  finally
    // Cleanup
    RESTResponse.Free;
    RESTRequest.Free;
    RESTClient.Free;
  end;
end;

end.
