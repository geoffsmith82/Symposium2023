unit uWindows.Engine;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Win.ComObj,
  System.Generics.Collections,
  Winapi.ActiveX,
  Vcl.Controls,
  System.Variants,
  uBaseSpeech,
  SpeechLib_TLB
  ;

type

  TWindowsSpeechService = class(TBaseTextToSpeech)
  protected
    function GetVoices: TObjectList<TVoiceInfo>; override;
  strict private
    FSpeech : ISpeechVoice;
    FSpFileStream: ISpeechFileStream;
  public
    function TextToSpeech(text: string; VoiceName: string = ''): TMemoryStream; override;
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
  end;

implementation

{ TWindowsSpeechService }

constructor TWindowsSpeechService.Create(AOwner: TComponent);
begin
  inherited Create(AOwner as TWinControl, '', '');
end;

destructor TWindowsSpeechService.Destroy;
begin
  inherited;
end;

function TWindowsSpeechService.GetVoices: TObjectList<TVoiceInfo>;
var
  SpVoice: ISpeechVoice;
  Voices: ISpeechObjectTokens;
  Token: ISpeechObjectToken;
  Description: WideString;
  VoiceInfo: TVoiceInfo;
  i : Integer;
begin
  Result := nil;
  FVoicesInfo.Clear;
  CoInitialize(nil);
  try
    try
      SpVoice := CoSpVoice.Create;
      Voices := SpVoice.GetVoices('', '');
      for I := 0 to Voices.Count - 1 do
      begin
        Token := Voices.Item(I) as ISpeechObjectToken;
        VoiceInfo := TVoiceInfo.Create;
        Description := Token.GetDescription(0);
        begin
          VoiceInfo.VoiceName := Description;
          VoiceInfo.VoiceId := Token.Id;
          FVoicesInfo.Add(VoiceInfo);
        end;
      end;
    finally
      CoUnInitialize;
      Result := FVoicesInfo;
    end;
  except
    on E: Exception do
    begin
      Result.Free;
      raise;
    end;
  end;
end;

function TWindowsSpeechService.TextToSpeech(text, VoiceName: string): TMemoryStream;
const
  OutputFileName = 'output.wav';
var
  FileName: string;
begin
  FFormatExt := '.wav';
  FSpeech := nil;
  FSpFileStream := nil;
  CoInitialize(nil);

  try
    // Create the voice instance
    FSpeech := CoSpVoice.Create;

    // Create the file stream for the speech output
    FSpFileStream := CoSpFileStream.Create;

    // Generate a temporary file name
    FileName := TPath.GetTempFileName;

    // Configure the speech file stream
    FSpFileStream.Format.Type_ := SAFT22kHz16BitMono;
    FSpFileStream.Open(FileName, SSFMCreateForWrite, False);

    // Assign the output file stream to the voice
    FSpeech.AudioOutputStream := FSpFileStream;

    // Perform the speech synthesis
    FSpeech.Speak(Text, 0);

    // Close the speech file stream
    FSpFileStream.Close;

    // Load the speech file into the provided memory stream
    Result := TMemoryStream.Create;
    Result.LoadFromFile(FileName);
  finally
    // Free the file stream and voice instances
    FSpFileStream := nil;
    FSpeech := nil;
    CoUnInitialize;

    // Delete the temporary file
    TFile.Delete(FileName);
  end;
end;


end.
