unit uTTS.Windows.Engine;

interface

{$IFDEF MSWINDOWS}
uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Win.ComObj,
  System.Generics.Collections,
  Winapi.ActiveX,
  Vcl.Controls,
  System.Variants,
  uTTS,
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
    constructor Create;
    destructor Destroy; override;
  end;

{$ENDIF}

implementation

{ TWindowsSpeechService }

{$IFDEF MSWINDOWS}

constructor TWindowsSpeechService.Create;
begin
  inherited Create('', '');
end;

destructor TWindowsSpeechService.Destroy;
begin
  inherited;
end;

function TWindowsSpeechService.GetVoices: TObjectList<TVoiceInfo>;
var
  LSpVoice: ISpeechVoice;
  LVoices: ISpeechObjectTokens;
  LToken: ISpeechObjectToken;
  LDescription: WideString;
  LVoiceInfo: TVoiceInfo;
  i : Integer;
begin
  Result := nil;
  FVoicesInfo.Clear;
  CoInitialize(nil);
  try
    try
      LSpVoice := CoSpVoice.Create;
      LVoices := LSpVoice.GetVoices('', '');
      for I := 0 to LVoices.Count - 1 do
      begin
        LToken := LVoices.Item(I) as ISpeechObjectToken;
        LVoiceInfo := TVoiceInfo.Create;
        LDescription := LToken.GetDescription(0);
        begin
          LVoiceInfo.VoiceName := LDescription;
          LVoiceInfo.VoiceId := LToken.Id;
          FVoicesInfo.Add(LVoiceInfo);
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
var
  LFileName: string;
  VoiceToken: ISpeechObjectToken;
  VoiceTokens: ISpeechObjectTokens;
  I: Integer;
begin
  FFormatExt := '.wav';
  FSpeech := nil;
  FSpFileStream := nil;
  CoInitialize(nil);

  try
    // Create the voice instance
    FSpeech := CoSpVoice.Create;

    // Get the list of available voices
    VoiceTokens := FSpeech.GetVoices('', '');

    // Find the voice that matches the provided VoiceName
    VoiceToken := nil;
    for I := 0 to VoiceTokens.Count - 1 do
    begin
      if SameText(VoiceTokens.Item(I).Id, VoiceName) then
      begin
        VoiceToken := VoiceTokens.Item(I);
        Break;
      end;
    end;

    // Set the voice if found
    if Assigned(VoiceToken) then
      FSpeech.Voice := VoiceToken;

    // Create the file stream for the speech output
    FSpFileStream := CoSpFileStream.Create;

    // Generate a temporary file name
    LFileName := TPath.GetTempFileName;

    // Configure the speech file stream
    FSpFileStream.Format.Type_ := SAFT22kHz16BitMono;
    FSpFileStream.Open(LFileName, SSFMCreateForWrite, False);

    // Assign the output file stream to the voice
    FSpeech.AudioOutputStream := FSpFileStream;

    // Perform the speech synthesis
    FSpeech.Speak(Text, 0);

    // Close the speech file stream
    FSpFileStream.Close;

    // Load the speech file into the provided memory stream
    Result := TMemoryStream.Create;
    Result.LoadFromFile(LFileName);
  finally
    // Free the file stream and voice instances
    FSpFileStream := nil;
    FSpeech := nil;
    CoUnInitialize;

    // Delete the temporary file
    TFile.Delete(LFileName);
  end;
end;

{$ENDIF}
end.
