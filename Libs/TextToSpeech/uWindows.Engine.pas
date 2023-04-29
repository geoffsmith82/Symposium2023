unit uWindows.Engine;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Win.ComObj,
  Winapi.ActiveX,
  Vcl.Controls,
  System.Variants,
  uBaseSpeech,
  SpeechLib_TLB
  ;

type

  TWindowsSpeechService = class(TBaseTextToSpeech)
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
  inherited Create(AOwner as TWinControl, '', '', '');
end;

destructor TWindowsSpeechService.Destroy;
begin
  inherited;
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
