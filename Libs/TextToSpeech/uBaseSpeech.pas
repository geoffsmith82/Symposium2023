unit uBaseSpeech;

interface

uses
  System.Classes,
  System.SysUtils,
  Vcl.Controls,
  System.IOUtils,
  Vcl.MPlayer;

type
  TBaseTextToSpeech = class
  strict private
    MediaPlayer1 : TMediaPlayer;
  protected
    FResourceKey: string;
    FApplicationName: string;
    FHost: string;
  public
    procedure PlayText(const text: string);
    constructor Create(Sender: TWinControl; const AResourceKey: string; const AApplicationName: string; const AHost: string);
    destructor Destroy; override;
    function TextToSpeech(text: string; VoiceName: string = ''): TMemoryStream; virtual; abstract;
    function SpeechEngineName: string; virtual; abstract;
    function Mode: TMPModes;
  end;

implementation

{ TBaseSpeech }

destructor TBaseTextToSpeech.Destroy;
begin
  FreeAndNil(MediaPlayer1);
end;

function TBaseTextToSpeech.Mode: TMPModes;
begin
  Result := MediaPlayer1.Mode;
end;

procedure TBaseTextToSpeech.PlayText(const text:string);
var
  Stream: TMemoryStream;
  FileName: string;
begin
  MediaPlayer1.Notify := true;
 // MediaPlayer1.OnNotify := NotifyProc;
  Stream := TMemoryStream.Create;
  try
    Stream := TextToSpeech(text);
    if not Assigned(Stream) then
      Exit;
    FileName := TPath.GetTempFileName + '.mp3';
    Stream.Position := 0;
    Stream.SaveToFile(FileName);
  finally
    Stream.Free;
  end;
  MediaPlayer1.FileName := FileName;
  MediaPlayer1.Open;
  MediaPlayer1.Notify := true;
  MediaPlayer1.Play;
end;

constructor TBaseTextToSpeech.Create(Sender: TWinControl; const AResourceKey: string; const AApplicationName: string; const AHost: string);
begin
  FResourceKey := AResourceKey;
  FApplicationName := AApplicationName;
  FHost := AHost;
  MediaPlayer1 := TMediaPlayer.Create(nil);
  MediaPlayer1.Parent := Sender;
  MediaPlayer1.Visible := False;
end;

end.
