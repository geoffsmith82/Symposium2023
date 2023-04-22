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
    MediaPlayer : TMediaPlayer;
  protected
    FFormatExt : string;
    FResourceKey: string;
    FApplicationName: string;
    FHost: string;
  public
    procedure PlayText(const text: string);
    constructor Create(Sender: TWinControl; const AResourceKey: string; const AApplicationName: string; const AHost: string);
    destructor Destroy; override;
    function TextToSpeech(text: string; VoiceName: string = ''): TMemoryStream; virtual; abstract;
    function Mode: TMPModes;
  end;

implementation

{ TBaseSpeech }

destructor TBaseTextToSpeech.Destroy;
begin
  FreeAndNil(MediaPlayer);
end;

function TBaseTextToSpeech.Mode: TMPModes;
begin
  Result := MediaPlayer.Mode;
end;

procedure TBaseTextToSpeech.PlayText(const text:string);
var
  Stream: TMemoryStream;
  FileName: string;
begin
  MediaPlayer.Notify := true;
  Stream := TMemoryStream.Create;
  try
    Stream := TextToSpeech(text);
    if not Assigned(Stream) then
      Exit;
    FileName := TPath.GetTempFileName + FFormatExt;
    Stream.Position := 0;
    Stream.SaveToFile(FileName);
  finally
    FreeAndNil(Stream);
  end;
  MediaPlayer.FileName := FileName;
  MediaPlayer.Open;
  MediaPlayer.Notify := true;
  MediaPlayer.Play;
end;

constructor TBaseTextToSpeech.Create(Sender: TWinControl; const AResourceKey: string; const AApplicationName: string; const AHost: string);
begin
  FResourceKey := AResourceKey;
  FApplicationName := AApplicationName;
  FHost := AHost;
  FFormatExt := '.mp3';
  MediaPlayer := TMediaPlayer.Create(nil);
  MediaPlayer.Parent := Sender;
  MediaPlayer.Visible := False;
end;

end.
