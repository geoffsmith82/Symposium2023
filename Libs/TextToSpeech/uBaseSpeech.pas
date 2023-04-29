unit uBaseSpeech;

interface

uses
  System.Classes,
  System.SysUtils,
  Vcl.Controls,
  System.IOUtils,
  System.Threading,
  Winapi.Windows,
  Dialogs,
  Forms,
  Vcl.MPlayer;

type
  TSpeechStatus = (ssPlayStarting, ssPlayStopping);
  TBaseTextToSpeech = class
  strict private
    MediaPlayer : TMediaPlayer;
  private
    procedure MediaPlayerNotify(Sender: TObject);
  protected
    FFormatExt : string;
    FResourceKey: string;
    FApplicationName: string;
    FHost: string;
    FStatus: TSpeechStatus;
  public
    OnFinishedPlaying:  TNotifyEvent;
    procedure PlayText(const text: string);
    constructor Create(Sender: TWinControl; const AResourceKey: string; const AHost: string);
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

procedure TBaseTextToSpeech.MediaPlayerNotify(Sender: TObject);
var
  ticks : UInt64;
begin
  ticks := GetTickCount64;
  repeat
  Application.ProcessMessages;
  until GetTickCount64 - ticks > 200;
 // if MediaPlayer.Mode = mpStopped then
  if FStatus = ssPlayStopping then
  begin
    if Assigned(OnFinishedPlaying) then
      OnFinishedPlaying(Sender);
  end;
  FStatus := ssPlayStopping;
end;

procedure TBaseTextToSpeech.PlayText(const text:string);
var
  Stream: TMemoryStream;
  FileName: string;
  task : ITask;
begin
  MediaPlayer.Notify := true;
  MediaPlayer.OnNotify := MediaPlayerNotify;
  task := TTask.Create(procedure ()
             begin
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

               TThread.Queue(nil, procedure ()
                 begin
                   MediaPlayer.FileName := FileName;
                   MediaPlayer.Open;
                   MediaPlayer.Notify := true;
                   FStatus := ssPlayStarting;
                   MediaPlayer.Play;
                 end);
             end).Start;
end;

constructor TBaseTextToSpeech.Create(Sender: TWinControl; const AResourceKey: string; const AHost: string);
begin
  FResourceKey := AResourceKey;
  FHost := AHost;
  FFormatExt := '.mp3';
  MediaPlayer := TMediaPlayer.Create(nil);
  MediaPlayer.Parent := Sender;
  MediaPlayer.Visible := False;
end;

end.
