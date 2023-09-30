unit uBaseSpeech;

interface

uses
  System.Classes,
  System.SysUtils,
  System.IOUtils,
  System.Threading,
  System.Generics.Collections,
  Winapi.Windows,
  Vcl.Dialogs,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.MPlayer;

type
  TVoiceInfo = class
    VoiceName : string;
    VoiceGender : string;
    VoiceId: string;
  end;

  TSpeechStatus = (ssPlayStarting, ssPlayStopping);

  TBaseTextToSpeech = class abstract
  strict protected
    FVoicesInfo : TObjectList<TVoiceInfo>;
    function GetVoices: TObjectList<TVoiceInfo>; virtual; abstract;
  strict private
    FMediaPlayer : TMediaPlayer;
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
    constructor Create(Sender: TWinControl; const AResourceKey: string; const AHost: string);
    destructor Destroy; override;
    function TextToSpeech(text: string; VoiceName: string = ''): TMemoryStream; virtual; abstract;
    procedure PlayText(const text:string; const VoiceName: string = '');
    function Mode: TMPModes;
  public
    property Voices: TObjectList<TVoiceInfo> read GetVoices;
  end;

implementation

{ TBaseSpeech }

destructor TBaseTextToSpeech.Destroy;
begin
  inherited;
  FreeAndNil(FMediaPlayer);
  FreeAndNil(FVoicesInfo);
end;

function TBaseTextToSpeech.Mode: TMPModes;
begin
  Result := FMediaPlayer.Mode;
end;

procedure TBaseTextToSpeech.MediaPlayerNotify(Sender: TObject);
var
  LTicks : UInt64;
begin
  LTicks := GetTickCount64;
  repeat
  Application.ProcessMessages;
  until GetTickCount64 - LTicks > 200;
 // if MediaPlayer.Mode = mpStopped then
  if FStatus = ssPlayStopping then
  begin
    if Assigned(OnFinishedPlaying) then
      OnFinishedPlaying(Sender);
  end;
  FStatus := ssPlayStopping;
end;

procedure TBaseTextToSpeech.PlayText(const text:string; const VoiceName: string = '');
var
  LFileName: string;
  LTask : ITask;
begin
  LTask := TTask.Create(procedure ()
             var
               Stream: TMemoryStream;
             begin
               Stream := nil;
               try
                 Stream := TextToSpeech(text, VoiceName);
                 if not Assigned(Stream) then
                   Exit;
                 LFileName := TPath.GetTempFileName + FFormatExt;
                 Stream.Position := 0;
                 Stream.SaveToFile(LFileName);
               finally
                 FreeAndNil(Stream);
               end;

               TThread.Queue(nil, procedure ()
                 begin
                   FMediaPlayer.OnNotify := MediaPlayerNotify;
                   FMediaPlayer.Notify := true;

                   FMediaPlayer.FileName := LFileName;
                   FMediaPlayer.Open;
                   FStatus := ssPlayStarting;
                   FMediaPlayer.Play;
                 end);
             end).Start;
end;

constructor TBaseTextToSpeech.Create(Sender: TWinControl; const AResourceKey: string; const AHost: string);
begin
  FResourceKey := AResourceKey;
  FHost := AHost;
  FFormatExt := '.mp3';
  FMediaPlayer := TMediaPlayer.Create(nil);
  FMediaPlayer.Parent := Sender;
  FMediaPlayer.Visible := False;
  FVoicesInfo := TObjectList<TVoiceInfo>.Create;
end;

end.
