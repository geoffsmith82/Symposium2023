unit uTTS;

interface

uses
  System.Classes,
  System.SysUtils,
  System.IOUtils,
  System.Threading,
  System.Generics.Collections,
  System.Win.ComObj,
  Winapi.ActiveX,
  Winapi.Windows,
  DirectShow9
  ;

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
  private
    procedure PlayMP3(const FileName: string);
  protected
    FFormatExt : string;
    FResourceKey: string;
    FApplicationName: string;
    FHost: string;
  public
    OnFinishedPlaying:  TNotifyEvent;
    constructor Create(const AResourceKey: string; const AHost: string);
    destructor Destroy; override;
    function TextToSpeech(text: string; VoiceName: string = ''): TMemoryStream; virtual; abstract;
    procedure PlayText(const text:string; const VoiceName: string = '');
  public
    property Voices: TObjectList<TVoiceInfo> read GetVoices;
  end;

implementation

{ TBaseSpeech }

destructor TBaseTextToSpeech.Destroy;
begin
  inherited;
  FreeAndNil(FVoicesInfo);
end;

procedure TBaseTextToSpeech.PlayMP3(const FileName: string);
var
  GraphBuilder: IGraphBuilder;
  MediaControl: IMediaControl;
  MediaEventEx: IMediaEventEx;
  EventCode: LongInt;
begin
  CoInitialize(nil);
  // Create the Filter Graph Manager
  CoCreateInstance(CLSID_FilterGraph, nil, CLSCTX_INPROC_SERVER, IID_IGraphBuilder, GraphBuilder);

  if GraphBuilder.RenderFile(PWideChar(WideString(FileName)), nil) = S_OK then
  begin
    // Get the IMediaControl interface
    MediaControl := GraphBuilder as IMediaControl;

    // Start playback
    MediaControl.Run;

    // Wait for completion using IMediaEventEx
    MediaEventEx := GraphBuilder as IMediaEventEx;
    MediaEventEx.WaitForCompletion(INFINITE, EventCode);

    TThread.Queue(nil, procedure ()
    begin
      if Assigned(OnFinishedPlaying) then
        OnFinishedPlaying(nil);
          OutputDebugString(PChar('Played'));
    end);

    // Stop playback
    MediaControl.Stop;
  end;
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
               PlayMP3(LFileName);
             end).Start;
end;

constructor TBaseTextToSpeech.Create(const AResourceKey: string; const AHost: string);
begin
  FResourceKey := AResourceKey;
  FHost := AHost;
  FFormatExt := '.mp3';
  FVoicesInfo := TObjectList<TVoiceInfo>.Create;
end;

end.
