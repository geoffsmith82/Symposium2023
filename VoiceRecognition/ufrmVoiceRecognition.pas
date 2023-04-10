unit ufrmVoiceRecognition;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections,
  System.NetEncoding,
  System.IniFiles,
  System.IOUtils,
  System.JSON,
  System.SyncObjs,
  Vcl.ExtCtrls,
  Vcl.Menus,
  Vcl.MPlayer,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  sgcBase_Classes,
  sgcSocket_Classes,
  sgcTCP_Classes,
  sgcWebSocket_Classes,
  sgcWebSocket_Classes_Indy,
  sgcWebSocket_Client,
  sgcWebSocket,
  ACS_Classes,
  ACS_DXAudio,
  ACS_Misc,
  Vcl.StdCtrls,
  ACS_Streams,
  ACS_LAME,
  ACS_FLAC,
  ACS_WinMedia,
  ACS_smpeg,
  NewACIndicators,
  ACS_Wave,
  OpenAI,
  uBaseSpeech,
  uMicrosoft.Cognitive.REST,
  uElevenLabs.REST,
  uGoogleSpeech,
  uAmazon.Polly,
  uWindows.Engine
  ;

type
  TTSendThread = class(TThread)
  private
    queueItems : TThreadedQueue<TMemoryStream>;
    procedure sgcWebSocketClient1Handshake(Connection: TsgcWSConnection; var Headers: TStringList);
    procedure sgcWebSocketClient1Message(Connection: TsgcWSConnection; const Text: string);
    procedure sgOnConnect(Connection: TsgcWSConnection);
    function Base64EncodedStream(fs: TStream): string;
  public
    procedure Execute; override;
    procedure Add(ms: TMemoryStream);
    constructor Create(CreateSuspended: Boolean);
  end;

  TForm3 = class(TForm)
    DXAudioIn1: TDXAudioIn;
    AudioProcessor1: TAudioProcessor;
    Button1: TButton;
    StreamOut1: TStreamOut;
    sgcWebSocketClient1: TsgcWebSocketClient;
    Memo1: TMemo;
    Button2: TButton;
    ListBox1: TListBox;
    Memo2: TMemo;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    New1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    SaveAs1: TMenuItem;
    Print1: TMenuItem;
    PrintSetup1: TMenuItem;
    Exit1: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    miElevenLabsSpeechEngine: TMenuItem;
    miMicrosoftSpeechEngine: TMenuItem;
    miAmazonSpeechEngine: TMenuItem;
    miGoogleSpeechEngine: TMenuItem;
    miWindowsSpeechEngine: TMenuItem;
    MediaPlayer1: TMediaPlayer;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure AudioProcessor1GetData(Sender: TComponent; var Buffer: Pointer; var Bytes: Cardinal);
    procedure Button1Click(Sender: TObject);
    procedure sgcWebSocketClient1Handshake(Connection: TsgcWSConnection; var Headers: TStringList);
    procedure sgcWebSocketClient1Message(Connection: TsgcWSConnection; const Text: string);
    procedure Button2Click(Sender: TObject);
    procedure AudioProcessor1GetChannels(Sender: TComponent; var Param: Cardinal);
    procedure AudioProcessor1GetBitsPerSample(Sender: TComponent; var Param: Cardinal);
    procedure AudioProcessor1GetSampleRate(Sender: TComponent; var Param: Cardinal);
    procedure ListBox1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure miAmazonSpeechEngineClick(Sender: TObject);
    procedure miElevenLabsSpeechEngineClick(Sender: TObject);
    procedure miGoogleSpeechEngineClick(Sender: TObject);
    procedure miMicrosoftSpeechEngineClick(Sender: TObject);
    procedure miWindowsSpeechEngineClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    Settings : TIniFile;
    SpeechEngine : TBaseSpeech;
    MsVoiceService : TMicrosoftCognitiveService;
    ElevenLabsVoiceService : TElevenLabsService;
    AmazonPolyVoiceService : TAmazonPollyService;
    GoogleVoiceService : TGoogleSpeechService;
    WindowsVoiceService : TWindowsSpeechService;
    FmemStream : TMemoryStream;
    sendThread : TTSendThread;

    procedure PlayTextWithSelectedEngine(text: string);
    procedure NotifyProc(Sender: TObject);
  public
    { Public declarations }

  end;

var
  Form3: TForm3;

implementation

{$R *.dfm}

{$I ..\Libs\apikey.inc}

procedure TForm3.PlayTextWithSelectedEngine(text:string);
var
  Stream: TMemoryStream;
  FileName: string;
begin
  MediaPlayer1.Notify := true;
  MediaPlayer1.OnNotify := NotifyProc;
  Stream := TMemoryStream.Create;
  try
    Stream := SpeechEngine.TextToSpeech(text);
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

procedure TForm3.FormCreate(Sender: TObject);
var
  i : Integer;
  lSpeechEngine : string;
begin
  Settings := TIniFile.Create(ChangeFileExt(ParamStr(0),'.ini'));
  lSpeechEngine := Settings.ReadString('Speech', 'SelectedEngine', 'Windows');
  MsVoiceService := TMicrosoftCognitiveService.Create(ms_cognative_service_resource_key, 'australiaeast.tts.speech.microsoft.com');
  MsVoiceService.GetAccessToken;
  ElevenLabsVoiceService := TElevenLabsService.Create(ElevenLabsAPIKey, 'ADUG Demo', 'ElevenLabsAPIKey');
  AmazonPolyVoiceService := TAmazonPollyService.Create(AWSAccessKey, AWSSecretkey);//'ADUG Demo', '');
  WindowsVoiceService := TWindowsSpeechService.Create('','','');
  GoogleVoiceService := TGoogleSpeechService.Create(google_clientid, google_clientsecret,'ADUG Demo', '', Settings);
  SpeechEngine := AmazonPolyVoiceService;


  if lSpeechEngine.Contains(ElevenLabsVoiceService.SpeechEngineName) then
  begin
    SpeechEngine := ElevenLabsVoiceService;
    miElevenLabsSpeechEngine.Checked := True;
  end
  else if lSpeechEngine.Contains(MsVoiceService.SpeechEngineName) then
  begin
    SpeechEngine := MsVoiceService;
    miMicrosoftSpeechEngine.Checked := True;
  end
  else if lSpeechEngine.Contains(AmazonPolyVoiceService.SpeechEngineName) then
  begin
    SpeechEngine := AmazonPolyVoiceService;
    miAmazonSpeechEngine.Checked := True;
  end
  else if lSpeechEngine.Contains(GoogleVoiceService.SpeechEngineName) then
  begin
    SpeechEngine := GoogleVoiceService;
    miGoogleSpeechEngine.Checked := True;
  end
  else if lSpeechEngine.Contains(WindowsVoiceService.SpeechEngineName) then
  begin
    SpeechEngine := WindowsVoiceService;
    miWindowsSpeechEngine.Checked := True;
  end
  else
  begin
    SpeechEngine := WindowsVoiceService;  // default engine
    miWindowsSpeechEngine.Checked := True;
  end;

  FmemStream := TMemoryStream.Create;
  FmemStream.SetSize(100*1024*1024);

  sendThread := TTSendThread.Create(True);


  ListBox1.Items.Clear;
  for i := 0 to DXAudioIn1.DeviceCount - 1 do
  begin
    ListBox1.Items.Add(DXAudioIn1.DeviceName[i]);
  end;
  ListBox1.ItemIndex := 0;
end;

procedure TForm3.FormDestroy(Sender: TObject);
begin
  FreeAndNil(sendThread);
end;

procedure TForm3.ListBox1Click(Sender: TObject);
begin
  DXAudioIn1.DeviceNumber := ListBox1.ItemIndex;
end;

procedure TForm3.sgcWebSocketClient1Handshake(Connection: TsgcWSConnection; var Headers: TStringList);
begin
  Headers.Add('Authorization: Token 9c04fed810f9e03bab6bd48b5ef9681995699f4d');
end;

procedure TForm3.sgcWebSocketClient1Message(Connection: TsgcWSConnection; const Text: string);
begin
  Memo1.Lines.Add(Text);
end;

procedure TForm3.Timer1Timer(Sender: TObject);
begin
  OutputDebugString(PChar(MediaPlayer1.EndPos.ToString + ' ' + MediaPlayer1.Position.ToString));
  if Mediaplayer1.Mode = mpStopped then
  begin
    if StreamOut1.Status <> tosPlaying then
      StreamOut1.Run;
  end;
end;

procedure TForm3.NotifyProc(Sender: TObject);
begin
  OutputDebugString(PChar('Mediaplayer NotifyProc'));
  with Sender as TMediaPlayer do
  begin
    case Mode of
      mpStopped:
      begin{do something here}
        OutputDebugString(PChar('Mediaplayer Stopped'));
    //    StreamOut1.Run;
      end;
    end;
    //must set to true to enable next-time notification
    Notify := True;
  end;
end;

procedure TForm3.AudioProcessor1GetBitsPerSample(Sender: TComponent; var Param: Cardinal);
begin
  Param := TAudioProcessor(Sender).Input.BitsPerSample;
end;

procedure TForm3.AudioProcessor1GetChannels(Sender: TComponent; var Param: Cardinal);
begin
  Param := TAudioProcessor(Sender).Input.Channels;
end;

procedure TForm3.AudioProcessor1GetData(Sender: TComponent; var Buffer: Pointer; var Bytes: Cardinal);
var
  mem : TMemoryStream;
begin
  TAudioProcessor(Sender).Input.GetData(Buffer, Bytes);

  mem := TMemoryStream.Create;
  mem.WriteData(Buffer, Bytes);
  mem.Position := 0;
  sendThread.Add(mem);

  OutputDebugString(PChar('Len ' + Bytes.ToString));
end;

procedure TForm3.AudioProcessor1GetSampleRate(Sender: TComponent; var Param: Cardinal);
begin
  Param := TAudioProcessor(Sender).Input.SampleRate;
end;

procedure TForm3.Button1Click(Sender: TObject);
begin
  sendThread.Resume;
  Sleep(1000);
  StreamOut1.Stream := FmemStream;
  StreamOut1.Run;
//  AudioProcessor1._Resume;
end;

procedure TForm3.Button2Click(Sender: TObject);
begin
  sgcWebSocketClient1.WriteData('{ "type": "CloseStream" }');
  StreamOut1.Stop;
end;

procedure TForm3.Exit1Click(Sender: TObject);
begin
  sendThread.Terminate;
  Application.Terminate;
end;

procedure TForm3.miAmazonSpeechEngineClick(Sender: TObject);
begin
  SpeechEngine := AmazonPolyVoiceService;
  Settings.WriteString('Speech', 'SelectedEngine', SpeechEngine.SpeechEngineName);
end;

procedure TForm3.miElevenLabsSpeechEngineClick(Sender: TObject);
begin
  SpeechEngine := ElevenLabsVoiceService;
  Settings.WriteString('Speech', 'SelectedEngine', SpeechEngine.SpeechEngineName);
end;

procedure TForm3.miGoogleSpeechEngineClick(Sender: TObject);
begin
  SpeechEngine := GoogleVoiceService;
  Settings.WriteString('Speech', 'SelectedEngine', SpeechEngine.SpeechEngineName);
end;

procedure TForm3.miMicrosoftSpeechEngineClick(Sender: TObject);
begin
  SpeechEngine := MsVoiceService;
  Settings.WriteString('Speech', 'SelectedEngine', SpeechEngine.SpeechEngineName);
end;

procedure TForm3.miWindowsSpeechEngineClick(Sender: TObject);
begin
  SpeechEngine := WindowsVoiceService;
  Settings.WriteString('Speech', 'SelectedEngine', SpeechEngine.SpeechEngineName);
end;

{ TTSendThread }

procedure TTSendThread.sgcWebSocketClient1Handshake(Connection: TsgcWSConnection; var Headers: TStringList);
begin
  Headers.Add('Authorization: e1c1c21347b54ddb86c426b8ae4a1096');
end;

procedure TTSendThread.sgcWebSocketClient1Message(Connection: TsgcWSConnection; const Text: string);
begin
  TThread.Queue(nil, procedure()
  var
    msg : TJSONObject;
    value : string;
    response : string;
    question : string;
  begin
    msg := TJSONObject.ParseJSONValue(Text) as TJSONObject;
    if msg.TryGetValue('message_type', Value) then
    begin
      if (value = 'FinalTranscript') and (msg.Values['text'].Value<>'') and
        (Form3.Mediaplayer1.Mode <> mpPlaying) then
      begin
         question := msg.Values['text'].Value;
         Form3.Memo1.Lines.Add(question);

         response := TOpenAI.AskChatGPT(question, 'text-davinci-003');
         Form3.Memo2.Lines.Text := response;
         Form3.Memo2.Update;
         Form3.StreamOut1.Stop(False);
         //Form3.AudioProcessor1._Pause;
         Form3.FmemStream.Clear;
         Sleep(100);
         Form3.PlayTextWithSelectedEngine(response);
        // Form3.StreamOut1.Resume;
      end;
    end;
  end);
end;

procedure TTSendThread.sgOnConnect(Connection: TsgcWSConnection);
begin
  TThread.Queue(nil, procedure()
  begin
    Form3.Memo1.Lines.Add('Connected');
  end);
end;


procedure TTSendThread.Add(ms: TMemoryStream);
begin
  queueItems.PushItem(ms);
end;

function TTSendThread.Base64EncodedStream(fs: TStream): string;
var
  mem : TStringStream;
begin
  mem := nil;
  Result := '';
  try
    mem := TStringStream.Create;
    if TNetEncoding.Base64String.Encode(fs, mem) > 0 then
    begin
      Result := mem.DataString;
    end;
  finally
    FreeAndNil(mem);
  end;
end;

constructor TTSendThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  queueItems := TThreadedQueue<TMemoryStream>.Create;
end;

procedure TTSendThread.Execute;
var
  m : TMemoryStream;
  mm : TMemoryStream;
  sgcWebSocketClient1 : TsgcWebSocketClient;
  msg : TJSONObject;
begin
  inherited;
  sgcWebSocketClient1 := TsgcWebSocketClient.Create(nil);
  sgcWebSocketClient1.URL := 'wss://api.assemblyai.com/v2/realtime/ws?sample_rate=16000';
  sgcWebSocketClient1.Proxy.Host := 'localhost';
  sgcWebSocketClient1.Proxy.Port := 8888;
  sgcWebSocketClient1.Proxy.Enabled := True;
  sgcWebSocketClient1.OnHandshake := sgcWebSocketClient1Handshake;
  sgcWebSocketClient1.OnMessage := sgcWebSocketClient1Message;
  sgcWebSocketClient1.OnConnect := sgOnConnect;
  sgcWebSocketClient1.NotifyEvents := Form3.sgcWebSocketClient1.NotifyEvents;
  sgcWebSocketClient1.Connect;
  Application.ProcessMessages;

  try
    mm := TMemoryStream.Create;
    while not Terminated do
    begin
      m := queueItems.PopItem;
      if mm.Size < 17000 then
      begin
        m.Position := 0;
        mm.CopyFrom(m, m.Size);
        FreeAndNil(m);
        continue;
      end;
      mm.Position := 0;
      OutputDebugString(PChar('Size:' + mm.Size.ToString));
      try
        if not sgcWebSocketClient1.Connected then
          sgcWebSocketClient1.Connect;
        msg := TJSONObject.Create;
        try
          msg.AddPair('audio_data', Base64EncodedStream(mm));
          sgcWebSocketClient1.WriteData(msg.ToJson);
        finally
          FreeAndNil(msg);
        end;

      finally
        FreeandNil(m);
        mm.Clear;
      end;
    end;
  finally
    FreeAndNil(sgcWebSocketClient1);
  end;
end;

end.
