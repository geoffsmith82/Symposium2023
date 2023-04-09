unit Unit3;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections,
  System.NetEncoding,
  System.JSON,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  ACS_Classes,
  ACS_DXAudio,
  ACS_Misc,
  Vcl.StdCtrls,
  ACS_Streams,
  ACS_LAME,
  sgcBase_Classes,
  sgcSocket_Classes,
  sgcTCP_Classes,
  sgcWebSocket_Classes,
  sgcWebSocket_Classes_Indy,
  sgcWebSocket_Client,
  sgcWebSocket,
  ACS_FLAC,
  ACS_WinMedia,
  ACS_smpeg,
  NewACIndicators,
  ACS_Wave,
  System.SyncObjs
  ;

type
  TTSendThread = class(TThread)
  private
    procedure sgcWebSocketClient1Handshake(Connection: TsgcWSConnection; var Headers: TStringList);
    procedure sgcWebSocketClient1Message(Connection: TsgcWSConnection;
      const Text: string);
    procedure sgOnConnect(Connection: TsgcWSConnection);
    function Base64EncodedStream(fs: TStream): string;
  public
    procedure Execute; override;
  end;

  TForm3 = class(TForm)
    DXAudioIn1: TDXAudioIn;
    AudioProcessor1: TAudioProcessor;
    Button1: TButton;
    StreamOut1: TStreamOut;
    sgcWebSocketClient1: TsgcWebSocketClient;
    Memo1: TMemo;
    Button2: TButton;
    WaveOut1: TWaveOut;
    ListBox1: TListBox;
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
  private
    { Private declarations }
    FmemStream : TMemoryStream;
    FLastSend : Int64;
    sendThread : TTSendThread;
  public
    { Public declarations }
    queueItems : TThreadedQueue<TMemoryStream>;
  end;

var
  Form3: TForm3;

implementation

{$R *.dfm}

procedure TForm3.FormCreate(Sender: TObject);
var
  i : Integer;
begin
  FmemStream := TMemoryStream.Create;
  FmemStream.SetSize(100*1024*1024);
  FLastSend := 0;

  sendThread := TTSendThread.Create(True);
  queueItems := TThreadedQueue<TMemoryStream>.Create;
{  sgcWebSocketClient1.URL := 'wss://api.deepgram.com/v1/listen?encoding=linear16&sample_rate&16000&channels=1&model=general';
  sgcWebSocketClient1.Proxy.Host := 'localhost';
  sgcWebSocketClient1.Proxy.Port := 8888;
  sgcWebSocketClient1.Connect;   }

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
  oldPos : Int64;
  mem : TMemoryStream;
begin
  TAudioProcessor(Sender).Input.GetData(Buffer, Bytes);
//  if not sgcWebSocketClient1.Connected then
//    sgcWebSocketClient1.Connect;

  mem := TMemoryStream.Create;
//  try
  mem.WriteData(Buffer, Bytes);
  mem.Position := 0;
  queueItems.PushItem(mem);
 //   sgcWebSocketClient1.WriteData(mem, bytes);

  OutputDebugString(PChar('Len ' + Bytes.ToString));
end;

procedure TForm3.AudioProcessor1GetSampleRate(Sender: TComponent; var Param: Cardinal);
begin
  Param := TAudioProcessor(Sender).Input.SampleRate;
end;

procedure TForm3.Button1Click(Sender: TObject);
begin
//  if not sgcWebSocketClient1.Connected then
//    sgcWebSocketClient1.Connect;
  sendThread.Resume;
  Sleep(2000);
  WaveOut1.Run;
end;

procedure TForm3.Button2Click(Sender: TObject);
begin
  sgcWebSocketClient1.WriteData('{ "type": "CloseStream" }');
//  StreamOut1.Stop;
  WaveOut1.Stop;
end;

{ TTSendThread }

procedure TTSendThread.sgcWebSocketClient1Handshake(Connection: TsgcWSConnection; var Headers: TStringList);
begin
  Headers.Add('Authorization: e1c1c21347b54ddb86c426b8ae4a1096');
//  Headers.Add('Authorization: Token 9c04fed810f9e03bab6bd48b5ef9681995699f4d');
end;

procedure TTSendThread.sgcWebSocketClient1Message(Connection: TsgcWSConnection; const Text: string);
begin
  TThread.Queue(nil, procedure()
  var
    msg : TJSONObject;
    value : string;
  begin
    msg := TJSONObject.ParseJSONValue(Text) as TJSONObject;
    if msg.TryGetValue('message_type', Value) then
    begin
      if value='FinalTranscript' then
         Form3.Memo1.Lines.Add(msg.Values['text'].Value);
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

procedure TTSendThread.Execute;
var
  m : TMemoryStream;
  mm : TMemoryStream;
  sgcWebSocketClient1 : TsgcWebSocketClient;
  msg : TJSONObject;
begin
  inherited;
  sgcWebSocketClient1 := TsgcWebSocketClient.Create(nil);
//  sgcWebSocketClient1.URL := 'wss://api.deepgram.com/v1/listen?encoding=linear16&sample_rate&16000&channels=1&model=general';
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
      m := Form3.queueItems.PopItem;
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
//       sgcWebSocketClient1.WriteData('{ "audio_data: "' + Base64EncodedStream(m) + '"}');
         sgcWebSocketClient1.WriteData(msg.ToJson);
       finally
         FreeAndNil(msg);
       end;

       // sgcWebSocketClient1.WriteData(m, m.Size);
      finally
        FreeandNil(m);
        mm.Clear;
//        FreeandNil(mm);
      end;
    end;
  finally
    FreeAndNil(sgcWebSocketClient1);
  end;
end;

end.
