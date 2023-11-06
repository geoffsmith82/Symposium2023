unit uAssemblyAI.SpeechToText;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.NetEncoding,
  System.IniFiles,
  System.JSON,
  System.SyncObjs,
  OverbyteIcsWebSocketCli,
  uBaseSpeechRecognition
  ;

type
  TAssemblyAiSendThread = class(TBaseSendThread)
  strict private
    FAssemblyai_key : string;
    FWebSocket : TSslWebSocketCli;
    function Base64EncodedStream(fs: TStream): string;
    procedure WSOnRecv(Sender: TSslWebSocketCli; const APacket: String; var AFrame: TWebSocketReceivedFrame);
    procedure WSOnConnected(Sender: TObject);
    procedure WSOnDisconnected(Sender: TObject);
    procedure WSOnSent(Sender: TSslWebSocketCli; var AFrame: TWebSocketOutgoingFrame);
  private
    procedure SetupWebSocket;
  public
    procedure WriteData(data: string); override;
    procedure WriteDataStream(m: TStream); override;
    procedure Execute; override;
    constructor Create(CreateSuspended: Boolean; const assemblyai_key: string); reintroduce;
    destructor Destroy; override;
  end;

  TAssemblyAiRecognition = class(TBaseSpeechRecognition)
  public
    constructor Create(const AResourceKey: string);
    destructor Destroy; override;
    procedure Resume; override;
    procedure Finish; override;
  end;


implementation


{ TTSendThread }

procedure TAssemblyAiSendThread.WriteData(data: string);
begin
  if Assigned(FWebSocket) then
    FWebSocket.WSSendText(nil, data);
end;

procedure TAssemblyAiSendThread.WriteDataStream(m: TStream);
begin
  raise EProgrammerNotFound.Create('WriteDataStream not used');
end;

procedure TAssemblyAiSendThread.WSOnConnected(Sender: TObject);
begin
  TThread.Queue(nil, procedure()
  begin
    if Assigned(OnConnect) then
    begin
      OnConnect(Sender);
    end;    
  end);
end;

procedure TAssemblyAiSendThread.WSOnDisconnected(Sender: TObject);
begin
  TThread.Queue(nil, procedure
  begin
    if Assigned(OnDisconnect) then
    begin
      OnDisconnect(Sender);
    end;
  end);
end;

function TAssemblyAiSendThread.Base64EncodedStream(fs: TStream): string;
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

constructor TAssemblyAiSendThread.Create(CreateSuspended: Boolean; const assemblyai_key: string);
begin
  inherited Create(CreateSuspended);
  FAssemblyai_key := assemblyai_key;
end;

destructor TAssemblyAiSendThread.Destroy;
begin
  inherited;
end;

procedure TAssemblyAiSendThread.SetupWebSocket;
begin
  FreeAndNil(FWebSocket);
  FWebSocket := TSslWebSocketCli.Create(nil);
  FWebSocket.URL := 'https://api.assemblyai.com/v2/realtime/ws?sample_rate=16000';
  FWebSocket.ExtraHeaders.Add('Authorization: ' + FAssemblyai_key);
  FWebSocket.ExtraHeaders.Add('Origin: api.assemblyai.com');
  FWebSocket.Connection := 'Upgrade';
  FWebSocket.OnWSFrameRcvd := WSOnRecv;
  FWebSocket.OnWSFrameSent := WSOnSent;
  FWebSocket.OnWSConnected := WSOnConnected;
  FWebSocket.OnWSDisconnected := WSOnDisconnected;
  FWebSocket.WSConnect;
end;

procedure TAssemblyAiSendThread.WSOnSent(Sender: TSslWebSocketCli; var AFrame: TWebSocketOutgoingFrame);
begin
  OutputDebugString(PChar('Sent'));
end;

procedure TAssemblyAiSendThread.WSOnRecv(Sender: TSslWebSocketCli; const APacket: String; var AFrame: TWebSocketReceivedFrame);
begin
  TThread.Queue(nil, procedure()
  var
    msg : TJSONObject;
    value : string;
    finalText : string;
  begin
    msg := TJSONObject.ParseJSONValue(APacket) as TJSONObject;
    if msg.TryGetValue('message_type', Value) then
    begin
      if (value = 'FinalTranscript') and (msg.Values['text'].Value <> '') and
        Assigned(OnHandleSpeechRecognitionCompletion) then
      begin
        finalText := msg.Values['text'].Value;
        OnHandleSpeechRecognitionCompletion(finalText);
      end;
    end;
  end);
end;

procedure TAssemblyAiSendThread.Execute;
var
  m : TMemoryStream;
  mm : TMemoryStream;
  msg : TJSONObject;
begin
  inherited;
  FWebSocket := nil;
  NameThreadForDebugging('Assembly.Ai');
  try
    mm := TMemoryStream.Create;
    while not Terminated do
    begin
      m := FQueueItems.PopItem;
      if m.ClassName = 'TExitStream' then
      begin
        Exit;
      end;
      if mm.Size < 17000 then // Assembly AI needs chunks of audio of 1 second in size minimum
      begin
        m.Position := 0;
        mm.CopyFrom(m, m.Size);
        FreeAndNil(m);
        continue;
      end;
      mm.Position := 0;
      OutputDebugString(PChar('Size:' + mm.Size.ToString));
      try
        if not Assigned(FWebSocket) or not FWebSocket.Connected then
          SetupWebSocket;

        msg := TJSONObject.Create;
        try
          msg.AddPair('audio_data', Base64EncodedStream(mm));
          WriteData(msg.ToJSON);
          FWebSocket.ProcessMessages;
        finally
          FreeAndNil(msg);
        end;

      finally
        FreeandNil(m);
        mm.Clear;
      end;
    end;
  finally
    FreeAndNil(mm);
    FreeAndNil(FWebSocket);
    FreeAndNil(FQueueItems);
  end;
end;

{ TAssemblyAiRecognition }

constructor TAssemblyAiRecognition.Create(const AResourceKey: string);
begin
  inherited Create(AResourceKey, '');
  FSendThread := TAssemblyAiSendThread.Create(True, AResourceKey);
end;

destructor TAssemblyAiRecognition.Destroy;
var
  x : TExitStream;
begin
  x := TExitStream.Create;
  FSendThread.Add(x);
  FSendThread.Terminate;
  FSendThread.WaitFor;
  FreeAndNil(FSendThread);
  inherited;
end;

procedure TAssemblyAiRecognition.Finish;
begin
  inherited;
  TThread.Queue(FSendThread, procedure
  begin
    FSendThread.WriteData('{ "terminate_session": True }');
  end);
end;


procedure TAssemblyAiRecognition.Resume;
begin
  inherited;
  FSendThread.Resume;
end;


end.
