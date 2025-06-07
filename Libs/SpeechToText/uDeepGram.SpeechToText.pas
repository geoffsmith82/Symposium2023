unit uDeepGram.SpeechToText;

interface

uses
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
  TDeepGramSendThread = class(TBaseSendThread)
  strict private
    FDeepGram_Key : string;
    FWebSocket : TSslWebSocketCli;
    procedure WSOnRecv(Sender: TSslWebSocketCli; const APacket: String; var AFrame: TWebSocketReceivedFrame);
    procedure WSOnConnected(Sender: TObject);
    procedure WSOnDisconnected(Sender: TObject);
  private
    procedure SetupWebSocket;
  public
    procedure WriteData(data: string); override;
    procedure WriteDataStream(m: TStream); override;
    procedure Execute; override;
    constructor Create(CreateSuspended: Boolean; const deepgram_key: string); reintroduce;
  end;

  TDeepGramRecognition = class(TBaseSpeechRecognition)
  public
    procedure Resume; override;
    procedure Finish; override;
    constructor Create(const AResourceKey: string);
    destructor Destroy; override;
  end;

implementation

uses
  FMX.Types
  ;

{ TTSendThread }

procedure TDeepGramSendThread.WSOnRecv(Sender: TSslWebSocketCli;
  const APacket: String; var AFrame: TWebSocketReceivedFrame);
begin
  TThread.Queue(nil, procedure()
  var
    msg : TJSONObject;
    channel : TJSONObject;
    alternativeObj : TJSONObject;
    alternativesArray : TJSONArray;
    value : string;
    finalValue : string;
  begin
    msg := TJSONObject.ParseJSONValue(APacket) as TJSONObject;
    if msg.TryGetValue('speech_final', Value) then
    begin
      if Assigned(OnHandleSpeechRecognitionCompletion) and (Value='true') then
      begin
        if msg.TryGetValue('channel', channel) then
        begin
          if channel.TryGetValue('alternatives', alternativesArray) then
          begin
            Log.d(alternativesArray.ToJSON);
            if alternativesArray.Count > 0 then
            begin
              alternativeObj := alternativesArray[0] as TJSONObject;
              finalValue := alternativeObj.Values['transcript'].Value;
              if finalValue.IsEmpty then
                Exit;
              OnHandleSpeechRecognitionCompletion(finalValue);
            end;
          end;
        end;
      end;
    end;
  end);
end;

procedure TDeepGramSendThread.WSOnConnected(Sender: TObject);
begin
  TThread.Queue(nil, procedure()
  begin
    if Assigned(OnConnect) then
    begin
      OnConnect(Sender);
    end;
  end);
end;

procedure TDeepGramSendThread.WSOnDisconnected(Sender: TObject);
begin
  TThread.Queue(nil, procedure()
  begin
    if Assigned(OnDisconnect) then
    begin
      OnDisconnect(Sender);
    end;
  end);
end;

constructor TDeepGramSendThread.Create(CreateSuspended: Boolean; const deepgram_key: string);
begin
  inherited Create(CreateSuspended);
  FDeepGram_Key := deepgram_key;
end;

procedure TDeepGramSendThread.SetupWebSocket;
begin
  FreeAndNil(FWebSocket);
  FWebSocket := TSslWebSocketCli.Create(nil);
  FWebSocket.URL := 'https://api.deepgram.com/v1/listen?sample_rate=16000&encoding=linear16';
  FWebSocket.ExtraHeaders.Add('Authorization: Token ' + FDeepGram_Key);
  FWebSocket.ExtraHeaders.Add('Origin: api.deepgram.com');
  FWebSocket.Connection := 'Upgrade';
  FWebSocket.OnWSFrameRcvd := WSOnRecv;
  FWebSocket.OnWSConnected := WSOnConnected;
  FWebSocket.OnWSDisconnected := WSOnDisconnected;
  FWebSocket.WSConnect;
end;

procedure TDeepGramSendThread.WriteData(data: string);
begin
  if Assigned(FWebSocket) then
    FWebSocket.WSSendText(nil, data);
end;

procedure TDeepGramSendThread.WriteDataStream(m: TStream);
begin
  FWebSocket.WSSendBinaryStream(nil, m);
end;

procedure TDeepGramSendThread.Execute;
var
  m : TMemoryStream;
  mm : TMemoryStream;
begin
  inherited;
  NameThreadForDebugging('DeepGram.Ai');
  FWebSocket := nil;
  try
    mm := TMemoryStream.Create;
    while not Terminated do
    begin
      m := FQueueItems.PopItem;
      if mm.Size < 3000 then
      begin
        m.Position := 0;
        mm.CopyFrom(m, m.Size);
        FreeAndNil(m);
        continue;
      end;
      mm.Position := 0;
      Log.d('Size:' + mm.Size.ToString);
      try
        if not Assigned(FWebSocket) or not FWebSocket.Connected then
          SetupWebSocket;
        WriteDataStream(mm);
        FWebSocket.ProcessMessages;
      finally
        FreeandNil(m);
        mm.Clear;
      end;
    end;
  finally
    FreeAndNil(mm);
    FreeAndNil(FWebSocket);
  end;
end;

{ TDeepGramRecognition }

constructor TDeepGramRecognition.Create(const AResourceKey: string);
begin
  inherited Create(AResourceKey, '');
  FSendThread := TDeepGramSendThread.Create(True, AResourceKey);
end;

destructor TDeepGramRecognition.Destroy;
var
  x : TExitStream;
begin
  x := TExitStream.Create;
  FSendThread.Add(x);
  FSendThread.Terminate;
  FSendThread.WaitFor;
  inherited;
end;

procedure TDeepGramRecognition.Finish;
begin
  inherited;
end;

procedure TDeepGramRecognition.Resume;
begin
  inherited;
  FSendThread.Resume;
end;

end.
