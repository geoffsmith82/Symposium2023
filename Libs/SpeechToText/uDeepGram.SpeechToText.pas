unit uDeepGram.SpeechToText;

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
  TDeepGramSendThread = class(TBaseSendThread)
  strict private
    FDeepGram_Key : string;
    FWebSocket : TSslWebSocketCli;
    procedure WSOnRecv(Sender: TSslWebSocketCli; const APacket: String; var AFrame: TWebSocketReceivedFrame);
    procedure WSOnConnected(Sender: TObject);
    procedure WSOnDisconnected(Sender: TObject);
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
    constructor Create(const AResourceKey, AApplicationName, AHost: string);
  end;

implementation

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
            OutputDebugString(PChar(alternativesArray.ToJSON));
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

procedure TDeepGramSendThread.WriteData(data: string);
begin
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
  FWebSocket := TSslWebSocketCli.Create(nil);
  try
    FWebSocket.URL := 'https://api.deepgram.com/v1/listen?sample_rate=16000&encoding=linear16';
    FWebSocket.Proxy := 'localhost';
    FWebSocket.ProxyPort := '8888';
    FWebSocket.ExtraHeaders.Add('Authorization: Token ' + FDeepGram_Key);
    FWebSocket.ExtraHeaders.Add('Origin: api.deepgram.com');
    FWebSocket.Connection := 'Upgrade';
    FWebSocket.OnWSFrameRcvd := WSOnRecv;
    FWebSocket.OnWSConnected := WSOnConnected;
    FWebSocket.OnWSDisconnected := WSOnDisconnected;
    FWebSocket.WSConnect;

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
      OutputDebugString(PChar('Size:' + mm.Size.ToString));
      try

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

constructor TDeepGramRecognition.Create(const AResourceKey, AApplicationName, AHost: string);
begin
  inherited Create(AResourceKey, AApplicationName, AHost);
  FSendThread := TDeepGramSendThread.Create(True, AResourceKey);
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
