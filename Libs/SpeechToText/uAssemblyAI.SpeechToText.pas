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
  sgcBase_Classes,
  sgcSocket_Classes,
  sgcTCP_Classes,
  sgcWebSocket_Classes,
  sgcWebSocket_Classes_Indy,
  sgcWebSocket_Client,
  sgcWebSocket
  ;

type
  TOnHandleMessage = procedure(const msg: string) of object;
  TOnConnect = procedure(Connection: TsgcWSConnection) of object;

  TAssemblyAiSendThread = class(TThread)
  private
    FAssemblyai_key : string;
    FQueueItems : TThreadedQueue<TMemoryStream>;
    procedure sgcWebSocketClient1Handshake(Connection: TsgcWSConnection; var Headers: TStringList);
    procedure sgcWebSocketClient1Message(Connection: TsgcWSConnection; const Text: string);
    procedure sgOnConnect(Connection: TsgcWSConnection);
    procedure sgOnDisconnect(Connection: TsgcWSConnection; Code: Integer);
    function Base64EncodedStream(fs: TStream): string;
  public
    procedure Execute; override;
    procedure Add(ms: TMemoryStream);
    constructor Create(CreateSuspended: Boolean; const assemblyai_key: string);
    destructor Destroy; override;
  public
    OnHandleMessage: TOnHandleMessage;
    OnConnect: TOnConnect;
    OnDisconnect: TOnConnect;
  end;

implementation


{ TTSendThread }

procedure TAssemblyAiSendThread.sgcWebSocketClient1Handshake(Connection: TsgcWSConnection; var Headers: TStringList);
begin
  Headers.Add('Authorization: ' + FAssemblyai_key);
end;

procedure TAssemblyAiSendThread.sgcWebSocketClient1Message(Connection: TsgcWSConnection; const Text: string);
begin
  TThread.Queue(nil, procedure()
  var
    msg : TJSONObject;
    value : string;
  begin
    msg := TJSONObject.ParseJSONValue(Text) as TJSONObject;
    if msg.TryGetValue('message_type', Value) then
    begin
      if Assigned(OnHandleMessage) then
      begin
        OnHandleMessage(Text);
      end;
    end;
  end);
end;

procedure TAssemblyAiSendThread.sgOnConnect(Connection: TsgcWSConnection);
begin
  TThread.Queue(nil, procedure()
  begin
    if Assigned(OnConnect) then
    begin
      OnConnect(Connection);
    end;    
  end);
end;

procedure TAssemblyAiSendThread.sgOnDisconnect(Connection: TsgcWSConnection; Code: Integer);
begin
  TThread.Queue(nil, procedure
  begin
    if Assigned(OnDisconnect) then
    begin
      OnDisconnect(Connection);
    end;
  end);
end;

procedure TAssemblyAiSendThread.Add(ms: TMemoryStream);
begin
  FQueueItems.PushItem(ms);
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
  FQueueItems := TThreadedQueue<TMemoryStream>.Create;
end;

destructor TAssemblyAiSendThread.Destroy;
begin
  FreeAndNil(FQueueItems);
  inherited;
end;

procedure TAssemblyAiSendThread.Execute;
var
  m : TMemoryStream;
  mm : TMemoryStream;
  sgcWebSocketClient1 : TsgcWebSocketClient;
  msg : TJSONObject;
begin
  inherited;
  sgcWebSocketClient1 := TsgcWebSocketClient.Create(nil);
  try
    sgcWebSocketClient1.URL := 'wss://api.assemblyai.com/v2/realtime/ws?sample_rate=16000';
    sgcWebSocketClient1.Proxy.Host := 'localhost';
    sgcWebSocketClient1.Proxy.Port := 8888;
    sgcWebSocketClient1.Proxy.Enabled := True;
    sgcWebSocketClient1.OnHandshake := sgcWebSocketClient1Handshake;
    sgcWebSocketClient1.OnMessage := sgcWebSocketClient1Message;
    sgcWebSocketClient1.OnConnect := sgOnConnect;
    sgcWebSocketClient1.OnDisconnect := sgOnDisconnect;
    sgcWebSocketClient1.Connect;



    mm := TMemoryStream.Create;
    while not Terminated do
    begin
      m := FQueueItems.PopItem;
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
    FreeAndNil(mm);
    FreeAndNil(sgcWebSocketClient1);
    FreeAndNil(FQueueItems);
  end;
end;

end.
