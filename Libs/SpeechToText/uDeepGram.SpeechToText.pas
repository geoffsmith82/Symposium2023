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
  sgcBase_Classes,
  sgcSocket_Classes,
  sgcTCP_Classes,
  sgcWebSocket_Classes,
  sgcWebSocket_Classes_Indy,
  sgcWebSocket_Client,
  sgcWebSocket,
  uBaseSpeechRecognition
  ;

type
  TOnHandleMessage = procedure(const msg: string) of object;
  TOnConnect = procedure(Connection: TsgcWSConnection) of object;

  TDeepGramSendThread = class(TThread)
  private
    FDeepGram_Key : string;
    FQueueItems : TThreadedQueue<TMemoryStream>;
    procedure sgcWebSocketClient1Handshake(Connection: TsgcWSConnection; var Headers: TStringList);
    procedure sgcWebSocketClient1Message(Connection: TsgcWSConnection; const Text: string);
    procedure sgOnConnect(Connection: TsgcWSConnection);
  public
    procedure Execute; override;
    procedure Add(ms: TMemoryStream);
    constructor Create(CreateSuspended: Boolean; const deepgram_key: string);
  public
    sgcWebSocketClient1 : TsgcWebSocketClient;
    OnHandleMessage: TOnHandleMessage;
    OnConnect: TOnConnect;
    OnDisconnect: TOnConnect;
  end;

  TDeepGramRecognition = class(TBaseSpeechRecognition)
  private
    FSendThread : TDeepGramSendThread;
    function GetOnConnect: TOnConnect;
    function GetOnDisconnect: TOnConnect;
    function GetOnHandleMessage: TOnHandleMessage;
    procedure SetOnConnect(const Value: TOnConnect);
    procedure SetOnDisconnect(const Value: TOnConnect);
    procedure SetOnHandleMessage(const Value: TOnHandleMessage);
  public
    procedure Resume; override;
    procedure Add(ms: TMemoryStream); override;
    procedure Finish; override;
    constructor Create(const AResourceKey, AApplicationName, AHost: string);
  published
    property OnHandleMessage: TOnHandleMessage read GetOnHandleMessage write SetOnHandleMessage;
    property OnConnect: TOnConnect read GetOnConnect write SetOnConnect;
    property OnDisconnect: TOnConnect read GetOnDisconnect write SetOnDisconnect;
  end;


implementation


{ TTSendThread }

procedure TDeepGramSendThread.sgcWebSocketClient1Handshake(Connection: TsgcWSConnection; var Headers: TStringList);
begin
  Headers.Add('Authorization: Token ' + FDeepGram_Key);
end;

procedure TDeepGramSendThread.sgcWebSocketClient1Message(Connection: TsgcWSConnection; const Text: string);
begin
  TThread.Queue(nil, procedure()
  var
    msg : TJSONObject;
    value : string;
  begin
    msg := TJSONObject.ParseJSONValue(Text) as TJSONObject;
    if msg.TryGetValue('speech_final', Value) then
    begin
      if Assigned(OnHandleMessage) and (Value='True') then
      begin
        OnHandleMessage(Text);
      end;
    end;
  end);
end;

procedure TDeepGramSendThread.sgOnConnect(Connection: TsgcWSConnection);
begin
  TThread.Queue(nil, procedure()
  begin
    if Assigned(OnConnect) then
    begin
      OnConnect(Connection);
    end;    
  end);
end;

procedure TDeepGramSendThread.Add(ms: TMemoryStream);
begin
  FQueueItems.PushItem(ms);
end;

constructor TDeepGramSendThread.Create(CreateSuspended: Boolean; const deepgram_key: string);
begin
  inherited Create(CreateSuspended);
  FDeepGram_Key := deepgram_key;
  FQueueItems := TThreadedQueue<TMemoryStream>.Create;
end;

procedure TDeepGramSendThread.Execute;
var
  m : TMemoryStream;
  mm : TMemoryStream;
begin
  inherited;
  sgcWebSocketClient1 := TsgcWebSocketClient.Create(nil);
  sgcWebSocketClient1.URL := 'wss://api.deepgram.com/v1/listen?sample_rate=16000&encoding=linear16';
  sgcWebSocketClient1.Proxy.Host := 'localhost';
  sgcWebSocketClient1.Proxy.Port := 8888;
  sgcWebSocketClient1.Proxy.Enabled := True;
  sgcWebSocketClient1.OnHandshake := sgcWebSocketClient1Handshake;
  sgcWebSocketClient1.OnMessage := sgcWebSocketClient1Message;
  sgcWebSocketClient1.OnConnect := sgOnConnect;
  sgcWebSocketClient1.Connect;

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
      OutputDebugString(PChar('Size:' + mm.Size.ToString));
      try
        if not sgcWebSocketClient1.Connected then
          sgcWebSocketClient1.Connect;

        sgcWebSocketClient1.WriteData(mm);

      finally
        FreeandNil(m);
        mm.Clear;
      end;
    end;
  finally
    FreeAndNil(mm);
    FreeAndNil(sgcWebSocketClient1);
  end;
end;

{ TDeepGramRecognition }

procedure TDeepGramRecognition.Add(ms: TMemoryStream);
begin
  inherited;
  FSendThread.Add(ms);
end;

constructor TDeepGramRecognition.Create(const AResourceKey, AApplicationName, AHost: string);
begin
  inherited Create(AResourceKey, AApplicationName, AHost);
  FSendThread := TDeepGramSendThread.Create(True, AResourceKey);
end;

procedure TDeepGramRecognition.Finish;
begin
  inherited;

end;

function TDeepGramRecognition.GetOnConnect: TOnConnect;
begin
  Result := FSendThread.OnConnect;
end;

function TDeepGramRecognition.GetOnDisconnect: TOnConnect;
begin
  Result := FSendThread.OnDisconnect;
end;

function TDeepGramRecognition.GetOnHandleMessage: TOnHandleMessage;
begin
  Result := FSendThread.OnHandleMessage;
end;

procedure TDeepGramRecognition.Resume;
begin
  inherited;
  FSendThread.Resume;
end;

procedure TDeepGramRecognition.SetOnConnect(const Value: TOnConnect);
begin
  FSendThread.OnConnect := Value;
end;

procedure TDeepGramRecognition.SetOnDisconnect(const Value: TOnConnect);
begin
  FSendThread.OnDisconnect := Value;
end;

procedure TDeepGramRecognition.SetOnHandleMessage(const Value: TOnHandleMessage);
begin
  FSendThread.OnHandleMessage := Value;
end;

end.
