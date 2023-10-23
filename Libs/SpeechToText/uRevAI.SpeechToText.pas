unit uRevAI.SpeechToText;

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
  System.Net.URLClient,
  OverbyteIcsWebSocketCli,
  uBaseSpeechRecognition
  ;

type
  TRevAiSendThread = class(TBaseSendThread)
  strict private
    FRevAi_Key : string;
    FWebSocket : TSslWebSocketCli;
    procedure WSOnRecv(Sender: TSslWebSocketCli; const APacket: String; var AFrame: TWebSocketReceivedFrame);
    procedure WSOnConnected(Sender: TObject);
    procedure WSOnDisconnected(Sender: TObject);
  private
    procedure SetupWebSocket;
  public
    function CreateFinalResult(jsonA : TJSONArray):string;
    procedure WriteData(data: string); override;
    procedure WriteDataStream(m: TStream); override;
    procedure Execute; override;
    constructor Create(CreateSuspended: Boolean; const revai_key: string); reintroduce;
    destructor Destroy; override;
  end;

  TRevAiRecognition = class(TBaseSpeechRecognition)
  public
    procedure Resume; override;
    procedure Finish; override;
    constructor Create(const AResourceKey: string);
    destructor Destroy; override;
  end;


implementation


{ TTSendThread }

procedure TRevAiSendThread.WriteData(data: string);
begin
  if Assigned(FWebSocket) then
    FWebSocket.WSSendText(nil, data);
end;

procedure TRevAiSendThread.WriteDataStream(m: TStream);
begin
  inherited;
  FWebSocket.WSSendBinaryStream(nil, m);
end;

procedure TRevAiSendThread.WSOnConnected(Sender: TObject);
begin
  TThread.Queue(nil, procedure()
  begin
    if Assigned(OnConnect) then
    begin
      OnConnect(Sender);
    end;
  end);
end;

procedure TRevAiSendThread.WSOnDisconnected(Sender: TObject);
begin
  TThread.Queue(nil, procedure
  begin
    if Assigned(OnDisconnect) then
    begin
      OnDisconnect(Sender);
    end;
  end);
end;

constructor TRevAiSendThread.Create(CreateSuspended: Boolean; const revai_key: string);
begin
  inherited Create(CreateSuspended);
  FRevAi_Key := revai_key;
end;

destructor TRevAiSendThread.Destroy;
begin
  inherited;
end;

procedure TRevAiSendThread.SetupWebSocket;
var
  wsURL : TURI;
  params : TStringList;
  param : string;
  url : string;
begin
  FreeAndNil(FWebSocket);
  FWebSocket := TSslWebSocketCli.Create(nil);

  params := TStringList.Create;
  try
  params.Add('audio/x-raw');
  params.AddPair('layout', 'interleaved');
  params.AddPair('rate', '16000');
  params.AddPair('format', 'S16LE');
  params.AddPair('channels', '1');

  for param in params do
  begin
    url := url + param + ';';
  end;

  wsURL := TURI.Create('https://api.rev.ai/speechtotext/v1/stream');
  wsURL.AddParameter('access_token', FRevAi_Key);
  wsURL.AddParameter('content_type', url);
  finally
    FreeAndNil(params);
  end;

  FWebSocket.URL := wsURL.ToString.TrimRight([';']);;

  FWebSocket.ExtraHeaders.Add('Origin: http://api.rev.ai');

  FWebSocket.Connection := 'Upgrade';
  FWebSocket.OnWSFrameRcvd := WSOnRecv;
  FWebSocket.OnWSConnected := WSOnConnected;
  FWebSocket.OnWSDisconnected := WSOnDisconnected;
  FWebSocket.Proxy := 'localhost';
  FWebSocket.ProxyPort := '8888';
  FWebSocket.WSConnect;
  Sleep(200);
end;

function TRevAiSendThread.CreateFinalResult(jsonA : TJSONArray):string;
var
  element: TJSONValue;
begin
  Result := '';
  for element in jsonA do
  begin
    Result := Result + (element as TJSONObject).Values['value'].Value;
  end;
end;


procedure TRevAiSendThread.WSOnRecv(Sender: TSslWebSocketCli; const APacket: String; var AFrame: TWebSocketReceivedFrame);
begin
  TThread.Queue(nil, procedure()
  var
    msg : TJSONObject;
    value : string;
    finalText : string;
  begin
    msg := TJSONObject.ParseJSONValue(APacket) as TJSONObject;
    if msg.TryGetValue('type', Value) then
    begin
      if (value = 'final') and
        Assigned(OnHandleSpeechRecognitionCompletion) then
      begin
        finalText := CreateFinalResult(msg.Values['elements'] as TJSONArray);
        OnHandleSpeechRecognitionCompletion(finalText);
      end;
    end;
  end);
end;

procedure TRevAiSendThread.Execute;
var
  m : TMemoryStream;
begin
  inherited;
  FWebSocket := nil;
  NameThreadForDebugging('Rev.Ai');
  try
    while not Terminated do
    begin
      m := FQueueItems.PopItem;
      if m.ClassName = 'TExitStream' then
      begin
        Exit;
      end;

      try
        if not Assigned(FWebSocket) or not FWebSocket.Connected then
          SetupWebSocket;
        m.Position := 0;
        WriteDataStream(m);
        FWebSocket.ProcessMessages;
      finally
        FreeandNil(m);
      end;
    end;
  finally
    FreeAndNil(FWebSocket);
    FreeAndNil(FQueueItems);
  end;
end;

{ TRevAiRecognition }

constructor TRevAiRecognition.Create(const AResourceKey: string);
begin
  inherited Create(AResourceKey, '');
  FSendThread := TRevAiSendThread.Create(True, AResourceKey);
end;

destructor TRevAiRecognition.Destroy;
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

procedure TRevAiRecognition.Finish;
begin
  inherited;
  TThread.Queue(FSendThread, procedure
  begin
  //  FSendThread.WriteData('{ "terminate_session": True }');
  end);
end;


procedure TRevAiRecognition.Resume;
begin
  inherited;
  FSendThread.Resume;
end;

end.
