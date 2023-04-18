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

  TDeepGramSendThread = class(TBaseSendThread)
  private
    FDeepGram_Key : string;
    sgcWebSocketClient1 : TsgcWebSocketClient;
    procedure sgcWebSocketClient1Handshake(Connection: TsgcWSConnection; var Headers: TStringList);
    procedure sgcWebSocketClient1Message(Connection: TsgcWSConnection; const Text: string);
    procedure sgOnConnect(Connection: TsgcWSConnection);
  public
    procedure WriteData(data: string); override;
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

procedure TDeepGramSendThread.sgcWebSocketClient1Handshake(Connection: TsgcWSConnection; var Headers: TStringList);
begin
  Headers.Add('Authorization: Token ' + FDeepGram_Key);
end;

procedure TDeepGramSendThread.sgcWebSocketClient1Message(Connection: TsgcWSConnection; const Text: string);
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
    msg := TJSONObject.ParseJSONValue(Text) as TJSONObject;
    if msg.TryGetValue('speech_final', Value) then
    begin
      if Assigned(OnHandleSpeechRecognitionCompletion) and (Value='true') then
      begin
        if msg.TryGetValue('channel', channel) then
        begin
          if channel.TryGetValue('alternatives', alternativesArray) then
          begin
     //       if alternativesObj.TryGetValue( then
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

constructor TDeepGramSendThread.Create(CreateSuspended: Boolean; const deepgram_key: string);
begin
  inherited Create(CreateSuspended);
  FDeepGram_Key := deepgram_key;
end;

procedure TDeepGramSendThread.WriteData(data: string);
begin
  sgcWebSocketClient1.WriteData(data);
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
