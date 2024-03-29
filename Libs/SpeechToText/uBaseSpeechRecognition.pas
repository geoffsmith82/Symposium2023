unit uBaseSpeechRecognition;

interface

uses
  Classes,
  System.SysUtils,
  System.Generics.Collections
  ;

type
  TOnHandleMessage = procedure(const msg: string) of object;
  TOnConnect = procedure(Sender: TObject) of object;

  TExitStream = TMemoryStream;

type
  TBaseSendThread = class(TThread)
  strict protected
    FQueueItems : TThreadedQueue<TMemoryStream>;
  public
    OnHandleSpeechRecognitionCompletion: TOnHandleMessage;
    OnConnect: TOnConnect;
    OnDisconnect: TOnConnect;
    procedure Add(ms: TMemoryStream);
    procedure WriteData(data: string); virtual; abstract;
    procedure WriteDataStream(m: TStream); virtual; abstract;
    constructor Create(CreateSuspended: Boolean); virtual;
    destructor Destroy; override;
  end;

  TBaseSpeechRecognition = class
  protected
    FResourceKey : string;
    FSendThread : TBaseSendThread;
  private
    function GetOnHandleMessage: TOnHandleMessage;
    procedure SetOnConnect(const Value: TOnConnect);
    procedure SetOnDisconnect(const Value: TOnConnect);
    procedure SetOnHandleMessage(const Value: TOnHandleMessage);
    function GetOnConnect: TOnConnect;
    function GetOnDisconnect: TOnConnect;
  public
    constructor Create(const AResourceKey, AHost: string);
    procedure Finish; virtual; abstract;
    procedure Add(ms: TMemoryStream);
    procedure Resume; virtual; abstract;

  published
    property OnHandleSpeechRecognitionCompletion: TOnHandleMessage read GetOnHandleMessage write SetOnHandleMessage;
    property OnConnect: TOnConnect read GetOnConnect write SetOnConnect;
    property OnDisconnect: TOnConnect read GetOnDisconnect write SetOnDisconnect;
  end;

implementation

procedure TBaseSendThread.Add(ms: TMemoryStream);
begin
  FQueueItems.PushItem(ms);
end;

{ TBaseSpeechRecognition }

procedure TBaseSpeechRecognition.Add(ms: TMemoryStream);
begin
  FSendThread.Add(ms);
end;

constructor TBaseSpeechRecognition.Create(const AResourceKey, AHost: string);
begin
  FResourceKey := AResourceKey;
end;

function TBaseSpeechRecognition.GetOnConnect: TOnConnect;
begin
  Result := FSendThread.OnConnect;
end;

function TBaseSpeechRecognition.GetOnDisconnect: TOnConnect;
begin
  Result := FSendThread.OnDisconnect;
end;

function TBaseSpeechRecognition.GetOnHandleMessage: TOnHandleMessage;
begin
  Result := FSendThread.OnHandleSpeechRecognitionCompletion;
end;

procedure TBaseSpeechRecognition.SetOnConnect(const Value: TOnConnect);
begin
  FSendThread.OnConnect := Value;;
end;

procedure TBaseSpeechRecognition.SetOnDisconnect(const Value: TOnConnect);
begin
  FSendThread.OnDisconnect := Value;
end;

procedure TBaseSpeechRecognition.SetOnHandleMessage(const Value: TOnHandleMessage);
begin
  FSendThread.OnHandleSpeechRecognitionCompletion := Value;
end;

constructor TBaseSendThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  FQueueItems := TThreadedQueue<TMemoryStream>.Create;
end;

destructor TBaseSendThread.Destroy;
begin
  FreeAndNil(FQueueItems);
  inherited;
end;

end.
