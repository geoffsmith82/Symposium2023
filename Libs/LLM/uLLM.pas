unit uLLM;

interface

uses
  System.SysUtils,
  System.Generics.Collections
  ;

type
  TChatMessage = class
    Role: string;
    Content: string;
  end;

  TChatResponse = record
    Content : string;
    Completion_Tokens : Cardinal;
    Prompt_Tokens : Cardinal;
    Total_Tokens : Cardinal;
    Log_Id : string;
    Model : string;
  end;

  TChatSettings = record
     model : string;
     temperature : Double;
     top_p : Double;
     n : Integer;
     stop : string;
     max_tokens : Integer;
     presence_penalty : Double;
     frequency_penalty : Double;
     user : string;
  end;

  TEmbedding = TArray<Double>;
  TEmbeddings = TArray<TEmbedding>;

  TParameterDictionary = TDictionary<string, string>;

  TOnChatMessageMessageResults = procedure(ASessionID: Int64; AChatResponse: TChatResponse) of object;

  TPrompt = class
  private
    FPromptText: string;
    FParameters: TParameterDictionary;
    function ReplaceParameters: string;
    procedure SetParameter(const Key, Value: string);
    function GetParameter(const Key: string): string;
  public
    constructor Create(const APromptText: string);
    destructor Destroy; override;
    function AsString: string;
    property Parameters[const Key: string]: string read GetParameter write SetParameter;
  end;

  ELLMException = class(Exception)

  end;


  TBaseModelInfo = class
    modelName: string;
    version: string;
  end;

  TBaseLLM = class abstract
  protected
    FAPIKey : string;
    FModelInfo : TObjectList<TBaseModelInfo>;
    function GetModelInfo: TObjectList<TBaseModelInfo>; virtual; abstract;
  public
    constructor Create(const APIKey: string);
    destructor Destroy; override;
    function ChatCompletion(ChatConfig: TChatSettings; AMessages: TObjectList<TChatMessage>): TChatResponse; virtual; abstract;
    function Completion(const AQuestion: string; const AModel: string): string; virtual; abstract;
  published
    property ModelInfo: TObjectList<TBaseModelInfo> read GetModelInfo;
  end;


implementation

{ TPrompt }

constructor TPrompt.Create(const APromptText: string);
begin
  FPromptText := APromptText;
  FParameters := TParameterDictionary.Create;
end;

destructor TPrompt.Destroy;
begin
  FParameters.Free;
  inherited;
end;

function TPrompt.GetParameter(const Key: string): string;
begin
  Result := FParameters[Key];
end;

function TPrompt.AsString: string;
begin
  Result := ReplaceParameters;
end;

function TPrompt.ReplaceParameters: string;
var
  LParam: TPair<string, string>;
begin
  Result := FPromptText;
  for LParam in FParameters do
  begin
    Result := StringReplace(Result, '{' + LParam.Key + '}', LParam.Value, [rfReplaceAll, rfIgnoreCase]);
  end;
end;

procedure TPrompt.SetParameter(const Key: string; const Value: string);
begin
    FParameters.AddOrSetValue(Key, Value);
end;

{ TBaseOpenAI }

constructor TBaseLLM.Create(const APIKey: string);
begin
  FAPIKey := APIKey;
  FModelInfo := TObjectList<TBaseModelInfo>.Create;
end;

destructor TBaseLLM.Destroy;
begin
  FreeAndNil(ModelInfo);
  inherited;
end;


end.
