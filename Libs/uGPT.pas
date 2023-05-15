unit uGPT;

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

  TBaseOpenAI = class
  protected
    FAPIKey : string;
  public
    constructor Create(APIKey: string);
//    procedure ListOpenAIModels(out AModelList: TStringList);
    function SendChatMessagesToOpenAI(ChatConfig: TChatSettings; AMessages: TObjectList<TChatMessage>): TChatResponse; virtual; abstract;
//    function CallDALL_E(const prompt: string; n: Integer; size: TDALLESize): TGeneratedImagesClass;
    function AskChatGPT(const AQuestion: string; const AModel: string): string; virtual; abstract;
//    function Embeddings(const Texts: TArray<string>): TEmbeddings;
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
  Param: TPair<string, string>;
begin
  Result := FPromptText;
  for Param in FParameters do
  begin
    Result := StringReplace(Result, '{' + Param.Key + '}', Param.Value, [rfReplaceAll, rfIgnoreCase]);
  end;
end;

procedure TPrompt.SetParameter(const Key: string; const Value: string);
begin
    FParameters.AddOrSetValue(Key, Value);
end;

{ TBaseOpenAI }

constructor TBaseOpenAI.Create(APIKey: string);
begin
  FAPIKey := APIKey;
end;

end.
