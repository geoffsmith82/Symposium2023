unit uLLM;

interface

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.Generics.Collections
  ;

type
  TChatMessage = class
    Role: string;
    Content: string;
    function AsJSON: TJSONObject; virtual;
  end;

  TChatVisionMessage = class(TChatMessage)
  private
    FImageURLs : TStringList;
  public
    MessageType: string;
    procedure AddImageURL(const Url: string);
    procedure AddImageFile(const Filename: string; const mimeType: string);
    procedure AddImageStream(stream: TStream; const mimeType: string);
    constructor Create;
    destructor Destroy; override;
    function AsJSON: TJSONObject; override;
  published
    property ImageURLs: TStringList read FImageURLs;
  end;



  TChatResponse = record
    Content : string;
    Completion_Tokens : Cardinal;
    Finish_Reason: string;
    System_Fingerprint: string;
    Prompt_Tokens : Cardinal;
    Total_Tokens : Cardinal;
    Log_Id : string;
    Model : string;
  end;

  TChatSettings = record
     model : string;
     temperature : Double;
     top_p : Double;
     seed : Integer;
     json_mode : Boolean;
     n : Integer;
     stop : string;
     max_tokens : Integer;
     presence_penalty : Double;
     frequency_penalty : Double;
     user : string;
  end;

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

uses
  netencoding
  ;

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


{ TChatVisionMessage }

procedure TChatVisionMessage.AddImageFile(const filename: string; const mimeType: string);
var
  fs : TFileStream;
begin
  fs := nil;
  try
    fs := TFileStream.Create(filename, fmOpenRead);
    AddImageStream(fs, mimeType);
  finally
    FreeAndNil(fs);
  end;
end;

procedure TChatVisionMessage.AddImageStream(stream: TStream; const mimeType: string);
var
  ds : TStringStream;
begin
  ds := TStringStream.Create;
  try
    TNetEncoding.Base64String.Encode(stream, ds);
    FImageURLs.Add('data:' + mimeType + ';base64,' + ds.DataString);
  finally
    FreeAndNil(ds);
  end;
end;

procedure TChatVisionMessage.AddImageURL(const Url: string);
begin
  FImageURLs.Add(Url);
end;

function TChatVisionMessage.AsJSON: TJSONObject;
var
  LJSONMsg : TJSONObject;
  LJSONSubMsgArray : TJSONArray;
  LJSONSubTextMsg : TJSONObject;
  LJSONSubImageMsg : TJSONObject;
  LJSONImageURLData : TJSONObject;
  i: Integer;
begin
  LJSONMsg := TJSONObject.Create;
  LJSONMsg.AddPair('role', Role.ToLower);
  if FImageURLs.Count = 0 then
  begin
    LJSONMsg.AddPair('content', Content);
  end
  else
  begin
    LJSONSubMsgArray := TJSONArray.Create;
    // Add text part of msg
    LJSONSubTextMsg := TJSONObject.Create;
    LJSONSubTextMsg.AddPair('type', 'text');
    LJSONSubTextMsg.AddPair('text', Content);
    LJSONSubMsgArray.Add(LJSONSubTextMsg);
    // Add images part of msg
    for i := 0 to FImageURLs.Count - 1 do
    begin
      LJSONSubImageMsg := TJSONObject.Create;
      LJSONSubImageMsg.AddPair('type', 'image_url');
      LJSONImageURLData := TJSONObject.Create;
      LJSONImageURLData.AddPair('url', FImageURLs[i]);
      LJSONSubImageMsg.AddPair('image_url', LJSONImageURLData);
      LJSONSubMsgArray.Add(LJSONSubImageMsg);
    end;

    LJSONMsg.AddPair('content', LJSONSubMsgArray);
  end;

  Result := LJSONMsg;
end;

constructor TChatVisionMessage.Create;
begin
  FImageURLs := TStringList.Create;
end;

destructor TChatVisionMessage.Destroy;
begin
  FreeAndNil(FImageURLs);
  inherited;
end;

{ TChatMessage }

function TChatMessage.AsJSON: TJSONObject;
var
  LJSONMsg : TJSONObject;
begin
  LJSONMsg := TJSONObject.Create;
  LJSONMsg.AddPair('role', Role.ToLower);
  LJSONMsg.AddPair('content', Content);
  Result := LJSONMsg;
end;

end.
