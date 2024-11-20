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

  TChatAttachment = class
    mimeType : string;
    data: string;
    url: string;
  end;

{$M+}
  TFunctionMessage = class(TChatMessage)
  private
    FId : string;
  public
    function_name: string;
    constructor Create;
    destructor Destroy; override;
    function AsJSON: TJSONObject; override;
  published
    property Id : string read FId write FId;
  end;

  TFunctionCallMessage = class(TChatMessage)
  private
    FJSON : TJSONObject;
  public
    constructor Create(function_call_json: TJSONArray);
    destructor Destroy; override;
    function AsJSON: TJSONObject; override;
  end;
{$M-}

{$M+}
  TChatVisionMessage = class(TChatMessage)
  private
    FImageURLs : TObjectList<TChatAttachment>;
  public
    MessageType: string;
    procedure AddImageURL(const Url: string);
    procedure AddImageFile(const Filename: string; const mimeType: string);
    procedure AddImageStream(stream: TStream; const mimeType: string);
    constructor Create;
    destructor Destroy; override;
    function AsJSON: TJSONObject; override;
  published
    property Attachments: TObjectList<TChatAttachment> read FImageURLs;
  end;
{$M-}

  TClaudeVisionMessage = class(TChatVisionMessage)
    function AsJSON: TJSONObject; override;
  end;

  TChatToolCall = record
    Id : String;
    &Type : string;
    FunctionCall: string;
    Arguments: string;
  end;


  TChatResponse = record
    Content : string;
    Completion_Tokens : Cardinal;
    Finish_Reason: string;
    System_Fingerprint: string;
    Prompt_Tokens : Cardinal;
    Total_Tokens : Cardinal;
    Tool_Calls : TArray<TChatToolCall>;
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

{$M+}
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
{$M-}

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
  attachment: TChatAttachment;
begin
  ds := TStringStream.Create;
  try
    TNetEncoding.Base64String.Encode(stream, ds);
    attachment := TChatAttachment.Create;
    attachment.mimeType := mimeType;
    attachment.url := '';
    attachment.data := ds.DataString;
    FImageURLs.Add(attachment);
  finally
    FreeAndNil(ds);
  end;
end;

procedure TChatVisionMessage.AddImageURL(const Url: string);
var
  attachment: TChatAttachment;
begin
  attachment := TChatAttachment.Create;
  attachment.mimeType := '';
  attachment.data := '';
  attachment.url := Url;
  FImageURLs.Add(attachment);
end;

function TChatVisionMessage.AsJSON: TJSONObject;
var
  LJSONMsg : TJSONObject;
  LJSONSubMsgArray : TJSONArray;
  LJSONSubTextMsg : TJSONObject;
  LJSONSubImageMsg : TJSONObject;
  LJSONImageURLData : TJSONObject;
  i: Integer;
  urlData : string;
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
      if not FImageURLs[i].url.IsEmpty then
        LJSONImageURLData.AddPair('url', FImageURLs[i].url)
      else
      begin
        urlData := 'data:' + FImageURLs[i].mimeType + ';base64,' + FImageURLs[i].data;
        LJSONImageURLData.AddPair('url', urlData);
      end;

      LJSONSubImageMsg.AddPair('image_url', LJSONImageURLData);
      LJSONSubMsgArray.Add(LJSONSubImageMsg);
    end;

    LJSONMsg.AddPair('content', LJSONSubMsgArray);
  end;

  Result := LJSONMsg;
end;

constructor TChatVisionMessage.Create;
begin
  FImageURLs := TObjectList<TChatAttachment>.Create;
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

{ TClaudeVisionMessage }

function TClaudeVisionMessage.AsJSON: TJSONObject;
var
  msg : TJSONObject;
  source: TJSONObject;
  contentObj: TJSONObject;
  contentArr: TJSONArray;
  I: Integer;
begin
  msg := TJSONObject.Create;
  if FImageURLs.Count = 0 then
  begin
    msg.AddPair('role', Role);
    msg.AddPair('content', content);
  end
  else
  begin
    contentArr := TJSONArray.Create;
    for I := 0 to FImageURLs.Count -1 do
    begin
      contentObj:= TJSONObject.Create;
      contentObj.AddPair('type', 'image');
      source := TJSONObject.Create;
      source.AddPair('type', 'base64');
      source.AddPair('media_type', FImageURLs[i].mimeType);
      source.AddPair('data', FImageURLs[i].data);
      contentObj.AddPair('source', source);
      contentArr.AddElement(contentObj);
    end;
    contentObj:= TJSONObject.Create;
    contentObj.AddPair('type', 'text');
    contentObj.AddPair('text', content);
    contentArr.AddElement(contentObj);
    msg.AddPair('role', 'user');
    msg.AddPair('content', contentArr);
  end;

  Result := msg;
end;

{ TFunctionMessage }

function TFunctionMessage.AsJSON: TJSONObject;
var
  LJSONMsg : TJSONObject;
begin
  LJSONMsg := TJSONObject.Create;
  LJSONMsg.AddPair('role', 'tool');
  LJSONMsg.AddPair('tool_call_id', FId);
  LJSONMsg.AddPair('name', function_name);
  LJSONMsg.AddPair('content', Content);

  Result := LJSONMsg;
end;

constructor TFunctionMessage.Create;
begin

end;

destructor TFunctionMessage.Destroy;
begin

  inherited;
end;

{ TFunctionCallMessage }

function TFunctionCallMessage.AsJSON: TJSONObject;
begin
  Result := FJSON.Clone as TJSONObject;
  if not Content.IsEmpty then
    Result.AddPair('content', Content);
end;

constructor TFunctionCallMessage.Create(function_call_json: TJSONArray);
begin
  FJSON := TJSONObject.Create;
  FJSON.AddPair('tool_calls', function_call_json);
  FJSON.AddPair('role', 'assistant');
end;

destructor TFunctionCallMessage.Destroy;
begin
  FreeAndNil(FJSON);
  inherited;
end;

end.
