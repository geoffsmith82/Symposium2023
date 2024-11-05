unit uLLM.OpenAI.Assistants;

interface

uses
  System.SysUtils,
  System.StrUtils,
  System.Classes,
  System.JSON,
  System.Generics.Collections,
  Windows,
  REST.Client,
  REST.Types,
  System.IOUtils;

type
{$M+}
  TOpenAIRun = class
  private
    FAPIKey: string;
    FAssistantID: string;
    FRESTClient: TRESTClient;
    FRESTRequest: TRESTRequest;
    FRESTResponse: TRESTResponse;
    FThreadID : string;
    FRunID: string;
    procedure ResetRequestToDefault;
  public

    function GetRunStatus: string;

    constructor Create(const APIKey: string; const inAssistantID: string; inThreadId: string; inRunId: string);
    destructor Destroy; override;
  end;

  TOpenAIRunList = class(TObjectList<TOpenAIRun>)
  private
    FAPIKey: string;
    FAssistantID: string;
    FThreadID: string;
    FRESTClient: TRESTClient;
    FRESTRequest: TRESTRequest;
    FRESTResponse: TRESTResponse;
    procedure ResetRequestToDefault;
  public
    function CreateRun: TOpenAIRun;
    constructor Create(const APIKey: string; const inAssistantID: string; inThreadId: string);
    destructor Destroy; override;
  end;

  TOpenAIThread = class
  private
    FAPIKey: string;
    FAssistantID: string;
    FRESTClient: TRESTClient;
    FRESTRequest: TRESTRequest;
    FRESTResponse: TRESTResponse;
    FThreadID : string;
    FRuns: TOpenAIRunList;
    procedure ResetRequestToDefault;
  public

    function AddMessage(const Content: string; const FileID: string): Boolean;

    function GetThreadMessages: TJSONArray;

    constructor Create(const APIKey: string; const inAssistantID: string; inThreadId: string);
    destructor Destroy; override;
  published
    property ThreadID: string read FThreadID;
    property Runs: TOpenAIRunList read FRuns;
  end;

  TOpenAIThreadList = class(TObjectList<TOpenAIThread>)
  private
    FAPIKey: string;
    FAssistantID: string;
    FRESTClient: TRESTClient;
    FRESTRequest: TRESTRequest;
    FRESTResponse: TRESTResponse;
    procedure ResetRequestToDefault;
  public
    function CreateThread: TOpenAIThread;
    constructor Create(const APIKey: string; const inAssistantID: string);
  end;

  TOpenAIFiles = class
  private
    FAPIKey: string;
    FRESTClient: TRESTClient;
    FRESTRequest: TRESTRequest;
    FRESTResponse: TRESTResponse;
    procedure ResetRequestToDefault;
  public
    function UploadFile(const FileName: string): string;
    constructor Create(const APIKey: string);
    destructor Destroy; override;
  end;

  TOpenAIAssistant = class
  private
    FAPIKey: string;
    FRESTClient: TRESTClient;
    FRESTRequest: TRESTRequest;
    FRESTResponse: TRESTResponse;
    FFiles : TOpenAIFiles;
    FThreads: TOpenAIThreadList;
    procedure ResetRequestToDefault;
    function GetThreads: TOpenAIThreadList;
  public
    FAssistantID: string;
    constructor Create(const APIKey: string);
    destructor Destroy; override;

    function CreateAssistant(model: string; name: string; instructions: string): string;

  published
    property Files: TOpenAIFiles read FFiles;
    property Threads: TOpenAIThreadList read GetThreads;
  end;
{$M-}

  function ExtractJsonString(const Input: string): string;

implementation

constructor TOpenAIAssistant.Create(const APIKey: string);
begin
  FAPIKey := APIKey;
  FRESTClient := TRESTClient.Create('https://api.openai.com/v1');
  FRESTResponse := TRESTResponse.Create(nil);
  FRESTRequest := TRESTRequest.Create(nil);
  FFiles := TOpenAIFiles.Create(APIKey);
end;

procedure TOpenAIAssistant.ResetRequestToDefault;
begin
  FRESTRequest.ResetToDefaults;
  FRESTRequest.Client := FRESTClient;
  FRESTRequest.Response := FRESTResponse;
  FRESTRequest.AddParameter('Authorization', 'Bearer ' + FAPIKey, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
  FRESTRequest.AddParameter('OpenAI-Beta', 'assistants=v2', TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
end;

destructor TOpenAIAssistant.Destroy;
begin
  FRESTResponse.Free;
  FRESTRequest.Free;
  FRESTClient.Free;
  FFiles.Free;
  FThreads.Free;
  inherited;
end;

function TOpenAIAssistant.GetThreads: TOpenAIThreadList;
begin
  if not Assigned(FThreads) then
    Result := TOpenAIThreadList.Create(FAPIKey, FAssistantID)
  else
    Result := FThreads;
end;

function TOpenAIAssistant.CreateAssistant(model: string; name: string; instructions: string): string;
var
  JsonBody: TJSONObject;
begin
  ResetRequestToDefault;
  FRESTRequest.Method := rmPOST;
  FRESTRequest.Resource := 'assistants';
  JsonBody := TJSONObject.Create;
  try
    JsonBody.AddPair('model', model);
    JsonBody.AddPair('name', name);
    JsonBody.AddPair('instructions', instructions);

    JsonBody.AddPair('tools', TJSONArray.Create(TJSONObject.Create.AddPair('type', 'file_search')));
    OutputDebugString(PChar(JsonBody.ToJSON));
    FRESTRequest.AddBody(JsonBody.ToJSON, TRESTContentType.ctAPPLICATION_JSON);
    FRESTRequest.Execute;
    Result := TJSONObject.ParseJSONValue(FRESTResponse.Content).GetValue<string>('id');
    FAssistantID := Result;
  finally
    JsonBody.Free;
  end;
end;

constructor TOpenAIFiles.Create(const APIKey: string);
begin
  FAPIKey := APIKey;
  FRESTClient := TRESTClient.Create('https://api.openai.com/v1');
  FRESTResponse := TRESTResponse.Create(nil);
  FRESTRequest := TRESTRequest.Create(nil);
end;

destructor TOpenAIFiles.Destroy;
begin
  FRESTResponse.Free;
  FRESTRequest.Free;
  FRESTClient.Free;
  inherited;
end;

procedure TOpenAIFiles.ResetRequestToDefault;
begin
  FRESTRequest.ResetToDefaults;
  FRESTRequest.Client := FRESTClient;
  FRESTRequest.Response := FRESTResponse;
  FRESTRequest.AddParameter('Authorization', 'Bearer ' + FAPIKey, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
  FRESTRequest.AddParameter('OpenAI-Beta', 'assistants=v2', TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
end;

function TOpenAIFiles.UploadFile(const FileName: string): string;
var
  jsonResponse: TJSONObject;
begin
  ResetRequestToDefault;
  FRESTRequest.Method := rmPOST;
  FRESTRequest.Resource := 'files';
  FRESTRequest.Params.AddItem('purpose', 'assistants').Kind := pkGETorPOST;
  FRESTRequest.Params.AddItem('file', FileName, TRESTRequestParameterKind.pkFILE);
  FRESTRequest.Execute;
  jsonResponse := FRESTResponse.JSONValue as TJSONObject;
  Result := jsonResponse.GetValue<string>('id');
end;

destructor TOpenAIThread.Destroy;
begin
  FRESTResponse.Free;
  FRESTRequest.Free;
  FRESTClient.Free;
  inherited;
end;

procedure TOpenAIThread.ResetRequestToDefault;
begin
  FRESTRequest.ResetToDefaults;
  FRESTRequest.Client := FRESTClient;
  FRESTRequest.Response := FRESTResponse;
  FRESTRequest.AddParameter('Authorization', 'Bearer ' + FAPIKey, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
  FRESTRequest.AddParameter('OpenAI-Beta', 'assistants=v2', TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
end;

procedure TOpenAIThreadList.ResetRequestToDefault;
begin
  FRESTRequest.ResetToDefaults;
  FRESTRequest.Client := FRESTClient;
  FRESTRequest.Response := FRESTResponse;
  FRESTRequest.AddParameter('Authorization', 'Bearer ' + FAPIKey, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
  FRESTRequest.AddParameter('OpenAI-Beta', 'assistants=v2', TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
end;

constructor TOpenAIThread.Create(const APIKey: string; const inAssistantID: string; inThreadId:string);
begin
  FAPIKey := APIKey;
  FAssistantID := inAssistantID;
  FThreadID := inThreadId;
  FRuns := TOpenAIRunList.Create(FAPIKey, FAssistantID, FThreadID);
  FRESTClient := TRESTClient.Create('https://api.openai.com/v1');
  FRESTResponse := TRESTResponse.Create(nil);
  FRESTRequest := TRESTRequest.Create(nil);
end;

constructor TOpenAIThreadList.Create(const APIKey: string; const inAssistantID: string);
begin
  inherited Create(True);
  FAPIKey := APIKey;
  FAssistantID := inAssistantID;
  FRESTClient := TRESTClient.Create('https://api.openai.com/v1');
  FRESTResponse := TRESTResponse.Create(nil);
  FRESTRequest := TRESTRequest.Create(nil);
end;

function TOpenAIThreadList.CreateThread: TOpenAIThread;
var
  threadID : string;
begin
  ResetRequestToDefault;
  FRESTRequest.Method := rmPOST;
  FRESTRequest.Resource := 'threads';
  FRESTRequest.Execute;
  threadID := TJSONObject.ParseJSONValue(FRESTResponse.Content).GetValue<string>('id');

  Result := TOpenAIThread.Create(FAPIKey, FAssistantID, threadID);
  Add(Result);
end;

function TOpenAIThread.AddMessage(const Content: string; const FileID: string): Boolean;
var
  JsonBody: TJSONObject;
  jsonAttachment : TJSONObject;
  jsonTools : TJSONArray;
  jsonTool: TJSONObject;
begin
  ResetRequestToDefault;
  FRESTRequest.Method := rmPOST;
  FRESTRequest.Resource := 'threads/' + FThreadID + '/messages';
  JsonBody := TJSONObject.Create;
  try
    JsonBody.AddPair('role', 'user');
    JsonBody.AddPair('content', Content);
    if FileID <> '' then
    begin
      jsonAttachment := TJSONObject.Create;
      jsonAttachment.AddPair('file_id', FileId);
      jsonTools := TJSONArray.Create;
      jsonTool := TJSONObject.Create;
      jsonTool.AddPair('type', 'file_search');
      jsonTools.Add(jsonTool);


      jsonAttachment.AddPair('tools', jsonTools);
      JsonBody.AddPair('attachments', TJSONArray.Create(jsonAttachment) as TJSONValue);
    end;
    FRESTRequest.AddBody(JsonBody.ToJSON, TRESTContentType.ctAPPLICATION_JSON);
    FRESTRequest.Execute;
    Result := FRESTResponse.StatusCode = 200;
  finally
    JsonBody.Free;
  end;
end;

procedure TOpenAIRunList.ResetRequestToDefault;
begin
  FRESTRequest.ResetToDefaults;
  FRESTRequest.Client := FRESTClient;
  FRESTRequest.Response := FRESTResponse;
  FRESTRequest.AddParameter('Authorization', 'Bearer ' + FAPIKey, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
  FRESTRequest.AddParameter('OpenAI-Beta', 'assistants=v2', TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
end;

constructor TOpenAIRunList.Create(const APIKey, inAssistantID: string; inThreadId: string);
begin
  inherited Create(True);
  FApiKey := APIKey;
  FAssistantID := inAssistantID;
  FThreadID := inThreadID;
  FRESTClient := TRESTClient.Create('https://api.openai.com/v1');
  FRESTResponse := TRESTResponse.Create(nil);
  FRESTRequest := TRESTRequest.Create(nil);
end;

function TOpenAIRunList.CreateRun: TOpenAIRun;
var
  JsonBody: TJSONObject;
  RunId : string;
begin
  ResetRequestToDefault;
  FRESTRequest.Method := rmPOST;
  FRESTRequest.Resource := 'threads/' + FThreadID + '/runs';
  JsonBody := TJSONObject.Create;
  try
    JsonBody.AddPair('assistant_id', FAssistantID);
    FRESTRequest.AddBody(JsonBody.ToJSON, TRESTContentType.ctAPPLICATION_JSON);
    FRESTRequest.Execute;
    RunId := TJSONObject.ParseJSONValue(FRESTResponse.Content).GetValue<string>('id');
    Result := TOpenAIRun.Create(FAPIKey, FAssistantID, FThreadID, RunId);
  finally
    JsonBody.Free;
  end;
end;

destructor TOpenAIRunList.Destroy;
begin
  FRESTResponse.Free;
  FRESTRequest.Free;
  FRESTClient.Free;
  inherited;
end;

constructor TOpenAIRun.Create(const APIKey, inAssistantID: string; inThreadId: string; inRunId: string);
begin
  FAPIKey := APIKey;
  FAssistantID := inAssistantID;
  FRunID := inRunId;
  FThreadID := inThreadId;
  FRESTClient := TRESTClient.Create('https://api.openai.com/v1');
  FRESTResponse := TRESTResponse.Create(nil);
  FRESTRequest := TRESTRequest.Create(nil);
end;

destructor TOpenAIRun.Destroy;
begin
  FRESTResponse.Free;
  FRESTRequest.Free;
  FRESTClient.Free;
  inherited;
end;

function TOpenAIRun.GetRunStatus: string;
begin
  ResetRequestToDefault;
  FRESTRequest.Method := rmGET;
  FRESTRequest.Resource := 'threads/' + FThreadID + '/runs/' + FRunID;
  FRESTRequest.Execute;
  Result := TJSONObject.ParseJSONValue(FRESTResponse.Content).GetValue<string>('status');
end;

function TOpenAIThread.GetThreadMessages: TJSONArray;
begin
  ResetRequestToDefault;
  FRESTRequest.Method := rmGET;
  FRESTRequest.Resource := 'threads/' + FThreadID + '/messages';
  FRESTRequest.Execute;
  Result := TJSONObject.ParseJSONValue(FRESTResponse.Content).GetValue<TJSONArray>('data');
end;

function ExtractJsonString(const Input: string): string;
var
  StartPos, EndPos: Integer;
begin
  // Find the position of the starting ```json
  StartPos := Pos('```json', Input);
  if StartPos > 0 then
  begin
    // Move past the ```json and the following newline (if any)
    StartPos := StartPos + Length('```json');
  end
  else if Input.TrimLeft.StartsWith('{') then
  begin
    var jsonObj := TJSONObject.ParseJSONValue(Input);
    FreeAndNil(jsonObj);
    Result := Input;
    Exit;
  end
  else
  begin
    raise Exception.Create('No starting ```json found in the input string.');
  end;
  // Find the position of the trailing ```
  EndPos := PosEx('```', Input, StartPos);
  if EndPos = 0 then
  begin
    raise Exception.Create('No trailing ``` found in the input string.');
  end;
  // Extract the JSON part of the string
  Result := Trim(Copy(Input, StartPos, EndPos - StartPos));
end;




procedure TOpenAIRun.ResetRequestToDefault;
begin
  FRESTRequest.ResetToDefaults;
  FRESTRequest.Client := FRESTClient;
  FRESTRequest.Response := FRESTResponse;
  FRESTRequest.AddParameter('Authorization', 'Bearer ' + FAPIKey, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
  FRESTRequest.AddParameter('OpenAI-Beta', 'assistants=v2', TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
end;

end.
