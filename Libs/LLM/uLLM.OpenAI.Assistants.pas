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
  TOpenAIAssistant = class
  private
    FAPIKey: string;
    FRESTClient: TRESTClient;
    FRESTRequest: TRESTRequest;
    FRESTResponse: TRESTResponse;
    procedure ResetRequestToDefault;
  public
    FAssistantID: string;
    FThreadID: string;
    constructor Create(const APIKey: string);
    destructor Destroy; override;

    function CreateAssistant(model: string; name: string; instructions: string): string;
    function UploadFile(const FileName: string): string;
    function CreateThread: string;
    function AddMessage(const Content: string; const FileID: string): Boolean;
    function CreateRun: string;
    function GetRunStatus(const RunID: string): string;
    function GetThreadMessages: TJSONArray;
  end;

  function ExtractJsonString(const Input: string): string;

implementation

constructor TOpenAIAssistant.Create(const APIKey: string);
begin
  FAPIKey := APIKey;
  FRESTClient := TRESTClient.Create('https://api.openai.com/v1');
  FRESTResponse := TRESTResponse.Create(nil);
  FRESTRequest := TRESTRequest.Create(nil);
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
  inherited;
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

function TOpenAIAssistant.UploadFile(const FileName: string): string;
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

function TOpenAIAssistant.CreateThread: string;
begin
  ResetRequestToDefault;
  FRESTRequest.Method := rmPOST;
  FRESTRequest.Resource := 'threads';
  FRESTRequest.Execute;
  Result := TJSONObject.ParseJSONValue(FRESTResponse.Content).GetValue<string>('id');
  FThreadID := Result;
end;

function TOpenAIAssistant.AddMessage(const Content: string; const FileID: string): Boolean;
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

function TOpenAIAssistant.CreateRun: string;
var
  JsonBody: TJSONObject;
begin
  ResetRequestToDefault;
  FRESTRequest.Method := rmPOST;
  FRESTRequest.Resource := 'threads/' + FThreadID + '/runs';
  JsonBody := TJSONObject.Create;
  try
    JsonBody.AddPair('assistant_id', FAssistantID);
    FRESTRequest.AddBody(JsonBody.ToJSON, TRESTContentType.ctAPPLICATION_JSON);
    FRESTRequest.Execute;
    Result := TJSONObject.ParseJSONValue(FRESTResponse.Content).GetValue<string>('id');
  finally
    JsonBody.Free;
  end;
end;

function TOpenAIAssistant.GetRunStatus(const RunID: string): string;
begin
  ResetRequestToDefault;
  FRESTRequest.Method := rmGET;
  FRESTRequest.Resource := 'threads/' + FThreadID + '/runs/' + RunID;
  FRESTRequest.Execute;
  Result := TJSONObject.ParseJSONValue(FRESTResponse.Content).GetValue<string>('status');
end;

function TOpenAIAssistant.GetThreadMessages: TJSONArray;
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




end.
