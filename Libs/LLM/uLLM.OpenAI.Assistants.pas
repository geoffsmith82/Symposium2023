unit uLLM.OpenAI.Assistants;

interface

uses
  System.SysUtils,
  System.StrUtils,
  System.Classes,
  System.JSON,
  System.Generics.Collections,
  REST.Client,
  REST.Types,
  System.IOUtils,
  uLLM;

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
  public

    function GetRunStatus: string;

    constructor Create(const APIKey: string; const inAssistantID: string; const inThreadId: string; const inRunId: string);
    destructor Destroy; override;
  published
    property RunID: string read FRunID;
  end;

  TOpenAIRunList = class(TObjectList<TOpenAIRun>)
  private
    FAPIKey: string;
    FAssistantID: string;
    FThreadID: string;
    FRESTClient: TRESTClient;
    FRESTRequest: TRESTRequest;
    FRESTResponse: TRESTResponse;
  public
    function CreateRun: TOpenAIRun;
    constructor Create(const APIKey: string; const inAssistantID: string; const inThreadId: string);
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
  public

    function AddMessage(const Content: string; const FileID: string): Boolean;

    function GetThreadMessages: TJSONArray;

    constructor Create(const APIKey: string; const inAssistantID: string; const inThreadId: string);
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
  public
    function UploadFile(const FileName: string): string;
    constructor Create(const APIKey: string);
    destructor Destroy; override;
  end;

  TOpenAIFileCounts = record
     InProgress : Integer;
     Completed : Integer;
     Failed : Integer;
     Cancelled : Integer;
     Total : Integer;
  end;

  TOpenAIVectorFile = class
  private
    FId: string;
    FCreatedAt: Int64;
    FVectorStoreId: string;
    FStatus: string;
  public
    property Id: string read FId write FId;
    property CreatedAt: Int64 read FCreatedAt write FCreatedAt;
    property VectorStoreId: string read FVectorStoreId write FVectorStoreId;
    property Status: string read FStatus write FStatus;
  end;

  TOpenAIVectorFileList = class(TObjectList<TOpenAIVectorFile>)
  private
    FAPIKey: string;
    FStoreId: string;
    FRESTClient: TRESTClient;
    FRESTRequest: TRESTRequest;
    FRESTResponse: TRESTResponse;
    function GetFiles(const StoreId: string): Boolean;
  public
    procedure AddFileToStore(const FileId: string);
    constructor Create(const APIKey: string; const StoreId: string);
    destructor Destroy; override;
  end;

  TOpenAIVectorStore = class
  private
    FAPIKey: string;
    FId: string;
    FName: string;
    FDescription: string;
    CreatedAt: Int64;
    Bytes: Int64;
    FileCounts: TOpenAIFileCounts;
    FFileList: TOpenAIVectorFileList;
    function GetFileList: TOpenAIVectorFileList;
  public
    property Id: string read FId write FId;
    property Name: string read FName write FName;
    property Description: string read FDescription write FDescription;
    property FileList: TOpenAIVectorFileList read GetFileList;

    constructor Create(const APIKey: string);
    destructor Destroy; override;
  end;


  TOpenAIVectorStoreList = class(TObjectList<TOpenAIVectorStore>)
  private
    FAPIKey: string;
    FRESTClient: TRESTClient;
    FRESTRequest: TRESTRequest;
    FRESTResponse: TRESTResponse;
  public
    constructor Create(const APIKey: string);
    destructor Destroy; override;
    procedure LoadStores;
    function CreateVectorStore(const AName, AExpiresAfter: Integer; const AMetadata: TJSONObject = nil): TOpenAIVectorStore;
  end;


  TOpenAIAssistant = class
  private
    FAPIKey: string;
    FRESTClient: TRESTClient;
    FRESTRequest: TRESTRequest;
    FRESTResponse: TRESTResponse;
    FFiles : TOpenAIFiles;
    FThreads: TOpenAIThreadList;
    FVectorStores : TOpenAIVectorStoreList;
    function GetVectorStores: TOpenAIVectorStoreList;
    function GetThreads: TOpenAIThreadList;
  public
    FAssistantID: string;
    constructor Create(const APIKey: string);
    destructor Destroy; override;

    function CreateAssistant(const model: string; const name: string; const instructions: string): string;

  published
    property Files: TOpenAIFiles read FFiles;
    property VectorStores: TOpenAIVectorStoreList read GetVectorStores;
    property Threads: TOpenAIThreadList read GetThreads;
  end;
{$M-}

  function ExtractJsonString(const Input: string): string;

implementation

uses
  FMX.Types
  ;

procedure ResetRequestToDefault(FRESTRequest : TRestRequest; FRESTClient : TRESTClient; FRESTResponse : TRESTResponse; const APIkey: string);
begin
  FRESTRequest.ResetToDefaults;
  FRESTRequest.Client := FRESTClient;
  FRESTRequest.Response := FRESTResponse;
  FRESTRequest.AddParameter('Authorization', 'Bearer ' + APIkey, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
  FRESTRequest.AddParameter('OpenAI-Beta', 'assistants=v2', TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);
end;

constructor TOpenAIAssistant.Create(const APIKey: string);
begin
  FAPIKey := APIKey;
  FRESTClient := TRESTClient.Create('https://api.openai.com/v1');
  FRESTResponse := TRESTResponse.Create(nil);
  FRESTRequest := TRESTRequest.Create(nil);
  FFiles := TOpenAIFiles.Create(APIKey);
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
    FThreads := TOpenAIThreadList.Create(FAPIKey, FAssistantID);

  Result := FThreads;
end;

function TOpenAIAssistant.GetVectorStores: TOpenAIVectorStoreList;
begin
  if not Assigned(FVectorStores) then
  begin
    FVectorStores := TOpenAIVectorStoreList.Create(FAPIKey);
//    FVectorStores.
    FVectorStores.LoadStores;
  end;
  Result := FVectorStores;
end;

function TOpenAIAssistant.CreateAssistant(const model: string; const name: string; const instructions: string): string;
var
  JsonBody: TJSONObject;
begin
  ResetRequestToDefault(FRESTRequest, FRESTClient, FRESTResponse, FAPIkey);
  FRESTRequest.Method := rmPOST;
  FRESTRequest.Resource := 'assistants';
  JsonBody := TJSONObject.Create;
  try
    JsonBody.AddPair('model', model);
    JsonBody.AddPair('name', name);
    JsonBody.AddPair('instructions', instructions);

    JsonBody.AddPair('tools', TJSONArray.Create(TJSONObject.Create.AddPair('type', 'file_search')));
    Log.d(JsonBody.ToJSON);
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

function TOpenAIFiles.UploadFile(const FileName: string): string;
var
  jsonResponse: TJSONObject;
begin
  ResetRequestToDefault(FRESTRequest, FRESTClient, FRESTResponse, FAPIkey);
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

constructor TOpenAIThread.Create(const APIKey: string; const inAssistantID: string; const inThreadId:string);
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
  ResetRequestToDefault(FRESTRequest, FRESTClient, FRESTResponse, FAPIkey);
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
  ResetRequestToDefault(FRESTRequest, FRESTClient, FRESTResponse, FAPIkey);
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

constructor TOpenAIRunList.Create(const APIKey: string; const inAssistantID: string; const inThreadId: string);
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
  ResetRequestToDefault(FRESTRequest, FRESTClient, FRESTResponse, FAPIkey);
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

constructor TOpenAIRun.Create(const APIKey: string; const inAssistantID: string; const inThreadId: string; const inRunId: string);
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
  ResetRequestToDefault(FRESTRequest, FRESTClient, FRESTResponse, FAPIkey);
  FRESTRequest.Method := rmGET;
  FRESTRequest.Resource := 'threads/' + FThreadID + '/runs/' + FRunID;
  FRESTRequest.Execute;
  Result := TJSONObject.ParseJSONValue(FRESTResponse.Content).GetValue<string>('status');
end;

function TOpenAIThread.GetThreadMessages: TJSONArray;
begin
  ResetRequestToDefault(FRESTRequest, FRESTClient, FRESTResponse, FAPIkey);
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

{ TOpenAIVectorStoreList }

constructor TOpenAIVectorStoreList.Create(const APIKey: string);
begin
  inherited Create(True);
  FAPIKey := APIKey;
  FRESTClient := TRESTClient.Create('https://api.openai.com/v1');
  FRESTResponse := TRESTResponse.Create(nil);
  FRESTRequest := TRESTRequest.Create(nil);
end;

destructor TOpenAIVectorStoreList.Destroy;
begin
  FRESTResponse.Free;
  FRESTRequest.Free;
  FRESTClient.Free;
  inherited;
end;

function TOpenAIVectorStoreList.CreateVectorStore(const AName, AExpiresAfter: Integer; const AMetadata: TJSONObject): TOpenAIVectorStore;
var
  JSONObject, ResponseObject: TJSONObject;
  Store: TOpenAIVectorStore;
  expires_after : TJSONObject;
begin
  Result := nil;
  // Initialize REST request
  ResetRequestToDefault(FRESTRequest, FRESTClient, FRESTResponse, FAPIkey);
  FRESTRequest.Method := rmPOST;
  FRESTRequest.Resource := 'vector_stores';

  // Build JSON object for the request body
  JSONObject := TJSONObject.Create;
  try
    JSONObject.AddPair('name', AName);
    expires_after := TJSONObject.Create;
    expires_after.AddPair('anchor', 'last_active_at');
    expires_after.AddPair('days', TJSONNumber.Create(AExpiresAfter));
    JSONObject.AddPair('expires_after', expires_after);

    if Assigned(AMetadata) then
      JSONObject.AddPair('metadata', AMetadata.Clone as TJSONObject);

    // Add JSON to request
    FRESTRequest.AddBody(JSONObject.ToJSON, TRESTContentType.ctAPPLICATION_JSON);

    // Execute the request
    FRESTRequest.Execute;

    // Check for successful response
    if FRESTResponse.StatusCode = 201 then
    begin
      ResponseObject := TJSONObject.ParseJSONValue(FRESTResponse.Content) as TJSONObject;
      if Assigned(ResponseObject) then
      begin
        try
          // Create and populate TOpenAIVectorStore instance
          Store := TOpenAIVectorStore.Create(FAPIKey);
          Store.Id := ResponseObject.GetValue<string>('id');
          Store.Name := ResponseObject.GetValue<string>('name');
          Store.Description := ResponseObject.GetValue<string>('description');
          Store.CreatedAt := ResponseObject.GetValue<Int64>('created_at');
          Result := Store; // Return the created store
          Add(Store);
        finally
          ResponseObject.Free;
        end;
      end;
    end
    else
      raise Exception.CreateFmt('Failed to create vector store. Status Code: %d', [FRESTResponse.StatusCode]);

  finally
    JSONObject.Free;
  end;
end;


procedure TOpenAIVectorStoreList.LoadStores;
var
  JSONArray: TJSONArray;
  JSONObject, FileCountsObject: TJSONObject;
  Store: TOpenAIVectorStore;
  I: Integer;
  HasMore: Boolean;
  NextPageId: string;
  v : Integer;
begin
  // Initialize the REST components and reset request
  ResetRequestToDefault(FRESTRequest, FRESTClient, FRESTResponse, FAPIkey);

  NextPageId := '';
  HasMore := True;

  while HasMore do
  begin
    try
      FRESTRequest.Method := rmGET;
      FRESTRequest.Resource := 'vector_stores';

      // Add pagination parameters if there's a NextPageId
      if NextPageId <> '' then
        FRESTRequest.AddParameter('starting_after', NextPageId, TRESTRequestParameterKind.pkGETorPOST);

      // Execute the request
      FRESTRequest.Execute;

      // Check if the response status is OK
      if FRESTResponse.StatusCode = 200 then
      begin
        JSONObject := TJSONObject.ParseJSONValue(FRESTResponse.Content) as TJSONObject;
        try
          JSONArray := JSONObject.GetValue<TJSONArray>('data');

          if Assigned(JSONArray) then
          begin
            for I := 0 to JSONArray.Count - 1 do
            begin
              // Parse each vector store object
              Store := TOpenAIVectorStore.Create(FAPIKey);
              try
                JSONObject := JSONArray.Items[I] as TJSONObject;
                Store.Id := JSONObject.GetValue<string>('id');
                Store.Name := JSONObject.GetValue<string>('name');
                Store.CreatedAt := JSONObject.GetValue<Int64>('created_at');
                if JSONObject.TryGetValue<Integer>('bytes', v) then
                  Store.Bytes := v;

                // Parse nested file_counts object
                FileCountsObject := JSONObject.GetValue<TJSONObject>('file_counts');
                if Assigned(FileCountsObject) then
                begin
                  Store.FileCounts.InProgress := FileCountsObject.GetValue<Integer>('in_progress');
                  Store.FileCounts.Completed := FileCountsObject.GetValue<Integer>('completed');
                  Store.FileCounts.Failed := FileCountsObject.GetValue<Integer>('failed');
                  Store.FileCounts.Cancelled := FileCountsObject.GetValue<Integer>('cancelled');
                  Store.FileCounts.Total := FileCountsObject.GetValue<Integer>('total');
                end;

                // Add the store to the list
                Add(Store);
              except
                Store.Free;
                raise;
              end;
            end;
          end;

          // Check if there's more data to load
          if JSONObject.TryGetValue<Boolean>('has_more', HasMore) then
          begin
            if HasMore then
              NextPageId := JSONObject.GetValue<string>('last_id'); // Set the ID for the next page
          end
          else
            HasMore := False;

        finally
          JSONObject.Free;
        end;
      end
      else
        raise Exception.CreateFmt('Failed to load stores. Status Code: %d', [FRESTResponse.StatusCode]);

    except
      on E: Exception do
        raise Exception.CreateFmt('Error loading vector stores: %s', [E.Message]);
    end;
  end;
end;

{ TOpenAIVectorStore }

constructor TOpenAIVectorStore.Create(const APIKey: string);
begin
  inherited Create;
  FAPIKey := APIKey;
end;

destructor TOpenAIVectorStore.Destroy;
begin
  FreeAndNil(FFileList);
  inherited;
end;

function TOpenAIVectorStore.GetFileList: TOpenAIVectorFileList;
begin
  if not Assigned(FFileList) then
  begin
    FFileList := TOpenAIVectorFileList.Create(FAPIKey, FId);
    FFileList.GetFiles(FId);
  end;
  Result := FFileList;
end;

{ TOpenAIVectorFileList }

procedure TOpenAIVectorFileList.AddFileToStore(const FileId: string);
var
  JSONObject, ResponseObject: TJSONObject;
  NewFile: TOpenAIVectorFile;
begin
  // Initialize the REST components and reset request
  ResetRequestToDefault(FRESTRequest, FRESTClient, FRESTResponse, FAPIkey);

  FRESTRequest.Resource := Format('vector_stores/%s/files', [FStoreId]);
  FRESTRequest.Method := rmPOST;

  // Build JSON object for the request body
  JSONObject := TJSONObject.Create;
  try
    JSONObject.AddPair('file_id', FileId);
    FRESTRequest.Body.Add(JSONObject.ToJSON, TRESTContentType.ctAPPLICATION_JSON);

    // Execute the request
    FRESTRequest.Execute;

    // Check for successful response
    if FRESTResponse.StatusCode = 200 then
    begin
      ResponseObject := TJSONObject.ParseJSONValue(FRESTResponse.Content) as TJSONObject;
      if Assigned(ResponseObject) then
      begin
        try
          // Create and populate TOpenAIVectorFile instance for the new file
          NewFile := TOpenAIVectorFile.Create;
          NewFile.Id := ResponseObject.GetValue<string>('id');
          NewFile.CreatedAt := ResponseObject.GetValue<Int64>('created_at');
          NewFile.VectorStoreId := ResponseObject.GetValue<string>('vector_store_id');
          NewFile.Status := ResponseObject.GetValue<string>('status');

          Add(NewFile); // Add to the list
        finally
          ResponseObject.Free;
        end;
      end;
    end
    else
      raise Exception.CreateFmt('Failed to add file to vector store. Status Code: %d', [FRESTResponse.StatusCode]);

  finally
    JSONObject.Free;
  end;
end;


constructor TOpenAIVectorFileList.Create(const APIKey: string; const StoreId: string);
begin
  inherited Create(True);
  FAPIKey := APIKey;
  FStoreId := StoreId;
end;

destructor TOpenAIVectorFileList.Destroy;
begin
  FRESTResponse.Free;
  FRESTRequest.Free;
  FRESTClient.Free;
  inherited;
end;

function TOpenAIVectorFileList.GetFiles(const StoreId: string): Boolean;
var
  JSONArray: TJSONArray;
  JSONObject: TJSONObject;
  FileItem: TOpenAIVectorFile;
  I: Integer;
begin
  Result := False;
  Clear; // Clear existing items in case we're re-fetching

  // Initialize the REST components and reset request
  ResetRequestToDefault(FRESTRequest, FRESTClient, FRESTResponse, FAPIkey);

  // Setup REST request
  FRESTRequest.Resource := Format('vector_stores/%s/files', [StoreId]);
  FRESTRequest.Method := rmGET;

  // Execute the request
  FRESTRequest.Execute;

  // Check response status
  if FRESTResponse.StatusCode = 200 then
  begin
    JSONArray := TJSONObject.ParseJSONValue(FRESTResponse.Content).GetValue<TJSONArray>('data');
    if Assigned(JSONArray) then
    begin
      try
        for I := 0 to JSONArray.Count - 1 do
        begin
          JSONObject := JSONArray.Items[I] as TJSONObject;
          FileItem := TOpenAIVectorFile.Create;
          try
            FileItem.Id := JSONObject.GetValue<string>('id');
            FileItem.CreatedAt := JSONObject.GetValue<Int64>('created_at');
            FileItem.VectorStoreId := JSONObject.GetValue<string>('vector_store_id');
            Add(FileItem); // Add to the list
          except
            FileItem.Free;
            raise;
          end;
        end;
        Result := True; // Files successfully retrieved
      finally
        JSONArray.Free;
      end;
    end;
  end
  else
    raise Exception.CreateFmt('Failed to retrieve files. Status Code: %d', [FRESTResponse.StatusCode]);
end;


end.
