unit DelphiIdeProxyController;

interface

uses
  System.SysUtils,
  System.IOUtils,
  System.Classes,
  System.JSON,
  System.Generics.Collections,
  REST.Client,
  REST.Types,
  Web.HTTPApp,
  MVCFramework,
  MVCFramework.Commons
  ;

type
  [MVCPath('/')]
  TDelphiIdeProxyController = class(TMVCController)
  protected
    procedure OnBeforeAction(AContext: TWebContext; const AActionName: string; var AHandled: Boolean); override;
    procedure OnAfterAction(AContext: TWebContext; const AActionName: string); override;
  private
    FAPiKeyId: string;
    function ProxyRequestToOpenAI(const Endpoint: string; const Method: TRESTRequestMethod; const Body: TJSONObject): TJSONObject;

  public
    [MVCPath('/v1/chat/completions')]
    [MVCHTTPMethod([httpPOST])]
    procedure CreateCompletion;

    [MVCPath('/v1/models')]
    [MVCHTTPMethod([httpGET])]
    procedure GetModels;
  end;

implementation

uses
  MVCFramework.Logger,
  System.DateUtils
  ;


procedure TDelphiIdeProxyController.CreateCompletion;
var
  RequestBody: TJSONObject;
  OriginalMessages, NewMessages: TJSONArray;
  MessageObj, CopiedObj: TJSONObject;
  ResponseBody: TJSONObject;
  i: Integer;
  SystemMsg: string;
begin
  RequestBody := TJSONObject.ParseJSONValue(Context.Request.Body) as TJSONObject;

  try
    // Retrieve or create the original messages array
    OriginalMessages := RequestBody.GetValue<TJSONArray>('messages');
    if not Assigned(OriginalMessages) then
      OriginalMessages := TJSONArray.Create;

    // Create a new messages array
    NewMessages := TJSONArray.Create;

    // Add the system message first
    MessageObj := TJSONObject.Create;
    MessageObj.AddPair('role', 'system');
    SystemMsg := TFile.ReadAllText(TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), 'Data\SystemMsg.txt'));
    MessageObj.AddPair('content', 'You are a helpful assistant inside the Delphi IDE. ' + SystemMsg);
    NewMessages.AddElement(MessageObj);

    // Append the original messages
    for i := 0 to OriginalMessages.Count - 1 do
    begin
      CopiedObj := OriginalMessages.Items[i].Clone as TJSONObject;
      NewMessages.AddElement(CopiedObj);
    end;

    // Replace the messages array in the request
    RequestBody.RemovePair('messages');
    RequestBody.AddPair('messages', NewMessages);

    // Proxy the request
    ResponseBody := ProxyRequestToOpenAI('https://api.openai.com/v1/chat/completions', rmPOST, RequestBody);
    Render(ResponseBody, False);
  finally
    FreeAndNil(RequestBody);
    FreeAndNil(ResponseBody);
  end;
end;

procedure TDelphiIdeProxyController.GetModels;
var
  RequestBody: TJSONObject;
  ResponseBody: TJSONObject;
begin
  // Parse the incoming JSON body from the request
  RequestBody := TJSONObject.ParseJSONValue(Context.Request.Body) as TJSONObject;
  try
    // Proxy the request to Replicate.com
    ResponseBody := ProxyRequestToOpenAI('https://api.openai.com/v1/models', rmGET, RequestBody);
    try
      // Send the JSON response back to the client
      Render(ResponseBody, False);
    finally
      FreeAndNil(ResponseBody);
    end;
  finally
    FreeAndNil(RequestBody);
  end;
end;

procedure TDelphiIdeProxyController.OnAfterAction(AContext: TWebContext; const AActionName: string);
begin
  inherited;
end;

procedure TDelphiIdeProxyController.OnBeforeAction(AContext: TWebContext; const AActionName: string; var AHandled: Boolean);
begin
  inherited;
  FAPiKeyId := Context.Request.Headers['Authorization'].Split([' '])[1];
end;

function TDelphiIdeProxyController.ProxyRequestToOpenAI(const Endpoint: string; const Method: TRESTRequestMethod; const Body: TJSONObject): TJSONObject;
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
begin
  if not Assigned(Body) then
    raise Exception.Create('OpenAI API request failed because the request contains no body');

  RESTClient := TRESTClient.Create(Endpoint);
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);
  try
    // Configure REST Client, Request, and Response
    RESTClient.Accept := 'application/json';
    RESTClient.ContentType := 'application/json';
    RESTClient.AddParameter('Authorization', 'Bearer ' + FAPiKeyId, pkHTTPHEADER, [poDoNotEncode]);

    // Assign components to the request
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Method := Method;

    // Set the request body
    RESTRequest.AddBody(Body);

    // Execute the request
    RESTRequest.Execute;

    // Check the status code
    if (RESTResponse.StatusCode >= 200) and (RESTResponse.StatusCode < 300) then
    begin
      // Parse response body into JSON
      Result := RESTResponse.JSONValue.Clone as TJSONObject;
    end
    else
    begin
      // Handle non-successful responses
      raise Exception.CreateFmt('OpenAI API request failed with status code %d: %s', [RESTResponse.StatusCode, RESTResponse.StatusText]);
    end;
  finally
    FreeAndNil(RESTClient);
    FreeAndNil(RESTRequest);
    FreeAndNil(RESTResponse);
  end;
end;

end.

