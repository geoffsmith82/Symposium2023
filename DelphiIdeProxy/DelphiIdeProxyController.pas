unit DelphiIdeProxyController;

interface

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
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
    function ProxyRequestToReplicate(const Endpoint: string; const Method: TRESTRequestMethod; const Body: TJSONObject): TJSONObject;

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
    MessageObj.AddPair('content', 'You are a helpful assistant inside the Delphi IDE.');
    NewMessages.AddElement(MessageObj);

    // Append the original messages
    for i := 0 to OriginalMessages.Count - 1 do
    begin
      CopiedObj := TJSONObject(TJSONObject.ParseJSONValue(OriginalMessages.Items[i].ToJSON));
      NewMessages.AddElement(CopiedObj);
    end;

    // Replace the messages array in the request
    RequestBody.RemovePair('messages');
    RequestBody.AddPair('messages', NewMessages);

    // Proxy the request
    ResponseBody := ProxyRequestToReplicate('https://api.openai.com/v1/chat/completions', rmPOST, RequestBody);
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
    ResponseBody := ProxyRequestToReplicate('https://api.openai.com/v1/models', rmGET, RequestBody);
    try
      // Send the JSON response back to the client
      Render(ResponseBody, False);
    finally
      ResponseBody.Free;
    end;
  finally
    RequestBody.Free;
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

function TDelphiIdeProxyController.ProxyRequestToReplicate(const Endpoint: string; const Method: TRESTRequestMethod; const Body: TJSONObject): TJSONObject;
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
  JSONResponse: TJSONObject;
begin
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
    if Assigned(Body) then
    begin
      RESTRequest.AddBody(Body.ToJSON, TRESTContentType.ctAPPLICATION_JSON);
    end;

    // Execute the request
    RESTRequest.Execute;

    // Check the status code
    if (RESTResponse.StatusCode >= 200) and (RESTResponse.StatusCode < 300) then
    begin
      // Parse response body into JSON
      JSONResponse := TJSONObject.ParseJSONValue(RESTResponse.Content) as TJSONObject;
      Result := JSONResponse.Clone as TJSONObject;
    end
    else
    begin
      // Handle non-successful responses
      raise Exception.CreateFmt('Replicate API request failed with status code %d: %s', [RESTResponse.StatusCode, RESTResponse.StatusText]);
    end;
  finally
    RESTClient.Free;
    RESTRequest.Free;
    RESTResponse.Free;
  end;
end;

end.

