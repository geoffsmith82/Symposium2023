unit MCP.BaseService;

interface

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.Generics.Collections,
  System.RTTI,
  System.TypInfo,
  MCP.Attributes;

type
  /// <summary>
  /// Base MCP Service implementation with core protocol handling
  /// </summary>
  TMCPBaseService = class(TInterfacedObject)
  protected
    // Protocol-related properties
    FProtocolVersion: string;
    FClientInfo: TJSONObject;
    FClientCapabilities: TJSONObject;
    FIsInitialized: Boolean;
    FServiceName: string;
    FServiceVersion: string;
    FServiceInstructions: string;
    // Resources and Prompts related properties
    FResources: TJSONArray;
    FPrompts: TJSONArray;
  protected
    // MCP Protocol implementation
    function CreateInitializeResponse(const RequestID: string): TJSONObject; virtual;
    function CreateJSONRPCResponse(const RequestID: string; inResult: TJSONValue): TJSONObject;
    function CreateJSONRPCErrorResponse(const RequestID: string; ErrorCode: Integer; const ErrorMsg: string; ErrorData: TJSONValue = nil): TJSONObject;
    function HandlePing(const RequestID: string): TJSONObject; virtual;
    function HandleInitialize(Request: TJSONObject): TJSONObject; virtual;
    function HandleToolsList(const RequestID: string): TJSONObject; virtual;
    function HandleToolsCall(const RequestID: string; Params: TJSONObject): TJSONObject; virtual;
    function HandleGetCapabilities(const RequestID: string): TJSONObject; virtual;
    function HandlePromptsList(const RequestID: string): TJSONObject; virtual;
    function HandleResourcesList(const RequestID: string): TJSONObject; virtual;
    procedure ProcessJSONRPCRequest(Request: TJSONObject; out Response: TJSONObject); virtual;
    procedure ProcessRequest(const RequestStr: string; out ResponseStr: string); virtual;

    // Tools-related methods
    function GetTools: TJSONArray; virtual;
    
    // Resources-related methods
    function GetResources: TJSONArray; virtual;
    
    // Prompts-related methods
    function GetPrompts: TJSONArray; virtual;

    // Property access
    property ProtocolVersion: string read FProtocolVersion;
    property IsInitialized: Boolean read FIsInitialized;
    property ClientInfo: TJSONObject read FClientInfo;
    property ClientCapabilities: TJSONObject read FClientCapabilities;
  public
    constructor Create(const ServiceName, ServiceVersion, ServiceInstructions: string);
    destructor Destroy; override;
    procedure Run; virtual;
  end;

implementation

uses
  FMX.Types,
  MCP.MethodProcessor,
  MCP.ServiceInterface;

{ TMCPBaseService }

constructor TMCPBaseService.Create(const ServiceName, ServiceVersion, ServiceInstructions: string);
begin
  inherited Create;

  // Store service information
  FServiceName := ServiceName;
  FServiceVersion := ServiceVersion;
  FServiceInstructions := ServiceInstructions;

  // Set default protocol version
  FProtocolVersion := '2024-11-05';
  FIsInitialized := False;
  
  // Initialize resources and prompts arrays
  FResources := TJSONArray.Create;
  FPrompts := TJSONArray.Create;
end;

destructor TMCPBaseService.Destroy;
begin
  // Free protocol-related objects
  if Assigned(FClientInfo) then
    FClientInfo.Free;
  if Assigned(FClientCapabilities) then
    FClientCapabilities.Free;
  
  // Free resources and prompts arrays
  if Assigned(FResources) then
    FResources.Free;
  if Assigned(FPrompts) then
    FPrompts.Free;

  inherited;
end;

function TMCPBaseService.CreateInitializeResponse(const RequestID: string): TJSONObject;
var
  ServerInfo, Capabilities: TJSONObject;
begin
  // Create the base response object
  Result := TJSONObject.Create;
  Result.AddPair('jsonrpc', '2.0');
  Result.AddPair('id', RequestID);

  // Create the response content
  var ResponseContent := TJSONObject.Create;

  // Add server info
  ServerInfo := TJSONObject.Create;
  ServerInfo.AddPair('name', FServiceName);
  ServerInfo.AddPair('version', FServiceVersion);
  ResponseContent.AddPair('serverInfo', ServerInfo);

  // Add protocol version
  ResponseContent.AddPair('protocolVersion', FProtocolVersion);

  // Add capabilities
  Capabilities := TJSONObject.Create;
  var ToolsObj := TJSONObject.Create;
  ToolsObj.AddPair('listChanged', TJSONBool.Create(False));
  Capabilities.AddPair('tools', ToolsObj);
  
  // Add resources capability
  var ResourcesObj := TJSONObject.Create;
  ResourcesObj.AddPair('listChanged', TJSONBool.Create(False));
  Capabilities.AddPair('resources', ResourcesObj);
  
  // Add prompts capability
  var PromptsObj := TJSONObject.Create;
  PromptsObj.AddPair('listChanged', TJSONBool.Create(False));
  Capabilities.AddPair('prompts', PromptsObj);
  
  ResponseContent.AddPair('capabilities', Capabilities);

  // Add instructions
  ResponseContent.AddPair('instructions', FServiceInstructions);

  // Add the response content as the result
  Result.AddPair('result', ResponseContent);
end;

function TMCPBaseService.CreateJSONRPCResponse(const RequestID: string; inResult: TJSONValue): TJSONObject;
var
  Response : TJSONObject;
begin
  // Create the JSON-RPC 2.0 response
  Response := TJSONObject.Create;
  Response.AddPair('jsonrpc', '2.0');
  Response.AddPair('id', RequestID);
  Response.AddPair('result', inResult);

  Result := Response;
end;

function TMCPBaseService.CreateJSONRPCErrorResponse(const RequestID: string; ErrorCode: Integer; const ErrorMsg: string; ErrorData: TJSONValue = nil): TJSONObject;
var
  Error: TJSONObject;
begin
  // Create the JSON-RPC 2.0 error response
  Result := TJSONObject.Create;
  Result.AddPair('jsonrpc', '2.0');

  // Add the ID (if provided)
  if RequestID <> '' then
    Result.AddPair('id', RequestID)
  else
    Result.AddPair('id', TJSONNull.Create);

  // Create the error object
  Error := TJSONObject.Create;
  Error.AddPair('code', TJSONNumber.Create(ErrorCode));
  Error.AddPair('message', ErrorMsg);

  // Add error data if provided
  if Assigned(ErrorData) then
    Error.AddPair('data', ErrorData);

  // Add the error to the response
  Result.AddPair('error', Error);
end;

function TMCPBaseService.HandlePing(const RequestID: string): TJSONObject;
begin
  // Return an empty result for ping
  Result := CreateJSONRPCResponse(RequestID, TJSONObject.Create);
end;

function TMCPBaseService.HandleInitialize(Request: TJSONObject): TJSONObject;
var
  Params: TJSONObject;
  RequestID: string;
  RequestIDValue: TJSONValue;
begin
  // Extract the request ID
  RequestIDValue := Request.GetValue('id');
  if RequestIDValue is TJSONString then
    RequestID := TJSONString(RequestIDValue).Value
  else if RequestIDValue is TJSONNumber then
    RequestID := TJSONNumber(RequestIDValue).ToString
  else
    RequestID := '';

  // Get and store client info and capabilities
  if Request.TryGetValue<TJSONObject>('params', Params) then
  begin
    // Store client capabilities
    if Params.TryGetValue<TJSONObject>('capabilities', FClientCapabilities) then
      FClientCapabilities := Params.GetValue<TJSONObject>('capabilities').Clone as TJSONObject;

    // Store client info
    if Params.TryGetValue<TJSONObject>('clientInfo', FClientInfo) then
      FClientInfo := Params.GetValue<TJSONObject>('clientInfo').Clone as TJSONObject;

    // Store protocol version
    if Params.TryGetValue<string>('protocolVersion', FProtocolVersion) then
      ; // Protocol version already stored
  end;

  // Mark as initialized
  FIsInitialized := True;

  // Create and return the initialize response
  Result := CreateInitializeResponse(RequestID);
end;

function TMCPBaseService.GetTools: TJSONArray;
begin
  // Use RTTI to discover tools based on attributes
  Result := TMCPRTTIHelper.GetToolsForObject(Self);
end;

function TMCPBaseService.GetResources: TJSONArray;
begin
  // Return the resources array (override in descendant classes)
  Result := FResources.Clone as TJSONArray;
end;

function TMCPBaseService.GetPrompts: TJSONArray;
begin
  // Return the prompts array (override in descendant classes)
  Result := FPrompts.Clone as TJSONArray;
end;

function TMCPBaseService.HandleToolsList(const RequestID: string): TJSONObject;
var
  ResultObj: TJSONObject;
begin
  // Create the response
  ResultObj := TJSONObject.Create;

  // Get tools array using RTTI
  ResultObj.AddPair('tools', GetTools);

  // Create and return the response
  Result := CreateJSONRPCResponse(RequestID, ResultObj);
end;

function TMCPBaseService.HandleResourcesList(const RequestID: string): TJSONObject;
var
  ResultObj: TJSONObject;
begin
  // Create the response
  ResultObj := TJSONObject.Create;

  // Get resources array
  ResultObj.AddPair('resources', GetResources);

  // Create and return the response
  Result := CreateJSONRPCResponse(RequestID, ResultObj);
end;

function TMCPBaseService.HandlePromptsList(const RequestID: string): TJSONObject;
var
  ResultObj: TJSONObject;
begin
  // Create the response
  ResultObj := TJSONObject.Create;

  // Get prompts array
  ResultObj.AddPair('prompts', GetPrompts);

  // Create and return the response
  Result := CreateJSONRPCResponse(RequestID, ResultObj);
end;

function TMCPBaseService.HandleToolsCall(const RequestID: string; Params: TJSONObject): TJSONObject;
var
  ToolName: string;
  Arguments: TJSONObject;
  ResponseObj: TJSONObject;
  ContentArray: TJSONArray;
  MethodProcessor: TMCPMethodProcessor;
  ResultValue: TJSONValue;
begin
  // Extract tool name and arguments
  if not Params.TryGetValue<string>('name', ToolName) then
  begin
    // Missing tool name
    Result := CreateJSONRPCErrorResponse(RequestID, -32602, 'Missing required parameter: name');
    Exit;
  end;

  // Get tool arguments (if any)
  if not Params.TryGetValue<TJSONObject>('arguments', Arguments) then
    Arguments := TJSONObject.Create;

  // Create the response object
  ResponseObj := TJSONObject.Create;

  // Create the content array
  ContentArray := TJSONArray.Create;

  try
    // Use method processor to invoke the tool
    MethodProcessor := TMCPMethodProcessor.Create(Self);
    try
      ResultValue := MethodProcessor.ExecuteToolMethod(ToolName, Arguments);

      // If the result is a TJSONArray, assume it's already formatted content
      if ResultValue is TJSONArray then
      begin
        // Use the result directly as content
        ResponseObj.AddPair('content', ResultValue);
      end
      else if ResultValue is TJSONObject then
      begin
        // Create text content from the object
        var TextContent := TJSONObject.Create;
        TextContent.AddPair('type', 'text');
        TextContent.AddPair('text', ResultValue.ToString);
        ContentArray.Add(TextContent);
        ResponseObj.AddPair('content', ContentArray);
      end
      else
      begin
        // Create text content from the value
        var TextContent := TJSONObject.Create;
        TextContent.AddPair('type', 'text');
        TextContent.AddPair('text', ResultValue.ToString);
        ContentArray.Add(TextContent);
        ResponseObj.AddPair('content', ContentArray);
      end;
    finally
      MethodProcessor.Free;
    end;
  except
    on E: Exception do
    begin
      // Handle errors
      ResponseObj.AddPair('isError', TJSONBool.Create(True));

      // Create error text content
      var ErrorContent := TJSONObject.Create;
      ErrorContent.AddPair('type', 'text');
      ErrorContent.AddPair('text', 'Error: ' + E.Message);
      ContentArray.Add(ErrorContent);

      // Add content array to response
      ResponseObj.AddPair('content', ContentArray);
    end;
  end;

  // Create the final response
  Result := CreateJSONRPCResponse(RequestID, ResponseObj);
end;

function TMCPBaseService.HandleGetCapabilities(const RequestID: string): TJSONObject;
var
  CapabilitiesObj, CommandsObj: TJSONObject;
  MethodProcessor: TMCPMethodProcessor;
  ToolMethods: TArray<TRttiMethod>;
  ToolAttr: MCPToolAttribute;
begin
  var ResultObj := TJSONObject.Create;

  // Add capabilities
  CapabilitiesObj := TJSONObject.Create;

  // Use method processor to get tool methods
  MethodProcessor := TMCPMethodProcessor.Create(Self);
  try
    ToolMethods := MethodProcessor.GetToolMethods;

    // Add each tool as a capability
    for var Method in ToolMethods do
    begin
      ToolAttr := Method.GetAttribute<MCPToolAttribute>;
      if Assigned(ToolAttr) then
      begin
        CapabilitiesObj.AddPair(ToolAttr.Name, ToolAttr.Description);
      end;
    end;
  finally
    MethodProcessor.Free;
  end;

  // Add resources and prompts capabilities if available
  if GetResources.Count > 0 then
    CapabilitiesObj.AddPair('resources/list', 'List available resources');
    
  if GetPrompts.Count > 0 then
    CapabilitiesObj.AddPair('prompts/list', 'List available prompts');

  ResultObj.AddPair('capabilities', CapabilitiesObj);

  // Add commands - this provides backward compatibility
  // but also makes it more evident what tools are available
  CommandsObj := TJSONObject.Create;
  for var Method in ToolMethods do
  begin
    ToolAttr := Method.GetAttribute<MCPToolAttribute>;
    if Assigned(ToolAttr) then
    begin
      CommandsObj.AddPair(ToolAttr.Name, ToolAttr.Description);
    end;
  end;
  
  // Add resources and prompts commands if available
  if GetResources.Count > 0 then
    CommandsObj.AddPair('resources/list', 'List available resources');
    
  if GetPrompts.Count > 0 then
    CommandsObj.AddPair('prompts/list', 'List available prompts');
    
  ResultObj.AddPair('commands', CommandsObj);

  // Add service description
  ResultObj.AddPair('description', FServiceInstructions);

  // Create and return the response
  Result := CreateJSONRPCResponse(RequestID, ResultObj);
end;

procedure TMCPBaseService.ProcessJSONRPCRequest(Request: TJSONObject; out Response: TJSONObject);
var
  Method: string;
  Params: TJSONObject;
  RequestID: string;
  RequestIDValue: TJSONValue;
  MethodProcessor: TMCPMethodProcessor;
begin
  // Get the method
  if not Request.TryGetValue<string>('method', Method) then
  begin
    Response := CreateJSONRPCErrorResponse('', -32600, 'Invalid Request: missing method');
    Exit;
  end;

  // Get the request ID (if any)
  if Request.TryGetValue<TJSONValue>('id', RequestIDValue) then
  begin
    if RequestIDValue is TJSONString then
      RequestID := TJSONString(RequestIDValue).Value
    else if RequestIDValue is TJSONNumber then
      RequestID := TJSONNumber(RequestIDValue).ToString
    else
      RequestID := '';
  end
  else
    RequestID := '';

  // Get the parameters (if any)
  if not Request.TryGetValue<TJSONObject>('params', Params) then
    Params := TJSONObject.Create;

  // Try to find and execute a method with the same name as the RPC method using RTTI
  MethodProcessor := TMCPMethodProcessor.Create(Self);
  try
    if Assigned(MethodProcessor.GetToolMethodByName(Method)) then
    begin
      // Execute the method
      var ResultValue := MethodProcessor.ExecuteToolMethod(Method, Params);

      // Format the result for MCP response
      var ContentArray := TJSONArray.Create;
      var ResponseContent := TJSONObject.Create;

      // If the result is a TJSONArray, assume it's already formatted content
      if ResultValue is TJSONArray then
      begin
        ResponseContent.AddPair('content', ResultValue);
      end
      else
      begin
        // Create text content from the value
        var TextContent := TJSONObject.Create;
        TextContent.AddPair('type', 'text');
        TextContent.AddPair('text', ResultValue.ToString);
        ContentArray.Add(TextContent);
        ResponseContent.AddPair('content', ContentArray);
      end;

      // Return the response
      Response := CreateJSONRPCResponse(RequestID, ResponseContent);
      Log.d(Response.ToJSON);
    end
    else
    begin
      // Standard MCP methods
      if Method = 'tools/call' then
      begin
        Response := HandleToolsCall(RequestID, Params);
      end
      else if Method = 'tools/list' then
      begin
        Response := HandleToolsList(RequestID);
      end
      else if Method = 'resources/list' then
      begin
        Response := HandleResourcesList(RequestID);
      end
      else if Method = 'prompts/list' then
      begin
        Response := HandlePromptsList(RequestID);
      end
      else if Method = 'getCapabilities' then
      begin
        Response := HandleGetCapabilities(RequestID);
      end
      else if Method = 'ping' then
      begin
        Response := HandlePing(RequestID);
      end
      else if Method = 'initialize' then
      begin
        Response := HandleInitialize(Request);
      end
      else if Method = 'notifications/initialized' then
      begin
        // This is a notification and doesn't need a response
        Response := nil;
      end
      else
      begin
        // Method not found
        Response := CreateJSONRPCErrorResponse(RequestID, -32601, Format('Method not found: %s', [Method]));
      end;
    end;
  finally
    MethodProcessor.Free;
  end;
end;

procedure TMCPBaseService.ProcessRequest(const RequestStr: string; out ResponseStr: string);
var
  Request, Response: TJSONObject;
begin
  // Parse request
  try
    Request := TJSONObject.ParseJSONValue(RequestStr) as TJSONObject;

    try
      // Process the request
      ProcessJSONRPCRequest(Request, Response);

      // Determine if a response is needed
      if Assigned(Response) then
        ResponseStr := Response.ToString
      else
        ResponseStr := ''; // No response needed (e.g., for notifications)
    finally
      Request.Free;
      if Assigned(Response) then
        Response.Free;
    end;
  except
    on E: Exception do
    begin
      // Handle parsing error
      Response := CreateJSONRPCErrorResponse('', -32700, Format('Parse error: %s', [E.Message]));
      try
        ResponseStr := Response.ToString;
      finally
        Response.Free;
      end;
    end;
  end;
end;

procedure TMCPBaseService.Run;
var
  InputLine, OutputLine: string;
begin
  while True do
  begin
    try
      // Read request from stdin
      ReadLn(InputLine);

      // Skip empty lines
      if InputLine.Trim.IsEmpty then
        Continue;

      Log.d('Input: ' + InputLine);

      // Process request and generate response
      ProcessRequest(InputLine, OutputLine);

      // If there's a response, write it to stdout
      if not OutputLine.IsEmpty then
      begin
        Log.d('Output: ' + OutputLine);
        WriteLn(OutputLine);
        Flush(Output);
      end;
    except
      on E: Exception do
      begin
        // Handle unexpected errors
        var ErrorResponse := CreateJSONRPCErrorResponse('', -32603, 'Internal error: ' + E.Message);
        try
          WriteLn(ErrorResponse.ToString);
          Flush(Output);
        finally
          ErrorResponse.Free;
        end;
      end;
    end;
  end;
end;

end.