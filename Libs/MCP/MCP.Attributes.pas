unit MCP.Attributes;

interface

uses
  System.SysUtils,
  System.Classes,
  System.RTTI,
  System.TypInfo,
  System.JSON;

type
  /// <summary>
  /// Base attribute for MCP-related annotations
  /// </summary>
  MCPAttribute = class(TCustomAttribute)
  end;

  /// <summary>
  /// Marks a method as an MCP tool
  /// </summary>
  MCPToolAttribute = class(MCPAttribute)
  private
    FName: string;
    FDescription: string;
  public
    constructor Create(const Name, Description: string);
    property Name: string read FName;
    property Description: string read FDescription;
  end;

  /// <summary>
  /// Describes a parameter for an MCP tool
  /// </summary>
  MCPParameterAttribute = class(MCPAttribute)
  private
    FDescription: string;
    FRequired: Boolean;
  public
    constructor Create(const Description: string; Required: Boolean = True);
    property Description: string read FDescription;
    property Required: Boolean read FRequired;
  end;

  /// <summary>
  /// Helper class for RTTI operations related to MCP
  /// </summary>
  TMCPRTTIHelper = class
  public
    class function GetToolsForObject(AObject: TObject): TJSONArray;
    class function GetMethodToolSchemaFromRTTI(AMethod: TRttiMethod): TJSONObject;
    class function GetParameterSchemaFromRTTI(AParam: TRttiParameter): TJSONObject;
  end;

implementation

{ MCPToolAttribute }

constructor MCPToolAttribute.Create(const Name, Description: string);
begin
  inherited Create;
  FName := Name;
  FDescription := Description;
end;

{ MCPParameterAttribute }

constructor MCPParameterAttribute.Create(const Description: string; Required: Boolean);
begin
  inherited Create;
  FDescription := Description;
  FRequired := Required;
end;

{ TMCPRTTIHelper }

class function TMCPRTTIHelper.GetToolsForObject(AObject: TObject): TJSONArray;
var
  RTTIContext: TRttiContext;
  ObjectType: TRttiType;
  Method: TRttiMethod;
  ToolAttrib: MCPToolAttribute;
  ToolSchema: TJSONObject;
begin
  Result := TJSONArray.Create;
  
  RTTIContext := TRttiContext.Create;
  try
    ObjectType := RTTIContext.GetType(AObject.ClassType);
    
    // Scan all methods for MCPTool attributes
    for Method in ObjectType.GetMethods do
    begin
      ToolAttrib := Method.GetAttribute<MCPToolAttribute>;
      if Assigned(ToolAttrib) then
      begin
        ToolSchema := GetMethodToolSchemaFromRTTI(Method);
        Result.Add(ToolSchema);
      end;
    end;
  finally
    RTTIContext.Free;
  end;
end;

class function TMCPRTTIHelper.GetMethodToolSchemaFromRTTI(AMethod: TRttiMethod): TJSONObject;
var
  ToolAttrib: MCPToolAttribute;
  InputSchema, Properties: TJSONObject;
  RequiredArray: TJSONArray;
  Param: TRttiParameter;
  ParamAttrib: MCPParameterAttribute;
  ParamSchema: TJSONObject;
begin
  Result := TJSONObject.Create;
  
  // Get tool attribute
  ToolAttrib := AMethod.GetAttribute<MCPToolAttribute>;
  if not Assigned(ToolAttrib) then
  begin
    Result.Free;
    raise Exception.Create('Method does not have MCPTool attribute');
  end;
  
  // Add basic tool information
  Result.AddPair('name', ToolAttrib.Name);
  Result.AddPair('description', ToolAttrib.Description);
  
  // Create input schema
  InputSchema := TJSONObject.Create;
  InputSchema.AddPair('type', 'object');
  
  // Create properties object
  Properties := TJSONObject.Create;
  
  // Create required array
  RequiredArray := TJSONArray.Create;
  
  // Process each parameter
  for Param in AMethod.GetParameters do
  begin
    ParamAttrib := Param.GetAttribute<MCPParameterAttribute>;
    if Assigned(ParamAttrib) then
    begin
      // Create parameter schema
      ParamSchema := GetParameterSchemaFromRTTI(Param);
      
      // Add description from attribute
      ParamSchema.AddPair('description', ParamAttrib.Description);
      
      // Add to properties
      Properties.AddPair(Param.Name.ToLower, ParamSchema);
      
      // Add to required list if necessary
      if ParamAttrib.Required then
        RequiredArray.Add(Param.Name.ToLower);
    end;
  end;
  
  // Add properties to input schema
  InputSchema.AddPair('properties', Properties);
  
  // Add required list if not empty
  if RequiredArray.Count > 0 then
    InputSchema.AddPair('required', RequiredArray)
  else
    RequiredArray.Free;
  
  // Add input schema to tool
  Result.AddPair('inputSchema', InputSchema);
end;

class function TMCPRTTIHelper.GetParameterSchemaFromRTTI(AParam: TRttiParameter): TJSONObject;
var
  ParamType: TRttiType;
begin
  Result := TJSONObject.Create;
  
  ParamType := AParam.ParamType;
  
  // Map Delphi types to JSON Schema types
  if ParamType.TypeKind in [tkInteger, tkInt64] then
    Result.AddPair('type', 'integer')
  else if ParamType.TypeKind in [tkFloat] then
    Result.AddPair('type', 'number')
  else if ParamType.TypeKind in [tkString, tkUString, tkLString, tkWString, tkChar, tkWChar] then
    Result.AddPair('type', 'string')
  else if ParamType.TypeKind in [tkEnumeration] then
    if ParamType.Handle = TypeInfo(Boolean) then
      Result.AddPair('type', 'boolean')
    else
      Result.AddPair('type', 'string')
  else if ParamType.TypeKind in [tkClass, tkRecord, tkInterface] then
    Result.AddPair('type', 'object')
  else if ParamType.TypeKind in [tkArray, tkDynArray] then
    Result.AddPair('type', 'array')
  else
    Result.AddPair('type', 'string'); // Default
end;

end.
