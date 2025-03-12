unit MCP.MethodProcessor;

interface

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.Generics.Collections,
  System.RTTI,
  System.TypInfo,
  Winapi.Windows,
  MCP.Attributes,
  MCP.ServiceInterface;

type
  /// <summary>
  /// Processor for executing MCP tool methods using RTTI
  /// </summary>
  TMCPMethodProcessor = class
  private
    FRttiContext: TRttiContext;
    FTargetObject: TObject;
    
    // Helper methods
    function ConvertJSONToMethodParam(ParamInfo: TRttiParameter; JsonValue: TJSONValue): TValue;
    function ConvertMethodResultToJSON(const Value: TValue): TJSONValue;
    function CreateClassToJSON(AObject: TObject): TJSONObject;
  public
    constructor Create(TargetObject: TObject);
    destructor Destroy; override;
    
    // Main processing methods
    function ExecuteToolMethod(const ToolName: string; Arguments: TJSONObject): TJSONValue;
    function GetToolMethodByName(const ToolName: string): TRttiMethod;
    function GetToolMethods: TArray<TRttiMethod>;
  end;

implementation

{ TMCPMethodProcessor }

constructor TMCPMethodProcessor.Create(TargetObject: TObject);
begin
  inherited Create;
  FRttiContext := TRttiContext.Create;
  FTargetObject := TargetObject;
end;

destructor TMCPMethodProcessor.Destroy;
begin
  FRttiContext.Free;
  inherited;
end;

function TMCPMethodProcessor.ConvertJSONToMethodParam(ParamInfo: TRttiParameter; JsonValue: TJSONValue): TValue;
var
  StrValue: string;
begin
  // Handle nil JSON value
  if not Assigned(JsonValue) or (JsonValue is TJSONNull) then
  begin
    Result := TValue.Empty;
    Exit;
  end;
  
  // Handle different parameter types
  case ParamInfo.ParamType.TypeKind of
    tkInteger, tkInt64:
      begin
        if JsonValue is TJSONNumber then
          Result := TValue.From<Integer>(TJSONNumber(JsonValue).AsInt)
        else
        begin
          StrValue := JsonValue.Value;
          Result := TValue.From<Integer>(StrToIntDef(StrValue, 0));
        end;
      end;
    
    tkFloat:
      begin
        if JsonValue is TJSONNumber then
          Result := TValue.From<Double>(TJSONNumber(JsonValue).AsDouble)
        else
        begin
          StrValue := JsonValue.Value;
          Result := TValue.From<Double>(StrToFloatDef(StrValue, 0));
        end;
      end;
    
    tkString, tkLString, tkWString, tkUString:
      begin
        StrValue := JsonValue.Value;
        Result := TValue.From<string>(StrValue);
      end;
    
    tkEnumeration:
      begin
        if ParamInfo.ParamType.Handle = TypeInfo(Boolean) then
        begin
          if JsonValue is TJSONBool then
            Result := TValue.From<Boolean>(TJSONBool(JsonValue).AsBoolean)
          else
          begin
            StrValue := JsonValue.Value;
            Result := TValue.From<Boolean>(StrToBoolDef(StrValue, False));
          end;
        end
        else
        begin
          // Handle other enumeration types
          StrValue := JsonValue.Value;
          Result := TValue.FromOrdinal(ParamInfo.ParamType.Handle, GetEnumValue(ParamInfo.ParamType.Handle, StrValue));
        end;
      end;
    
    // Other types would be handled here
    
    else
      // Default to string if type is unknown
      Result := TValue.From<string>(JsonValue.Value);
  end;
end;

function TMCPMethodProcessor.ConvertMethodResultToJSON(const Value: TValue): TJSONValue;
var
  RttiContext: TRttiContext;
  RttiType: TRttiType;
  RttiMethod: TRttiMethod;
  PropValue: TValue;
  JSONObj: TJSONObject;
  HasToJSON: Boolean;
begin
  // Handle empty result
  if Value.IsEmpty then
  begin
    Result := TJSONNull.Create;
    Exit;
  end;

  // Convert based on type
  case Value.Kind of
    tkInteger, tkInt64:
      Result := TJSONNumber.Create(Value.AsInt64);

    tkFloat:
      Result := TJSONNumber.Create(Value.AsExtended);

    tkString, tkLString, tkWString, tkUString:
      Result := TJSONString.Create(Value.AsString);

    tkEnumeration:
      if Value.TypeInfo = TypeInfo(Boolean) then
        Result := TJSONBool.Create(Value.AsBoolean)
      else
        Result := TJSONString.Create(GetEnumName(Value.TypeInfo, Value.AsOrdinal));

    tkClass:
      begin
        // Check if it's already a JSON value
        if Value.AsObject is TJSONValue then
        begin
          Result := TJSONValue(Value.AsObject).Clone as TJSONValue;
        end
        else if Value.AsObject.ClassType.InheritsFrom(TJSONValue) then
        begin
          // Handle derived JSON types
          Result := TJSONValue(Value.AsObject).Clone as TJSONValue;
        end
        else if Value.AsObject = nil then
        begin
          Result := TJSONNull.Create;
        end
        else
        begin
          // Try to use ToJSON method if available
          RttiContext := TRttiContext.Create;
          try
            RttiType := RttiContext.GetType(Value.AsObject.ClassType);
            RttiMethod := RttiType.GetMethod('ToJSON');

            HasToJSON := Assigned(RttiMethod) and
                        (RttiMethod.MethodKind = mkFunction) and
                        (RttiMethod.ReturnType <> nil) and
                        ((RttiMethod.ReturnType.Handle = TypeInfo(TJSONObject)) or
                         (RttiMethod.ReturnType.IsInstance and
                          RttiMethod.ReturnType.AsInstance.MetaclassType.InheritsFrom(TJSONObject)));

            OutputDebugString(PChar('HasToJSON = ' + BoolToStr(HasToJSON, True)));

            if HasToJSON then
            begin
              try
                // Make sure the method has no parameters or all have defaults
                if (Length(RttiMethod.GetParameters) = 0) then
                begin
                  OutputDebugString(PChar('Invoking ToJSON method'));
                  var ResultValue := RttiMethod.Invoke(Value.AsObject, []);
                  if not ResultValue.IsEmpty and (ResultValue.AsObject is TJSONObject) then
                  begin
                    OutputDebugString(PChar('ToJSON returned a valid TJSONObject'));
                    Result := ResultValue.AsObject as TJSONObject;
                    Exit;
                  end;
                end;
              except
                on E: Exception do
                  OutputDebugString(PChar('ToJSON exception: ' + E.Message));
              end;
            end;

            // Fall back to RTTI-based conversion
            OutputDebugString(PChar('Using CreateClassToJSON fallback'));
            Result := CreateClassToJSON(Value.AsObject);
          finally
            RttiContext.Free;
          end;
        end;
      end;

    tkRecord, tkInterface:
      begin
        // Handle records with RTTI
        if Value.Kind = tkRecord then
        begin
          JSONObj := TJSONObject.Create;
          RttiContext := TRttiContext.Create;
          try
            RttiType := RttiContext.GetType(Value.TypeInfo);

            // Process fields of the record
            for var Field in RttiType.GetFields do
            begin
              try
                PropValue := Field.GetValue(Value.GetReferenceToRawData);
                JSONObj.AddPair(Field.Name, ConvertMethodResultToJSON(PropValue));
              except
                JSONObj.AddPair(Field.Name, TJSONNull.Create);
              end;
            end;

            Result := JSONObj;
          finally
            RttiContext.Free;
          end;
        end
        else
        begin
          // Simple representation for interfaces
          Result := TJSONString.Create(Value.ToString);
        end;
      end;

    tkArray, tkDynArray:
      begin
        // Create a JSON array from Delphi array
        var JSONArray := TJSONArray.Create;
        var ElementCount := Value.GetArrayLength;

        for var i := 0 to ElementCount - 1 do
        begin
          var ElementValue := Value.GetArrayElement(i);
          JSONArray.Add(ConvertMethodResultToJSON(ElementValue) as TJSONObject);
        end;

        Result := JSONArray;
      end;

    else
      // Default to string representation
      Result := TJSONString.Create(Value.ToString);
  end;
end;

function TMCPMethodProcessor.CreateClassToJSON(AObject: TObject): TJSONObject;
var
  RttiContext: TRttiContext;
  RttiType: TRttiType;
  RttiProperty: TRttiProperty;
  PropValue: TValue;
  ProcessedObjects: TList<TObject>;
begin
  Result := TJSONObject.Create;

  if AObject = nil then
    Exit;

  // Keep track of processed objects to avoid circular references
  ProcessedObjects := TList<TObject>.Create;
  try
    ProcessedObjects.Add(AObject);

    // Get RTTI info for the class
    RttiContext := TRttiContext.Create;
    try
      RttiType := RttiContext.GetType(AObject.ClassType);

      // Process properties
      for RttiProperty in RttiType.GetProperties do
      begin
        if (RttiProperty.Visibility >= TMemberVisibility.mvPublic) and
           RttiProperty.IsReadable then
        begin
          try
            // Get property value
            PropValue := RttiProperty.GetValue(AObject);

            // Handle object properties to avoid circular references
            if (PropValue.Kind = tkClass) and
               not PropValue.IsEmpty and
               (PropValue.AsObject <> nil) then
            begin
              if ProcessedObjects.Contains(PropValue.AsObject) then
              begin
                // Circular reference found
                Result.AddPair(RttiProperty.Name,
                  TJSONString.Create(Format('Instance of %s', [PropValue.AsObject.ClassName])));
              end
              else
              begin
                ProcessedObjects.Add(PropValue.AsObject);
                Result.AddPair(RttiProperty.Name, ConvertMethodResultToJSON(PropValue));
              end;
            end
            else
            begin
              Result.AddPair(RttiProperty.Name, ConvertMethodResultToJSON(PropValue));
            end;
          except
            on E: Exception do
            begin
              // Handle property conversion errors
              Result.AddPair(RttiProperty.Name, TJSONString.Create(Format('Error: %s', [E.Message])));
            end;
          end;
        end;
      end;

      // Also include published fields
      for var Field in RttiType.GetFields do
      begin
        if Field.Visibility >= TMemberVisibility.mvPublic then
        begin
          try
            PropValue := Field.GetValue(AObject);
            Result.AddPair(Field.Name, ConvertMethodResultToJSON(PropValue));
          except
            on E: Exception do
            begin
              Result.AddPair(Field.Name, TJSONString.Create(Format('Error: %s', [E.Message])));
            end;
          end;
        end;
      end;
    finally
      RttiContext.Free;
    end;
  finally
    ProcessedObjects.Free;
  end;
end;



function TMCPMethodProcessor.ExecuteToolMethod(const ToolName: string; Arguments: TJSONObject): TJSONValue;
var
  Method: TRttiMethod;
  Params: TArray<TRttiParameter>;
  Args: TArray<TValue>;
  JsonParamValue: TJSONValue;
  MethodResult: TValue;
begin
  // Find the method
  Method := GetToolMethodByName(ToolName);
  if not Assigned(Method) then
    raise Exception.CreateFmt('Tool method "%s" not found', [ToolName]);

  // Get parameters
  Params := Method.GetParameters;
  SetLength(Args, Length(Params));
  
  // Convert JSON arguments to method parameters
  for var i := 0 to Length(Params) - 1 do
  begin
    // Get parameter value from JSON
    if Assigned(Arguments) and Arguments.TryGetValue<TJSONValue>(Params[i].Name.ToLower, JsonParamValue) then
      Args[i] := ConvertJSONToMethodParam(Params[i], JsonParamValue)
    else
      // Parameter not provided
      Args[i] := TValue.Empty;
  end;
  
  // Call the method
  MethodResult := Method.Invoke(FTargetObject, Args);
  
  // Convert result to JSON
  Result := ConvertMethodResultToJSON(MethodResult);
end;

function TMCPMethodProcessor.GetToolMethodByName(const ToolName: string): TRttiMethod;
var
  ToolMethods: TArray<TRttiMethod>;
  Method: TRttiMethod;
  ToolAttr: MCPToolAttribute;
begin
  Result := nil;
  ToolMethods := GetToolMethods;
  
  // Look for a method with the matching tool name
  for Method in ToolMethods do
  begin
    ToolAttr := Method.GetAttribute<MCPToolAttribute>;
    if Assigned(ToolAttr) and SameText(ToolAttr.Name, ToolName) then
    begin
      Result := Method;
      Break;
    end;
  end;
end;

function TMCPMethodProcessor.GetToolMethods: TArray<TRttiMethod>;
var
  RttiType: TRttiType;
  Methods: TArray<TRttiMethod>;
  MethodList: TList<TRttiMethod>;
begin
  MethodList := TList<TRttiMethod>.Create;
  try
    RttiType := FRttiContext.GetType(FTargetObject.ClassType);
    Methods := RttiType.GetMethods;
    
    // Filter methods with MCPTool attribute
    for var Method in Methods do
    begin
      if Method.GetAttribute<MCPToolAttribute> <> nil then
        MethodList.Add(Method);
    end;
    
    Result := MethodList.ToArray;
  finally
    MethodList.Free;
  end;
end;

end.
