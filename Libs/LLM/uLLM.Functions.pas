unit uLLM.Functions;

interface

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.Rtti,
  System.TypInfo,
  FMX.Types,
  System.Generics.Collections,
  uAttributes
  ;

type
  TFunctionDescription = class
    Name: string;
    Method: System.TMethod;
    Description: string;
    Parameters: TJSONObject;
    constructor Create(const AName, ADescription: string; const AParameters: TJSONObject);
    destructor Destroy; override;
  end;

  TFunctionRegistry = class
  protected
    FMethods: TObjectDictionary<string, TFunctionDescription>;
    procedure InvokeFunctionFromJSON(const Method: System.TMethod; const JSONObject: TJSONObject; out ReturnValue: string); virtual;
    function GenerateParameterJSON(Method: TRttiMethod): TJSONObject; virtual;
    function GetJSONTypeFromRTTI(AType: TRttiType): string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RegisterFunction(const Func: Pointer; const Instance: TObject); virtual;
    procedure InvokeFunction(const JSONObject: TJSONObject; out ReturnValue: string); virtual;
    function GetAvailableFunctionsJSON(UseStrict: Boolean = True): TJSONArray; virtual;
    function Count: Integer;
  end;

implementation

constructor TFunctionDescription.Create(const AName, ADescription: string; const AParameters: TJSONObject);
begin
  Name := AName;
  Description := ADescription;
  Parameters := AParameters;
end;

function TFunctionRegistry.Count: Integer;
begin
  Result := FMethods.Count;
end;

constructor TFunctionRegistry.Create;
begin
  FMethods := TObjectDictionary<string, TFunctionDescription>.Create([doOwnsValues]);
end;

destructor TFunctionRegistry.Destroy;
begin
  FMethods.Free;
  inherited;
end;

procedure TFunctionRegistry.RegisterFunction(const Func: Pointer; const Instance: TObject);
var
  Method: TMethod;
  Context: TRttiContext;
  Typ: TRttiType;
  MethodRtti: TRttiMethod;
  Parameters: TJSONObject;
  FunctionDescription: string;
  Attr: TCustomAttribute;
  functionInfo: TFunctionDescription;
begin
  Method.Code := Func;
  Method.Data := Instance;

  Context := TRttiContext.Create;
  try
    Typ := Context.GetType(Instance.ClassType);
    for MethodRtti in Typ.GetMethods do
    begin
      if MethodRtti.CodeAddress = Func then
      begin
        // Get the function description from the attribute
        FunctionDescription := '';
        for Attr in MethodRtti.GetAttributes do
        begin
          if Attr is FunctionDescriptionAttribute then
          begin
            FunctionDescription := FunctionDescriptionAttribute(Attr).Description;
            Break;
          end;
        end;
        Parameters := GenerateParameterJSON(MethodRtti);
        functionInfo := TFunctionDescription.Create(MethodRtti.Name, FunctionDescription, Parameters);
        functionInfo.Method := Method;
        FMethods.Add(MethodRtti.Name, functionInfo);
        Break;
      end;
    end;
  finally
    Context.Free;
  end;
end;

function TFunctionRegistry.GenerateParameterJSON(Method: TRttiMethod): TJSONObject;
var
  Params: TJSONObject;
  Properties: TJSONObject;
  RequiredArray: TJSONArray;
  Param: TRttiParameter;
  Attr: TCustomAttribute;
  ParamDesc: string;
  ParamObj: TJSONObject;
  ParamType: string;
begin
  Properties := TJSONObject.Create;
  RequiredArray := TJSONArray.Create;
  for Param in Method.GetParameters do
  begin
    ParamDesc := '';
    for Attr in Param.GetAttributes do
    begin
      if Attr is ParamDescriptionAttribute then
      begin
        ParamDesc := ParamDescriptionAttribute(Attr).Description;
        Break;
      end;
    end;

    ParamType := GetJSONTypeFromRTTI(Param.ParamType);

    ParamObj := TJSONObject.Create;
    ParamObj.AddPair('type', ParamType);
    ParamObj.AddPair('description', ParamDesc);
    Properties.AddPair(Param.Name, ParamObj);

    // Add parameter name to required array
    RequiredArray.AddElement(TJSONString.Create(Param.Name));
  end;

  Params := TJSONObject.Create;
  Params.AddPair('type', 'object');
  Params.AddPair('properties', Properties);
  Params.AddPair('required', RequiredArray);
  Params.AddPair('additionalProperties', TJSONBool.Create(false));
  Result := Params;
end;

function TFunctionRegistry.GetJSONTypeFromRTTI(AType: TRttiType): string;
begin
  case AType.TypeKind of
    tkInteger, tkInt64:
      Result := 'integer';
    tkFloat:
      if AType.Handle = TypeInfo(TDateTime) then
        Result := 'string' // JSON Schema does not have a date type, use string
      else
        Result := 'number';
    tkChar, tkWChar, tkString, tkLString, tkUString, tkWString:
      Result := 'string';
    tkEnumeration:
      if AType.Handle = TypeInfo(Boolean) then
        Result := 'boolean'
      else
        Result := 'string'; // Handle other enumerations as strings for now
    else
      Result := 'string'; // Default to string for any other type
  end;
end;

procedure TFunctionRegistry.InvokeFunction(const JSONObject: TJSONObject; out ReturnValue: string);
var
  FunctionName: string;
  Method: TFunctionDescription;
begin
  FunctionName := JSONObject.GetValue<string>('name');
  if FMethods.TryGetValue(FunctionName, Method) then
  begin
    InvokeFunctionFromJSON(Method.Method, JSONObject, ReturnValue);
  end
  else
    raise Exception.Create('Function not registered');
end;

procedure TFunctionRegistry.InvokeFunctionFromJSON(const Method: TMethod; const JSONObject: TJSONObject; out ReturnValue: string);
var
  ArgsObject: TJSONObject;
  Context: TRttiContext;
  MethodType: TRttiMethod;
  Params: TArray<TRttiParameter>;
  Args: TArray<TValue>;
  ParamValue: TJSONValue;
  I: Integer;
  ResultValue: TValue;
begin
  Log.d(JSONObject.ToJSON);

  // Extract the 'input' JSON object properly
  ArgsObject := JSONObject.GetValue<TJSONObject>('input');

  if ArgsObject = nil then
    raise Exception.Create('Invalid JSON: "input" field is missing or not a valid JSON object');

  Context := TRttiContext.Create;
  try
    MethodType := Context.GetType(TObject(Method.Data).ClassType).GetMethod(JSONObject.GetValue<string>('name'));

    if Assigned(MethodType) then
    begin
      Params := MethodType.GetParameters;
      SetLength(Args, Length(Params));

      for I := 0 to High(Params) do
      begin
        ParamValue := ArgsObject.GetValue(Params[I].Name);

        if ParamValue = nil then
          raise Exception.CreateFmt('Missing required parameter: %s', [Params[I].Name]);

        case Params[I].ParamType.TypeKind of
          tkInteger: Args[I] := StrToIntDef(ParamValue.Value, 0);
          tkFloat: Args[I] := StrToFloatDef(ParamValue.Value, 0.0);
          tkString, tkLString, tkUString, tkWString: Args[I] := ParamValue.Value;
          tkEnumeration:
            if Params[I].ParamType.Handle = TypeInfo(Boolean) then
              Args[I] := SameText(ParamValue.Value, 'true')
            else
              raise Exception.CreateFmt('Unsupported enumeration type for parameter %s', [Params[I].Name]);
          tkClass: Args[I] := TObject(StrToIntDef(ParamValue.Value, 0));
          tkChar: Args[I] := ParamValue.Value[1];
        else
          raise Exception.CreateFmt('Unsupported parameter type for %s', [Params[I].Name]);
        end;
      end;

      ResultValue := MethodType.Invoke(TObject(Method.Data), Args);
      if ResultValue.Kind = tkUString then
        ReturnValue := ResultValue.AsString
      else
        ReturnValue := '';
    end
    else
      raise Exception.Create('Method not found');
  finally
    Context.Free;
  end;
end;


function TFunctionRegistry.GetAvailableFunctionsJSON(UseStrict: Boolean): TJSONArray;
var
  ToolsArray: TJSONArray;
  FuncDesc: TFunctionDescription;
  FunctionJSON, ToolObject: TJSONObject;
  functionArray: TArray<string>;
  functionName: string;
begin
  ToolsArray := TJSONArray.Create;
  functionArray := FMethods.Keys.ToArray;

  for functionName in functionArray do
  begin
    if FMethods.TryGetValue(functionName, FuncDesc) then
    begin
      ToolObject := TJSONObject.Create;
      try
        ToolObject.AddPair('name', FuncDesc.Name);
        ToolObject.AddPair('description', FuncDesc.Description);
        if UseStrict then
          ToolObject.AddPair('strict', TJSONBool.Create(True));
        ToolObject.AddPair('parameters', FuncDesc.Parameters.Clone as TJSONObject);

        FunctionJSON := TJSONObject.Create;
        try
          FunctionJSON.AddPair('type', 'function'); // Ensure 'type' is included
          FunctionJSON.AddPair('function', ToolObject);

          ToolsArray.AddElement(FunctionJSON);
        except
          FreeAndNil(FunctionJSON);
          raise;
        end;
      except
        FreeAndNil(ToolObject);
        raise;
      end;
    end;
  end;

  Result := ToolsArray;
end;

destructor TFunctionDescription.Destroy;
begin
  FreeAndNil(Parameters);
  inherited;
end;

end.

