unit uLLM.Functions;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Rtti, System.TypInfo, System.Generics.Collections, uAttributes;

type
  TFunctionDescription = class
    Name: string;
    Description: string;
    Parameters: TJSONObject;
    constructor Create(const AName, ADescription: string; const AParameters: TJSONObject);
  end;

  TFunctionRegistry = class
  private
    FMethods: TDictionary<string, TMethod>;
    FFunctionDescriptions: TObjectList<TFunctionDescription>;
    procedure InvokeFunctionFromJSON(const Method: TMethod; const JSONStr: string; out ReturnValue: string);
    function GenerateParameterJSON(Method: TRttiMethod): TJSONObject;
    function GetJSONTypeFromRTTI(AType: TRttiType): string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RegisterFunction(const Func: Pointer; const Instance: TObject);
    procedure InvokeFunction(const JSONObject: TJSONObject; out ReturnValue: string);
    function GetAvailableFunctionsJSON: string;
  end;

implementation

constructor TFunctionDescription.Create(const AName, ADescription: string; const AParameters: TJSONObject);
begin
  Name := AName;
  Description := ADescription;
  Parameters := AParameters;
end;

constructor TFunctionRegistry.Create;
begin
  FMethods := TDictionary<string, TMethod>.Create;
  FFunctionDescriptions := TObjectList<TFunctionDescription>.Create;
end;

destructor TFunctionRegistry.Destroy;
begin
  FMethods.Free;
  FFunctionDescriptions.Free;
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

        FMethods.Add(MethodRtti.Name, Method);
        Parameters := GenerateParameterJSON(MethodRtti);
        FFunctionDescriptions.Add(TFunctionDescription.Create(MethodRtti.Name, FunctionDescription, Parameters));
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
  Method: TMethod;
begin
  FunctionName := JSONObject.GetValue<string>('name');
  if FMethods.TryGetValue(FunctionName, Method) then
  begin
    InvokeFunctionFromJSON(Method, JSONObject.ToJSON, ReturnValue);
  end
  else
    raise Exception.Create('Function not registered');
end;

procedure TFunctionRegistry.InvokeFunctionFromJSON(const Method: TMethod; const JSONStr: string; out ReturnValue: string);
var
  JSONObject: TJSONObject;
  ArgsObject: TJSONObject;
  Context: TRttiContext;
  MethodType: TRttiMethod;
  Params: TArray<TRttiParameter>;
  Args: TArray<TValue>;
  ParamValue: TJSONValue;
  ParamStr: string;
  I: Integer;
  ResultValue: TValue;
begin
  JSONObject := TJSONObject.ParseJSONValue(JSONStr) as TJSONObject;
  try
    ParamStr := JSONObject.GetValue<string>('arguments');
    ArgsObject := TJSONObject.ParseJSONValue(ParamStr) as TJSONObject;

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

          case Params[I].ParamType.TypeKind of
            tkInteger: Args[I] := StrToInt(ParamValue.Value);
            tkString, tkLString, tkUString, tkWString: Args[I] := ParamValue.Value;
            tkFloat: Args[I] := StrToFloat(ParamValue.Value);
            tkEnumeration:
              if Params[I].ParamType.Handle = TypeInfo(Boolean) then
                Args[I] := SameText(ParamValue.Value, 'true')
              else
                raise Exception.Create('Unsupported enumeration type');
            tkClass: Args[I] := TObject(StrToInt(ParamValue.Value));
            // Add more cases for other types as needed
          else
            raise Exception.Create('Unsupported parameter type');
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
  finally
    JSONObject.Free;
  end;
end;


function TFunctionRegistry.GetAvailableFunctionsJSON: string;
var
  ToolsArray: TJSONArray;
  FuncDesc: TFunctionDescription;
  FunctionJSON: TJSONObject;
  ToolObject: TJSONObject;
  AvailableFunctions: TJSONObject;
begin
  ToolsArray := TJSONArray.Create;
  try
    for FuncDesc in FFunctionDescriptions do
    begin
      ToolObject := TJSONObject.Create;
      ToolObject.AddPair('name', FuncDesc.Name);
      ToolObject.AddPair('description', FuncDesc.Description);
      ToolObject.AddPair('parameters', FuncDesc.Parameters);
      FunctionJSON := TJSONObject.Create;
      FunctionJSON.AddPair('type', 'function'); // Ensure 'type' is included
      FunctionJSON.AddPair('function', ToolObject);

      ToolsArray.AddElement(FunctionJSON);
    end;
    AvailableFunctions := TJSONObject.Create;
    AvailableFunctions.AddPair('tools', ToolsArray);
    Result := AvailableFunctions.ToJSON;
  finally
    FreeAndNil(AvailableFunctions);
  end;
end;

end.

