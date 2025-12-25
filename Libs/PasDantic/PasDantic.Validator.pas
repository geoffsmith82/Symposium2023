unit PasDantic.Validator;

interface

uses
  System.Rtti,
  System.SysUtils,
  System.Generics.Collections,
  System.RegularExpressions,
  FMX.Types,
  Pasdantic.Attributes;

type
  TValidationError = record
    PropertyPath: string;
    Message: string;
    constructor Create(const APath: string; const AMessage: string);
  end;

  TValidationResult = record
    Errors: TArray<TValidationError>;
    function IsValid: Boolean;
    function ErrorCount: Integer;
  end;

function ValidateModel(AInstance: TObject; const APath: string = ''): TValidationResult;

implementation

function ValidateModel(AInstance: TObject; const APath: string): TValidationResult;
var
  Ctx: TRttiContext;
  RttiType: TRttiType;
//  Prop: TRttiProperty;
  Field: TRttiField;
  Attr: TCustomAttribute;
  Value: TValue;
  Errors: TList<TValidationError>;
  NestedResult: TValidationResult;
  FullPath: string;

  SLAttr: StringLengthAttribute;
  RA: RangeAttribute;
  RegexAttr: RegexAttribute;
  AllowedAttr: AllowedValuesAttribute;

  NumValue: Double;
  Found: Boolean;
  V: string;
begin
  Errors := TList<TValidationError>.Create;
  try
    Ctx := TRttiContext.Create;
    RttiType := Ctx.GetType(AInstance.ClassType);

    for Field in RttiType.GetFields  do
    begin
      Log.d('Type: ' + TObject.ClassName + ' FieldName: ' + Field.Name);
      if not Field.IsReadable then
        Continue;

      Value := Field.GetValue(AInstance);
      FullPath := APath + Field.Name;

      { ---- Nested object validation ---- }
      if (Value.Kind = tkClass) and (Value.AsObject <> nil) then
      begin
        NestedResult := ValidateModel(Value.AsObject, FullPath + '.');
        for var E in NestedResult.Errors do
          Errors.Add(E);
        Continue;
      end;

      for Attr in Field.GetAttributes do
      begin
        { ---- Required ---- }
        if Attr is RequiredAttribute then
        begin
          if Value.IsEmpty or
             ((Value.Kind in [tkString, tkUString]) and (Value.AsString = '')) then
          begin
            Errors.Add(
              TValidationError.Create(FullPath, 'field required')
            );
          end;
        end

        { ---- String length ---- }
        else if Attr is StringLengthAttribute then
        begin
          if Value.Kind in [tkString, tkUString] then
          begin
            SLAttr := StringLengthAttribute(Attr);
            if (Length(Value.AsString) < SLAttr.Min) or
               (Length(Value.AsString) > SLAttr.Max) then
            begin
              Errors.Add(
                TValidationError.Create(
                  FullPath,
                  Format('length must be between %d and %d',
                    [SLAttr.Min, SLAttr.Max])
                )
              );
            end;
          end;
        end

        { ---- Numeric range ---- }
        else if Attr is RangeAttribute then
        begin
          if Value.TryAsType<Double>(NumValue) then
          begin
            RA := RangeAttribute(Attr);
            if (NumValue < RA.Min) or (NumValue > RA.Max) then
            begin
              Errors.Add(
                TValidationError.Create(
                  FullPath,
                  Format('must be between %g and %g', [RA.Min, RA.Max])
                )
              );
            end;
          end;
        end

        { ---- Regex ---- }
        else if Attr is RegexAttribute then
        begin
          if Value.Kind in [tkString, tkUString] then
          begin
            RegexAttr := RegexAttribute(Attr);
            if not TRegEx.IsMatch(Value.AsString, RegexAttr.Pattern) then
            begin
              Errors.Add(
                TValidationError.Create(
                  FullPath,
                  Format('string does not match pattern "%s"',
                    [RegexAttr.Pattern])
                )
              );
            end;
          end;
        end

        { ---- Allowed values ---- }
        else if Attr is AllowedValuesAttribute then
        begin
          if Value.Kind in [tkString, tkUString] then
          begin
            AllowedAttr := AllowedValuesAttribute(Attr);
            Found := False;

            for V in AllowedAttr.Allowed do
              if SameText(Value.AsString, V) then
              begin
                Found := True;
                Break;
              end;

            if not Found then
            begin
              Errors.Add(
                TValidationError.Create(
                  FullPath,
                  Format('must be one of: %s',
                    [string.Join(', ', AllowedAttr.Allowed)])
                )
              );
            end;
          end;
        end;
      end;
    end;

    Result.Errors := Errors.ToArray;
  finally
    Errors.Free;
  end;
end;

{ TValidationError }

constructor TValidationError.Create(const APath, AMessage: string);
begin
  PropertyPath := APath;
  Message := AMessage;
end;

{ TValidationResult }

function TValidationResult.ErrorCount: Integer;
begin
  Result := Length(Errors);
end;

function TValidationResult.IsValid: Boolean;
begin
  Result := ErrorCount = 0;
end;

end.