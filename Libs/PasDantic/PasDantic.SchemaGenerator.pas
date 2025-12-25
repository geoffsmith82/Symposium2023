unit PasDantic.SchemaGenerator;

interface

uses
  System.SysUtils,
  System.Rtti,
  System.TypInfo,
  System.JSON,
  System.Generics.Collections,
  PasDantic.Attributes;

function GenerateJSONSchema(AClass: TClass): TJSONObject;

implementation


function IsIntegerType(const AType: TRttiType): Boolean;
begin
  Result := AType.TypeKind in [tkInteger, tkInt64];
end;

function IsFloatType(const AType: TRttiType): Boolean;
begin
  Result := AType.TypeKind in [tkFloat];
end;

function GenerateJSONSchema(AClass: TClass): TJSONObject;
var
  Ctx: TRttiContext;
  RttiType: TRttiType;
  Field: TRttiField;
  Attr: TCustomAttribute;
  DefaultAttr: DefaultAttribute;
  PropsObj, PropObj, NestedSchema: TJSONObject;
  RequiredArr: TJSONArray;
  IsRequired: Boolean;
begin
  Result := TJSONObject.Create;
  PropsObj := TJSONObject.Create;
  RequiredArr := TJSONArray.Create;

  RttiType := Ctx.GetType(AClass);

  for Field in RttiType.GetFields do
  begin
    if not Field.IsReadable then
      Continue;

    PropObj := TJSONObject.Create;
    IsRequired := False;

    if Field.FieldType.TypeKind in [tkString, tkUString] then
      PropObj.AddPair('type', 'string')
    else if Field.FieldType.TypeKind = tkInteger then
      PropObj.AddPair('type', 'integer')
    else if Field.FieldType.TypeKind = tkFloat then
      PropObj.AddPair('type', 'number')
    else if Field.FieldType.TypeKind = tkClass then
    begin
      if Field.FieldType.Name.StartsWith('TObjectList<') then
      begin
        PropObj.AddPair('type', 'array');
        var GenericName := Field.FieldType.ToString;
        var InnerClassName := Copy(GenericName, Pos('<', GenericName) + 1, Length(GenericName) - Pos('<', GenericName) - 1);
        var InnerRttiType := Ctx.FindType(InnerClassName);
        if Assigned(InnerRttiType) and (InnerRttiType.TypeKind = tkClass) then
        begin
          NestedSchema := GenerateJSONSchema(InnerRttiType.AsInstance.MetaclassType);
          PropObj.AddPair('items', NestedSchema);
        end;
      end
      else
      begin
        PropObj := GenerateJSONSchema(Field.FieldType.AsInstance.MetaclassType);
      end;
    end;

    for Attr in Field.GetAttributes do
    begin
      if Attr is RequiredAttribute then
        IsRequired := True;
      if Attr is StringLengthAttribute then
      begin
        var SL := StringLengthAttribute(Attr);
        PropObj.AddPair('minLength', TJSONNumber.Create(SL.Min));
        PropObj.AddPair('maxLength', TJSONNumber.Create(SL.Max));
      end;
      if Attr is RangeAttribute then
      begin
        var RA := RangeAttribute(Attr);

        if IsIntegerType(Field.FieldType) then
        begin
          PropObj.AddPair('minimum', TJSONNumber.Create(Trunc(RA.Min)));
          PropObj.AddPair('maximum', TJSONNumber.Create(Trunc(RA.Max)));
        end
        else
        begin
          PropObj.AddPair('minimum', TJSONNumber.Create(RA.Min));
          PropObj.AddPair('maximum', TJSONNumber.Create(RA.Max));
        end;
      end;
      if Attr is DefaultAttribute then
      begin
        DefaultAttr := DefaultAttribute(Attr);

        case Field.FieldType.TypeKind of
          tkInteger, tkInt64:
            PropObj.AddPair('default', TJSONNumber.Create(DefaultAttr.Value.AsInteger));
          tkFloat:
            PropObj.AddPair('default', TJSONNumber.Create(DefaultAttr.Value.AsExtended));
          tkString, tkUString:
            PropObj.AddPair('default', DefaultAttr.Value.AsString);
        end;
      end;
      if Attr is AllowedValuesAttribute then
      begin
        var AA := AllowedValuesAttribute(Attr);
        var EnumArr := TJSONArray.Create;
        for var V in AA.Allowed do
          EnumArr.Add(V);
        PropObj.AddPair('enum', EnumArr);
      end;
      if Attr is RegexAttribute then
        PropObj.AddPair('pattern', RegexAttribute(Attr).Pattern);
      if Attr is ExampleAttribute then
        PropObj.AddPair('example', ExampleAttribute(Attr).Example);
    end;

    PropsObj.AddPair(Field.Name, PropObj);

    IsRequired := True;

    if Field.HasAttribute<OptionalAttribute> then
      IsRequired := False;

    if IsRequired then
    begin
      var HasDefault := Field.HasAttribute<DefaultAttribute>;
      if not HasDefault then
        RequiredArr.Add(Field.Name);
    end;
  end;

  Result.AddPair('type', 'object');
  Result.AddPair('properties', PropsObj);

  if RequiredArr.Count > 0 then
    Result.AddPair('required', RequiredArr);
end;

end.