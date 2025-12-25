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

function GenerateJSONSchema(AClass: TClass): TJSONObject;
var
  Ctx: TRttiContext;
  RttiType: TRttiType;
  Prop: TRttiProperty;
  Attr: TCustomAttribute;
  PropsObj, PropObj, NestedSchema: TJSONObject;
  RequiredArr: TJSONArray;
  IsRequired: Boolean;
begin
  Result := TJSONObject.Create;
  PropsObj := TJSONObject.Create;
  RequiredArr := TJSONArray.Create;

  RttiType := Ctx.GetType(AClass);

  for Prop in RttiType.GetProperties do
  begin
    if not Prop.IsReadable then
      Continue;

    PropObj := TJSONObject.Create;
    IsRequired := False;

    if Prop.PropertyType.TypeKind in [tkString, tkUString] then
      PropObj.AddPair('type', 'string')
    else if Prop.PropertyType.TypeKind = tkInteger then
      PropObj.AddPair('type', 'integer')
    else if Prop.PropertyType.TypeKind = tkFloat then
      PropObj.AddPair('type', 'number')
    else if Prop.PropertyType.TypeKind = tkClass then
    begin
      if Prop.PropertyType.Name.StartsWith('TObjectList<') then
      begin
        PropObj.AddPair('type', 'array');
        var GenericName := Prop.PropertyType.ToString;
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
        PropObj := GenerateJSONSchema(Prop.PropertyType.AsInstance.MetaclassType);
      end;
    end;

    for Attr in Prop.GetAttributes do
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
        PropObj.AddPair('minimum', TJSONNumber.Create(RA.Min));
        PropObj.AddPair('maximum', TJSONNumber.Create(RA.Max));
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

    PropsObj.AddPair(Prop.Name, PropObj);

    if IsRequired then
      RequiredArr.Add(Prop.Name);
  end;

  Result.AddPair('type', 'object');
  Result.AddPair('properties', PropsObj);

  if RequiredArr.Count > 0 then
    Result.AddPair('required', RequiredArr);
end;

end.