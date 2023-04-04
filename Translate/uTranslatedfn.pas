
{****************************************************************************************************************************************}
{                                                                                                                                        }
{                                                            XML Data Binding                                                            }
{                                                                                                                                        }
{         Generated on: 3/04/2023 12:59:03 AM                                                                                            }
{       Generated from: D:\Programming\ADUG\Symposium2023\Translate\ComponentDinosOffice-OpenOffice-versionOficial\Demo\ENA\Unit12.xml   }
{   Settings stored in: D:\Programming\ADUG\Symposium2023\Translate\ComponentDinosOffice-OpenOffice-versionOficial\Demo\ENA\Unit12.xdb   }
{                                                                                                                                        }
{****************************************************************************************************************************************}

unit uTranslatedfn;

interface

uses Xml.xmldom, Xml.XMLDoc, Xml.XMLIntf;

type

{ Forward Decls }

  IXMLXliffType = interface;
  IXMLFileType = interface;
  IXMLHeaderType = interface;
  IXMLPropgroupType = interface;
  IXMLPropType = interface;
  IXMLBodyType = interface;
  IXMLTransunitType = interface;

{ IXMLXliffType }

  IXMLXliffType = interface(IXMLNode)
    ['{7403AA5B-478C-4525-893F-715B42EACB86}']
    { Property Accessors }
    function Get_Version: Double;
    function Get_File_: IXMLFileType;
    procedure Set_Version(const Value: Double);
    { Methods & Properties }
    property Version: Double read Get_Version write Set_Version;
    property File_: IXMLFileType read Get_File_;
  end;

{ IXMLFileType }

  IXMLFileType = interface(IXMLNode)
    ['{2794DFA9-74BA-418B-8054-30342D2A1267}']
    { Property Accessors }
    function Get_Original: UnicodeString;
    function Get_Sourcelanguage: UnicodeString;
    function Get_Targetlanguage: UnicodeString;
    function Get_Datatype: UnicodeString;
    function Get_Tool: UnicodeString;
    function Get_Ts: UnicodeString;
    function Get_Header: IXMLHeaderType;
    function Get_Body: IXMLBodyType;
    procedure Set_Original(const Value: UnicodeString);
    procedure Set_Sourcelanguage(const Value: UnicodeString);
    procedure Set_Targetlanguage(const Value: UnicodeString);
    procedure Set_Datatype(const Value: UnicodeString);
    procedure Set_Tool(const Value: UnicodeString);
    procedure Set_Ts(const Value: UnicodeString);
    { Methods & Properties }
    property Original: UnicodeString read Get_Original write Set_Original;
    property Sourcelanguage: UnicodeString read Get_Sourcelanguage write Set_Sourcelanguage;
    property Targetlanguage: UnicodeString read Get_Targetlanguage write Set_Targetlanguage;
    property Datatype: UnicodeString read Get_Datatype write Set_Datatype;
    property Tool: UnicodeString read Get_Tool write Set_Tool;
    property Ts: UnicodeString read Get_Ts write Set_Ts;
    property Header: IXMLHeaderType read Get_Header;
    property Body: IXMLBodyType read Get_Body;
  end;

{ IXMLHeaderType }

  IXMLHeaderType = interface(IXMLNode)
    ['{50CEC91D-64C6-467C-8ADB-F6900701CD6B}']
    { Property Accessors }
    function Get_Propgroup: IXMLPropgroupType;
    { Methods & Properties }
    property Propgroup: IXMLPropgroupType read Get_Propgroup;
  end;

{ IXMLPropgroupType }

  IXMLPropgroupType = interface(IXMLNodeCollection)
    ['{5D27C9A3-9C6C-4BF6-8D8D-3136A7DF0964}']
    { Property Accessors }
    function Get_Name: UnicodeString;
    function Get_Prop(const Index: Integer): IXMLPropType;
    procedure Set_Name(const Value: UnicodeString);
    { Methods & Properties }
    function Add: IXMLPropType;
    function Insert(const Index: Integer): IXMLPropType;
    property Name: UnicodeString read Get_Name write Set_Name;
    property Prop[const Index: Integer]: IXMLPropType read Get_Prop; default;
  end;

{ IXMLPropType }

  IXMLPropType = interface(IXMLNode)
    ['{16A28731-B664-4032-A8B8-488D423E2811}']
    { Property Accessors }
    function Get_Proptype: UnicodeString;
    procedure Set_Proptype(const Value: UnicodeString);
    { Methods & Properties }
    property Proptype: UnicodeString read Get_Proptype write Set_Proptype;
  end;

{ IXMLBodyType }

  IXMLBodyType = interface(IXMLNodeCollection)
    ['{29303ADB-0BD6-4988-91CE-66F76CB3F5CC}']
    { Property Accessors }
    function Get_Transunit(const Index: Integer): IXMLTransunitType;
    { Methods & Properties }
    function Add: IXMLTransunitType;
    function Insert(const Index: Integer): IXMLTransunitType;
    property Transunit[const Index: Integer]: IXMLTransunitType read Get_Transunit; default;
  end;

{ IXMLTransunitType }

  IXMLTransunitType = interface(IXMLNode)
    ['{7FC976B8-94AB-4BC2-AA87-F85766D57C5D}']
    { Property Accessors }
    function Get_Id: UnicodeString;
    function Get_Resname: UnicodeString;
    function Get_Source: UnicodeString;
    function Get_Propgroup: IXMLPropgroupType;
    function Get_Target: UnicodeString;
    procedure Set_Id(const Value: UnicodeString);
    procedure Set_Resname(const Value: UnicodeString);
    procedure Set_Source(const Value: UnicodeString);
    procedure Set_Target(const Value: UnicodeString);
    { Methods & Properties }
    property Id: UnicodeString read Get_Id write Set_Id;
    property Resname: UnicodeString read Get_Resname write Set_Resname;
    property Source: UnicodeString read Get_Source write Set_Source;
    property Propgroup: IXMLPropgroupType read Get_Propgroup;
    property Target: UnicodeString read Get_Target write Set_Target;
  end;

{ Forward Decls }

  TXMLXliffType = class;
  TXMLFileType = class;
  TXMLHeaderType = class;
  TXMLPropgroupType = class;
  TXMLPropType = class;
  TXMLBodyType = class;
  TXMLTransunitType = class;

{ TXMLXliffType }

  TXMLXliffType = class(TXMLNode, IXMLXliffType)
  protected
    { IXMLXliffType }
    function Get_Version: Double;
    function Get_File_: IXMLFileType;
    procedure Set_Version(const Value: Double);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLFileType }

  TXMLFileType = class(TXMLNode, IXMLFileType)
  protected
    { IXMLFileType }
    function Get_Original: UnicodeString;
    function Get_Sourcelanguage: UnicodeString;
    function Get_Targetlanguage: UnicodeString;
    function Get_Datatype: UnicodeString;
    function Get_Tool: UnicodeString;
    function Get_Ts: UnicodeString;
    function Get_Header: IXMLHeaderType;
    function Get_Body: IXMLBodyType;
    procedure Set_Original(const Value: UnicodeString);
    procedure Set_Sourcelanguage(const Value: UnicodeString);
    procedure Set_Targetlanguage(const Value: UnicodeString);
    procedure Set_Datatype(const Value: UnicodeString);
    procedure Set_Tool(const Value: UnicodeString);
    procedure Set_Ts(const Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLHeaderType }

  TXMLHeaderType = class(TXMLNode, IXMLHeaderType)
  protected
    { IXMLHeaderType }
    function Get_Propgroup: IXMLPropgroupType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLPropgroupType }

  TXMLPropgroupType = class(TXMLNodeCollection, IXMLPropgroupType)
  protected
    { IXMLPropgroupType }
    function Get_Name: UnicodeString;
    function Get_Prop(const Index: Integer): IXMLPropType;
    procedure Set_Name(const Value: UnicodeString);
    function Add: IXMLPropType;
    function Insert(const Index: Integer): IXMLPropType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLPropType }

  TXMLPropType = class(TXMLNode, IXMLPropType)
  protected
    { IXMLPropType }
    function Get_Proptype: UnicodeString;
    procedure Set_Proptype(const Value: UnicodeString);
  end;

{ TXMLBodyType }

  TXMLBodyType = class(TXMLNodeCollection, IXMLBodyType)
  protected
    { IXMLBodyType }
    function Get_Transunit(const Index: Integer): IXMLTransunitType;
    function Add: IXMLTransunitType;
    function Insert(const Index: Integer): IXMLTransunitType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLTransunitType }

  TXMLTransunitType = class(TXMLNode, IXMLTransunitType)
  protected
    { IXMLTransunitType }
    function Get_Id: UnicodeString;
    function Get_Resname: UnicodeString;
    function Get_Source: UnicodeString;
    function Get_Propgroup: IXMLPropgroupType;
    function Get_Target: UnicodeString;
    procedure Set_Id(const Value: UnicodeString);
    procedure Set_Resname(const Value: UnicodeString);
    procedure Set_Source(const Value: UnicodeString);
    procedure Set_Target(const Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ Global Functions }

function Getxliff(Doc: IXMLDocument): IXMLXliffType;
function Loadxliff(const FileName: string): IXMLXliffType;
function Newxliff: IXMLXliffType;

const
  TargetNamespace = '';

implementation

uses System.Variants, System.SysUtils, Xml.xmlutil;

{ Global Functions }

function Getxliff(Doc: IXMLDocument): IXMLXliffType;
begin
  Result := Doc.GetDocBinding('xliff', TXMLXliffType, TargetNamespace) as IXMLXliffType;
end;

function Loadxliff(const FileName: string): IXMLXliffType;
begin
  Result := LoadXMLDocument(FileName).GetDocBinding('xliff', TXMLXliffType, TargetNamespace) as IXMLXliffType;
end;

function Newxliff: IXMLXliffType;
begin
  Result := NewXMLDocument.GetDocBinding('xliff', TXMLXliffType, TargetNamespace) as IXMLXliffType;
end;

{ TXMLXliffType }

procedure TXMLXliffType.AfterConstruction;
begin
  RegisterChildNode('file', TXMLFileType);
  inherited;
end;

function TXMLXliffType.Get_Version: Double;
begin
  Result := XmlStrToFloatExt(AttributeNodes['version'].Text);
end;

procedure TXMLXliffType.Set_Version(const Value: Double);
begin
  SetAttribute('version', Value);
end;

function TXMLXliffType.Get_File_: IXMLFileType;
begin
  Result := ChildNodes['file'] as IXMLFileType;
end;

{ TXMLFileType }

procedure TXMLFileType.AfterConstruction;
begin
  RegisterChildNode('header', TXMLHeaderType);
  RegisterChildNode('body', TXMLBodyType);
  inherited;
end;

function TXMLFileType.Get_Original: UnicodeString;
begin
  Result := AttributeNodes['original'].Text;
end;

procedure TXMLFileType.Set_Original(const Value: UnicodeString);
begin
  SetAttribute('original', Value);
end;

function TXMLFileType.Get_Sourcelanguage: UnicodeString;
begin
  Result := AttributeNodes['source-language'].Text;
end;

procedure TXMLFileType.Set_Sourcelanguage(const Value: UnicodeString);
begin
  SetAttribute('source-language', Value);
end;

function TXMLFileType.Get_Targetlanguage: UnicodeString;
begin
  Result := AttributeNodes['target-language'].Text;
end;

procedure TXMLFileType.Set_Targetlanguage(const Value: UnicodeString);
begin
  SetAttribute('target-language', Value);
end;

function TXMLFileType.Get_Datatype: UnicodeString;
begin
  Result := AttributeNodes['datatype'].Text;
end;

procedure TXMLFileType.Set_Datatype(const Value: UnicodeString);
begin
  SetAttribute('datatype', Value);
end;

function TXMLFileType.Get_Tool: UnicodeString;
begin
  Result := AttributeNodes['tool'].Text;
end;

procedure TXMLFileType.Set_Tool(const Value: UnicodeString);
begin
  SetAttribute('tool', Value);
end;

function TXMLFileType.Get_Ts: UnicodeString;
begin
  Result := AttributeNodes['ts'].Text;
end;

procedure TXMLFileType.Set_Ts(const Value: UnicodeString);
begin
  SetAttribute('ts', Value);
end;

function TXMLFileType.Get_Header: IXMLHeaderType;
begin
  Result := ChildNodes['header'] as IXMLHeaderType;
end;

function TXMLFileType.Get_Body: IXMLBodyType;
begin
  Result := ChildNodes['body'] as IXMLBodyType;
end;

{ TXMLHeaderType }

procedure TXMLHeaderType.AfterConstruction;
begin
  RegisterChildNode('prop-group', TXMLPropgroupType);
  inherited;
end;

function TXMLHeaderType.Get_Propgroup: IXMLPropgroupType;
begin
  Result := ChildNodes['prop-group'] as IXMLPropgroupType;
end;

{ TXMLPropgroupType }

procedure TXMLPropgroupType.AfterConstruction;
begin
  RegisterChildNode('prop', TXMLPropType);
  ItemTag := 'prop';
  ItemInterface := IXMLPropType;
  inherited;
end;

function TXMLPropgroupType.Get_Name: UnicodeString;
begin
  Result := AttributeNodes['name'].Text;
end;

procedure TXMLPropgroupType.Set_Name(const Value: UnicodeString);
begin
  SetAttribute('name', Value);
end;

function TXMLPropgroupType.Get_Prop(const Index: Integer): IXMLPropType;
begin
  Result := List[Index] as IXMLPropType;
end;

function TXMLPropgroupType.Add: IXMLPropType;
begin
  Result := AddItem(-1) as IXMLPropType;
end;

function TXMLPropgroupType.Insert(const Index: Integer): IXMLPropType;
begin
  Result := AddItem(Index) as IXMLPropType;
end;

{ TXMLPropType }

function TXMLPropType.Get_Proptype: UnicodeString;
begin
  Result := AttributeNodes['prop-type'].Text;
end;

procedure TXMLPropType.Set_Proptype(const Value: UnicodeString);
begin
  SetAttribute('prop-type', Value);
end;

{ TXMLBodyType }

procedure TXMLBodyType.AfterConstruction;
begin
  RegisterChildNode('trans-unit', TXMLTransunitType);
  ItemTag := 'trans-unit';
  ItemInterface := IXMLTransunitType;
  inherited;
end;

function TXMLBodyType.Get_Transunit(const Index: Integer): IXMLTransunitType;
begin
  Result := List[Index] as IXMLTransunitType;
end;

function TXMLBodyType.Add: IXMLTransunitType;
begin
  Result := AddItem(-1) as IXMLTransunitType;
end;

function TXMLBodyType.Insert(const Index: Integer): IXMLTransunitType;
begin
  Result := AddItem(Index) as IXMLTransunitType;
end;

{ TXMLTransunitType }

procedure TXMLTransunitType.AfterConstruction;
begin
  RegisterChildNode('prop-group', TXMLPropgroupType);
  inherited;
end;

function TXMLTransunitType.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLTransunitType.Set_Id(const Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLTransunitType.Get_Resname: UnicodeString;
begin
  Result := AttributeNodes['resname'].Text;
end;

procedure TXMLTransunitType.Set_Resname(const Value: UnicodeString);
begin
  SetAttribute('resname', Value);
end;

function TXMLTransunitType.Get_Source: UnicodeString;
begin
  Result := ChildNodes['source'].Text;
end;

procedure TXMLTransunitType.Set_Source(const Value: UnicodeString);
begin
  ChildNodes['source'].NodeValue := Value;
end;

function TXMLTransunitType.Get_Propgroup: IXMLPropgroupType;
begin
  Result := ChildNodes['prop-group'] as IXMLPropgroupType;
end;

function TXMLTransunitType.Get_Target: UnicodeString;
begin
  Result := ChildNodes['target'].Text;
end;

procedure TXMLTransunitType.Set_Target(const Value: UnicodeString);
begin
  ChildNodes['target'].NodeValue := Value;
end;

end.