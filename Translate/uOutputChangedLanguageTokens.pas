
{****************************************************************************************************************************************}
{                                                                                                                                        }
{                                                            XML Data Binding                                                            }
{                                                                                                                                        }
{         Generated on: 4/04/2023 8:46:02 PM                                                                                             }
{       Generated from: D:\Programming\ADUG\Symposium2023\Translate\ComponentDinosOffice-OpenOffice-versionOficial\Demo\ENA\output.xml   }
{   Settings stored in: D:\Programming\ADUG\Symposium2023\Translate\ComponentDinosOffice-OpenOffice-versionOficial\Demo\ENA\output.xdb   }
{                                                                                                                                        }
{****************************************************************************************************************************************}

unit uOutputChangedLanguageTokens;

interface

uses Xml.xmldom, Xml.XMLDoc, Xml.XMLIntf;

type

{ Forward Decls }

  IXMLVType = interface;
  IXMLWType = interface;

{ IXMLVType }

  IXMLVType = interface(IXMLNodeCollection)
    ['{318F4F52-9127-4304-A61B-FC24EEF679A4}']
    { Property Accessors }
    function Get_W(const Index: Integer): IXMLWType;
    { Methods & Properties }
    function Add: IXMLWType;
    function Insert(const Index: Integer): IXMLWType;
    property W[const Index: Integer]: IXMLWType read Get_W; default;
  end;

{ IXMLWType }

  IXMLWType = interface(IXMLNode)
    ['{80D24FE3-C0E7-4225-A503-53F2C6D8B48C}']
    { Property Accessors }
    function Get_X: Integer;
    function Get_Y: UnicodeString;
    procedure Set_X(const Value: Integer);
    procedure Set_Y(const Value: UnicodeString);
    { Methods & Properties }
    property X: Integer read Get_X write Set_X;
    property Y: UnicodeString read Get_Y write Set_Y;
  end;

{ Forward Decls }

  TXMLVType = class;
  TXMLWType = class;

{ TXMLVType }

  TXMLVType = class(TXMLNodeCollection, IXMLVType)
  protected
    { IXMLVType }
    function Get_W(const Index: Integer): IXMLWType;
    function Add: IXMLWType;
    function Insert(const Index: Integer): IXMLWType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLWType }

  TXMLWType = class(TXMLNode, IXMLWType)
  protected
    { IXMLWType }
    function Get_X: Integer;
    function Get_Y: UnicodeString;
    procedure Set_X(const Value: Integer);
    procedure Set_Y(const Value: UnicodeString);
  end;

{ Global Functions }

function Getv(Doc: IXMLDocument): IXMLVType;
function Loadv(const FileName: string): IXMLVType;
function Newv: IXMLVType;

const
  TargetNamespace = '';

implementation

uses System.Variants, System.SysUtils, Xml.xmlutil;

{ Global Functions }

function Getv(Doc: IXMLDocument): IXMLVType;
begin
  Result := Doc.GetDocBinding('v', TXMLVType, TargetNamespace) as IXMLVType;
end;

function Loadv(const FileName: string): IXMLVType;
begin
  Result := LoadXMLDocument(FileName).GetDocBinding('v', TXMLVType, TargetNamespace) as IXMLVType;
end;

function Newv: IXMLVType;
begin
  Result := NewXMLDocument.GetDocBinding('v', TXMLVType, TargetNamespace) as IXMLVType;
end;

{ TXMLVType }

procedure TXMLVType.AfterConstruction;
begin
  RegisterChildNode('w', TXMLWType);
  ItemTag := 'w';
  ItemInterface := IXMLWType;
  inherited;
end;

function TXMLVType.Get_W(const Index: Integer): IXMLWType;
begin
  Result := List[Index] as IXMLWType;
end;

function TXMLVType.Add: IXMLWType;
begin
  Result := AddItem(-1) as IXMLWType;
end;

function TXMLVType.Insert(const Index: Integer): IXMLWType;
begin
  Result := AddItem(Index) as IXMLWType;
end;

{ TXMLWType }

function TXMLWType.Get_X: Integer;
begin
  Result := XmlStrToInt(ChildNodes[WideString('x')].Text);
end;

procedure TXMLWType.Set_X(const Value: Integer);
begin
  ChildNodes[WideString('x')].NodeValue := Value;
end;

function TXMLWType.Get_Y: UnicodeString;
begin
  Result := ChildNodes[WideString('y')].Text;
end;

procedure TXMLWType.Set_Y(const Value: UnicodeString);
begin
  ChildNodes[WideString('y')].NodeValue := Value;
end;

end.