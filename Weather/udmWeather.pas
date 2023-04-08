unit udmWeather;

interface

uses
  System.SysUtils,
  System.Classes,
  IdBaseComponent,
  IdComponent,
  IdTCPConnection,
  IdTCPClient,
  IdExplicitTLSClientServerBase,
  IdFTP,
  Xml.xmldom,
  Xml.XMLIntf,
  Xml.XMLDoc;

type
  TdmWeather = class(TDataModule)
    IdFTP1: TIdFTP;
    XMLDocument1: TXMLDocument;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dmWeather: TdmWeather;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

end.
