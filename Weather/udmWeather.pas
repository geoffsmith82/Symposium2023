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
  ActiveX,
  Xml.xmldom,
  Xml.XMLIntf,
  Xml.XMLDoc,
  uXMLBOMPrecis;

type
  TdmWeather = class(TDataModule)
    IdFTP1: TIdFTP;
    XMLDocument1: TXMLDocument;
    procedure DataModuleDestroy(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    function GetBomForecast(location: string; state: string): IXMLAreaType;
  end;


implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

procedure TdmWeather.DataModuleDestroy(Sender: TObject);
begin
  CoUninitialize;
end;

procedure TdmWeather.DataModuleCreate(Sender: TObject);
begin
  CoInitialize(nil);
end;

{ TdmWeather }

function TdmWeather.GetBomForecast(location: string; state: string): IXMLAreaType;
var
  memStream : TMemoryStream;
  forcast : IXMLForecastType;
  i : Integer;
  product : IXMLProductType;
begin
  IdFTP1.Connect;

  memStream := TStringStream.Create;
  try
    IdFTP1.Get('/anon/gen/fwo/IDV10753.xml', memStream, False);
    XMLDocument1.LoadFromStream(memStream);
    product := uXMLBOMPrecis.Getproduct(XMLDocument1);
    for i := 0 to product.Forecast.Count - 1 do
    begin
      if product.Forecast[i].Description = location then
      begin
        Result := product.Forecast.Area[i];
        Exit;
      end;
    end;

  finally
    FreeAndNil(memStream)
  end;
end;

end.
