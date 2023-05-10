unit udmEmbeddings;

interface

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Error,
  FireDAC.UI.Intf,
  FireDAC.Phys.Intf,
  FireDAC.Stan.Def,
  FireDAC.Stan.Pool,
  FireDAC.Stan.Async,
  FireDAC.Phys,
  FireDAC.Phys.PG,
  FireDAC.Phys.PGDef,
  FireDAC.VCLUI.Wait,
  FireDAC.Comp.Client,
  Data.DB,
  uGPT
  ;

type
  TdmEmbeddings = class(TDataModule)
    FDConnection1: TFDConnection;
    PhysPgDriverLink: TFDPhysPgDriverLink;
    procedure DataModuleCreate(Sender: TObject);
  private
    procedure AddSegment(docID: Int64; segString: String; segmentEmbedding: TEmbedding);
    procedure AddDocument(docString: string; docFilename: String; docMetadata: TJSONObject);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dmEmbeddings: TdmEmbeddings;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

procedure TdmEmbeddings.AddDocument(docString: string; docFilename: String; docMetadata: TJSONObject);
begin

end;

procedure TdmEmbeddings.AddSegment(docID: Int64; segString: String; segmentEmbedding: TEmbedding);
begin

end;

procedure TdmEmbeddings.DataModuleCreate(Sender: TObject);
begin
  FDConnection1.Connected := True;
end;

end.
