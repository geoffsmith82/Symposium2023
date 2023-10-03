unit udmEmbeddings;

interface

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  FireDAC.DApt,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Error,
  FireDAC.Stan.Param,
  FireDAC.Stan.Def,
  FireDAC.Stan.Pool,
  FireDAC.Stan.Async,
  FireDAC.Phys.Intf,
  FireDAC.Phys,
  FireDAC.Phys.PG,
  FireDAC.Phys.PGDef,
  FireDAC.UI.Intf,
  FireDAC.VCLUI.Wait,
  FireDAC.Comp.Client,
  Data.DB,
  uEmbeddings,
  uLLM
  ;

type
  TdmEmbeddings = class(TDataModule)
    EmbeddingConnection: TFDConnection;
    PhysPgDriverLink: TFDPhysPgDriverLink;
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure AddSegment(docID: Int64; segString: String; segmentEmbedding: TEmbedding);
    procedure AddDocument(embedding: TEmbeddings ; docString: TArray<string>; docFilename: String; docMetadata: TJSONObject);
    function LookupSections(embedding: TEmbedding; inCount: Integer): TArray<string>;
  end;

var
  dmEmbeddings: TdmEmbeddings;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

procedure TdmEmbeddings.AddDocument(embedding: TEmbeddings; docString: TArray<string>; docFilename: String; docMetadata: TJSONObject);
var
  sql : string;
  query : TFDQuery;
  I: Integer;
  docID : Int64;
begin
  sql := 'INSERT INTO documents (documentName, "meta-data") VALUES (:docName, :meta);';
  query := TFDQuery.Create(nil);
  try
    query.Connection := EmbeddingConnection;
    query.SQL.Text := sql;
    query.ParamByName('docName').AsString := docFilename;
    query.ParamByName('meta').AsString := docMetadata.ToJSON;
   // query.ExecSQL;
    docID := 1;
    for I := 0 to High(docString) do
    begin
      AddSegment(docID, docString[i], embedding[i]);
    end;
  finally
    FreeAndNil(query);
  end;
end;

procedure TdmEmbeddings.AddSegment(docID: Int64; segString: String; segmentEmbedding: TEmbedding);
var
  sql : string;
  embeddingVector : string;
  I: Integer;
  query : TFDQuery;
begin
  embeddingVector := '[' + segmentEmbedding[0].ToString;
  for I := 1 to High(segmentEmbedding) do
  begin
    embeddingVector := embeddingVector + ',' + segmentEmbedding[i].ToString;
  end;
  embeddingVector := embeddingVector + ']';

  sql := 'INSERT INTO "document-segment" ("doc-id", text, embedding) VALUES (:docID, :text, cast(:embedding as VECTOR));';

  query := TFDQuery.Create(nil);
  try
    query.Connection := EmbeddingConnection;
    query.SQL.Text := sql;
    query.ParamByName('docID').AsLargeInt := docID;
    query.ParamByName('text').AsString := segString;
    query.ParamByName('embedding').Size := 40000;
    query.ParamByName('embedding').AsString := embeddingVector;
    query.ExecSQL;
  finally
    FreeAndNil(query);
  end;
end;

procedure TdmEmbeddings.DataModuleCreate(Sender: TObject);
begin
  EmbeddingConnection.Connected := True;
end;

function TdmEmbeddings.LookupSections(embedding: TEmbedding; inCount: Integer): TArray<string>;
var
  query : TFDQuery;
  embeddingVector : string;
  i, j : Integer;
begin
  SetLength(Result, inCount);
  query := TFDQuery.Create(nil);
  try
    embeddingVector := '[' + embedding[0].ToString;
    for I := 1 to High(embedding) do
    begin
      embeddingVector := embeddingVector + ',' + embedding[i].ToString;
    end;
    embeddingVector := embeddingVector + ']';

    query.Connection := EmbeddingConnection;
    query.SQL.Text := 'SELECT *, embedding <-> CAST(:embed as VECTOR) as Distance FROM "document-segment" ORDER BY Distance LIMIT :count;';
    query.Params.ParamByName('count').AsInteger := inCount;
    query.Params.ParamByName('embed').Size := 30000;
    query.Params.ParamByName('embed').AsString := embeddingVector;
    query.Active := True;
    j := 0;
    repeat
      Result[j] := query.FieldByName('text').AsString;

      Inc(j);
      query.Next;
    until query.Eof;
  finally
    FreeAndNil(query);
  end;
end;

end.
