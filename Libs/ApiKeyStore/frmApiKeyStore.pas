unit frmApiKeyStore;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Grids,
  Vcl.StdCtrls,
  ApiKeyStore
  ;

type
  TfrmApiKeyStores = class(TForm)
    StringGrid: TStringGrid;
    btnClose: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FApiKeyStore : TApiKeyStore;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}



procedure TfrmApiKeyStores.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmApiKeyStores.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  StringGrid.Cells[0, 0] := 'Name';
  StringGrid.Cells[1, 0] := 'Key';

  StringGrid.ColWidths[0] := StringGrid.Width div 2;
  StringGrid.ColWidths[1] := StringGrid.Width div 2;

  StringGrid.Cells[0, 1] := 'chatgpt_apikey';
  StringGrid.Cells[0, 2] := 'X_AI';
  StringGrid.Cells[0, 3] := 'groq_apikey';
  StringGrid.Cells[0, 4] := 'ElevenLabsAPIKey';
  StringGrid.Cells[0, 5] := 'revai_key';
  StringGrid.Cells[0, 6] := 'assemblyai_key';
  StringGrid.Cells[0, 7] := 'deepgram_key';
  StringGrid.Cells[0, 8] := 'HuggingFace_APIKey';
  FApiKeyStore := TApiKeyStore.GetInstance;

  for i := 1 to StringGrid.RowCount - 1 do
  begin
    StringGrid.Cells[1, i] :=  FApiKeyStore.LoadApiKey(StringGrid.Cells[0, i]);
  end;


end;

procedure TfrmApiKeyStores.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FApiKeyStore);
end;

end.
