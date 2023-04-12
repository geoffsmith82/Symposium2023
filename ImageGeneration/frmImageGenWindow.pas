unit frmImageGenWindow;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Threading,
  System.Generics.Collections,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Menus,
  Vcl.StdCtrls,
  Vcl.Samples.Spin,
  Vcl.ExtCtrls,
  Vcl.Imaging.pngimage,
  System.Net.URLClient,
  System.Net.HttpClient,
  System.Net.HttpClientComponent;

type
  TfrmImageGenerator = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    New1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    SaveAs1: TMenuItem;
    Print1: TMenuItem;
    PrintSetup1: TMenuItem;
    Exit1: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    Generator1: TMenuItem;
    Generator2: TMenuItem;
    mmoImagePrompt: TMemo;
    btnExecute: TButton;
    Label1: TLabel;
    seImageCount: TSpinEdit;
    cboSize: TComboBox;
    ScrollBox1: TScrollBox;
    GridPanel1: TGridPanel;
    PopupMenu1: TPopupMenu;
    SaveImage1: TMenuItem;
    SaveDialog: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure btnExecuteClick(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Image1ContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure SaveImage1Click(Sender: TObject);
    procedure seImageCountChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FImageList : TObjectList<TImage>;
    FCurrentImage : TImage;
  public
    { Public declarations }
  end;

var
  frmImageGenerator: TfrmImageGenerator;

implementation

{$R *.dfm}

uses OpenAI,
  uDALLe2.DTO;

procedure TfrmImageGenerator.FormCreate(Sender: TObject);
begin
  FImageList := TObjectList<TImage>.Create;
  FCurrentImage := nil;
end;

procedure TfrmImageGenerator.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FImageList);
end;

procedure TfrmImageGenerator.btnExecuteClick(Sender: TObject);
var
  size : TDALLESize;
  images : TGeneratedImagesClass;
  i : Integer;
begin
  if cboSize.Text = '256x256' then
  begin
    size := DALLE256;
  end
  else if cboSize.Text = '512x512' then
  begin
    size := DALLE512;
  end
  else if cboSize.Text = '1024x1024' then
  begin
    size := DALLE1024;
  end
  else
  begin
    size := DALLE1024;
  end;

  images := TOpenAI.CallDALL_E(mmoImagePrompt.Lines.Text, seImageCount.Value, size);

  for i := 0 to length(images.data) - 1 do
  begin
    var imageURL : string := images.data[i].url;
    TTask.Run(procedure ()
    var
      NetRequest : TNetHTTPRequest;
      NetClient : TNetHTTPClient;
      memStream : TMemoryStream;
      png : TImage;

    begin
      NetRequest := nil;
      NetClient := nil;
      memStream := nil;
      png := nil;
      try
        memStream := TMemoryStream.Create;
        NetClient := TNetHTTPClient.Create(nil);
        NetRequest := TNetHTTPRequest.Create(nil);
        NetRequest.Client := NetClient;
        NetRequest.Get(imageURL, memStream);
        png := TImage.Create(nil);
        png.Proportional := True;
        png.Stretch := True;
        png.Align := alClient;
        png.PopupMenu := PopupMenu1;
        png.OnContextPopup := Image1ContextPopup;
        png.Picture.LoadFromStream(memStream);
        TThread.Synchronize(nil, procedure ()
        begin
          ImageList.Add(png);
          png.Parent := GridPanel1;
        end
        );
      finally
        FreeAndNil(memStream);
        FreeAndNil(NetRequest);
        FreeAndNil(NetClient);
      end;
    end);
  end;
end;

procedure TfrmImageGenerator.Exit1Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfrmImageGenerator.Image1ContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
begin
  CurrentImage := Sender as TImage;
end;

procedure TfrmImageGenerator.SaveImage1Click(Sender: TObject);
begin
  if SaveDialog.Execute then
  begin
    OutputDebugString(PChar(Sender.ClassName));
    CurrentImage.Picture.SaveToFile(SaveDialog.FileName);
  end;
end;

procedure TfrmImageGenerator.seImageCountChange(Sender: TObject);
var
  k, i : Integer;
  colCount : Integer;
  rowCount : Integer;
begin
  GridPanel1.RowCollection.Clear;
  GridPanel1.ColumnCollection.Clear;

  if seImageCount.Value > 4 then
  begin
    colCount := 3;
  end
  else
  begin
    colCount := 2;
  end;

  for i := 1 to colCount do
  begin
    with GridPanel1.ColumnCollection.Add do
    begin
      SizeStyle := ssPercent;
      Value := 100 / colCount; //have cells evenly distributed
    end;
  end;

  for k := 0 to 10 do
  begin
    for i := 0 to colCount - 1 do
    begin
      GridPanel1.ColumnCollection[i].SizeStyle := ssPercent;
      GridPanel1.ColumnCollection[i].Value := 100 / colCount; //have cells evenly distributed
    end;
  end;

  rowCount := (seImageCount.Value div colCount) + 1;
  GridPanel1.Top := 0;
  GridPanel1.Left := 0;
  GridPanel1.Height := rowCount * 500;
  ScrollBox1.VertScrollBar.Range := rowCount * 500;
  for i := 1 to rowCount do
  begin
    with GridPanel1.RowCollection.Add do
    begin
      SizeStyle := ssPercent;
      Value := 100 / rowCount; //have cells evenly distributed
    end;
  end;

  for k := 0 to 10 do
  begin
    for i := 0 to rowCount - 1 do
    begin
      GridPanel1.RowCollection[i].SizeStyle := ssPercent;
      GridPanel1.RowCollection[i].Value := 100 / rowCount; //have cells evenly distributed
    end;
  end;
end;

end.
