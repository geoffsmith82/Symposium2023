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
  System.Net.HttpClientComponent,
  ApiKeyStore,
  uImageGeneration.OpenAI,
  uImageGeneration,
  uDALLe2.DTO
  ;

type
  TfrmImageGenerator = class(TForm)
    mmMainMenu: TMainMenu;
    miFile: TMenuItem;
    miExit: TMenuItem;
    miGenerator: TMenuItem;
    miDALLE2: TMenuItem;
    mmoImagePrompt: TMemo;
    btnExecute: TButton;
    Label1: TLabel;
    seImageCount: TSpinEdit;
    cboSize: TComboBox;
    ScrollBox1: TScrollBox;
    pmPopupMenu: TPopupMenu;
    miSaveImage: TMenuItem;
    SaveDialog: TSaveDialog;
    ImagesFlowPanel: TFlowPanel;
    miDALLE3: TMenuItem;
    miSetup: TMenuItem;
    miAPIKeys: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure btnExecuteClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure Image1ContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure miSaveImageClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure miAPIKeysClick(Sender: TObject);
  private
    { Private declarations }
    FImageList : TObjectList<TImage>;
    FCurrentImage : TImage;
    FApiKeyStore : TApiKeyStore;
    FOpenAI : TImageGenerationOpenAI;
  public
    { Public declarations }
  end;

var
  frmImageGenerator: TfrmImageGenerator;

implementation

{$R *.dfm}

uses
  frmApiKeyStore
  ;

procedure TfrmImageGenerator.FormCreate(Sender: TObject);
begin
  FImageList := TObjectList<TImage>.Create;
  FApiKeyStore := TApiKeyStore.GetInstance;
  FOpenAI := TImageGenerationOpenAI.Create(FApiKeyStore.LoadApiKey('chatgpt_apikey'));
  FCurrentImage := nil;
end;

procedure TfrmImageGenerator.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FImageList);
  FreeAndNil(FOpenAI);
end;

procedure TfrmImageGenerator.btnExecuteClick(Sender: TObject);
var
  size : TDALLESize;
  images : TGeneratedImagesClass;
  i : Integer;
  model : string;
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

  if miDALLE2.Checked then
    model := 'dall-e-2'
  else if miDALLE3.Checked then      
    model := 'dall-e-3';

  images := FOpenAI.Generate(mmoImagePrompt.Lines.Text, seImageCount.Value, size, model);
  try
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
          png.Width := 512;
          png.Height := 512;
          png.Proportional := True;
          png.Stretch := True;
          png.PopupMenu := pmPopupMenu;
          png.OnContextPopup := Image1ContextPopup;
          png.Picture.LoadFromStream(memStream);
          TThread.Synchronize(nil, procedure ()
          begin
            FImageList.Add(png);
            png.Parent := ImagesFlowPanel;
          end
          );
        finally
          FreeAndNil(memStream);
          FreeAndNil(NetRequest);
          FreeAndNil(NetClient);
        end;
      end);
    end;
  finally
    FreeAndNil(images);
  end;
end;

procedure TfrmImageGenerator.miAPIKeysClick(Sender: TObject);
var
  frmApiKeyStores : TfrmApiKeyStores;
begin
  frmApiKeyStores := TfrmApiKeyStores.Create(nil);
  try
    frmApiKeyStores.ShowModal;
  finally
    FreeAndNil(frmApiKeyStores)
  end;
end;

procedure TfrmImageGenerator.miExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfrmImageGenerator.Image1ContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
begin
  FCurrentImage := Sender as TImage;
end;

procedure TfrmImageGenerator.miSaveImageClick(Sender: TObject);
begin
  if SaveDialog.Execute then
  begin
    OutputDebugString(PChar(Sender.ClassName));
    FCurrentImage.Picture.SaveToFile(SaveDialog.FileName);
  end;
end;

end.
