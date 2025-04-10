unit frmImageGenWindow;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Threading,
  Winapi.ActiveX,
  System.Generics.Collections,
  System.Math,
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
  uImageGeneration.Replicate,
  uImageGeneration.XAI,
  uImageGeneration,
  uDALLe2.DTO
  ;

type
  TModelMenuItem = class(TMenuItem)
  public
    Version : string;
    Model: string;
    Engine: TBaseImageGeneration;
  end;

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
    ScrollBox: TScrollBox;
    pmPopupMenu: TPopupMenu;
    miSaveImage: TMenuItem;
    SaveDialog: TSaveDialog;
    miDALLE3: TMenuItem;
    miSetup: TMenuItem;
    miAPIKeys: TMenuItem;
    Label2: TLabel;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnExecuteClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure Image1ContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure miSaveImageClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure miAPIKeysClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
    ImageWidth, ImageHeight : Integer;
    FImageList : TObjectList<TImage>;
    FCurrentImage : TImage;
    FApiKeyStore : TApiKeyStore;
    FOpenAI : TBaseImageGeneration;
    FReplicate : TImageGenerationReplicate;
    FXAI : TImageGenerationXAI;
    FImageGenerator : TBaseImageGeneration;
    FModels : TObjectList<TImageModelInfo>;
    procedure AddImageToScrollBox(ImageWidth, ImageHeight, Margin: Integer);
    procedure UpdateImages(images: TGeneratedImagesClass);
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
var
  I: Integer;
  mi : TModelMenuItem;
begin
  ImageHeight := 1024;
  ImageWidth := 1024;
  FImageList := TObjectList<TImage>.Create;
  FApiKeyStore := TApiKeyStore.GetInstance;
  FOpenAI := TImageGenerationOpenAI.Create(FApiKeyStore.LoadApiKey('chatgpt_apikey'));
  if not FApiKeyStore.LoadApiKey('Replicate_APIKey').IsEmpty then
  begin
    FReplicate := TImageGenerationReplicate.Create(FApiKeyStore.LoadApiKey('Replicate_APIKey'){ {'http://192.168.1.18:8080'});
    FModels := FReplicate.ModelInfo;
    for I := 0 to FModels.Count - 1 do
    begin
      mi := TModelMenuItem.Create(nil);
      mi.Caption := FModels[i].modelName;
      mi.Model := FModels[i].modelName;
      mi.Version := FModels[i].version;
      mi.RadioItem := True;
      mi.AutoCheck := True;
      mi.GroupIndex := 100;
      mi.Engine := FReplicate;
      miGenerator.Add(mi)
    end;

{      TTask.Run(procedure ()
      var
        balance : Double;
      begin
        balance := FReplicate.GetBalance;
          TThread.Synchronize(nil, procedure ()
          begin
            Label2.Caption := 'Balance: ' + FormatFloat('$0.00', balance);
          end
          );
      end);}
  end;
  if not FApiKeyStore.LoadApiKey('X_AI').IsEmpty then
  begin
    FXAI := TImageGenerationXAI.Create(FApiKeyStore.LoadApiKey('X_AI'){ {'http://192.168.1.18:8080'});
    FModels := FXAI.ModelInfo;
    for I := 0 to FModels.Count - 1 do
    begin
      mi := TModelMenuItem.Create(nil);
      mi.Caption := FModels[i].modelName;
      mi.Model := FModels[i].modelName;
      mi.Version := FModels[i].version;
      mi.RadioItem := True;
      mi.AutoCheck := True;
      mi.GroupIndex := 100;
      mi.Engine := FXAI;
      miGenerator.Add(mi)
    end;

  end;
  FCurrentImage := nil;
end;

procedure TfrmImageGenerator.AddImageToScrollBox(ImageWidth, ImageHeight, Margin: Integer);
var
  i, XPos, YPos, MaxWidth: Integer;
  Control: TControl;
begin
  MaxWidth := ScrollBox.ClientWidth;
  XPos := Margin;
  YPos := Margin;  // Fixed top margin for the first row

  for i := 0 to ScrollBox.ControlCount - 1 do
  begin
    Control := ScrollBox.Controls[i];

    // Check if the control is a TImage
    if Control is TImage then
    begin
      // Move to the next row if adding the image would exceed the width
      if XPos + ImageWidth > MaxWidth then
      begin
        XPos := Margin;
        YPos := YPos + ImageHeight + Margin;  // Move to the next row
      end;

      // Set the position of the current image
      Control.SetBounds(XPos, YPos - ScrollBox.VertScrollBar.Position, ImageWidth, ImageHeight);

      // Update XPos for the next image
      XPos := XPos + ImageWidth + Margin;
    end;
  end;

  // Adjust scrollable area if needed, based on the final YPos position
  ScrollBox.VertScrollBar.Range := Max(YPos + ImageHeight + Margin, ScrollBox.ClientHeight);
end;


procedure TfrmImageGenerator.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FImageList);
  FreeAndNil(FOpenAI);
  FreeAndNil(FReplicate);
end;

procedure TfrmImageGenerator.FormResize(Sender: TObject);
begin
  AddImageToScrollBox(ImageWidth,ImageHeight, 10);
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
    ImageWidth := 256;
    ImageHeight := 256;
  end
  else if cboSize.Text = '512x512' then
  begin
    size := DALLE512;
    ImageWidth := 512;
    ImageHeight := 512;
  end
  else if cboSize.Text = '1024x1024' then
  begin
    size := DALLE1024;
    ImageWidth := 1024;
    ImageHeight := 1024;
  end
  else
  begin
    size := DALLE1024;
    ImageWidth := 1024;
    ImageHeight := 1024;
  end;


  for i := 0 to miGenerator.Count - 1 do
  begin
    var mi := miGenerator.Items[i];
    if mi is TModelMenuItem and mi.Checked then
    begin
      model := (mi as TModelMenuItem).Model;
      FImageGenerator := (mi as TModelMenuItem).Engine;
      break;
    end
  end;

  if miDALLE2.Checked then
  begin
    model := 'dall-e-2';
    FImageGenerator := FOpenAI;
  end
  else if miDALLE3.Checked then
  begin
    FImageGenerator := FOpenAI;
    model := 'dall-e-3';
  end;

  btnExecute.Enabled := False;
  images := FImageGenerator.Generate(mmoImagePrompt.Lines.Text, seImageCount.Value, size, model);
  TTask.Run(procedure ()
  begin
    UpdateImages(images);
  end);
end;

procedure TfrmImageGenerator.UpdateImages(images : TGeneratedImagesClass);
var
  i : Integer;
begin
  try
    for i := 0 to length(images.data) - 1 do
    begin
       if not Assigned(images.data[i]) then
         continue;
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
          CoInitialize(nil);
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
            png.Parent := ScrollBox;
            AddImageToScrollBox(ImageWidth,ImageHeight, 10);
          end
          );
{          var balance := FReplicate.GetBalance;
          TThread.Synchronize(nil, procedure ()
          begin
            Label2.Caption := 'Balance: ' + FormatFloat('$0.00', balance);
            btnExecute.Enabled := True;
          end
          );}
        finally
          CoUninitialize;
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
