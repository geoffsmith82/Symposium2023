unit frmImageGenWindow;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Threading,
  System.Generics.Collections,
  System.Math,
  System.Types,
  System.Net.HttpClient,
  System.Net.HttpClientComponent,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Menus,
  FMX.ScrollBox,
  FMX.StdCtrls,
  FMX.Edit,
  FMX.Controls.Presentation,
  FMX.Objects,
  FMX.Memo.Types,
  FMX.Layouts,
  FMX.ListBox,
  FMX.EditBox,
  FMX.SpinBox,
  FMX.Memo,
  ApiKeyStore,
  uImageGeneration.OpenAI,
  uImageGeneration.Replicate,
  uImageGeneration.XAI,
  uImageGeneration.Imagen,
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
    miSetup: TMenuItem;
    miAPIKeys: TMenuItem;
    ScrollBox: TScrollBox;
    pmPopupMenu: TPopupMenu;
    miSaveImage: TMenuItem;
    SaveDialog: TSaveDialog;
    Layout1: TLayout;
    btnExecute: TButton;
    mmoImagePrompt: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    cboSize: TComboBox;
    seImageCount: TSpinBox;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnExecuteClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure ImageContextPopup(Sender: TObject; const Point: TPointF; var Handled: Boolean);
    procedure miSaveImageClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure miAPIKeysClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    ImageWidth, ImageHeight: Integer;
    FImageList: TObjectList<TImage>;
    FCurrentImage: TImage;
    FApiKeyStore: TApiKeyStore;
    FOpenAI: TImageGenerationOpenAI;
    FReplicate : TImageGenerationReplicate;
    FXAI : TImageGenerationXAI;
    FModels: TObjectList<TImageModelInfo>;
    FImageGenerator : TBaseImageGeneration;

    procedure AddImageToScrollBox(AWidth, AHeight, AMargin: Integer);
    procedure UpdateImages(images: TGeneratedImagesClass);
  public
  end;

var
  frmImageGenerator: TfrmImageGenerator;

implementation

{$R *.fmx}

uses
  fmx.frmApiKeyStore
{$IFDEF MSWINDOWS}
  , Winapi.ActiveX
{$ENDIF}
  ;

{------------------------------------------------------------------------------}
procedure TfrmImageGenerator.FormCreate(Sender: TObject);
var
  I: Integer;
  mi: TModelMenuItem;
begin
  // --- same init as VCL, just FMX controls under the hood ---
  ImageWidth := 1024;
  ImageHeight := 1024;
  FImageList := TObjectList<TImage>.Create;
  FApiKeyStore := TApiKeyStore.GetInstance;

  // OpenAI
  FOpenAI := TImageGenerationOpenAI.Create(FApiKeyStore.LoadApiKey('chatgpt_apikey'));
  FModels := FOpenAI.ModelInfo;
  for I := 0 to FModels.Count - 1 do
  begin
    mi := TModelMenuItem.Create(nil);
    mi.Text := FModels[i].modelName;
    mi.Model := FModels[i].modelName;
    mi.Version := FModels[i].version;
    mi.RadioItem := True;
    mi.AutoCheck := True;
    mi.GroupIndex := 100;
    mi.Engine := FOpenAI;
    miGenerator.AddObject(mi)
  end;

  if not FApiKeyStore.LoadApiKey('Replicate_APIKey').IsEmpty then
  begin
    FReplicate := TImageGenerationReplicate.Create(FApiKeyStore.LoadApiKey('Replicate_APIKey'){ {'http://192.168.1.18:8080'});
    FModels := FReplicate.ModelInfo;
    for I := 0 to FModels.Count - 1 do
    begin
      mi := TModelMenuItem.Create(nil);
      mi.Text := FModels[i].modelName;
      mi.Model := FModels[i].modelName;
      mi.Version := FModels[i].version;
      mi.RadioItem := True;
      mi.AutoCheck := True;
      mi.GroupIndex := 100;
      mi.Engine := FReplicate;
      miGenerator.AddObject(mi)
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
      mi.Text := FModels[i].modelName;
      mi.Model := FModels[i].modelName;
      mi.Version := FModels[i].version;
      mi.RadioItem := True;
      mi.AutoCheck := True;
      mi.GroupIndex := 100;
      mi.Engine := FXAI;
      miGenerator.AddObject(mi)
    end;

  end;

//  if not FApiKeyStore.LoadApiKey('X_AI').IsEmpty then
{  begin
    FImagen := TImageGenerationImagen.Create(FApiKeyStore.LoadApiKey('X_AI'){ {'http://192.168.1.18:8080'}//);
{    FModels := FImagen.ModelInfo;
    for I := 0 to FModels.Count - 1 do
    begin
      mi := TModelMenuItem.Create(nil);
      mi.Caption := FModels[i].modelName;
      mi.Model := FModels[i].modelName;
      mi.Version := FModels[i].version;
      mi.RadioItem := True;
      mi.AutoCheck := True;
      mi.GroupIndex := 100;
      mi.Engine := FImagen;
      miGenerator.Add(mi)
    end;
  end; }
  FCurrentImage := nil;
end;

procedure TfrmImageGenerator.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FImageList);
  FreeAndNil(FOpenAI);
  FreeAndNil(FReplicate);
end;

{------------------------------------------------------------------------------}
procedure TfrmImageGenerator.AddImageToScrollBox(AWidth, AHeight, AMargin: Integer);
var
  I: Integer;
  XPos, YPos, MaxW: Single;
  Img: TImage;
begin
  MaxW := ScrollBox.Width;
  XPos := AMargin;
  YPos := AMargin;
  for I := 0 to ScrollBox.Content.ChildrenCount - 1 do
  begin
    if ScrollBox.Content.Children[I] is TImage then
    begin
      Img := TImage(ScrollBox.Content.Children[I]);
      if XPos + AWidth > MaxW then
      begin
        XPos := AMargin;
        YPos := YPos + AHeight + AMargin;
      end;
      Img.Position.X := XPos;
      Img.Position.Y := YPos;
      Img.Width      := AWidth;
      Img.Height     := AHeight;
      XPos := XPos + AWidth + AMargin;
    end;
  end;
end;

{------------------------------------------------------------------------------}
procedure TfrmImageGenerator.UpdateImages(images: TGeneratedImagesClass);
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
{$IFDEF MSWINDOWS}
          CoInitialize(nil);
{$ENDIF}
          memStream := TMemoryStream.Create;
          NetClient := TNetHTTPClient.Create(nil);
          NetRequest := TNetHTTPRequest.Create(nil);
          NetRequest.Client := NetClient;
          NetRequest.Get(imageURL, memStream);
          png := TImage.Create(nil);
          png.Width := 512;
          png.Height := 512;
//          png.Proportional := True;
//          png.Stretch := True;
          png.PopupMenu := pmPopupMenu;
          png.Bitmap.LoadFromStream(memStream);
//          png.On
//          png.OnContextPopup := Image1ContextPopup;
//          png.Picture.LoadFromStream(memStream);
          TThread.Synchronize(nil, procedure ()
          begin
            FImageList.Add(png);
            png.Parent := ScrollBox;
            AddImageToScrollBox(ImageWidth,ImageHeight, 10);
          end
          );
{          var balance := FReplicate.GetBalance;}
          TThread.Synchronize(nil, procedure ()
          begin
//            Label2.Caption := 'Balance: ' + FormatFloat('$0.00', balance);
            btnExecute.Enabled := True;
          end
          );
        finally
{$IFDEF MSWINDOWS}
          CoUninitialize;
{$ENDIF}
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

{------------------------------------------------------------------------------}
procedure TfrmImageGenerator.btnExecuteClick(Sender: TObject);
var
  size : TDALLESize;
  images : TGeneratedImagesClass;
  i : Integer;
  model : string;
  text2 : string;
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


  for i := 0 to miGenerator.ItemsCount - 1 do
  begin
    var mi := miGenerator.Items[i];
    if mi is TModelMenuItem and mi.IsChecked then
    begin
      model := (mi as TModelMenuItem).Model;
      FImageGenerator := (mi as TModelMenuItem).Engine;
      break;
    end
  end;

  btnExecute.Enabled := False;
  text2 := mmoImagePrompt.Lines.Text;
  TTask.Run(procedure ()
  begin
    try
      images := FImageGenerator.Generate(text2, Trunc(seImageCount.Value), size, model);
    except
      on e: Exception do
      begin
        TThread.Synchronize(nil, procedure ()
        begin
          ShowMessage(e.Message);
          btnExecute.Enabled := True;
        end);
        Exit;
      end;
    end;
    UpdateImages(images);
  end);
end;

{------------------------------------------------------------------------------}
procedure TfrmImageGenerator.FormResize(Sender: TObject);
begin
  // if you need to reflow on resize, call AddImageToScrollBox here
  AddImageToScrollBox(ImageWidth, ImageHeight, 10);
end;

{------------------------------------------------------------------------------}
procedure TfrmImageGenerator.ImageContextPopup(Sender: TObject; const Point: TPointF; var Handled: Boolean);
begin
  FCurrentImage := Sender as TImage;
  Handled := True;
end;

{------------------------------------------------------------------------------}
procedure TfrmImageGenerator.miSaveImageClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    FCurrentImage.Bitmap.SaveToFile(SaveDialog.FileName);
end;

{------------------------------------------------------------------------------}
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
  Close;
end;

end.
