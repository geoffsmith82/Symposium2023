unit frmDescribe;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.IniFiles,
  System.Generics.Collections,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Menus,
  FMX.Objects,
  FMX.Memo.Types,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.Layouts,
  FMX.frmApiKeyStore,
  ApiKeyStore,
  uLLM,
  uLLM.OpenAI
  ;

type
  TfrmDescribePicture = class(TForm)
    MainMenu: TMainMenu;
    miFile: TMenuItem;
    miExit: TMenuItem;
    miSettings: TMenuItem;
    miGoogleAuthenticate: TMenuItem;
    miAPIKeys: TMenuItem;
    btnOpen: TButton;
    OpenDialog: TOpenDialog;
    Image1: TImage;
    btnDescribe: TButton;
    Memo: TMemo;
    Layout1: TLayout;
    Layout2: TLayout;
    miModelMenu: TMenuItem;
    procedure miExitClick(Sender: TObject);
    procedure miAPIKeysClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnDescribeClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FApiKeyStore : TApiKeyStore;
    FSettings : TInifile;
    FOpenAI : TOpenAI;
  public
    { Public declarations }
  end;

var
  frmDescribePicture: TfrmDescribePicture;

implementation

{$R *.fmx}

procedure TfrmDescribePicture.btnDescribeClick(Sender: TObject);
var
  sysmsg : TChatMessage;
  visionMsg : TChatVisionMessage;
  msgs : TObjectList<TChatMessage>;
  config : TChatSettings;
  strm : TMemoryStream;
  response : TChatResponse;
begin
  visionMsg := nil;
  msgs := nil;
  sysmsg := nil;
  try
    msgs := TObjectList<TChatMessage>.Create;
    sysmsg := FOpenAI.CreateChatMessage;
    sysmsg.Role := 'system';
    sysmsg.Content := 'You are a helpful assistant';
    visionMsg := FOpenAI.CreateChatVisionMessage;
    visionMsg.Role := 'user';
    visionMsg.Content := 'Describe the image';
    strm := TMemoryStream.Create;
    Image1.Bitmap.SaveToStream(strm);
    strm.Position := 0;
    visionMsg.AddImageStream(strm, 'image/png');
    msgs.Add(sysmsg);
    msgs.Add(visionMsg);

    config.model := 'gpt-4.1';
    config.max_tokens := 4096;
    config.json_mode := False;
    config.store := True;
    config.n := 1;

    response := FOpenAI.ChatCompletion(config, msgs);
    Memo.Text := response.Content;
  finally
    FreeAndNil(strm);
    FreeAndNil(msgs);
  end;
end;

procedure TfrmDescribePicture.btnOpenClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    Image1.Bitmap.LoadFromFile(OpenDialog.FileName);
  end;
end;

procedure TfrmDescribePicture.FormCreate(Sender: TObject);
var
  i: Integer;
  currentModel : string;
  modelList : TObjectList<TBaseModelInfo>;
begin
  FApiKeyStore := TApiKeyStore.GetInstance;
  FSettings := TIniFile.Create(ChangeFileExt(ParamStr(0),'.ini'));
  FOpenAI := TOpenAI.Create(FApiKeyStore.LoadApiKey('chatgpt_apikey'));

  modelList := FOpenAI.ModelInfo;
  for i := 0 to modelList.Count - 1 do
  begin
    var MenuModelItem := TMenuItem.Create(miModelMenu);
    MenuModelItem.Text := modelList[i].modelName;
    miModelMenu.AddObject(MenuModelItem);
  end;



  currentModel := FSettings.ReadString('ChatGPT', 'Model', 'text-davinci-003').Replace('&', '');
  for i := 0 to miModelMenu.ItemsCount - 1 do
  begin
    if miModelMenu.Items[i].Text = currentModel then
    begin
      if Assigned(miModelMenu.Items[i].OnClick) then
        miModelMenu.Items[i].OnClick(nil);
      break;
    end;
  end;
end;

procedure TfrmDescribePicture.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSettings);
  FreeAndNil(FOpenAI);
end;

procedure TfrmDescribePicture.miAPIKeysClick(Sender: TObject);
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

procedure TfrmDescribePicture.miExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

end.
