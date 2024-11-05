unit Unit2;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.JSON,
  System.IOUtils,
  System.UITypes,
  System.IniFiles,
  System.Generics.Collections,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.Menus,
  ApiKeyStore,
  frmApiKeyStore,
  uLLM.OpenAI.Assistants
  ;

type
  TForm2 = class(TForm)
    Memo1: TMemo;
    Button7: TButton;
    MainMenu: TMainMenu;
    miFile: TMenuItem;
    miExit: TMenuItem;
    miSettings: TMenuItem;
    miAPIKeys: TMenuItem;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure miAPIKeysClick(Sender: TObject);
  private
    { Private declarations }
    FAssistant : TOpenAIAssistant;
    FThread : TOpenAIThread;
    FIniFile : TIniFile;
    FApiKeyStore : TApiKeyStore;
    function ExtractInvoiceInfo(const PDFFileName: string): string;
    function DataFilename(filename: string): string;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.Button7Click(Sender: TObject);
var
  sl : TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Text := ExtractInvoiceInfo(DataFilename('example1.pdf'));
  Memo1.Lines.AddStrings(sl);
  finally
    FreeAndNil(sl);
  end;
end;

function TForm2.DataFilename(filename: string): string;
begin
  Result := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), '..\..\Data');
  Result := TPath.Combine(Result, filename);

  Result := TPath.GetFullPath(Result);
end;

procedure TForm2.miAPIKeysClick(Sender: TObject);
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

procedure TForm2.miExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

function TForm2.ExtractInvoiceInfo(const PDFFileName: string): string;
var
  FileID, Status: string;
  JobRun : TOpenAIRun;
  Messages: TJSONArray;
  I: Integer;
  ContentArr : TJSONArray;
  msg : string;
begin
  FAssistant.FAssistantID := FIniFile.ReadString('Settings', 'AssistantID', '');
  if FAssistant.FAssistantID.IsEmpty then
  begin
    FAssistant.FAssistantID := FAssistant.CreateAssistant('gpt-4o-mini', 'Invoice Extractor',
               'You are an expert at extracting information from invoices. Extract key details such as invoice number, date, total amount, and line items and return in json format.')
  end;


  FileID := FAssistant.Files.UploadFile(PDFFileName);
  FThread := FAssistant.Threads.CreateThread;

  msg := TFile.ReadAllText(DataFilename('invoiceRead.txt'));


  FThread.AddMessage(msg, FileID);
  JobRun := FThread.Runs.CreateRun;
  repeat
    Sleep(1000);  // Wait for 1 second before checking status again
    Status := JobRun.GetRunStatus;
  until (Status = 'completed') or (Status = 'failed');
  if Status = 'completed' then
  begin
    Messages := FThread.GetThreadMessages;
    for I := 0 to Messages.Count - 1 do
    begin
      if Messages.Items[I].GetValue<string>('role') = 'assistant' then
      begin
        ContentArr := Messages.Items[I].GetValue<TJSONArray>('content');
        Result := ExtractJsonString(ContentArr[0].GetValue<string>('text.value'));
        Break;
      end;
    end;
  end
  else
    Result := 'Failed to extract information from the invoice.';
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FAssistant);
  FreeAndNil(FIniFile);
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  FIniFile := TIniFile.Create(DataFilename('invoice.ini'));
  FApiKeyStore := TApiKeyStore.GetInstance;
  FAssistant := TOpenAIAssistant.Create(FApiKeyStore.LoadApiKey('chatgpt_apikey'));
  Memo1.Clear;
end;

end.
