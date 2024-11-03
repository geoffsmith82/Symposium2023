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
  uLLM.OpenAI.Assistants
  ;

type
  TForm2 = class(TForm)
    Memo1: TMemo;
    Button7: TButton;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button7Click(Sender: TObject);
  private
    { Private declarations }
    FAssistant : TOpenAIAssistant;
    FThreadID : string;
    FIniFile : TIniFile;
    function ExtractInvoiceInfo(const PDFFileName: string): string;
    function DataFilename(filename: string): string;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

{$I ..\Libs\apikey.inc}

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

function TForm2.ExtractInvoiceInfo(const PDFFileName: string): string;
var
  FileID, RunID, Status: string;
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


  FileID := FAssistant.UploadFile(PDFFileName);
  FThreadID := FAssistant.CreateThread;

  msg := TFile.ReadAllText(DataFilename('invoiceRead.txt'));


  FAssistant.AddMessage(msg, FileID);
  RunID := FAssistant.CreateRun;
  repeat
    Sleep(1000);  // Wait for 1 second before checking status again
    Status := FAssistant.GetRunStatus(RunID);
  until (Status = 'completed') or (Status = 'failed');
  if Status = 'completed' then
  begin
    Messages := FAssistant.GetThreadMessages;
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
  FAssistant := TOpenAIAssistant.Create(chatgpt_apikey);
  Memo1.Clear;
end;

end.
