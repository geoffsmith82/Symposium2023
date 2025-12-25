unit Unit1;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Generics.Collections,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.JSON,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.Menus,
  ApiKeyStore,
  uLLM.Streaming.OpenAI,
  uLLM
  ;

type
  TForm1 = class(TForm)
    mmoQuestion: TMemo;
    Button1: TButton;
    mmoResponse: TMemo;
    MainMenu: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    Settings1: TMenuItem;
    APIKeys1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure APIKeys1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
  private
    { Private declarations }
    FApiKeyStore : TApiKeyStore;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses frmApiKeyStore;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FApiKeyStore := TApiKeyStore.GetInstance;
end;

procedure TForm1.APIKeys1Click(Sender: TObject);
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

procedure TForm1.Button1Click(Sender: TObject);
var
  Streamer: TOpenAIStreamer;
  jsonReq : TJSONObject;
  LMessages: TObjectList<TChatMessage>;
  LMessage: TChatMessage;
begin
  mmoResponse.Clear;

  jsonReq := TJSONObject.Create;
  LMessages := TObjectList<TChatMessage>.Create;
  LMessage := TChatMessage.Create;
  LMessage.Role := 'user';
  LMessage.Content := mmoQuestion.Text;
  LMessages.Add(LMessage);

  Streamer := TOpenAIStreamer.Create(FApiKeyStore.LoadApiKey('chatgpt_apikey'), 'gpt-4o-mini', LMessages);

Streamer.OnToken :=
  procedure(Sender: TObject; const AText: string)
  var
    TextOnly: string;
    Newlines: Integer;
    LastIndex, StartPos, I: Integer;
    NeedScroll : Boolean;
  begin
    // Count trailing newlines (\n or \r\n)
    Newlines := 0;
    I := Length(AText);
    while (I > 0) and (AText[I] in [#10, #13]) do
    begin
      Dec(I);
      Inc(Newlines);
      NeedScroll := True;
    end;

    // Extract text part (without trailing newlines)
    TextOnly := Copy(AText, 1, I);

    // Append text if present
    if TextOnly <> '' then
    begin
      if mmoResponse.Lines.Count = 0 then
      begin
        NeedScroll := True;
        mmoResponse.Lines.Add(TextOnly)
      end
      else
      begin
        LastIndex := mmoResponse.Lines.Count - 1;
        mmoResponse.Lines[LastIndex] := mmoResponse.Lines[LastIndex] + TextOnly;
      end;

      // Highlight the new text
      StartPos := mmoResponse.GetTextLen - Length(TextOnly);
      if StartPos < 0 then StartPos := 0;
      mmoResponse.SelStart := StartPos;
      mmoResponse.SelLength := Length(TextOnly);
    end;

    // Add blank lines for each newline
    while Newlines > 0 do
    begin
      mmoResponse.Lines.Add('');
      Dec(Newlines);
    end;

    // Ensure scroll follows
    if NeedScroll then
      mmoResponse.Perform(EM_SCROLLCARET, 0, 0);
    Application.ProcessMessages;
  end;



//  Streamer.OnCurrentResponse :=
//    procedure(Sender: TObject; const AText: string)
//    begin
//      Memo2.Lines.Text := Memo2.Lines.Text + AText + system.sLineBreak + ' ##############################' + system.sLineBreak;
//      Memo2.SelStart := Length(Memo2.Text + system.sLineBreak);
//    end;

  Streamer.Start; // run in background
end;

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Application.Terminate;
end;


end.
