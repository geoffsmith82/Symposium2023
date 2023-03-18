unit frmVoiceRecognition;

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
  Vcl.Menus,
  Vcl.StdCtrls,
  uBaseSpeechToText,
  uGoogle.SpeechToText,
  uAmazon.SpeechToText,
  uMicrosoft.SpeechToText,
  uOpenAI.Whisper.Online.SpeechToText
  ;

type
  TVoiceRecognitionForm = class(TForm)
    edtFilename: TEdit;
    btnBrowse: TButton;
    ComboBox1: TComboBox;
    OpenDialog: TOpenDialog;
    btnRecognizeSpeech: TButton;
    Memo1: TMemo;
    Label1: TLabel;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    New1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    SaveAs1: TMenuItem;
    Print1: TMenuItem;
    PrintSetup1: TMenuItem;
    miExit: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    miEngine: TMenuItem;
    miMicrosoft: TMenuItem;
    miGoogle: TMenuItem;
    miAmazon: TMenuItem;
    miOpenAIWhisper: TMenuItem;
    miOpenAIWhisperLocal: TMenuItem;
    procedure FormDestroy(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure btnRecognizeSpeechClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure miEngineSelectedClick(Sender: TObject);
  private
    { Private declarations }
    FSpeechToText : TBaseSpeechToText;
  public
    { Public declarations }
  end;

var
  VoiceRecognitionForm: TVoiceRecognitionForm;

implementation

{$R *.dfm}

procedure TVoiceRecognitionForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSpeechToText);
end;

procedure TVoiceRecognitionForm.btnBrowseClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    edtFilename.Text := OpenDialog.FileName;
  end;
end;

procedure TVoiceRecognitionForm.btnRecognizeSpeechClick(Sender: TObject);
var
  filename : string;
begin
  FSpeechToText := TOpenAiWhisperOnline.Create;
  filename := edtFilename.Text;
  Memo1.Text := FSpeechToText.TranscribeAudio(filename, 'whisper-1');
end;

procedure TVoiceRecognitionForm.miExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TVoiceRecognitionForm.miEngineSelectedClick(Sender: TObject);
var
  engine : string;
begin
  engine := (Sender as TMenuItem).Caption.Replace('&', '');
  if engine = 'Microsoft' then
  begin
    FSpeechToText := TMicrosoftSpeechToText.Create;
  end
  else if engine = 'Google' then
  begin
    FSpeechToText := TGoogleSpeechToText.Create('','ADUG Demo', '');
  end
  else if engine = 'Amazon' then
  begin
    FSpeechToText := TAmazonSpeechToText.Create;
  end
  else if engine = 'Open AI Whisper' then
  begin
    FSpeechToText := TOpenAiWhisperOnline.Create;
  end;
end;

end.
