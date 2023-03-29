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
  Inifiles,
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
    GoogleAuthenticate1: TMenuItem;
    procedure FormDestroy(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure btnRecognizeSpeechClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure miEngineSelectedClick(Sender: TObject);
    procedure GoogleAuthenticate1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FSpeechToText : TBaseSpeechToText;
    FSettings : TIniFile;
  public
    { Public declarations }
  end;

var
  VoiceRecognitionForm: TVoiceRecognitionForm;

implementation

{$R *.dfm}

procedure TVoiceRecognitionForm.FormCreate(Sender: TObject);
var
  engine : string;
begin
  FSettings := TIniFile.Create(ChangeFileExt(ParamStr(0),'.ini'));
  engine := FSettings.ReadString('Settings', 'Engine', '');
  if engine = 'GoogleSpeech' then
  begin
    FSpeechToText := TGoogleSpeechToText.Create('','ADUG Demo', '', FSettings);
    miGoogle.Checked :=  True;
  end
  else if engine = 'MicrosoftSpeech' then
  begin
    FSpeechToText := TMicrosoftSpeechToText.Create;
    miMicrosoft.Checked :=  True;
  end
  else if engine = 'AmazonSpeech' then
  begin
    FSpeechToText := TAmazonSpeechToText.Create;
    miAmazon.Checked :=  True;
  end
  else if engine = 'OpenAiWhisperOnlineSpeech' then
  begin
    FSpeechToText := TOpenAiWhisperOnline.Create;
    miOpenAIWhisper.Checked :=  True;
  end
  else
  begin
    FSpeechToText := TOpenAiWhisperOnline.Create;
    miOpenAIWhisper.Checked :=  True;
  end
end;

procedure TVoiceRecognitionForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSpeechToText);
  FreeAndNil(FSettings);
end;

procedure TVoiceRecognitionForm.GoogleAuthenticate1Click(Sender: TObject);
begin
  (FSpeechToText as TGoogleSpeechToText).Authenticate;
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
  FreeAndNil(FSpeechToText);
  engine := (Sender as TMenuItem).Caption.Replace('&', '');
  if engine = 'Microsoft' then
  begin
    FSpeechToText := TMicrosoftSpeechToText.Create;
    FSettings.WriteString('Settings', 'Engine', FSpeechToText.SpeechEngineName);
  end
  else if engine = 'Google' then
  begin
    FSpeechToText := TGoogleSpeechToText.Create('','ADUG Demo', '', FSettings);
    FSettings.WriteString('Settings', 'Engine', FSpeechToText.SpeechEngineName);
  end
  else if engine = 'Amazon' then
  begin
    FSpeechToText := TAmazonSpeechToText.Create;
    FSettings.WriteString('Settings', 'Engine', FSpeechToText.SpeechEngineName);
  end
  else if engine = 'Open AI Whisper' then
  begin
    FSpeechToText := TOpenAiWhisperOnline.Create;
    FSettings.WriteString('Settings', 'Engine', FSpeechToText.SpeechEngineName);
  end;
end;

end.
