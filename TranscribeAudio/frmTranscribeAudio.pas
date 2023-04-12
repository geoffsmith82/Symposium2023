unit frmTranscribeAudio;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Inifiles,
  System.Generics.Collections,
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
    FEngines : TObjectDictionary<string, TBaseSpeechToText>;
    FMenuEngine : TDictionary<string, TMenuItem>;
    FNameFromMenu: TDictionary<TMenuItem, string>;
    FSpeechToText : TBaseSpeechToText;
    FSettings : TIniFile;
    procedure RegisterEngine(engine: string; menuItem: TMenuItem;
      Speechengine: TBaseSpeechToText);
  public
    { Public declarations }
  end;

var
  VoiceRecognitionForm: TVoiceRecognitionForm;

implementation

{$R *.dfm}

{$I ..\Libs\apikey.inc}

procedure TVoiceRecognitionForm.RegisterEngine(engine: string; menuItem: TMenuItem; Speechengine: TBaseSpeechToText);
begin
  FEngines.AddOrSetValue(engine, Speechengine);
  FMenuEngine.AddOrSetValue(engine, menuItem);
  FNameFromMenu.AddOrSetValue(menuItem, engine);
end;

procedure TVoiceRecognitionForm.FormCreate(Sender: TObject);
var
  engine : string;
begin
  FEngines := TObjectDictionary<string,TBaseSpeechToText>.Create([doOwnsValues]);
  FMenuEngine := TDictionary<string, TMenuItem>.Create;
  FNameFromMenu := TDictionary<TMenuItem, string>.Create;

  FSettings := TIniFile.Create(ChangeFileExt(ParamStr(0),'.ini'));
  RegisterEngine('GoogleSpeech', miGoogle,
    TGoogleSpeechToText.Create(google_clientid, google_clientsecret, 'ADUG Demo', '', FSettings));
  RegisterEngine('MicrosoftSpeech', miMicrosoft,
    TMicrosoftSpeechToText.Create('', '', ''));
  RegisterEngine('AmazonSpeech', miAmazon,
    TAmazonSpeechToText.Create('', '', ''));
  RegisterEngine('OpenAiWhisperOnlineSpeech', miOpenAIWhisper,
    TOpenAiWhisperOnline.Create('', '', ''));

  engine := FSettings.ReadString('Settings', 'Engine', 'MicrosoftSpeech');

  FSpeechToText := FEngines[engine];
  FMenuEngine[engine].Checked := True;
end;

procedure TVoiceRecognitionForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSettings);
  FreeAndNil(FEngines);
  FreeAndNil(FMenuEngine);
  FreeAndNil(FNameFromMenu);
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
  engine := FNameFromMenu[(Sender as TMenuItem)];
  FSettings.WriteString('Settings', 'Engine', engine);
end;

end.
