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
  Vcl.StdCtrls;

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
    procedure btnBrowseClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  VoiceRecognitionForm: TVoiceRecognitionForm;

implementation

{$R *.dfm}

procedure TVoiceRecognitionForm.btnBrowseClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    edtFilename.Text := OpenDialog.FileName;
  end;
end;

procedure TVoiceRecognitionForm.miExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

end.
