unit frmImageGenWindow;

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
  Vcl.Samples.Spin,
  JvExForms,
  JvCustomItemViewer,
  JvImagesViewer;

type
  TfrmImageGenerator = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    New1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    SaveAs1: TMenuItem;
    Print1: TMenuItem;
    PrintSetup1: TMenuItem;
    Exit1: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    Generator1: TMenuItem;
    Generator2: TMenuItem;
    mmoImagePrompt: TMemo;
    btnExecute: TButton;
    Label1: TLabel;
    JvImagesViewer1: TJvImagesViewer;
    seImageCount: TSpinEdit;
    mmoOutput: TMemo;
    procedure btnExecuteClick(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmImageGenerator: TfrmImageGenerator;

implementation

{$R *.dfm}

uses OpenAI;

procedure TfrmImageGenerator.btnExecuteClick(Sender: TObject);
begin
  mmoOutput.Text := TOpenAI.CallDALL_E(mmoImagePrompt.Lines.Text, seImageCount.Value);
end;

procedure TfrmImageGenerator.Exit1Click(Sender: TObject);
begin
  Application.Terminate;
end;

end.
