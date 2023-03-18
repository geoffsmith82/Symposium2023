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
  Vcl.StdCtrls;

type
  TVoiceRecognitionForm = class(TForm)
    Edit1: TEdit;
    Button1: TButton;
    ComboBox1: TComboBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  VoiceRecognitionForm: TVoiceRecognitionForm;

implementation

{$R *.dfm}

end.
