unit frmFaceWindow;

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
  Vcl.StdCtrls,
  uBaseFaceRecognition,
  uMicrosoft.FaceRecognition
  ;

type
  TfrmFaceDetection = class(TForm)
    edtImageURL: TEdit;
    btnDetectFaces: TButton;
    Label1: TLabel;
    Memo1: TMemo;
    procedure btnDetectFacesClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FFaceRecognition : TBaseFaceRecognition;
  public
    { Public declarations }
  end;

var
  frmFaceDetection: TfrmFaceDetection;

implementation

{$R *.dfm}

{$I ..\LIBS\APIKEY.INC}

procedure TfrmFaceDetection.btnDetectFacesClick(Sender: TObject);
begin
  Memo1.Text := FFaceRecognition.DetectFacesFromURL(edtImageURL.Text);
end;

procedure TfrmFaceDetection.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FFaceRecognition);
end;

procedure TfrmFaceDetection.FormCreate(Sender: TObject);
begin
  FFaceRecognition := TMicrosoftFaceRecognition.Create(ms_face_key);
end;

end.
