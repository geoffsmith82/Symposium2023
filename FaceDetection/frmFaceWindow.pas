unit frmFaceWindow;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.IniFiles,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.Menus,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  Vcl.Imaging.Jpeg,
  Vcl.Imaging.PngImage,
  System.Net.HttpClientComponent,
  System.Net.URLClient,
  System.Net.HttpClient,
  uBaseFaceRecognition,
  uMicrosoft.FaceRecognition,
  uGoogle.FaceRecognition,
  uEngineManager
  ;

type
  TfrmFaceDetection = class(TForm)
    edtImageURL: TEdit;
    btnDetectFaces: TButton;
    Label1: TLabel;
    mmMainMenu: TMainMenu;
    miFile: TMenuItem;
    New1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    SaveAs1: TMenuItem;
    Print1: TMenuItem;
    PrintSetup1: TMenuItem;
    miExit: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    miFaceRecognitionEngine: TMenuItem;
    miGoogle: TMenuItem;
    miMicrosoft: TMenuItem;
    PageControl1: TPageControl;
    tsOriginalPhoto: TTabSheet;
    tsDetectedPhoto: TTabSheet;
    tsResults: TTabSheet;
    mmoResults: TMemo;
    imgOriginal: TImage;
    imgDetectedPhoto: TImage;
    miGoogleMenu: TMenuItem;
    miGoogleLogin: TMenuItem;
    procedure btnDetectFacesClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure miSelectEngineClick(Sender: TObject);
    procedure miGoogleLoginClick(Sender: TObject);
  private
    { Private declarations }
    FSettings : TIniFile;
    FFaceRecognitionEngines: TEngineManager<TBaseFaceRecognition>;
    procedure DownloadAndLoadImage(const AUrl: string; AImage: TImage);
    procedure OnMicrosoftSelected(Sender: TObject);
    procedure OnGoogleSelected(Sender: TObject);
  public
    { Public declarations }
  end;

var
  frmFaceDetection: TfrmFaceDetection;

implementation

{$R *.dfm}

{$I ..\LIBS\APIKEY.INC}

procedure TfrmFaceDetection.DownloadAndLoadImage(const AUrl: string; AImage: TImage);
var
  HTTPClient: TNetHTTPClient;
  HTTPRequest: TNetHTTPRequest;
  LStream: TMemoryStream;
  LImage: TGraphic;
  LResponse: IHTTPResponse;
begin
  if not Assigned(AImage) then
    raise Exception.Create('AImage parameter cannot be nil.');

  HTTPClient := TNetHTTPClient.Create(nil);
  HTTPRequest := TNetHTTPRequest.Create(nil);
  LStream := TMemoryStream.Create;
  try
    HTTPRequest.Client := HTTPClient;
    LResponse := HTTPRequest.Get(AUrl, LStream);

    if LResponse.StatusCode = 200 then
    begin
      LStream.Position := 0;
      if CompareText(LResponse.MimeType, 'image/jpeg') = 0 then
        LImage := TJPEGImage.Create
      else if CompareText(LResponse.MimeType, 'image/png') = 0 then
        LImage := TPngImage.Create
      else
        raise Exception.Create('Unsupported image format.');

      try
        LImage.LoadFromStream(LStream);
        AImage.Picture.Graphic := LImage;
      finally
        LImage.Free;
      end;
    end
    else
      raise Exception.CreateFmt('Error downloading image: %d %s',
        [LResponse.StatusCode, LResponse.StatusText]);
  finally
    LStream.Free;
    HTTPRequest.Free;
    HTTPClient.Free;
  end;
end;

procedure TfrmFaceDetection.btnDetectFacesClick(Sender: TObject);
var
  results : string;
begin
  results := FFaceRecognitionEngines.ActiveEngine.DetectFacesFromURL(edtImageURL.Text);
  mmoResults.Text := results;
  DownloadAndLoadImage(edtImageURL.Text, imgOriginal);
  DownloadAndLoadImage(edtImageURL.Text, imgDetectedPhoto);
end;

procedure TfrmFaceDetection.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FFaceRecognitionEngines);
  FreeAndNil(FSettings);
end;

procedure TfrmFaceDetection.miExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfrmFaceDetection.miGoogleLoginClick(Sender: TObject);
begin
  (FFaceRecognitionEngines.ActiveEngine as TGoogleFaceRecognition).Authenticate;
end;

procedure TfrmFaceDetection.miSelectEngineClick(Sender: TObject);
begin
  FFaceRecognitionEngines.SelectEngine(Sender as TMenuItem);
  FFaceRecognitionEngines.ActiveMenuItem.Checked := True;
end;

procedure TfrmFaceDetection.OnMicrosoftSelected(Sender: TObject);
begin
  miGoogleMenu.Visible := False;
end;

procedure TfrmFaceDetection.OnGoogleSelected(Sender: TObject);
begin
  miGoogleMenu.Visible := True;
end;

procedure TfrmFaceDetection.FormCreate(Sender: TObject);
var
  engine : TBaseFaceRecognition;
begin
  FFaceRecognitionEngines:= TEngineManager<TBaseFaceRecognition>.Create;
  FSettings := TIniFile.Create(ChangeFileExt(ParamStr(0),'.ini'));

  engine := TMicrosoftFaceRecognition.Create(ms_face_key);
  FFaceRecognitionEngines.RegisterEngine(engine, miMicrosoft, OnMicrosoftSelected);
  engine := TGoogleFaceRecognition.Create(google_clientid, google_clientsecret, '' , '', FSettings);
  FFaceRecognitionEngines.RegisterEngine(engine, miGoogle, OnGoogleSelected);
  edtImageURL.Text := 'https://andergrovess.eq.edu.au/HomePagePictures/slide-01.jpg';
end;

end.
