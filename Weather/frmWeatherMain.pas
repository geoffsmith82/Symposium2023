unit frmWeatherMain;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Threading,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
{$IFNDEF NOPOLLY}
  uAmazon.Polly,
{$ENDIF}
  Vcl.MPlayer
  ;

type
  TfrmWeatherWindow = class(TForm)
    btnLatestForcast: TButton;
    mmoWeatherQuestion: TMemo;
    MediaPlayer1: TMediaPlayer;
    GridPanel1: TGridPanel;
    mmWeatherAnswer: TMemo;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnLatestForcastClick(Sender: TObject);
  private
    { Private declarations }
    FAmazonPolyVoiceService : TAmazonPollyService;
    procedure PlayTextAmazon(const text:string);
  public
    { Public declarations }
  end;

var
  frmWeatherWindow: TfrmWeatherWindow;

implementation

{$R *.dfm}

uses
  udmWeather,
  uXMLBOMPrecis,
  OpenAI,
  System.IOUtils
  ;

{$I ..\Libs\apikey.inc}

procedure TfrmWeatherWindow.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FAmazonPolyVoiceService);
end;

procedure TfrmWeatherWindow.FormCreate(Sender: TObject);
begin
  FAmazonPolyVoiceService := TAmazonPollyService.Create(Self, AWSAccessKey, AWSSecretkey);//'ADUG Demo', '');
end;

procedure TfrmWeatherWindow.PlayTextAmazon(const text:string);
var
  Stream: TMemoryStream;
  FileName: string;
  aTask: ITask;
begin
  aTask := TTask.Create (procedure ()
      begin
        Stream := TMemoryStream.Create;
        try
          Stream := FAmazonPolyVoiceService.TextToSpeech(text);
          FileName := TPath.GetTempFileName + '.mp3';
          Stream.Position := 0;
          Stream.SaveToFile(FileName);
        finally
          Stream.Free;
        end;
        TThread.Synchronize(nil, procedure
        begin
          MediaPlayer1.FileName := FileName;
          MediaPlayer1.Open;
          MediaPlayer1.Play;
        end);
      end);
  aTask.Start;
end;

procedure TfrmWeatherWindow.btnLatestForcastClick(Sender: TObject);
var
  memStream : TStringStream;
  product : IXMLProductType;
  i: Integer;
  question : string;
  forcastInfo : string;
begin
  question := 'Using the xml I provide below, generate some text that includes the forcast of weather.  ' +
    ' Start with variations on "now time for the weather". ' +
    ' Do not mention or specify temperature in Fahrenheit. When saying temperature, just say the number. ' +
    ' Change the word precipitation to rain.  Don''t give a percentage probability of rain.' +
    ' If something doesn''t occur of the forcast, it should "over the forecast period".' +
    ' Include the names of the days based on the date of the start time local. Do it in the style of Jane Bunn' + System.sLineBreak + System.sLineBreak;

  dmWeather.IdFTP1.Connect;
  memStream := TStringStream.Create;
  try
    dmWeather.IdFTP1.Get('/anon/gen/fwo/IDV10753.xml', memStream, False);
    dmWeather.XMLDocument1.LoadFromStream(memStream);
    product := Getproduct(dmWeather.XMLDocument1);
    for i := 0 to product.Forecast.Count - 1 do
    begin
      if product.Forecast[i].Description = 'Bendigo' then
      begin
        var aTask: ITask :=  TTask.Create (procedure ()
            var
              answer : string;
            begin
                forcastInfo := product.Forecast[i].XML;
                mmoWeatherQuestion.Lines.Add(question);
                mmoWeatherQuestion.Lines.Add('');
                mmoWeatherQuestion.Lines.Add(forcastInfo);
                answer := TOpenAI.AskChatGPT(question + forcastInfo, 'text-davinci-003');
                TThread.Synchronize(nil, procedure
                  begin
                    mmWeatherAnswer.Text := answer;
                    PlayTextAmazon(mmWeatherAnswer.Text);
                  end);
            end);
         aTask.Start;
      end;
    end;
  finally
    FreeAndNil(memStream);
  end;

end;

end.
