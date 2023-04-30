unit frmWeatherMain;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Threading,
  System.Generics.Collections,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  uElevenLabs.REST,
  Vcl.ExtCtrls
  ;

type
  TfrmWeatherWindow = class(TForm)
    mmoWeatherQuestion: TMemo;
    GridPanel1: TGridPanel;
    mmWeatherAnswer: TMemo;
    Panel1: TPanel;
    btnLatestForcast: TButton;
    chkUseGPT4: TCheckBox;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnLatestForcastClick(Sender: TObject);
  private
    { Private declarations }
    FElevenLabsVoiceService : TElevenLabsService;
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
  FreeAndNil(FElevenLabsVoiceService);
end;

procedure TfrmWeatherWindow.FormCreate(Sender: TObject);
begin
  FElevenLabsVoiceService := TElevenLabsService.Create(Self, ElevenLabsAPIKey);
end;

procedure TfrmWeatherWindow.btnLatestForcastClick(Sender: TObject);
var
  memStream : TStringStream;
  product : IXMLProductType;
  i: Integer;
  question : string;
  forcastInfo : string;
  model : string;
  chatConfig : TChatSettings;
begin
  question := 'Using the xml I provide below, generate some text that includes the forcast of weather.  ' +
    ' Start with variations on "now time for the weather". ' +
    ' Do not mention or specify temperature in Fahrenheit. When saying temperature, just say the number. ' +
    ' Change the word precipitation to rain.  Don''t give a percentage probability of rain.' +
    ' If something doesn''t occur of the forcast, it should "over the forecast period".' +
    ' Include the names of the days based on the date of the start time local. Do it in the style of Jane Bunn' + System.sLineBreak + System.sLineBreak;

  if chkUseGPT4.Checked then
    model := 'gpt-4'
  else
    model := 'gpt-3.5-turbo';


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
              answer : TChatResponse;
              chatMessage : TChatMessage;
              chatMessages : TObjectList<TChatMessage>;
            begin
                forcastInfo := product.Forecast[i].XML;
                mmoWeatherQuestion.Lines.Add(question);
                mmoWeatherQuestion.Lines.Add('');
                mmoWeatherQuestion.Lines.Add(forcastInfo);
                chatMessages := TObjectList<TChatMessage>.Create;
                try
                  chatMessage := TChatMessage.Create;
                  chatMessage.Role := 'System';
                  chatMessage.Content := question;
                  chatMessages.Add(chatMessage);

                  chatMessage := TChatMessage.Create;
                  chatMessage.Role := 'User';
                  chatMessage.Content := forcastInfo;
                  chatMessages.Add(chatMessage);

                  chatConfig.model := model;

                  answer := TOpenAI.SendChatMessagesToOpenAI(CHATGPT_APIKEY, chatConfig, chatMessages);
                finally
                  FreeAndNil(chatMessages);
                end;

                TThread.Synchronize(nil, procedure
                  begin
                    mmWeatherAnswer.Text := answer.Content;
                    // The second parameter needs to be the voice id for the voice you want to use.
                    FElevenLabsVoiceService.PlayText(mmWeatherAnswer.Text, '7ikeV0TLG4sgVpT58eeU');
                  end);
            end);
         aTask.Start;
      end;
    end;
  finally
    FreeAndNil(memStream);
    product := nil;
  end;

end;

end.
