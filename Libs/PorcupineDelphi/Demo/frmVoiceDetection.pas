unit frmVoiceDetection;

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
  Picovoice,
  Pv_Porcupine,
  Pv_Recorder
  ;

type
  TVoiceDetectionForm = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  VoiceDetectionForm: TVoiceDetectionForm;

implementation

{$R *.dfm}

{$I ..\..\..\Libs\apikey.inc}


procedure TVoiceDetectionForm.Button1Click(Sender: TObject);
var
  num_keywords  : Integer;
  model_path : string;
  keyword_paths: string;
  sensitivities: Single;
  porcupine: pv_porcupine_t;
  porcupine_status : pv_status_t;
begin
  Memo1.Lines.Add(pv_porcupine_version);
  Memo1.Lines.Add(Pv_Recorder_Version);
  Memo1.Lines.Add('pv_porcupine_frame_length = ' + pv_porcupine_frame_length.ToString);
  Memo1.Lines.Add('pv_sample_rate = ' + pv_sample_rate.ToString);

  model_path := '';

  porcupine_status := pv_porcupine_init(Porcupine_Key,
        PAnsiChar(model_path),
        num_keywords,
        @PAnsiChar(keyword_paths),
        @sensitivities,
        porcupine);
  if (porcupine_status <> PV_STATUS_SUCCESS) then
  begin
    raise Exception.Create('pv_porcupine_init failed with ' + pv_status_to_string(porcupine_status));
  end;

  try

  finally
    pv_porcupine_delete(porcupine);
  end;
end;

end.
