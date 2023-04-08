unit uBaseSpeech;

interface

uses
  System.Classes;

type
  TBaseSpeech = class
  protected
    FResourceKey: string;
    FApplicationName: string;
    FHost: string;
  public
    constructor Create(const AResourceKey: string; const AApplicationName: string; const AHost: string);
    function TextToSpeech(text: string; VoiceName: string = ''): TMemoryStream; virtual; abstract;
    function SpeechEngineName: string; virtual; abstract;
  end;

implementation

{ TBaseSpeech }

constructor TBaseSpeech.Create(const AResourceKey: string; const AApplicationName: string; const AHost: string);
begin
  FResourceKey := AResourceKey;
  FApplicationName := AApplicationName;
  FHost := AHost;
end;

end.
