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
      constructor Create(AResourceKey: string; AApplicationName:string; AHost: string);
      function TextToSpeech(text: string; VoiceName: string = ''): TMemoryStream; virtual; abstract;
      function SpeechEngineName: string; virtual; abstract;
  end;

implementation

{ TBaseSpeech }

constructor TBaseSpeech.Create(AResourceKey, AApplicationName, AHost: string);
begin
  FResourceKey := AResourceKey;
  FApplicationName := AApplicationName;
  FHost := AHost;
end;

end.
