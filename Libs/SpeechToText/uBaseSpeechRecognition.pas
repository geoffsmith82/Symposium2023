unit uBaseSpeechRecognition;

interface

uses
  Classes
  ;

type
  TBaseSpeechRecognition = class
  protected
    FResourceKey : string;
  public
    OnSelectEngine : TNotifyEvent;
    procedure DoSelectEngine;
    constructor Create(const AResourceKey: string; const AApplicationName: string; const AHost: string);
    procedure Finish; virtual; abstract;
  end;

implementation

{ TBaseSpeechRecognition }

constructor TBaseSpeechRecognition.Create(const AResourceKey, AApplicationName,
  AHost: string);
begin

end;

procedure TBaseSpeechRecognition.DoSelectEngine;
begin
  if Assigned(OnSelectEngine) then
    OnSelectEngine(Self);
end;

end.
