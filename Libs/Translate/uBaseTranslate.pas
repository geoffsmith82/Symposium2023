unit uBaseTranslate;

interface

uses
  Classes;

type
  TBaseTranslate = class
  public
    OnSelectEngine : TNotifyEvent;
    procedure DoSelectEngine;
    function FromLanguages: TArray<string>; virtual; abstract;
    function ToLanguages: TArray<string>; virtual; abstract;
    function Translate(const SourceText: string; const toLang: string; const fromLang: string): string; virtual; abstract;
  end;

implementation

{ TBaseTranslate }

procedure TBaseTranslate.DoSelectEngine;
begin
  if Assigned(OnSelectEngine) then
    OnSelectEngine(Self);
end;

end.
