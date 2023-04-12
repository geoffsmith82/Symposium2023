unit uBaseTranslate;

interface

type
  TBaseTranslate = class
  public
    function EngineName: string; virtual; abstract;
    function FromLanguages: TArray<string>; virtual; abstract;
    function ToLanguages: TArray<string>; virtual; abstract;
    function Translate(const SourceText: string; const toLang: string; const fromLang: string): string; virtual; abstract;
  end;

implementation

end.
