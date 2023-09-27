unit uImageGeneration;

interface

uses
  System.Classes,
  System.Generics.Collections,
  uDALLe2.DTO
  ;


type
  TDALLESize = (DALLE256, DALLE512, DALLE1024);

  TImageModelInfo = class
    modelName: string;
    version: string;
  end;

  TBaseImageGeneration = class abstract
  protected
    FAPIKey : string;
    FModelInfo : TObjectList<TImageModelInfo>;
    function GetModelInfo: TObjectList<TImageModelInfo>; virtual; abstract;
  private
    procedure ListOpenAIModels(out AModelList: TStringList); virtual; abstract;
  public
    constructor Create(APIKey: string);
    destructor Destroy; override;
    function CallDALL_E(const prompt: string; n: Integer; size: TDALLESize): TGeneratedImagesClass; virtual; abstract;
    property ModelInfo: TObjectList<TImageModelInfo> read GetModelInfo;
  end;

implementation

uses
  System.SysUtils
  ;

{ TBaseImageGeneration }

constructor TBaseImageGeneration.Create(APIKey: string);
begin
  FAPIKey := APIKey;
  FModelInfo := TObjectList<TImageModelInfo>.Create;
end;

destructor TBaseImageGeneration.Destroy;
begin
  FreeAndNil(FModelInfo);
  inherited;
end;

end.
