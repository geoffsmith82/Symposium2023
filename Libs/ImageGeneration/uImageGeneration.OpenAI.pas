unit uImageGeneration.OpenAI;

interface

uses
  uImageGeneration,
  System.Generics.Collections,
  System.Classes,
  uDALLe2.DTO
  ;

type
  TImageGenerationOpenAI = class(TBaseImageGeneration)
  protected
    function GetModelInfo: TObjectList<TImageModelInfo>; override;
  public
    function CallDALL_E(const prompt: string; n: Integer; size: TDALLESize): TGeneratedImagesClass; override;
  end;

implementation

{ TImageGenerationOpenAI }

function TImageGenerationOpenAI.CallDALL_E(const prompt: string; n: Integer; size: TDALLESize): TGeneratedImagesClass;
begin

end;

function TImageGenerationOpenAI.GetModelInfo: TObjectList<TImageModelInfo>;
var
  model : TImageModelInfo;
begin
  FModelInfo.Clear;
  model := TImageModelInfo.Create;
  model.ModelName := 'DALLE-2';
  model.Version := 'DALLE-2';
  FModelInfo.Add(model);
  Result := FModelInfo;
end;

end.
