unit uEmbeddings;

interface

uses
  System.Classes,
  System.JSON,
  System.SysUtils,
  System.Generics.Collections,
  REST.Client,
  REST.Types
  ;

type
  TEmbedding = TArray<Double>;
  TEmbeddings = TArray<TEmbedding>;


  TEmbeddingService = class abstract
  protected
    FApiKey: string;
  public
    function Embeddings(const Texts: TArray<string>): TEmbeddings; virtual; abstract;
    constructor Create(APIKey: string);
  end;

function CosineDistance(const Vector1, Vector2: TEmbedding): Double;

implementation


function CosineDistance(const Vector1, Vector2: TEmbedding): Double;
var
  DotProduct, Magnitude1, Magnitude2: Double;
  i: Integer;
begin
  DotProduct := 0;
  Magnitude1 := 0;
  Magnitude2 := 0;

  // Calculate dot product and magnitudes
  for i := 0 to Length(Vector1) - 1 do
  begin
    DotProduct := DotProduct + (Vector1[i] * Vector2[i]);
    Magnitude1 := Magnitude1 + Sqr(Vector1[i]);
    Magnitude2 := Magnitude2 + Sqr(Vector2[i]);
  end;

  Magnitude1 := Sqrt(Magnitude1);
  Magnitude2 := Sqrt(Magnitude2);

  // Calculate cosine distance
  Result := 1 - (DotProduct / (Magnitude1 * Magnitude2));
end;

{ TEmbeddingService }

constructor TEmbeddingService.Create(APIKey: string);
begin
  FAPIKey := APIKey;
end;

end.
