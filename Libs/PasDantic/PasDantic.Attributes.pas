unit PasDantic.Attributes;

interface

uses
  System.SysUtils,
  Rtti;

type
  RequiredAttribute = class(TCustomAttribute)
  end;

  OptionalAttribute = class(TCustomAttribute)
  end;

  StringLengthAttribute = class(TCustomAttribute)
  private
    FMin, FMax: Integer;
  public
    constructor Create(AMin, AMax: Integer);
    property Min: Integer read FMin;
    property Max: Integer read FMax;
  end;

  RangeAttribute = class(TCustomAttribute)
  private
    FMin, FMax: Double;
  public
    constructor Create(AMin, AMax: Double);
    property Min: Double read FMin;
    property Max: Double read FMax;
  end;

  DefaultAttribute = class(TCustomAttribute)
  private
    FValue: TValue;
  public
    constructor Create(const AValue: string); overload;
    constructor Create(const AValue: Integer); overload;
    constructor Create(const AValue: Double); overload;

    property Value: TValue read FValue;
  end;

  RegexAttribute = class(TCustomAttribute)
  private
    FPattern: string;
  public
    constructor Create(const APattern: string);
    property Pattern: string read FPattern;
  end;

  AllowedValuesAttribute = class(TCustomAttribute)
  private
    FAllowed: TArray<string>;
  public
    constructor Create(const Csv: string);
    property Allowed: TArray<string> read FAllowed;
  end;

  ExampleAttribute = class(TCustomAttribute)
  private
    FExample: string;
  public
    constructor Create(const AExample: string);
    property Example: string read FExample;
  end;

implementation

constructor StringLengthAttribute.Create(AMin, AMax: Integer);
begin
  inherited Create;
  FMin := AMin;
  FMax := AMax;
end;

constructor RangeAttribute.Create(AMin, AMax: Double);
begin
  inherited Create;
  FMin := AMin;
  FMax := AMax;
end;

constructor DefaultAttribute.Create(const AValue: string);
begin
  inherited Create;
  FValue := AValue;
end;

constructor RegexAttribute.Create(const APattern: string);
begin
  inherited Create;
  FPattern := APattern;
end;

constructor AllowedValuesAttribute.Create(const Csv: string);
begin
  FAllowed := Csv.Split([',']); // or ';' etc
end;

constructor ExampleAttribute.Create(const AExample: string);
begin
  inherited Create;
  FExample := AExample;
end;

constructor DefaultAttribute.Create(const AValue: Double);
begin
  FValue := AValue;
end;

constructor DefaultAttribute.Create(const AValue: Integer);
begin
  FValue := AValue;
end;

end.