unit Test.PasDantic.Validator;

interface

uses
  DUnitX.TestFramework,
  System.SysUtils,
  System.Generics.Collections,
  PasDantic.Validator,
  PasDantic.Attributes;

type
  [TestFixture]
  TPasDanticValidatorTests = class
  private
    function FindError(const Errors: TArray<TValidationError>; const Path: string; out Err: TValidationError): Boolean;
  public
    [Test] procedure Valid_Object_Passes;
    [Test] procedure Required_String_Empty_Fails;
    [Test] procedure StringLength_Fails;
    [Test] procedure Range_Fails;
    [Test] procedure Regex_Fails;
    [Test] procedure AllowedValues_Fails;
    [Test] procedure Nested_Path_Is_Dotted;
    [Test] procedure Multiple_Errors_Aggregated;
  end;

implementation

type
  TAddress = class
  public
    [Required]
    Street: string;

    [Required]
    City: string;
  end;

  TUser = class
  public
    [Required]
    [StringLength(3, 10)]
    Name: string;

    [Range(0, 120)]
    Age: Integer;

    [Regex('^\S+@\S+\.\S+$')]
    Email: string;

    [AllowedValues('admin,user,guest')]
    Role: string;

    Address: TAddress;

    constructor Create;
    destructor Destroy; override;
  end;

constructor TUser.Create;
begin
  inherited Create;
  Address := TAddress.Create;
end;

destructor TUser.Destroy;
begin
  Address.Free;
  inherited Destroy;
end;

{ TPasDanticValidatorTests }

function TPasDanticValidatorTests.FindError(const Errors: TArray<TValidationError>; const Path: string; out Err: TValidationError): Boolean;
var
  E: TValidationError;
begin
  Result := False;
  for E in Errors do
  begin
    if SameText(E.PropertyPath, Path) then
    begin
      Err := E;
      Exit(True);
    end;
  end;
end;

procedure TPasDanticValidatorTests.Valid_Object_Passes;
var
  U: TUser;
  R: TValidationResult;
begin
  U := TUser.Create;
  try
    U.Name := 'Alice';
    U.Age := 30;
    U.Email := 'alice@example.com';
    U.Role := 'admin';
    U.Address.Street := '123 High St';
    U.Address.City := 'Bendigo';

    R := ValidateModel(U);

    Assert.IsTrue(R.IsValid, 'Expected object to be valid');
    Assert.AreEqual(0, Length(R.Errors), 'Expected no validation errors');
  finally
    U.Free;
  end;
end;

procedure TPasDanticValidatorTests.Required_String_Empty_Fails;
var
  U: TUser;
  R: TValidationResult;
  E: TValidationError;
begin
  U := TUser.Create;
  try
    // Name required + empty => error
    U.Name := '';
    U.Address.Street := '123 High St';
    U.Address.City := 'Bendigo';

    R := ValidateModel(U);

    Assert.IsFalse(R.IsValid);
    Assert.IsTrue(FindError(R.Errors, 'Name', E), 'Expected an error for Name');

    // Match your intended pydantic-ish message (adjust if you kept "is required.")
    Assert.AreEqual('field required', E.Message);
  finally
    U.Free;
  end;
end;

procedure TPasDanticValidatorTests.StringLength_Fails;
var
  U: TUser;
  R: TValidationResult;
  E: TValidationError;
begin
  U := TUser.Create;
  try
    U.Name := 'Al'; // min 3
    U.Address.Street := '123 High St';
    U.Address.City := 'Bendigo';

    R := ValidateModel(U);

    Assert.IsFalse(R.IsValid);
    Assert.IsTrue(FindError(R.Errors, 'Name', E), 'Expected an error for Name');
    Assert.IsTrue(E.Message.Contains('length'), 'Expected a length-related error message');
  finally
    U.Free;
  end;
end;

procedure TPasDanticValidatorTests.Range_Fails;
var
  U: TUser;
  R: TValidationResult;
  E: TValidationError;
begin
  U := TUser.Create;
  try
    U.Name := 'Alice';
    U.Age := 999; // max 120
    U.Address.Street := '123 High St';
    U.Address.City := 'Bendigo';

    R := ValidateModel(U);

    Assert.IsFalse(R.IsValid);
    Assert.IsTrue(FindError(R.Errors, 'Age', E), 'Expected an error for Age');
    Assert.IsTrue(E.Message.Contains('between') or E.Message.Contains('<='), 'Expected a range-related message');
  finally
    U.Free;
  end;
end;

procedure TPasDanticValidatorTests.Regex_Fails;
var
  U: TUser;
  R: TValidationResult;
  E: TValidationError;
begin
  U := TUser.Create;
  try
    U.Name := 'Alice';
    U.Email := 'not-an-email';
    U.Address.Street := '123 High St';
    U.Address.City := 'Bendigo';

    R := ValidateModel(U);

    Assert.IsFalse(R.IsValid);
    Assert.IsTrue(FindError(R.Errors, 'Email', E), 'Expected an error for Email');
    Assert.IsTrue(E.Message.Contains('pattern') or E.Message.Contains('regex'), 'Expected a regex-related message');
  finally
    U.Free;
  end;
end;

procedure TPasDanticValidatorTests.AllowedValues_Fails;
var
  U: TUser;
  R: TValidationResult;
  E: TValidationError;
begin
  U := TUser.Create;
  try
    U.Name := 'Alice';
    U.Role := 'superuser';
    U.Address.Street := '123 High St';
    U.Address.City := 'Bendigo';

    R := ValidateModel(U);

    Assert.IsFalse(R.IsValid);
    Assert.IsTrue(FindError(R.Errors, 'Role', E), 'Expected an error for Role');
    Assert.IsTrue(E.Message.Contains('one of'), 'Expected an allowed-values message');
  finally
    U.Free;
  end;
end;

procedure TPasDanticValidatorTests.Nested_Path_Is_Dotted;
var
  U: TUser;
  R: TValidationResult;
  E: TValidationError;
begin
  U := TUser.Create;
  try
    U.Name := 'Alice';
    // Missing Address.City (required)
    U.Address.Street := '123 High St';
    U.Address.City := '';

    R := ValidateModel(U);

    Assert.IsFalse(R.IsValid);
    Assert.IsTrue(FindError(R.Errors, 'Address.City', E), 'Expected dotted path Address.City');
    Assert.AreEqual('field required', E.Message);
  finally
    U.Free;
  end;
end;

procedure TPasDanticValidatorTests.Multiple_Errors_Aggregated;
var
  U: TUser;
  R: TValidationResult;
begin
  U := TUser.Create;
  try
    U.Name := 'A';               // too short
    U.Age := -1;                 // out of range
    U.Email := 'bad';            // regex fail
    U.Role := 'superuser';       // allowed values fail
    U.Address.Street := '';      // required
    U.Address.City := '';        // required

    R := ValidateModel(U);

    Assert.IsFalse(R.IsValid);
    Assert.IsTrue(Length(R.Errors) >= 5, 'Expected multiple aggregated errors');
  finally
    U.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TPasDanticValidatorTests);

end.

