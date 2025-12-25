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
    [Test] procedure StringLength_WithinBounds_Passes;
    [Test] procedure StringLength_ToLong_Passes;
    [Test] procedure Range_WithinBounds_Passes;
    [Test] procedure AllowedValues_Match_Passes;
    [Test] procedure Multiple_Constraints_All_Satisfied_Passes;
    [Test] procedure Required_Passes;
    [Test] procedure Optional_Passes;
    [Test] procedure Regex_Pass;
    [Test] procedure Range_OutsideBounds_Fails;
  end;

implementation

type
  TOptionalObject = class
    [Optional]
    Street: string;
  end;


  TRequiredObject = class
    [Required]
    Street: string;
  end;


  TAddress = class
  public
    [Required]
    Street: string;

    [Required]
    City: string;
  end;

  TRange_WithinBounds_Passes = class
    [Range(0, 120)]
    Age: Integer;
  end;

  TStringLength_WithinBounds_Passes = class
    [Required]
    [StringLength(3, 10)]
    Name: string;
  end;

  TRegExTest = class
  public
    [Regex('^\S+@\S+\.\S+$')]
    Email: string;
  end;

  TAllowedValuesTest = class
    [AllowedValues('admin,user,guest')]
    Role: string;
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
    Assert.AreEqual(0, R.ErrorCount, 'Expected no validation errors');
  finally
    U.Free;
  end;
end;

procedure TPasDanticValidatorTests.Required_Passes;
var
  U: TRequiredObject;
  R: TValidationResult;
  E: TValidationError;
begin
  U := TRequiredObject.Create;
  try
    // Name required + empty => error
    U.Street := '123 High St';

    R := ValidateModel(U);

    Assert.IsTrue(R.IsValid);
//    Assert.IsTrue(FindError(R.Errors, 'Name', E), 'Expected an error for Name');

    // Match your intended pydantic-ish message (adjust if you kept "is required.")
//    Assert.AreEqual('field required', E.Message);
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
    U.Age := 30;
    U.Email := 'alice@example.com';
    U.Role := 'admin';
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

procedure TPasDanticValidatorTests.StringLength_WithinBounds_Passes;
var
  U: TStringLength_WithinBounds_Passes;
  R: TValidationResult;
begin
  U := TStringLength_WithinBounds_Passes.Create;
  try
    U.Name := 'Robert';              // length 6, min=3 max=10

    R := ValidateModel(U);

    Assert.IsTrue(R.IsValid);
  finally
    U.Free;
  end;
end;


procedure TPasDanticValidatorTests.StringLength_ToLong_Passes;
var
  U: TStringLength_WithinBounds_Passes;
  R: TValidationResult;
begin
  U := TStringLength_WithinBounds_Passes.Create;
  try
    U.Name := 'This name is to long';              // length 6, min=3 max=10

    R := ValidateModel(U);

    Assert.IsTrue(not R.IsValid);
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

procedure TPasDanticValidatorTests.Range_WithinBounds_Passes;
var
  U: TRange_WithinBounds_Passes;
  R: TValidationResult;
begin
  U := TRange_WithinBounds_Passes.Create;
  try
    U.Age := 42;                     // valid range 0..120


    R := ValidateModel(U);

    Assert.IsTrue(R.IsValid);
  finally
    U.Free;
  end;
end;

procedure TPasDanticValidatorTests.Range_OutsideBounds_Fails;
var
  U: TRange_WithinBounds_Passes;
  R: TValidationResult;
begin
  U := TRange_WithinBounds_Passes.Create;
  try
    U.Age := 400;                     // valid range 0..120


    R := ValidateModel(U);

    Assert.IsFalse(R.IsValid);
  finally
    U.Free;
  end;
end;


procedure TPasDanticValidatorTests.Regex_Fails;
var
  U: TRegExTest;
  R: TValidationResult;
  E: TValidationError;
begin
  U := TRegExTest.Create;
  try
    U.Email := 'not-an-email';


    R := ValidateModel(U);

    Assert.IsFalse(R.IsValid);
    Assert.IsTrue(FindError(R.Errors, 'Email', E), 'Expected an error for Email');
    Assert.IsTrue(E.Message.Contains('pattern') or E.Message.Contains('regex'), 'Expected a regex-related message');
  finally
    U.Free;
  end;
end;

procedure TPasDanticValidatorTests.Regex_Pass;
var
  U: TRegExTest;
  R: TValidationResult;
  E: TValidationError;
begin
  U := TRegExTest.Create;
  try
    U.Email := 'email@test.com';

    R := ValidateModel(U);

    Assert.IsTrue(R.IsValid);
  finally
    U.Free;
  end;
end;


procedure TPasDanticValidatorTests.AllowedValues_Fails;
var
  U: TAllowedValuesTest;
  R: TValidationResult;
  E: TValidationError;
begin
  U := TAllowedValuesTest.Create;
  try
    U.Role := 'superuser';

    R := ValidateModel(U);

    Assert.IsFalse(R.IsValid);
    Assert.IsTrue(FindError(R.Errors, 'Role', E), 'Expected an error for Role');
    Assert.IsTrue(E.Message.Contains('one of'), 'Expected an allowed-values message');
  finally
    U.Free;
  end;
end;

procedure TPasDanticValidatorTests.AllowedValues_Match_Passes;
var
  U: TAllowedValuesTest;
  R: TValidationResult;
begin
  U := TAllowedValuesTest.Create;
  try
    U.Role := 'user';                // in allowed set

    R := ValidateModel(U);

    Assert.IsTrue(R.IsValid);
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

procedure TPasDanticValidatorTests.Optional_Passes;
var
  U: TOptionalObject;
  R: TValidationResult;
begin
  U := TOptionalObject.Create;
  try
    U.Street := '';  // Empty

    R := ValidateModel(U);

    Assert.IsTrue(R.IsValid);
    Assert.AreEqual(0, R.ErrorCount);
  finally
    U.Free;
  end;

end;

procedure TPasDanticValidatorTests.Multiple_Constraints_All_Satisfied_Passes;
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

    Assert.IsTrue(R.IsValid);
    Assert.AreEqual(0, R.ErrorCount);
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

