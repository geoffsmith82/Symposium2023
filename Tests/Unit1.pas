unit Unit1;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TMyTestObject = class
  public
    [Setup]
    procedure Setup;
    [Test]
    procedure Test1;
    [TearDown]
    procedure TearDown;
  end;

implementation

procedure TMyTestObject.Setup;
begin
end;

procedure TMyTestObject.TearDown;
begin
end;

procedure TMyTestObject.Test1;
begin

end;

initialization
  TDUnitX.RegisterTestFixture(TMyTestObject);

end.
