unit uBaseFaceRecognition;

interface

uses
  System.Classes
  ;

type
  TBaseFaceRecognition = class
  protected
    FResourceKey : string;
    FHost : string;
  public
    function DetectFacesFromURL(imageUrl: string): string; virtual; abstract;
    function DetectFacesFromStream(imageStream: TStream): string; virtual; abstract;
    function DetectFacesFromFile(imageFilename: string): string; virtual; abstract;
    constructor Create(const AResourceKey: string; const AHost: string);
  end;

implementation

{ TBaseFaceRecognition }

constructor TBaseFaceRecognition.Create(const AResourceKey: string; const AHost: string);
begin
  FResourceKey := AResourceKey;
  FHost := AHost;
end;

end.
