unit uBaseFaceRecognition;

interface

uses
  System.Classes
  ;

type
  TBaseFaceRecognition = class
  protected
    FResourceKey : string;
  public
    function DetectFacesFromURL(imageUrl: string): string; virtual; abstract;
    function DetectFacesFromStream(imageStream: TStream): string; virtual; abstract;
    function DetectFacesFromFile(imageFilename: string): string; virtual; abstract;
    constructor Create(const AResourceKey, AApplicationName: string; AHost: string);
  end;

implementation

{ TBaseFaceRecognition }

constructor TBaseFaceRecognition.Create(const AResourceKey, AApplicationName: string; AHost: string);
begin
  FResourceKey := AResourceKey;
end;

end.
