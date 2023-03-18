unit uBaseFaceRecognition;

interface

uses
  System.Classes
  ;

type
  TBaseFaceRecognition = class

  public
    function DetectFacesFromURL(imageUrl: string): string; virtual; abstract;
    function DetectFacesFromStream(imageStream: TStream): string; virtual; abstract;
    function DetectFacesFromFile(imageFilename: string): string; virtual; abstract;
  end;

implementation

end.
