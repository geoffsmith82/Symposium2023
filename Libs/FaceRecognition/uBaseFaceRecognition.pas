unit uBaseFaceRecognition;

interface

type
  TBaseFaceRecognition = class

  public
    function DetectFacesFromURL(imageUrl: string): string; virtual; abstract;

  end;

implementation

end.
