# TGoogleFaceRecognition Class Documentation

The `TGoogleFaceRecognition` class is a wrapper around the Google Cloud Vision API, specifically designed for face recognition tasks. This class inherits from the `TBaseFaceRecognition` class and provides methods for detecting faces in images, either by passing the image URL, a file, or a stream.

## TGoogleFaceRecognition Methods

### Authenticate

```pascal
procedure Authenticate;
```

Authenticate with the Google Cloud Vision API using the provided credentials.

### Constructor

```pascal
constructor Create(const AClientID, AClientSecret: string; AHost: string; ASettings : TIniFile);
```

Create a new `TGoogleFaceRecognition` object with the given `AClientID`, `AClientSecret`, `AHost`, and `ASettings` (an instance of `TIniFile`).

### DetectFacesFromURL

```pascal
function DetectFacesFromURL(imageUrl: string): string; override;
```

Detect faces in an image by providing the image URL (`imageUrl`). Returns a JSON string containing the face recognition results.

### DetectFacesFromStream

```pascal
function DetectFacesFromStream(imageStream: TStream): string; override;
```

Detect faces in an image provided as a `TStream` object (`imageStream`). Returns a JSON string containing the face recognition results.

### DetectFacesFromFile

```pascal
function DetectFacesFromFile(imageFilename: string): string; override;
```

Detect faces in an image by providing the local image file path (`imageFilename`). Returns a JSON string containing the face recognition results.

## Example Usage

Here's an example of how to use the `TGoogleFaceRecognition` class to detect faces in an image:

```pascal
var
  GoogleFaceRecognition: TGoogleFaceRecognition;
  ImageUrl: string;
  DetectionResult: string;

begin
  // Initialize TGoogleFaceRecognition
  GoogleFaceRecognition := TGoogleFaceRecognition.Create('your-resource-key', 'your-secret-key', 'your-host', YourSettings);
  try
    // Authenticate
    GoogleFaceRecognition.Authenticate;

    // Define the image URL and detect faces
    ImageUrl := 'https://example.com/image.jpg';
    DetectionResult := GoogleFaceRecognition.DetectFacesFromURL(ImageUrl);

    // Print the detection result
    WriteLn('Detection result: ' + DetectionResult);
  finally
    GoogleFaceRecognition.Free;
  end;
end;
```

Here's an example of how to use the `TGoogleFaceRecognition` class to detect faces in a local image file:

```pascal
var
  GoogleFaceRecognition: TGoogleFaceRecognition;
  ImageFilename: string;
  DetectionResult: string;

begin
  // Initialize TGoogleFaceRecognition
  GoogleFaceRecognition := TGoogleFaceRecognition.Create('your-client-id', 'your-client-secret', 'your-host', YourSettings);
  try
    // Authenticate
    GoogleFaceRecognition.Authenticate;

    // Define the image file path and detect faces
    ImageFilename := 'C:\path\to\image.jpg';
    DetectionResult := GoogleFaceRecognition.DetectFacesFromFile(ImageFilename);

    // Print the detection result
    WriteLn('Detection result: ' + DetectionResult);
  finally
    GoogleFaceRecognition.Free;
  end;
end;
```