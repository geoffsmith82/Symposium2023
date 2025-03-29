unit MCP.Resources;

interface

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  System.Generics.Collections;

type
  /// <summary>
  /// Resource types supported by MCP
  /// </summary>
  TMCPResourceType = (rtDocument, rtImage, rtVideo, rtAudio, rtLink, rtData, rtOther);
  
  /// <summary>
  /// Converts resource type enum to string
  /// </summary>
  function ResourceTypeToString(ResourceType: TMCPResourceType): string;
  
  /// <summary>
  /// Converts string to resource type enum
  /// </summary>
  function StringToResourceType(const TypeStr: string): TMCPResourceType;

type
  /// <summary>
  /// Represents a resource in MCP
  /// </summary>
  TMCPResource = class
  private
    FID: string;
    FName: string;
    FDescription: string;
    FResourceType: TMCPResourceType;
    FURL: string;
    FContentType: string;
    FTags: TArray<string>;
    FMetadata: TJSONObject;
  public
    constructor Create(const AID, AName, ADescription: string; 
      AResourceType: TMCPResourceType = rtLink);
    destructor Destroy; override;
    
    function ToJSON: TJSONObject;
    procedure AddTag(const ATag: string);
    procedure AddMetadata(const AKey: string; AValue: TJSONValue);
    function GetMetadataAsString: string;
    
    property ID: string read FID write FID;
    property Name: string read FName write FName;
    property Description: string read FDescription write FDescription;
    property ResourceType: TMCPResourceType read FResourceType write FResourceType;
    property URL: string read FURL write FURL;
    property ContentType: string read FContentType write FContentType;
    property Tags: TArray<string> read FTags write FTags;
  end;

  /// <summary>
  /// Manages a collection of MCP resources
  /// </summary>
  TMCPResourceManager = class
  private
    FResources: TObjectList<TMCPResource>;
    function GetResource(Index: Integer): TMCPResource;
    function GetResourceCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    
    function AddResource(const AID, AName, ADescription: string; 
      AResourceType: TMCPResourceType = rtLink): TMCPResource;
    function FindResourceByID(const AID: string): TMCPResource;
    procedure DeleteResource(const AID: string);
    function ToJSON: TJSONArray;
    
    property Resources[Index: Integer]: TMCPResource read GetResource;
    property ResourceCount: Integer read GetResourceCount;
  end;

implementation

function ResourceTypeToString(ResourceType: TMCPResourceType): string;
begin
  case ResourceType of
    rtDocument: Result := 'document';
    rtImage: Result := 'image';
    rtVideo: Result := 'video';
    rtAudio: Result := 'audio';
    rtLink: Result := 'link';
    rtData: Result := 'data';
    rtOther: Result := 'other';
  else
    Result := 'link';
  end;
end;

function StringToResourceType(const TypeStr: string): TMCPResourceType;
begin
  if SameText(TypeStr, 'document') then
    Result := rtDocument
  else if SameText(TypeStr, 'image') then
    Result := rtImage
  else if SameText(TypeStr, 'video') then
    Result := rtVideo
  else if SameText(TypeStr, 'audio') then
    Result := rtAudio
  else if SameText(TypeStr, 'link') then
    Result := rtLink
  else if SameText(TypeStr, 'data') then
    Result := rtData
  else
    Result := rtOther;
end;

{ TMCPResource }

constructor TMCPResource.Create(const AID, AName, ADescription: string;
  AResourceType: TMCPResourceType);
begin
  inherited Create;
  
  if AID.Trim.IsEmpty then
    raise Exception.Create('Resource ID cannot be empty');
    
  if AName.Trim.IsEmpty then
    raise Exception.Create('Resource name cannot be empty');
    
  FID := AID;
  FName := AName;
  FDescription := ADescription;
  FResourceType := AResourceType;
  FMetadata := TJSONObject.Create;
  SetLength(FTags, 0);
end;

destructor TMCPResource.Destroy;
begin
  FMetadata.Free;
  inherited;
end;

procedure TMCPResource.AddTag(const ATag: string);
begin
  if ATag.Trim.IsEmpty then
    Exit;
    
  // Check if tag already exists
  for var Tag in FTags do
  begin
    if SameText(Tag, ATag) then
      Exit;
  end;
  
  // Add the tag
  SetLength(FTags, Length(FTags) + 1);
  FTags[High(FTags)] := ATag;
end;

procedure TMCPResource.AddMetadata(const AKey: string; AValue: TJSONValue);
begin
  // Remove existing key if present
  for var i := 0 to FMetadata.Count - 1 do
  begin
    if SameText(FMetadata.Pairs[i].JsonString.Value, AKey) then
    begin
      FMetadata.RemovePair(AKey);
      Break;
    end;
  end;
    
  // Add the new key-value pair
  FMetadata.AddPair(AKey, AValue);
end;

function TMCPResource.GetMetadataAsString: string;
begin
  Result := FMetadata.ToString;
end;

function TMCPResource.ToJSON: TJSONObject;
var
  TagsArray: TJSONArray;
begin
  Result := TJSONObject.Create;
  
  // Add required properties
  Result.AddPair('id', FID);
  Result.AddPair('name', FName);
  Result.AddPair('description', FDescription);
  Result.AddPair('type', ResourceTypeToString(FResourceType));
  
  // Add URL if present
  if not FURL.Trim.IsEmpty then
    Result.AddPair('url', FURL);
    
  // Add content type if present
  if not FContentType.Trim.IsEmpty then
    Result.AddPair('content_type', FContentType);
  
  // Add tags array if any
  if Length(FTags) > 0 then
  begin
    TagsArray := TJSONArray.Create;
    for var Tag in FTags do
      TagsArray.Add(Tag);
      
    Result.AddPair('tags', TagsArray);
  end;
  
  // Add metadata if not empty
  if FMetadata.Count > 0 then
    Result.AddPair('metadata', FMetadata.Clone as TJSONObject);
end;

{ TMCPResourceManager }

constructor TMCPResourceManager.Create;
begin
  inherited;
  FResources := TObjectList<TMCPResource>.Create(True); // Owns objects
end;

destructor TMCPResourceManager.Destroy;
begin
  FResources.Free;
  inherited;
end;

function TMCPResourceManager.GetResource(Index: Integer): TMCPResource;
begin
  Result := FResources[Index];
end;

function TMCPResourceManager.GetResourceCount: Integer;
begin
  Result := FResources.Count;
end;

function TMCPResourceManager.AddResource(const AID, AName, ADescription: string;
  AResourceType: TMCPResourceType): TMCPResource;
begin
  // Check if resource with same ID already exists
  for var Resource in FResources do
  begin
    if SameText(Resource.ID, AID) then
      raise Exception.CreateFmt('Resource with ID "%s" already exists', [AID]);
  end;
  
  Result := TMCPResource.Create(AID, AName, ADescription, AResourceType);
  FResources.Add(Result);
end;

function TMCPResourceManager.FindResourceByID(const AID: string): TMCPResource;
begin
  Result := nil;
  for var Resource in FResources do
  begin
    if SameText(Resource.ID, AID) then
    begin
      Result := Resource;
      Break;
    end;
  end;
end;

procedure TMCPResourceManager.DeleteResource(const AID: string);
var
  i: Integer;
begin
  for i := FResources.Count - 1 downto 0 do
  begin
    if SameText(FResources[i].ID, AID) then
    begin
      FResources.Delete(i);
      Break;
    end;
  end;
end;

function TMCPResourceManager.ToJSON: TJSONArray;
begin
  Result := TJSONArray.Create;
  
  // Add each resource as a JSON object
  for var Resource in FResources do
    Result.Add(Resource.ToJSON);
end;

end.