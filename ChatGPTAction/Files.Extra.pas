unit Files.Extra;

interface

uses
  sysutils,
  Winapi.ShLwApi,
  FileCtrl,
  windows;

const
  FILE_ID_DESCRIPTOR_GUID = $0;

type
  // Define the structure for FILE_ID_DESCRIPTOR
  FILE_ID_TYPE = DWORD;

  FILE_ID_128 = packed record
    Identifier: array[0..15] of Byte;
  end;

  FILE_ID_DESCRIPTOR = packed record
    dwSize: DWORD;
    Type_: FILE_ID_TYPE;
    case Integer of
      0: (FileId: LARGE_INTEGER);
      1: (ObjectId: TGUID);
      2: (ExtendedFileId: FILE_ID_128);
  end;
  LPFILE_ID_DESCRIPTOR = ^FILE_ID_DESCRIPTOR;

const
  FILE_ID_TYPE_OBJECTID = 2;
  FILE_FLAG_OPEN_REQUIRING_OPLOCK = $40000;


type
  // Define the structure for FILE_OBJECTID_INFORMATION
  TFileObjectIdInformation = record
    ObjectId: TGUID;
    BirthVolumeId: TGUID;
    BirthObjectId: TGUID;
    DomainId: TGUID;
  end;

const
  FileObjectIdInfo = 1; // Corresponding value for FileObjectIdInformation class


function OpenFileById(
  hVolumeHint: THandle;
  lpFileId: LPFILE_ID_DESCRIPTOR;
  dwDesiredAccess: DWORD;
  dwShareMode: DWORD;
  lpSecurityAttributes: PSecurityAttributes;
  dwFlagsAndAttributes: DWORD
): THandle; stdcall; external 'kernel32.dll';

function GetFileInformationByHandleEx(
  hFile: THandle;
  FileInformationClass: Integer;
  lpFileInformation: Pointer;
  dwBufferSize: DWORD
): BOOL; stdcall; external 'kernel32.dll';

function GetRelativePath(const FullPath, BasePath: string): string;
function GetFileObjectID(const FilePath: string): string;
function GetFilePathFromObjectID(const VolumePath, ObjectID: string): string;
function GetVolumeHandle(const VolumePath: string): THandle;

implementation

function GetRelativePath(const FullPath, BasePath: string): string;
var
  RelativePath: array[0..MAX_PATH] of Char;
begin
  if PathRelativePathTo(RelativePath, PChar(BasePath), FILE_ATTRIBUTE_DIRECTORY, PChar(FullPath), FILE_ATTRIBUTE_NORMAL) then
    Result := string(RelativePath)
  else
    Result := FullPath; // Return the full path if the function fails
end;

function GetFileObjectID(const FilePath: string): string;
var
  FileHandle: THandle;
  ObjectId: TFileObjectIdInformation;
  FileInformation: TByHandleFileInformation;
begin
  Result := '';
  FileHandle := CreateFile(PChar(FilePath), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if FileHandle = INVALID_HANDLE_VALUE then
    RaiseLastOSError;

  try
    if not GetFileInformationByHandleEx(FileHandle, FileObjectIdInfo, @ObjectId, SizeOf(ObjectId)) then
      RaiseLastOSError;

    if IsEqualGUID(ObjectId.ObjectId, TGUID.Empty) then
    begin
      if not GetFileInformationByHandle(FileHandle, FileInformation) then
        RaiseLastOSError;

      Result := '';
    end
    else
    begin
      Result := GUIDToString(ObjectId.ObjectId);
    end;
  finally
    CloseHandle(FileHandle);
  end;
end;


function TryStringToGUID(const S: string; out GUID: TGUID): Boolean;
begin
  try
    GUID := StringToGUID(S);
    Result := True;
  except
    on E: EConvertError do
      Result := False;
  end;
end;

function GetVolumeHandle(const VolumePath: string): THandle;
begin
  Result := CreateFile(
    PChar(VolumePath),
    GENERIC_READ,
    FILE_SHARE_READ or FILE_SHARE_WRITE,
    nil,
    OPEN_EXISTING,
    FILE_ATTRIBUTE_NORMAL,
    0
  );
end;

function GetFilePathFromObjectID(const VolumePath, ObjectID: string): string;
var
  VolumeHandle: THandle;
  FileHandle: THandle;
  ObjectIDGUID: TGUID;
  ObjectIdBuffer: FILE_ID_DESCRIPTOR;
  FilePathBuffer: array[0..MAX_PATH] of Char;
  FilePathSize: DWORD;
  LastError: DWORD;
begin
  Result := '';

  // Convert the ObjectID string to a TGUID
  if not TryStringToGUID(ObjectID, ObjectIDGUID) then
    Exit('');

  // Get the volume handle
  VolumeHandle := GetVolumeHandle(VolumePath);
  if VolumeHandle = INVALID_HANDLE_VALUE then
    Exit('');

  try
    // Initialize the FILE_ID_DESCRIPTOR structure
    ObjectIdBuffer.dwSize := SizeOf(FILE_ID_DESCRIPTOR);
    ObjectIdBuffer.Type_ := FILE_ID_TYPE_OBJECTID;
    ObjectIdBuffer.ObjectId := ObjectIDGUID;

    // Open the file by its ObjectID
    FileHandle := OpenFileById(
      VolumeHandle,
      @ObjectIdBuffer,
      GENERIC_READ,
      FILE_SHARE_READ,
      nil,
      0//FILE_FLAG_BACKUP_SEMANTICS
    );

    if FileHandle = INVALID_HANDLE_VALUE then
    begin
      LastError := GetLastError;
      Exit('');
    end;

    try
      // Get the file path
      FilePathSize := GetFinalPathNameByHandle(FileHandle, FilePathBuffer, MAX_PATH, 0);
      if FilePathSize > 0 then
        Result := StrPas(FilePathBuffer);
    finally
      CloseHandle(FileHandle);
    end;
  finally
    CloseHandle(VolumeHandle);
  end;
end;

end.
