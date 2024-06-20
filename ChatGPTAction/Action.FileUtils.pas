unit Action.FileUtils;

interface

uses
  Files.Extra;


type
  TFileID = UInt64;

function GetFilenameFromFileID(FileID: TFileID): string;
function GetFileIDFromFilename(const FileName: string): TFileID;

implementation

uses
  Windows, SysUtils;

function GetFileIDFromFilename(const FileName: string): TFileID;
var
  hFile: THandle;
  FileInfo: BY_HANDLE_FILE_INFORMATION;
begin
  hFile := CreateFile(PChar(FileName), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if hFile = INVALID_HANDLE_VALUE then
    RaiseLastOSError;

  try
    if not GetFileInformationByHandle(hFile, FileInfo) then
      RaiseLastOSError;

    Result := TFileID(FileInfo.nFileIndexHigh) shl 32 or FileInfo.nFileIndexLow;
  finally
    CloseHandle(hFile);
  end;
end;

function GetFilenameFromFileID(FileID: TFileID): string;
var
  hFile: THandle;
  ObjectIDBuffer: FILE_ID_DESCRIPTOR;
  FileNameBuffer: array[0..MAX_PATH - 1] of Char;
  FileNameLength: DWORD;
  hDrive : THandle;
begin
  FillChar(ObjectIDBuffer, SizeOf(ObjectIDBuffer), 0);
  ObjectIDBuffer.dwSize := SizeOf(ObjectIDBuffer);
  ObjectIDBuffer.Type_ := 0;
  ObjectIDBuffer.FileId.LowPart := 0;//Default(FILE_ID_128);  // Initialize to default
  ObjectIDBuffer.FileId.HighPart := 0;
  ObjectIDBuffer.FileId.QuadPart := 0;
  PUInt64(@ObjectIDBuffer.FileId)^ := FileID; // Assign the 64-bit file ID

  hDrive := GetVolumeHandle('d:\Programming\Project2.dpr');

  hFile := OpenFileById(hDrive,
                        @ObjectIDBuffer,
                        GENERIC_READ,
                        FILE_SHARE_READ,
                        nil,
                        0);
  if hFile = INVALID_HANDLE_VALUE then
    RaiseLastOSError;

  try
    FileNameLength := GetFinalPathNameByHandle(hFile, FileNameBuffer, MAX_PATH, 0);
    if FileNameLength = 0 then
      RaiseLastOSError;

    SetString(Result, FileNameBuffer, FileNameLength);
  finally
    CloseHandle(hFile);
  end;
end;



end.
