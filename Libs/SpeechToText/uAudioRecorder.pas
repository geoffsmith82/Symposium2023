unit uAudioRecorder;

interface
uses
  Classes,
  Windows,
  MMSystem,
  SysUtils
  ;
type
  TAudioDataEvent = procedure(Sender: TObject; Data: TMemoryStream) of object;
  TAudioDevice = record
    DeviceID: UINT;
    DeviceName: string;
    DeviceDescription: string;
  end;

  TAudioDevices = array of TAudioDevice;

  TAudioRecorder = class
  private
    FSelectedDeviceID: UINT;
    FWaveFormat: TWaveFormatEx;
    FHandle: HWAVEIN;
    FBuffer: array[0..1] of TWaveHdr;
    FStream: TMemoryStream;
    FOnAudioData: TAudioDataEvent;
    procedure AllocateBuffer(var Buf: TWaveHdr);
    procedure FreeBuffer(var Buf: TWaveHdr);
    procedure ProcessBuffer(const Buffer: TWaveHdr);
  public
    constructor Create;
    destructor Destroy; override;
    function GetAvailableDevices: TAudioDevices;
    procedure SelectDevice(DeviceID: UINT);
    procedure Start;
    procedure Stop;
    property OnAudioData: TAudioDataEvent read FOnAudioData write FOnAudioData;
  end;

implementation


procedure WaveInCallback(hwi: HWAVEIN; uMsg: UINT; dwInstance, dwParam1, dwParam2: DWORD); stdcall;
var
  Recorder: TAudioRecorder;
begin
  if uMsg = WIM_DATA then
  begin
    Recorder := TAudioRecorder(dwInstance);
    // Here you process the data from the buffer. For example:
    Recorder.ProcessBuffer(TWaveHdr(Pointer(dwParam1)^));

    // Re-add the buffer to continue recording
    waveInAddBuffer(hwi, PWaveHdr(dwParam1), sizeof(TWaveHdr));
  end;
end;

procedure TAudioRecorder.ProcessBuffer(const Buffer: TWaveHdr);
var
  stream : TMemoryStream;
begin
  if Assigned(FOnAudioData) then
  begin
    stream := TMemoryStream.Create;
    stream.WriteBuffer(Buffer.lpData^, Buffer.dwBytesRecorded);
    FOnAudioData(Self, Stream);
  end;
  // Here, you could also trigger an event to notify the main application
  // that there's new data, if needed.
end;

constructor TAudioRecorder.Create;
begin
  FStream := TMemoryStream.Create;
  FHandle := INVALID_HANDLE_VALUE;
  with FWaveFormat do
  begin
    wFormatTag := WAVE_FORMAT_PCM;
    nChannels := 1;
    nSamplesPerSec := 16000;
    wBitsPerSample := 16;
    nBlockAlign := nChannels * wBitsPerSample div 8;
    nAvgBytesPerSec := nSamplesPerSec * nBlockAlign;
    cbSize := 0;
  end;
  FSelectedDeviceID := WAVE_MAPPER; // Default to system default audio device
end;

destructor TAudioRecorder.Destroy;
begin
  Stop;
  FreeBuffer(FBuffer[0]);
  FreeBuffer(FBuffer[1]);
  FStream.Free;
  inherited;
end;
procedure TAudioRecorder.SelectDevice(DeviceID: UINT);
begin
  FSelectedDeviceID := DeviceID;
end;
function TAudioRecorder.GetAvailableDevices: TAudioDevices;
var
  NumDevices, i: UINT;
  Caps: TWaveInCaps;
begin
  NumDevices := waveInGetNumDevs;  // Get the number of audio input devices.
  SetLength(Result, NumDevices);

  for i := 0 to NumDevices - 1 do
  begin
    if waveInGetDevCaps(i, @Caps, SizeOf(Caps)) = MMSYSERR_NOERROR then
    begin
      Result[i].DeviceID := i;
      Result[i].DeviceName := Caps.szPname;
      Result[i].DeviceDescription := Format('Channels: %d', [Caps.wChannels]);
    end;
  end;
end;
procedure TAudioRecorder.AllocateBuffer(var Buf: TWaveHdr);
var
  mmResult: UINT;
begin
  GetMem(Buf.lpData, 3200);
  Buf.dwBufferLength := 3200;
  Buf.dwFlags := 0;

  mmResult := waveInPrepareHeader(FHandle, @Buf, SizeOf(TWaveHdr));
  if mmResult <> MMSYSERR_NOERROR then
    raise Exception.CreateFmt('Error preparing buffer header: %d', [mmResult]);

end;

procedure TAudioRecorder.FreeBuffer(var Buf: TWaveHdr);
var
  mmResult: UINT;
begin
  if Buf.lpData <> nil then
  begin
    mmResult := waveInUnprepareHeader(FHandle, @Buf, SizeOf(TWaveHdr));
    if mmResult <> MMSYSERR_NOERROR then
      raise Exception.CreateFmt('Error unpreparing buffer header: %d', [mmResult]);

    FreeMem(Buf.lpData);
  end;
end;

procedure TAudioRecorder.Start;
var
  mmResult: UINT;
begin
  mmResult := waveInOpen(@FHandle, FSelectedDeviceID, @FWaveFormat, DWORD(@WaveInCallback), DWORD(Self), CALLBACK_FUNCTION);
  if mmResult <> MMSYSERR_NOERROR then
  begin
    raise Exception.CreateFmt('Error initializing audio device: %d', [mmResult]);
  end;
  AllocateBuffer(FBuffer[0]);
  AllocateBuffer(FBuffer[1]);


  mmResult := waveInAddBuffer(FHandle, @FBuffer[0], sizeof(TWaveHdr));
  if mmResult <> MMSYSERR_NOERROR then
  begin
    waveInClose(FHandle);
    raise Exception.CreateFmt('Error adding buffer to device: %d', [mmResult]);
  end;
  mmResult := waveInAddBuffer(FHandle, @FBuffer[1], sizeof(TWaveHdr));
  if mmResult <> MMSYSERR_NOERROR then
  begin
    waveInClose(FHandle);
    raise Exception.CreateFmt('Error adding buffer to device: %d', [mmResult]);
  end;
  mmResult := waveInStart(FHandle);
  if mmResult <> MMSYSERR_NOERROR then
  begin
    waveInClose(FHandle);
    raise Exception.CreateFmt('Error starting recording: %d', [mmResult]);
  end;
end;


procedure TAudioRecorder.Stop;
begin
  if FHandle <> 0 then
  begin
    waveInStop(FHandle);
    waveInClose(FHandle);
    FHandle := 0;
  end;
end;
end.

