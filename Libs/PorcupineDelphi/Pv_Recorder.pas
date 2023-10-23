{
    Copyright 2021-2022 Picovoice Inc.

    You may not use this file except in compliance with the license. A copy of the license is located in the "LICENSE"
    file accompanying this source.

    Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
    an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
    specific language governing permissions and limitations under the License.
}

unit pv_recorder;

interface

const
  PV_RECORDER_DEFAULT_DEVICE_INDEX = -1; // Default device index

type
  PInt16 = ^Int16;
  // Forward declaration of PV_Recorder object. It contains everything related to recording
  // audio, and audio frame information.
  pv_recorder_t = record
  end;
  Ppv_recorder_t = ^pv_recorder_t;

  // Status codes.
  pv_recorder_status_t = (
    PV_RECORDER_STATUS_SUCCESS = 0,
    PV_RECORDER_STATUS_OUT_OF_MEMORY,
    PV_RECORDER_STATUS_INVALID_ARGUMENT,
    PV_RECORDER_STATUS_INVALID_STATE,
    PV_RECORDER_STATUS_BACKEND_ERROR,
    PV_RECORDER_STATUS_DEVICE_ALREADY_INITIALIZED,
    PV_RECORDER_STATUS_DEVICE_NOT_INITIALIZED,
    PV_RECORDER_STATUS_IO_ERROR,
    PV_RECORDER_STATUS_RUNTIME_ERROR
  );

// Constructor for Picovoice Audio Recorder.
function pv_recorder_init(
        device_index: Int32; // The index of the audio device to use. A value of (-1) will resort to default device.
        frame_length: Int32; // The length of audio frame to get for each read call.
        buffer_size_msec: Int32; // Time in milliseconds to store audio frames to a temporary buffer.
        log_overflow: Boolean; // Boolean variable to enable overflow logs. This will enable warning logs when buffer overflow occurs.
        log_silence: Boolean; // Boolean variable to enable silence logs. This will log when continuous audio buffers are detected as silent.
        out obj: Ppv_recorder_t // Audio Recorder object to initialize.
      ): pv_recorder_status_t; stdcall; external 'libpv_recorder.dll';

// Destructor.
procedure pv_recorder_delete(obj: Ppv_recorder_t); stdcall; external 'libpv_recorder.dll';

// Starts recording audio and processing audio frames.
function pv_recorder_start(obj: Ppv_recorder_t): pv_recorder_status_t; stdcall; external 'libpv_recorder.dll';

// Stops recording audio.
function pv_recorder_stop(obj: Ppv_recorder_t): pv_recorder_status_t; stdcall; external 'libpv_recorder.dll';

// Synchronous call to read frames. Copies param ${length} amount of frames to param ${pcm} array provided to input.
function pv_recorder_read(obj: Ppv_recorder_t; pcm: PInt16): pv_recorder_status_t; stdcall; external 'libpv_recorder.dll';

// Getter to get the current selected audio device name.
function pv_recorder_get_selected_device(obj: Ppv_recorder_t): PAnsiChar; stdcall; external 'libpv_recorder.dll';

// Gets the input audio devices currently available. Each device name has a separate pointer, so the
// caller must free each item in the output array individually and free the output array itself.
function pv_recorder_get_audio_devices(out count: Int32; out devices: PPAnsiChar): pv_recorder_status_t; stdcall; external 'libpv_recorder.dll';

// Frees the device list initialized by pv_recorder_get_audio_devices. The function does not do
// any checks, providing correct count and pointer is up to the caller.
procedure pv_recorder_free_device_list(count: Int32; devices: PPAnsiChar); stdcall; external 'libpv_recorder.dll';

// Provides string representations of status codes.
function pv_recorder_status_to_string(status: pv_recorder_status_t): PAnsiChar; stdcall; external 'libpv_recorder.dll';

// Getter for version.
function pv_recorder_version: PAnsiChar; stdcall; external 'libpv_recorder.dll';

implementation

end.

