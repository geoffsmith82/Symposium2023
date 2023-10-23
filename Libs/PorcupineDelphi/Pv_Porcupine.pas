(*
    Copyright 2018-2023 Picovoice Inc.

    You may not use this file except in compliance with the license. A copy of
    the license is located in the "LICENSE" file accompanying this source.

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
    WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
    License for the specific language governing permissions and limitations under
    the License.
*)

unit Pv_Porcupine;

interface

uses
  System.SysUtils, picovoice;

type
  // Forward declaration for Porcupine wake word engine. It detects utterances of
  // given keywords within an incoming stream of audio in real-time. It processes
  // incoming audio in consecutive frames and for each frame emits the detection
  // result. The number of samples per frame can be attained by calling
  // 'pv_porcupine_frame_length()'. The incoming audio needs to have a sample rate
  // equal to 'pv_sample_rate()' and be 16-bit linearly-encoded. Porcupine
  // operates on single-channel audio.
  pv_porcupine_t = record
  end;

  PInt16 = ^Int16;

//  pv_status_t = (PV_STATUS_INVALID_ARGUMENT, PV_STATUS_IO_ERROR, PV_STATUS_OUT_OF_MEMORY);

function pv_porcupine_init(
        access_key: PAnsiChar;
        model_path: PAnsiChar;
        num_keywords: Int32;
        keyword_paths: PPAnsiChar;
        sensitivities: PSingle;
        out object2: pv_porcupine_t): pv_status_t; cdecl; external 'libpv_porcupine.dll';

// Destructor.
procedure pv_porcupine_delete(var obj: pv_porcupine_t); cdecl; external 'libpv_porcupine.dll';

// Processes a frame of the incoming audio stream and emits the detection
// result.
function pv_porcupine_process(
        var obj: pv_porcupine_t;
        pcm: PInt16;
        out keyword_index: Int32): pv_status_t; cdecl; external 'libpv_porcupine.dll';

// Getter for version.
function pv_porcupine_version: PAnsiChar; cdecl; external 'libpv_porcupine.dll';

// Getter for number of audio samples per frame.
function pv_porcupine_frame_length: Int32; cdecl; external 'libpv_porcupine.dll';

implementation

end.
