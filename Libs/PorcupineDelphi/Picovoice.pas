{
    Copyright 2018-2023 Picovoice Inc.

    You may not use this file except in compliance with the license. A copy of
    the license is located in the "LICENSE" file accompanying this source.

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
    WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
    License for the specific language governing permissions and limitations under
    the License.
}

unit Picovoice;

interface

const
  // Audio sample rate accepted by Picovoice.
  PV_API: integer = 0; // dummy value; you may want to replace it with the actual value or function call.

type
  // Status codes.
  pv_status_t = (
    PV_STATUS_SUCCESS = 0,
    PV_STATUS_OUT_OF_MEMORY,
    PV_STATUS_IO_ERROR,
    PV_STATUS_INVALID_ARGUMENT,
    PV_STATUS_STOP_ITERATION,
    PV_STATUS_KEY_ERROR,
    PV_STATUS_INVALID_STATE,
    PV_STATUS_RUNTIME_ERROR,
    PV_STATUS_ACTIVATION_ERROR,
    PV_STATUS_ACTIVATION_LIMIT_REACHED,
    PV_STATUS_ACTIVATION_THROTTLED,
    PV_STATUS_ACTIVATION_REFUSED
  );

// Function to get the sample rate accepted by Picovoice.
function pv_sample_rate: Integer; cdecl; external 'libpv_porcupine.dll';

// Function to get string representation of status codes.
function pv_status_to_string(status: pv_status_t): PAnsiChar; cdecl; external 'libpv_porcupine.dll';

implementation

end.
