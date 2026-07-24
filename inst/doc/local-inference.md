# Local inference

This document describes genflow's local inference configuration and its three
local speech-to-text routes:

- `service = "hf-local"` runs a Hugging Face model through an isolated Python
  subprocess.
- `service = "local-native"` invokes an external native CLI. Its separate
  `native_engine`, `native_backend`, and `model` choices determine what can
  actually run.
- `service = "local-openai"` calls a separately managed
  OpenAI-compatible `/v1/audio/transcriptions` server.

The existing Ollama and llama.cpp text backends share the same local
configuration and diagnostics screen, but they are not STT engines.

## Architecture and ownership

```text
gen_stt()
  |
  +-- hf-local ------> genflow_stt.py subprocess
  |                       |
  |                       +-- PyTorch + Transformers
  |                       +-- generic ASR pipeline or MOSS adapter
  |                       +-- temporary JSON result returned to R
  |
  +-- local-native --> configured native CLI
  |                       |
  |                       +-- CrispASR (multi-architecture, beta)
  |                       |     or moss-transcribe (MOSS-specific)
  |                       +-- engine-compatible local model
  |                       +-- CrispASR also supports auto/hf:// selectors
  |                       +-- CPU or compiled GPU backend
  |
  +-- local-openai --> user-managed HTTP server
                          |
                          +-- POST /v1/audio/transcriptions
```

The R process prepares the audio, selects configuration, starts the Python or
native child process, or sends the HTTP request, and normalizes the result into
the regular `gen_stt()` return contract. It does not embed Python with
reticulate.

For `hf-local`, model memory and Python dependencies live in a child process.
The bridge writes one temporary JSON result containing the transcript,
structured metadata, or a structured error. For `local-openai`, genflow does
not install, launch, stop, or supervise the server.

For `local-native`, genflow also does not install the external project or
own the lifecycle of its model cache. It invokes one configured binary per
transcription. Explicit `auto` and `hf://` selectors authorize CrispASR itself
to download into its cache. Diagnostics may inspect that cache read-only to
report whether an explicitly selected filename is already present. The native
route does not use the Python, PyTorch, Transformers, `device`, or `dtype`
settings.

`local-native` is a transport/runtime contract, not a promise that one binary
can run every Hugging Face speech model. CrispASR supports multiple ASR
architectures and is the broad native engine, but remains experimental/beta.
`moss-transcribe` implements the MOSS architecture specifically. In every
case, the chosen engine must support both the model architecture and its file
format. A repository being tagged for ASR on Hugging Face is not sufficient.

Audio URLs are downloaded before either local route is called. Unsupported
local file formats are converted when `convert = TRUE`, which requires FFmpeg.

## Configuration

### Configuration file location

`gen_local_config()` stores non-secret settings as JSON. The path is resolved
in this order:

1. Its explicit `path` argument.
2. The `genflow.local_config_path` R option.
3. The `GENFLOW_LOCAL_CONFIG` environment variable.
4. `local-inference.json` under `tools::R_user_dir("genflow", "config")`.

An explicit `path` applies only to that `gen_local_config()` call. Set the R
option or environment variable when the runtime and the app should use a
non-default file.

```r
options(
  genflow.local_config_path =
    "/absolute/path/to/local-inference.json"
)

cfg <- gen_local_config()
```

The JSON file deliberately excludes secrets. Keep Hugging Face credentials in
the normal Hugging Face login or environment, and keep a local server token in
`GENFLOW_STT_API_KEY` or pass it as `api_key`.

### Runtime precedence

For `hf-local` settings, the effective order is:

1. An explicit `gen_stt()` argument.
2. The setting's environment variable.
3. The saved JSON setting.
4. The built-in fallback.

`local-openai` follows the same order for `base_url` and `model`. Its API key
is resolved only from the explicit `api_key` argument and
`GENFLOW_STT_API_KEY`; it is never read from the JSON file.

`local-native` uses the same explicit argument, environment, saved JSON,
fallback precedence for its engine, executable, model, model backend, and
native device. Depending on the selected engine, `model` can be a local path,
`auto`, or a supported `hf://` reference. These selectors are interpreted by
the native engine; genflow does not translate an arbitrary Hub repository into
a compatible native format.

Python executable discovery adds a final fallback to `python3`, then `python`,
on `PATH`. `trust_remote_code`, `chunk_length_s`, `return_timestamps`,
`max_new_tokens`, `response_format`, and `api_key` are call-level controls and
are not written to the local configuration file. Hugging Face `revision` is
config-backed because reproducible model/code selection is a stable local
runtime choice.

For a virtual environment, configure its own `bin/python` path. genflow
deliberately preserves that final symlink instead of canonicalizing it to the
base interpreter, because following the symlink would detach Python from the
virtual environment and its installed packages.

For a native engine, configure an executable file rather than a source or
build directory. A CrispASR source build normally produces
`build/bin/crispasr` (or `build-vulkan/bin/crispasr` when that build directory
name is used). Saved `hf://OWNER/REPO/FILE` selectors are normalized to
`hf://OWNER/REPO:FILE`; invalid selectors remain visible to diagnostics so the
app can explain the problem.

The supported saved fields are:

| JSON field | Environment override | Default or fallback |
| --- | --- | --- |
| `python` | `GENFLOW_PYTHON` | `python3`, then `python` on `PATH` |
| `hf_cache_dir` | `HF_HOME` | Hugging Face default cache |
| `hf_stt_model` | `GENFLOW_HF_STT_MODEL` | `openai/whisper-large-v3-turbo` |
| `hf_revision` | `GENFLOW_HF_REVISION` | empty; use the Hub default revision |
| `hf_stt_profile` | `GENFLOW_HF_STT_PROFILE` | `auto` |
| `device` | `GENFLOW_LOCAL_DEVICE` | `auto` |
| `dtype` | `GENFLOW_LOCAL_DTYPE` | `auto` |
| `stt_server_base_url` | `GENFLOW_STT_BASE_URL` | `http://127.0.0.1:8000` |
| `stt_server_model` | `GENFLOW_STT_MODEL` | empty; runtime fallback `local-model` |
| `stt_native_engine` | `GENFLOW_STT_NATIVE_ENGINE` | `auto`; resolves a supported installed engine |
| `stt_native_executable` | `GENFLOW_STT_NATIVE_EXECUTABLE` | empty; engine executable on `PATH` |
| `stt_native_model` | `GENFLOW_STT_NATIVE_MODEL` | empty; local path, `auto`, or supported `hf://` reference |
| `stt_native_backend` | `GENFLOW_STT_NATIVE_BACKEND` | empty; engine-specific model architecture/backend |
| `stt_native_device` | `GENFLOW_STT_NATIVE_DEVICE` | `auto` |
| `ollama_base_url` | `OLLAMA_BASE_URL` | `http://127.0.0.1:11434` |
| `ollama_model` | `OLLAMA_MODEL` | empty |
| `llamacpp_base_url` | `LLAMACPP_BASE_URL`, then `LLAMA_CPP_BASE_URL` | `http://127.0.0.1:8080` |
| `llamacpp_model` | `LLAMACPP_MODEL`, then `LLAMA_CPP_MODEL` | empty |

URL fields must be HTTP or HTTPS URLs. Trailing slashes are removed. Device
and dtype aliases are normalized when the configuration is saved.

### Reading and updating configuration

Calling `gen_local_config()` without updates returns the normalized saved
configuration:

```r
cfg <- gen_local_config()

cfg[c(
  "python",
  "hf_stt_model",
  "hf_revision",
  "hf_stt_profile",
  "device",
  "dtype",
  "stt_native_engine",
  "stt_native_executable",
  "stt_native_model",
  "stt_native_backend",
  "stt_native_device"
)]
```

Update selected fields with named arguments or a named `config` list:

```r
cfg <- gen_local_config(
  python = "/absolute/path/to/venv/bin/python",
  hf_cache_dir = "/absolute/path/to/huggingface-cache",
  hf_stt_model = "openai/whisper-large-v3-turbo",
  hf_revision = "YOUR_REVIEWED_COMMIT_SHA",
  hf_stt_profile = "auto",
  device = "rocm",
  dtype = "auto"
)
```

Use `save = FALSE` to validate and preview a merged configuration without
writing it:

```r
preview <- gen_local_config(
  device = "cpu",
  dtype = "float32",
  save = FALSE
)
```

Replace `YOUR_REVIEWED_COMMIT_SHA` with a real revision from the selected model
repository. The app groups the same fields into independent **Local inference**
sub-tabs for Ollama, llama.cpp, Hugging Face STT, native STT, and an
OpenAI-compatible STT server. Global save/reload actions preserve all adapters,
while **Check adapter** reports only the selected sub-tab.

## Diagnostics

`gen_local_diagnostics()` performs read-only readiness checks:

- resolves the selected Python executable;
- imports PyTorch and Transformers and reports their versions;
- reports the installed HIP or CUDA PyTorch build,
  `torch.cuda.is_available()`, and the first accelerator name;
- compares the requested device with the installed PyTorch build, so selecting
  ROCm with a CUDA wheel is reported as an explicit error;
- looks for FFmpeg on `PATH`;
- reports the configured Hugging Face cache;
- checks the optional native STT engine, rejects directories/non-executable
  files in the executable field, and validates the backend and model selector;
- recognizes an explicitly selected CrispASR model in its canonical cache or
  other well-known search locations, respecting a `.src` origin sidecar;
- checks the Vulkan loader and reports the visible GPU;
- optionally probes Ollama `/api/tags`, llama.cpp `/v1/models`, and the local
  STT server `/v1/models`.

```r
diagnostics <- gen_local_diagnostics(
  adapters = "hf-local",
  check_endpoints = TRUE,
  timeout = 5
)

print(diagnostics)
subset(diagnostics, status != "ok")
```

Statuses are `ok`, `warning`, `error`, or `info`. A warning for one optional
backend does not disable other providers. Omit `adapters` to check everything,
or pass one or more of `ollama`, `llamacpp`, `hf-local`, `local-native`, and
`local-openai` to avoid unrelated warnings.

When `config` is passed to `gen_local_diagnostics()`, it replaces the saved
configuration as the diagnostic base, but mapped environment variables still
override its fields.

These checks do not download or load a model and do not run a transcription.
A cached remote filename receives `ok`, but only a real `gen_stt()` call proves
that its GGUF architecture is compatible and that VRAM, output, and performance
are adequate.

## Direct Hugging Face inference with `hf-local`

The local bridge supports two profiles.

### `transformers` profile

This is the generic path documented by the
[Hugging Face ASR guide](https://huggingface.co/docs/transformers/tasks/asr).
It creates a Transformers `automatic-speech-recognition` pipeline and can use
compatible Hugging Face ASR checkpoints.

```r
result <- gen_stt(
  "audio.wav",
  service = "hf-local",
  model = "openai/whisper-large-v3-turbo",
  local_profile = "transformers",
  device = "auto",
  dtype = "auto",
  chunk_length_s = 30,
  return_timestamps = "word",
  save_txt = FALSE
)

result$response_value
result$metadata
```

Use `openai/whisper-tiny` for the smallest first-run smoke test. It exercises
the same generic bridge with a much smaller download; switch to
`openai/whisper-large-v3-turbo` after the Python/audio path is known to work.

For generic models, `trust_remote_code = NULL` means false. Language and prompt
hints are automatically forwarded only when the loaded pipeline exposes the
corresponding Whisper behavior; otherwise the result metadata includes a
warning.

### Pinning the Hub revision

`revision` is forwarded to the generic Transformers pipeline and to both MOSS
loaders (`AutoModelForCausalLM` and `AutoProcessor`). Its precedence is:

1. `revision` passed to `gen_stt()`;
2. `GENFLOW_HF_REVISION`;
3. saved `hf_revision`;
4. no pin, which lets Hugging Face use the repository's default revision.

Branches and tags can move. Use a full reviewed commit SHA when immutable model
weights and remote repository code matter:

```r
result <- gen_stt(
  "audio.wav",
  service = "hf-local",
  model = "owner/model",
  revision = "YOUR_REVIEWED_COMMIT_SHA",
  save_txt = FALSE
)
```

Replace the placeholder before running the call. Use `revision = ""` to
explicitly ignore an environment or saved pin for one call. A saved pin belongs
to one model repository: when overriding `model`, also provide its matching
`revision`, clear the pin, or set `revision = ""`. Local filesystem model paths
should normally use `revision = ""`.

### `moss` profile

`local_profile = "auto"` selects `moss` when the model id ends in
`MOSS-Transcribe-Diarize`; other models use `transformers`. The explicit
profile names are `auto`, `transformers`, and `moss`.

The dedicated integration follows the official
[MOSS model card](https://huggingface.co/OpenMOSS-Team/MOSS-Transcribe-Diarize)
and uses helpers from the
[MOSS source repository](https://github.com/OpenMOSS/MOSS-Transcribe-Diarize).
These are separate artifacts: `trust_remote_code = TRUE` loads the custom model
classes from Hugging Face, while `parse_transcript` and
`inference_utils` come from the GitHub Python project. The model repository
does not install those helpers. The official project currently documents a
GitHub checkout rather than a PyPI release.

The Python environment needs all of the following:

- a compatible Python installation;
- a PyTorch build for the intended CPU, CUDA, or ROCm device;
- Transformers `>=5.6.0,<6.0.0` for the pinned helper revision;
- the official `moss_transcribe_diarize` Python package from the MOSS
  repository;
- an audio decoder supported by that environment; FFmpeg is strongly
  recommended and is required for genflow's format-conversion path.

genflow pins the helper source revision used by its adapter. Install it with
the exact Python configured in genflow:

```bash
/absolute/path/to/python -m pip install \
  "moss-transcribe-diarize @ https://github.com/OpenMOSS/MOSS-Transcribe-Diarize/archive/9990574e6ac62390a21bcce25a914d66ac92c25e.zip"
```

This installs normal Python dependencies but deliberately does not select a
PyTorch accelerator wheel. Install PyTorch for the actual platform first. The
command can upgrade Transformers; if another application owns the environment
and pins an incompatible version, use a separate environment or use genflow's
native STT route instead. For AMD, follow AMD's current compatibility and
installation documentation rather than using a CUDA wheel.

After installing the environment, configure and inspect it:

```r
gen_local_config(
  python = "/absolute/path/to/moss-venv/bin/python",
  hf_stt_model = "OpenMOSS-Team/MOSS-Transcribe-Diarize",
  hf_revision = "YOUR_REVIEWED_COMMIT_SHA",
  hf_stt_profile = "moss",
  device = "cpu",
  dtype = "auto"
)

gen_local_diagnostics(
  check_endpoints = FALSE,
  timeout = 10
)
```

Use `device = "rocm"` only after installing a ROCm-enabled PyTorch build that
supports the host distribution and GPU.

Then transcribe:

```r
result <- gen_stt(
  "meeting.wav",
  service = "hf-local",
  model = "OpenMOSS-Team/MOSS-Transcribe-Diarize",
  local_profile = "moss",
  trust_remote_code = TRUE,
  max_new_tokens = 8192,
  timeout_api = 1800,
  save_txt = TRUE
)

result$response_value
result$metadata$segments
```

MOSS performs language detection and long-form handling internally, so
`language` and `chunk_length_s` are not used by this profile. Its transcript
already contains segment timestamps and anonymous speaker labels. Word-level
timestamps are not available through `return_timestamps = "word"`. Increase
`max_new_tokens` for long multi-speaker recordings; the bridge default is
2048.

## Native inference with `local-native`

This route is for machines where an external C/C++ runtime is preferable to a
large Python environment. The canonical service id is `local-native`.
Transport/runtime selection is intentionally separate from model selection:

- `native_engine` chooses the CLI contract: `crispasr`,
  `moss-transcribe`, or `auto`;
- `native_backend` identifies an engine-specific model architecture/backend;
- `model` chooses a local model path, `auto`, or an engine-supported `hf://`
  reference;
- `native_device` selects `auto`, `cpu`, `vulkan`, `hip`, `cuda`, or `metal`.

These layers make the route extensible without inventing one service id per
model family. They do not make model formats interchangeable. A native engine
can load only architectures and formats it implements.

### CrispASR: broader multi-architecture engine

[CrispASR](https://github.com/CrispStrobe/CrispASR) provides one native CLI
covering multiple speech-recognition architecture families, including
Whisper, Parakeet, Canary, Voxtral, Granite, and MOSS backends. It auto-detects
supported GGUF models and also accepts an explicit backend. This is genflow's
broad native engine, but both CrispASR and this adapter remain
experimental/beta; pin a reviewed engine revision and test the exact
model/backend combination.

For AMD hardware, compile CrispASR with its Vulkan option:

```sh
git clone https://github.com/CrispStrobe/CrispASR
cd CrispASR
git checkout REVIEWED_TAG_OR_COMMIT
git submodule update --init --recursive --depth 1
cmake -B build-vulkan -DCMAKE_BUILD_TYPE=Release -DGGML_VULKAN=ON
cmake --build build-vulkan -j
```

Replace `REVIEWED_TAG_OR_COMMIT` with the upstream tag or full commit SHA you
reviewed. Omitting that checkout follows the changing default branch. For an
existing clone, update all submodules after checking out the intended
revision. Current default builds need both `ggml` and
`third_party/c2pa-audio`; the latter supplies the embedded native C2PA signer,
independently of the optional external c2pa-rs signer:

```sh
git submodule update --init --recursive --depth 1
```

For an STT-only build that deliberately omits the embedded native C2PA signer,
configure with `-DCRISPASR_NO_C2PA_NATIVE=ON`; review the resulting provenance
trade-off before distributing outputs.

The Vulkan runtime/driver alone is not enough to compile ggml. CMake also
needs a C++ toolchain, the Vulkan development headers, and a shader compiler.
On a clean Arch Linux installation for an AMD GPU, for example:

```sh
sudo pacman -S --needed \
  base-devel cmake git curl \
  vulkan-icd-loader vulkan-radeon vulkan-tools \
  vulkan-headers spirv-headers shaderc glslang
```

Common configure/build failures have direct causes:

- `ggml` has no `CMakeLists.txt`: initialize the recursive submodules;
- `third_party/c2pa-audio/src/c2pa_native.cpp` is missing: initialize the
  `third_party/c2pa-audio` submodule as well;
- `spirv/unified1/spirv.hpp` is missing: install `spirv-headers`.

Before testing genflow, verify that the Vulkan loader sees the intended GPU:

```sh
vulkaninfo --summary
```

Then configure its executable and a model it supports:

```r
gen_local_config(
  stt_native_engine = "crispasr",
  stt_native_executable =
    "/absolute/path/to/CrispASR/build-vulkan/bin/crispasr",
  stt_native_model = "/absolute/path/to/whisper-model.gguf",
  stt_native_backend = "whisper",
  stt_native_device = "vulkan"
)

gen_local_diagnostics(check_endpoints = FALSE)

result <- gen_stt(
  "meeting.wav",
  service = "local-native",
  timeout_api = 1800,
  save_txt = FALSE
)

result$response_value
result$metadata$segments
```

### STT result and console status

`gen_stt()` follows the same output contract as the other generators: the call
writes a concise status, file, and transcript preview to the console, then
invisibly returns a regular list:

```r
result
result$response_value
result$metadata$segments
```

Evaluating `result` later uses R's ordinary list display, consistently with
`gen_txt()`, `gen_img()`, and `gen_tts()`. For CrispASR, genflow removes token
entries only when they contain no text and no valid non-negative timing
information; useful word, token, segment, speaker, and timestamp data remain
available. Native runtime metadata distinguishes the requested backend from
CrispASR's internal runtime backend and records accelerator selection as
`confirmed`, `fallback`, or `unknown`. A requested Vulkan backend that falls
back produces a warning while preserving the successful transcription returned
by CrispASR's automatic fallback selection.

`stt_native_model = "auto"` delegates model selection/download to the engine
and should be paired with a supported `stt_native_backend`. An `hf://`
selector is passed to the engine only when that engine supports it. Neither
form means that an arbitrary Hugging Face repository is compatible.

```r
remote_result <- gen_stt(
  "meeting.wav",
  service = "local-native",
  native_engine = "crispasr",
  native_backend = "parakeet",
  model = paste0(
    "hf://cstr/parakeet-tdt-0.6b-v3-GGUF:",
    "parakeet-tdt-0.6b-v3-q4_k.gguf"
  ),
  native_device = "vulkan",
  save_txt = FALSE
)
```

The `hf://` prefix is genflow's explicit opt-in to CrispASR's `--hf-repo`
download path. A filename is required. The canonical form is
`hf://OWNER/REPO:FILE`; genflow also accepts the copy-and-paste-friendly
`hf://OWNER/REPO/FILE` form and normalizes it before invoking CrispASR. A
normal `OWNER/REPO` string is never silently treated as a native model
download.

CrispASR downloads into its canonical cache selected in this order:
`CRISPASR_CACHE_DIR`, `CRISPASR_MODELS_DIR`, then `~/.cache/crispasr`. It can
also reuse a same-basename file from its other well-known search locations.
genflow mirrors that read-only search and honors CrispASR's `.src` origin
sidecar when present; diagnostics report a matching non-empty file as `ok`.
They do not parse all tensors or load the model.

The native CrispASR cache is independent of genflow's Python
`hf_cache_dir`/`HF_HOME` setting. Point `CRISPASR_CACHE_DIR` or
`CRISPASR_MODELS_DIR` at an existing model disk to avoid another native copy.
Private or gated repositories require `HF_TOKEN` or
`HUGGING_FACE_HUB_TOKEN` in the CrispASR process environment.

CrispASR currently resolves an `hf://` reference through the repository's
mutable `main` revision. For an auditable model deployment, fetch the exact
reviewed model revision separately, verify it, and configure
`stt_native_model` with the resulting local file path. Pinning the CrispASR
source revision and pinning the model artifact are separate decisions.

#### Granite 4.1 packaging example

Two GGUF repositories derived from the same base checkpoint are not
necessarily interchangeable. The IBM llama.cpp
[Granite Speech repository](https://huggingface.co/ibm-granite/granite-speech-4.1-2b-GGUF/tree/main)
publishes the language-model GGUF and `mmproj-model-f16.gguf` separately.
CrispASR's Granite runtime expects its own monolithic `granite_speech`
conversion containing the speech encoder, projector, and language model.

Use the CrispASR conversion directly:

```r
gen_local_config(
  stt_native_engine = "crispasr",
  stt_native_executable =
    "/absolute/path/to/CrispASR/build-vulkan/bin/crispasr",
  stt_native_model = paste0(
    "hf://cstr/granite-speech-4.1-2b-GGUF/",
    "granite-speech-4.1-2b-q4_k.gguf"
  ),
  stt_native_backend = "granite-4.1",
  stt_native_device = "vulkan"
)

gen_local_diagnostics(
  adapters = "local-native",
  check_endpoints = FALSE
)

result <- gen_stt(
  "speech.wav",
  service = "local-native",
  timeout_api = 1800,
  save_txt = FALSE
)

stopifnot(
  identical(result$status_api, "SUCCESS"),
  is.character(result$response_value),
  length(result$response_value) == 1L,
  nzchar(result$response_value),
  identical(result$metadata$backend, "granite-4.1"),
  nzchar(result$metadata$resolved_model)
)
result$metadata[c("backend", "resolved_model", "native_device")]
```

The `native_device` metadata records the requested selector; it is not proof
of GPU offload. To verify the actual backend, run the same binary and model
once with CrispASR's verbose output and confirm the selected Vulkan device:

```sh
/absolute/path/to/CrispASR/build-vulkan/bin/crispasr \
  -m /absolute/path/to/model.gguf \
  --backend whisper \
  -f speech.wav \
  --gpu-backend vulkan \
  -v
```

The equivalent `model = "auto"` configuration is valid when paired with
`stt_native_backend = "granite-4.1"`; it delegates the exact default artifact
to CrispASR's model registry.

### `moss-transcribe`: MOSS-specific engine

The community
[moss-transcribe.cpp](https://github.com/localai-org/moss-transcribe.cpp)
project provides a native CLI specifically for the MOSS architecture, accepts
a compatible GGUF, and can be compiled with Vulkan. It is not a generic
Transformers or generic Hugging Face loader.

```sh
git clone --recursive https://github.com/localai-org/moss-transcribe.cpp
cd moss-transcribe.cpp
cmake -B build-vulkan -DMT_GGML_VULKAN=ON
cmake --build build-vulkan -j
```

Use one of the project's
[published GGUF files](https://huggingface.co/mudler/moss-transcribe.cpp-gguf)
or follow its conversion instructions. The original Transformers checkpoint
is not itself a GGUF and cannot be passed directly to this engine.

```r
gen_local_config(
  stt_native_engine = "moss-transcribe",
  stt_native_executable =
    "/absolute/path/to/moss-transcribe.cpp/build-vulkan/moss-transcribe",
  stt_native_model = "/absolute/path/to/moss-transcribe-q5_k.gguf",
  stt_native_backend = "moss-diarize",
  stt_native_device = "vulkan"
)

result <- gen_stt(
  "meeting.wav",
  service = "local-native",
  timeout_api = 1800,
  save_txt = FALSE
)
```

The pre-release `service = "moss-cpp"` name is accepted only as a compatibility
alias for `service = "local-native"` with
`native_engine = "moss-transcribe"`. New code and documentation should use the
canonical service.

`native_device = "vulkan"` overrides the saved native device for one call.
Vulkan capability comes from how the external engine was compiled; selecting
it in genflow cannot add Vulkan support to a CPU-only binary. A real
transcription with the selected model is the end-to-end validation.

## `trust_remote_code` security

Hugging Face `trust_remote_code = TRUE` allows Python from a model repository
to execute in the selected environment. Treat it as code execution, not as a
data-only model download.

- Generic `transformers` inference defaults to false.
- The known MOSS profile defaults to true because the official model requires
  its custom Transformers implementation.
- Explicitly setting `trust_remote_code = FALSE` with the MOSS profile stops
  before Python is launched.
- Review the model repository, use an isolated environment, minimize secrets
  available to that process, and control dependency/model updates.

Passing `trust_remote_code = TRUE` explicitly in production code makes this
security decision visible to reviewers. Pair it with a full commit SHA through
`revision`/`hf_revision`; a branch or tag is not an immutable security pin.

The Hugging Face revision pins files loaded from the model repository. It does
not pin the separately installed `moss_transcribe_diarize` Python package.
genflow's install hint uses a separate reviewed GitHub commit for that helper;
both pins must be updated and reviewed independently.

## OpenAI-compatible local STT with `local-openai`

Use this route when a separately managed server implements the multipart
OpenAI transcription contract. The MOSS model card documents compatible
SGLang Omni and vLLM serving options, but genflow is server-agnostic.

The configured base URL may be the server root, `/v1`, or the complete
`/v1/audio/transcriptions` URL. genflow normalizes all three forms.

```r
gen_local_config(
  stt_server_base_url = "http://127.0.0.1:8000",
  stt_server_model = "OpenMOSS-Team/MOSS-Transcribe-Diarize"
)

result <- gen_stt(
  "meeting.wav",
  service = "local-openai",
  response_format = "verbose_json",
  max_new_tokens = 8192,
  timeout_api = 1800,
  save_txt = FALSE
)

result$response_value
result$metadata$segments
```

For an authenticated local or remote server, keep the token outside the JSON
configuration:

```r
Sys.setenv(GENFLOW_STT_API_KEY = "replace-with-local-server-token")

result <- gen_stt(
  "audio.wav",
  service = "local-openai",
  base_url = "https://stt.internal.example/v1",
  model = "server-model-id",
  response_format = "json",
  save_txt = FALSE
)
```

No authorization header is sent when both `api_key` and
`GENFLOW_STT_API_KEY` are empty. The multipart request can contain `file`,
`model`, `response_format`, `language`, `prompt`, and `max_new_tokens`.

## Device and dtype selection

The following `device` values apply only to `hf-local` and PyTorch:

- `auto`: accelerator first, then Apple MPS, then CPU;
- `cpu`;
- `cuda` or `cuda:N`;
- `rocm` or `hip`, which are genflow aliases for the first PyTorch HIP device;
- `mps`.

Accepted dtypes are `auto`, `float32`, `float16`, and `bfloat16`. The aliases
`fp32`, `fp16`, and `bf16` are normalized.

With `dtype = "auto"`:

- CPU uses `float32`;
- generic accelerator inference uses `float16`;
- MOSS prefers `bfloat16` when the accelerator reports support and otherwise
  falls back to `float16` with a warning.

An explicit unsupported `bfloat16` request fails rather than silently changing
precision. Start with `auto`; use `float32` for conservative CPU diagnosis.

For `local-native`, use the independent `native_device` argument or
`stt_native_device` setting. Accepted values are `auto`, `cpu`, `vulkan`,
`hip`, `cuda`, and `metal`. An accepted value is still subject to the selected
engine and how its binary was compiled. For CrispASR on AMD, use a
Vulkan-enabled build with `native_device = "vulkan"`; CrispASR does not expose
HIP as a runtime selector.

## AMD and ROCm diagnosis

PyTorch intentionally reuses the CUDA-facing API for HIP/ROCm. The official
[PyTorch HIP semantics](https://docs.pytorch.org/docs/stable/notes/hip.html)
specify `torch.device("cuda")`, `torch.cuda.is_available()`, and
`torch.cuda.get_device_name()` for AMD devices too.

Accordingly, genflow accepts `device = "rocm"` and `device = "hip"` as
user-facing aliases, maps them to `cuda:0`, and additionally requires
`torch.version.hip` to be present. Seeing `device = "cuda:0"` in result
metadata is therefore expected on ROCm; `metadata$accelerator` distinguishes
`rocm` from NVIDIA `cuda`.

Inspect the exact Python selected by genflow:

```r
cfg <- gen_local_config()
python <- cfg$python
if (!nzchar(python)) {
  python <- unname(Sys.which("python3"))
}
stopifnot(nzchar(python))

probe <- paste(
  "import torch",
  "print('torch:', torch.__version__)",
  "print('hip:', torch.version.hip)",
  "print('available:', torch.cuda.is_available())",
  paste0(
    "print('device:', torch.cuda.get_device_name(0) ",
    "if torch.cuda.is_available() else None)"
  ),
  sep = "; "
)

system2(
  python,
  c("-c", shQuote(probe)),
  stdout = TRUE,
  stderr = TRUE
)
```

For a working ROCm PyTorch environment, `torch.version.hip` should be non-empty
and `torch.cuda.is_available()` should be true. If HIP is empty, the selected
environment probably contains a CPU or CUDA PyTorch build. If availability is
false, check the driver, ROCm installation, supported OS/GPU combination,
permissions for the GPU devices, and the selected Python environment.

Always verify the current
[ROCm Linux system requirements](https://rocm.docs.amd.com/projects/install-on-linux/en/develop/reference/system-requirements.html)
before installation, then follow AMD's current
[PyTorch for ROCm installation guide](https://rocm.docs.amd.com/projects/ai-ecosystem/en/latest/frameworks/pytorch/install.html).
GPU support and operating-system support are separate requirements.

AMD lists the Radeon RX 9070 XT as `gfx1201`, but the operating system must also
appear in the applicable compatibility matrix. Arch Linux is not currently an
officially validated Radeon ROCm distribution. Passing package tests and
dependency diagnostics is not a claim of end-to-end model execution; verify a
real transcription before relying on the environment.

Vulkan is not a drop-in PyTorch device for the official Transformers MOSS
implementation. PyTorch's former desktop Vulkan backend is unmaintained and is
not included in normal wheels. Use a Vulkan-enabled engine through
`local-native`—CrispASR is the broader experimental/beta option—or a compatible
server through `local-openai` when avoiding PyTorch is the goal.
