# Local runtimes

genflow keeps model selection and runtime configuration separate:

- **Models** discovers or inventories models and is where a setup or agent
  chooses its provider and model.
- **Local** configures local runtimes and checks their health.
- **Native STT** inside Local also verifies, downloads, and deletes model files.
  It does not select the model used by an agent.

There is no embedded Python or Transformers runtime. A Python speech service can
still be used through the OpenAI-compatible STT server adapter.

## Runtime map

```text
Models
  |-- ollama.csv ---------> service = "ollama"
  |-- llamacpp.csv -------> service = "llamacpp"
  |-- local-native.csv ---> service = "local-native"
  `-- provider CSVs ------> remote services, including service = "hf"

Local
  |-- Ollama -------------> existing Ollama HTTP server
  |-- llama.cpp ----------> existing llama-server
  |-- Native STT ---------> CrispASR or moss-transcribe.cpp process
  `-- STT server ---------> /v1/audio/transcriptions
```

`service = "hf"` means the remote Hugging Face inference provider. Hugging Face
file links in Native STT are only download sources for compatible native model
artifacts; they do not create another Hugging Face runtime.

## Saved configuration

Read the current configuration:

```r
gen_local_config()
```

Update selected fields:

```r
gen_local_config(
  ollama_base_url = "http://127.0.0.1:11434",
  llamacpp_base_url = "http://127.0.0.1:8080",
  stt_server_base_url = "http://127.0.0.1:8000",
  stt_native_engine = "crispasr",
  stt_native_device = "vulkan"
)
```

The default file is:

```text
tools::R_user_dir("genflow", "config")/local-inference.json
```

Override it with `options(genflow.local_config_path = "...")`,
`GENFLOW_LOCAL_CONFIG`, or the `path` argument.

Tokens and passwords are not written to this file.

### Effective precedence

For a direct runtime call, explicit function arguments win over environment
variables, which win over saved configuration, which wins over defaults.

| Setting | Environment | Default |
|---|---|---|
| `ollama_base_url` | `OLLAMA_BASE_URL` | `http://127.0.0.1:11434` |
| `ollama_model` | `OLLAMA_MODEL` | empty; discover from `/api/tags` |
| `llamacpp_base_url` | `LLAMACPP_BASE_URL`, `LLAMA_CPP_BASE_URL` | `http://127.0.0.1:8080` |
| `llamacpp_model` | `LLAMACPP_MODEL`, `LLAMA_CPP_MODEL` | empty; discover from `/v1/models` |
| `stt_server_base_url` | `GENFLOW_STT_BASE_URL` | `http://127.0.0.1:8000` |
| `stt_server_model` | `GENFLOW_STT_MODEL` | empty; runtime default `local-model` |
| `stt_native_engine` | `GENFLOW_STT_NATIVE_ENGINE` | `auto` |
| `stt_native_executable` | `GENFLOW_STT_NATIVE_EXECUTABLE` | find the engine on `PATH` |
| `stt_native_model` | `GENFLOW_STT_NATIVE_MODEL` | empty |
| `stt_native_backend` | `GENFLOW_STT_NATIVE_BACKEND` | empty |
| `stt_native_quant` | `GENFLOW_STT_NATIVE_QUANT` | empty |
| `stt_native_device` | `GENFLOW_STT_NATIVE_DEVICE` | `auto` |

The model/backend/quant fields remain available to programmatic callers and old
configuration files, but the app does not use them as a model picker. A concrete
model selected in Models is authoritative and does not inherit a stale saved
backend.

## Ollama

Ollama owns its model files, process lifetime, and GPU selection. genflow calls
its HTTP API:

```r
gen_txt("Explain duration risk", service = "ollama", model = "qwen3:8b")
```

Refresh installed models in **Models**, or programmatically:

```r
gen_update_models(provider = "ollama")
```

## llama.cpp

Start `llama-server` separately, then point genflow at its base URL:

```r
gen_local_config(llamacpp_base_url = "http://127.0.0.1:8080")
gen_txt("Summarize this note", service = "llamacpp", model = "local-model")
```

The server owns model loading, Vulkan/CUDA/Metal flags, context size, and GPU
layers. Refresh its `/v1/models` response through **Models**.

## Native STT

Native STT uses a registered command-line engine:

- `crispasr`: multiple supported GGUF speech families and Vulkan acceleration;
- `moss-transcribe`: the MOSS-specific C++ runtime.

Configure only the runtime in **Local > Native STT**:

```r
gen_local_config(
  stt_native_engine = "crispasr",
  stt_native_device = "vulkan"
)
```

For an executable outside `PATH`, use `GENFLOW_STT_NATIVE_EXECUTABLE`, the
`executable` argument, or `gen_local_config(stt_native_executable = "...")`.

### Download manager

Native STT accepts either compact references or normal Hugging Face file links:

```text
hf://OWNER/REPOSITORY:MODEL.gguf
hf://OWNER/REPOSITORY/MODEL.gguf
hf download hf://OWNER/REPOSITORY/MODEL.gguf
https://huggingface.co/OWNER/REPOSITORY/blob/main/MODEL.gguf
```

For command-style input, clicking either **Verify** or **Download** removes the
leading `hf download` text from the field and uses the remaining `hf://`
reference.

**Verify** checks the repository, exact filename, immutable revision, file size,
and LFS SHA-256 metadata without downloading. **Download** repeats validation,
streams into a temporary file, verifies the SHA-256, and atomically publishes
the model in the managed CrispASR cache. The downloaded-model table displays
the validated Hugging Face `OWNER/REPOSITORY` recorded for each artifact.
Manually copied and legacy artifacts without a valid source record display
`—`; genflow does not infer provenance from filenames.

The managed cache is:

1. `CRISPASR_CACHE_DIR`, when set;
2. otherwise `CRISPASR_MODELS_DIR`, when set;
3. otherwise `~/.cache/crispasr`.

Only regular, non-symlink model files directly inside that canonical directory
can be deleted in the app or published to `local-native.csv`.

After a download or deletion, the app synchronizes the Native STT provider in
**Models**. A manual refresh is also available:

```r
gen_update_models(provider = "local-native")
gen_show_models(provider = "local-native", type = "Audio")
```

Catalog model ids are flat filenames, not machine-specific absolute paths.
At runtime genflow resolves those names only inside the managed cache.

### Select and run a model

Choose `Native STT (local)` and a downloaded model in **Models**, then use that
setup in an agent. A direct call uses the same contract:

```r
res <- gen_stt(
  "audio.wav",
  service = "local-native",
  model = "granite-speech-4.1-2b-plus-q4_k.gguf"
)
res$response_value
```

An explicit local path and an explicit `hf://...:FILE` reference are also
accepted. `model = "auto"` is a CrispASR registry selector and requires an
unambiguous `native_backend`; it is not replaced by a hidden app selection.

`gen_stt(diarize = TRUE)` activates CrispASR's native speaker-attributed ASR
mode for Granite Speech 4.1 Plus models. Genflow detects the standard Plus
artifact name or its recorded Hugging Face source; use
`native_backend = "granite-4.1-plus"` when a Plus artifact has been renamed or
is selected through the registry. Other model families do not receive this
model-specific `--diarize` switch automatically. In this mode genflow also
disables CrispASR's generic 30-second chunking so speaker numbering spans the
complete input passed to `gen_stt()`; orchestration clients should split long
recordings into model-supported windows before calling it.

For models without native speaker labels, set `diarize_speakers = TRUE` to use
CrispASR's generic Pyannote speaker segmentation. Its default
`diarize_embedder = TRUE` also runs TitaNet clustering, which makes anonymous
speaker IDs stable across the supplied recording but adds a CPU-heavy pass.
Set `diarize_embedder = FALSE` to omit TitaNet:

```r
res <- gen_stt(
  "meeting.wav",
  service = "local-native",
  model = "cohere-transcribe-q8_0.gguf",
  diarize = TRUE,
  diarize_speakers = TRUE,
  diarize_embedder = FALSE
)
```

This Pyannote-only mode is faster, but its best-effort speaker numbers can swap
within a long recording because no embedding step globally clusters voices.

For the CrispASR `moss-diarize` backend, `max_new_tokens = NULL` is
duration-aware: genflow forwards an output budget between 2,048 and 65,536
tokens instead of leaving CrispASR at its truncation-prone 1,024-token default.
An explicit `max_new_tokens` value is always preserved. The budget does not
increase MOSS's audio context and genflow does not split implicitly; recordings
longer than the documented 90-minute single-window limit are attempted with a
warning. Callers that need reliable processing beyond that limit should split
the input into model-supported windows.

### AMD and Vulkan

For an AMD GPU, use a CrispASR build compiled with Vulkan and select
`stt_native_device = "vulkan"`. `hip` is accepted by the generic native engine
contract, but CrispASR itself does not expose HIP as a runtime selector.

The diagnostics report the actual Vulkan device when `vulkaninfo` is available.
A successful real transcription remains the definitive compatibility test.

## OpenAI-compatible STT server

Any service exposing `POST /v1/audio/transcriptions` can be used, regardless of
whether its implementation is Python, C++, Rust, or something else:

```r
gen_local_config(
  stt_server_base_url = "http://127.0.0.1:8000"
)

res <- gen_stt(
  "audio.wav",
  service = "local-openai",
  model = "whisper-large-v3-turbo"
)
```

If authentication is needed, set `GENFLOW_STT_API_KEY`. The app stores only the
server URL; model selection belongs to Models/setups/agents or the direct call.

## Diagnostics

Run every local check:

```r
gen_local_diagnostics()
```

Or target one adapter:

```r
gen_local_diagnostics(adapters = "ollama")
gen_local_diagnostics(adapters = "llamacpp")
gen_local_diagnostics(adapters = "local-native")
gen_local_diagnostics(adapters = "local-openai")
```

Native diagnostics check FFmpeg, the selected CLI, the managed model cache, and
Vulkan. HTTP adapters probe their configured discovery endpoint. Diagnostics
are read-only and do not load a speech model.

## Process lifetime

Ollama, llama-server, and an STT server own their own process and model lifetime.
genflow does not restart or unload them.

A direct native CLI call is process-based: the executable loads the selected
model for that invocation and exits. Persistent native model residency requires
a server runtime; use the STT server adapter for that deployment model.
