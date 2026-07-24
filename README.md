# genflow

[![Lifecycle: Experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R](https://img.shields.io/badge/R-%E2%89%A54.1-blue)](https://www.r-project.org/)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

genflow provides one R interface for cloud and local generative-AI workflows.
It normalizes provider responses, persists reusable agents, runs resumable
batches, maintains model catalogs, and includes a Shiny/RStudio management app.

Current runtime surfaces:

- `gen_txt()`: cloud providers plus local Ollama and llama.cpp servers.
- `gen_img()`: OpenAI, Hugging Face Inference Providers, Replicate, and FAL.
- `gen_stt()`: cloud transcription, local Hugging Face/Transformers models,
  native STT engines, and OpenAI-compatible local STT servers.
- `gen_tts()`: OpenAI and Replicate.

The package is experimental. Provider APIs and individual model schemas can
change independently of genflow, so production workflows should pin models,
use focused smoke tests, and inspect structured `status_api`/`status_msg`
results.

## Installation

```r
# install.packages("pak")
pak::pak("hugorteixeira/genflow")
```

## First call

Put cloud credentials in environment variables, not in scripts:

```text
OPENAI_API_KEY=...
GOOGLE_API_KEY=...       # or GEMINI_API_KEY
HUGGINGFACE_API_TOKEN=...
REPLICATE_API_TOKEN=...
FAL_KEY=...
```

The app can manage supported credentials in the user `.Renviron`:

```r
library(genflow)
gen_interface()
```

Use **Models > Credentials** to add, import, or remove credentials. Writes are
locked, backed up, privately permissioned on Unix-like systems, and atomically
replaced. Secrets are not stored in agents, model catalogs, local-inference
configuration, or exported bundles. See
[the credential and catalog workflow](inst/doc/credential-and-model-catalog-workflow.md).

Generate text:

```r
result <- gen_txt(
  context = "Explain why reproducible backtests need point-in-time data.",
  service = "openai",
  model = "gpt-5-mini",
  reasoning = "medium"
)

result$status_api
result$response_value
```

High-level generators return a structured list containing at least:

- `response_value`
- `service`
- `model`
- `duration`
- `status_api` (`"SUCCESS"` or `"ERROR"`)
- `status_msg`

Text results also include token estimates. Media results include a saved path
on success.

Generated files default to subdirectories of `~/.genflow`. Override the root
without changing every call:

```r
options(genflow.output_dir = "/absolute/path/to/genflow-output")
```

`GENFLOW_OUTPUT_DIR` provides the equivalent environment override.

See [Architecture and runtime contract](inst/doc/architecture-and-runtime-contract.md)
for the active provider matrix, persistence boundaries, and provider-extension
checklist.

## Model catalogs

Refresh one or more catalogs explicitly:

```r
gen_update_models(
  provider = c("openai", "hf", "hf-local"),
  fail_on_error = TRUE
)

gen_show_models(provider = "hf", type = "Chat")
gen_show_models(provider = "hf-local", type = "Audio")
```

The two Hugging Face catalogs have intentionally different contracts:

- `hf.csv` contains models with a live Hugging Face Inference Provider mapping.
- `hf-local.csv` contains Hub models suitable for local discovery, including
  ASR and audio-to-text candidates that are not hosted by an Inference
  Provider.

This prevents the UI from presenting a local-only model as if `service = "hf"`
could call it remotely. The model
`OpenMOSS-Team/MOSS-Transcribe-Diarize`, for example, belongs in the local
catalog.

Gemini model discovery and text generation accept `GOOGLE_API_KEY` first and
`GEMINI_API_KEY` as its compatibility alias. The catalog follows all model-list
pages; the runtime uses the same model ids through `gen_txt(service =
"gemini")`.

Catalog refreshes are provider-specific network operations. With
`fail_on_error = TRUE`, genflow attempts the requested updates and then reports
every failure instead of presenting a false success.

## Local text inference

Ollama:

```r
ollama_result <- gen_txt(
  "Summarize this release note in five bullets.",
  service = "ollama",
  model = "llama3.2"
)
```

llama.cpp's OpenAI-compatible server:

```r
llamacpp_result <- gen_txt(
  "Draft a concise incident report.",
  service = "llamacpp",
  model = "local-model"
)
```

Default endpoints are `http://127.0.0.1:11434` for Ollama and
`http://127.0.0.1:8080` for llama.cpp. Configure them in **Local inference**,
with `gen_local_config()`, or through `OLLAMA_*` and
`LLAMACPP_*` environment variables.

genflow talks to these servers; it does not install or supervise them.
The app keeps Ollama, llama.cpp, Hugging Face STT, native STT, and compatible
STT servers in separate sub-tabs. Saving applies the complete configuration;
**Check adapter** diagnoses only the currently selected adapter.

## Local speech-to-text

`service = "hf-local"` starts an isolated Python subprocess for each
transcription. Its default is the generic Transformers ASR pipeline with
`openai/whisper-large-v3-turbo`; `openai/whisper-tiny` is a smaller first-run
smoke test. A dedicated `OpenMOSS-Team/MOSS-Transcribe-Diarize` adapter remains
available as an advanced profile with additional dependencies.

```r
gen_local_config(
  python = "/absolute/path/to/stt-venv/bin/python",
  hf_cache_dir = "/absolute/path/to/huggingface-cache",
  hf_stt_model = "openai/whisper-large-v3-turbo",
  hf_stt_profile = "transformers",
  device = "cpu",
  dtype = "auto"
)

gen_local_diagnostics(
  adapters = "hf-local",
  check_endpoints = FALSE
)

transcript <- gen_stt(
  "meeting.wav",
  service = "hf-local",
  timeout_api = 1800
)

transcript$response_value
transcript$metadata
```

To avoid Python, `service = "local-native"` invokes an external native STT
engine. The experimental CrispASR integration is the broader option: it
supports multiple ASR architectures and chooses a model-specific backend.
`moss-transcribe` remains available as a MOSS-specific engine. A Vulkan build
is independent of the PyTorch accelerator setting. Configure the compiled
file under the CMake build directory, not the CrispASR source directory:

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

native_transcript <- gen_stt(
  "meeting.wav",
  service = "local-native",
  timeout_api = 1800
)

native_transcript$response_value
native_transcript$metadata
```

Like the other generators, `gen_stt()` writes a concise `[SUCCESS]` or
`[ERROR]` summary while the call runs and invisibly returns a regular list.
Printing `native_transcript` therefore uses R's normal list representation;
the transcript and complete structured provider/runtime data remain available
through `response_value` and `metadata`.

The native model selector may be a local model path, `auto`, or a supported
`hf://OWNER/REPO:FILE` reference. The copy-and-paste form
`hf://OWNER/REPO/FILE` is also accepted and normalized; a filename is
required. This is not universal Hugging Face compatibility: the selected
engine must implement the architecture and the exact model packaging.
In the app, the Native STT panel inventories the CrispASR cache, lets you use
or remove a downloaded model, and can download an exact `hf://` selection
in a cancellable background process with byte progress. Before downloading,
genflow verifies that the named file is actually published by that repository;
it never invents a quantized filename or silently substitutes another
quantization. The transfer uses the repository's immutable revision and
verifies the Hugging Face LFS SHA-256 before installing the file.

For `service = "local-native"`, an omitted model or `model = "auto"` uses the
model selected in the saved local configuration. Select `auto` in that
configuration to delegate to CrispASR's registry, together with a model
architecture and optional preferred quantization.

For example, the IBM llama.cpp
[Granite Speech GGUF repository](https://huggingface.co/ibm-granite/granite-speech-4.1-2b-GGUF/tree/main)
uses a model GGUF plus a separate `mmproj` file. CrispASR instead needs its
[single-file Granite Speech conversion](https://huggingface.co/cstr/granite-speech-4.1-2b-GGUF).
Models downloaded by CrispASR normally live under `~/.cache/crispasr`, or the
directory selected by `CRISPASR_CACHE_DIR`/`CRISPASR_MODELS_DIR`;
`gen_local_diagnostics()` reports when the configured filename is already
cached and refuses to reuse an entry whose CrispASR `.src` sidecar identifies
a different artifact. This native
cache is independent of the Python `hf_cache_dir`/`HF_HOME` setting. An
`hf://` reference is resolved against the repository's current metadata, then
the transfer is pinned to that exact commit and checked against its LFS
SHA-256. For a permanently user-pinned revision, download the reviewed
artifact yourself and configure its local file path.

CrispASR is currently experimental/beta despite its broader model coverage.
The pre-release `moss-cpp` service name is retained only as a compatibility
alias for `local-native` with `native_engine = "moss-transcribe"`.

For any separately managed server implementing the OpenAI multipart
transcription contract:

```r
server_result <- gen_stt(
  "meeting.wav",
  service = "local-openai",
  base_url = "http://127.0.0.1:8000",
  model = "local-model",
  response_format = "verbose_json"
)
```

Local Python inference is optional. It requires a compatible Python
environment, PyTorch, Transformers, and usually FFmpeg. Whisper uses the
generic profile without remote repository code. MOSS additionally requires
helper code from its official GitHub project plus the custom code in its
Hugging Face model repository. The two artifacts are separate.

The pinned MOSS helper currently requires Transformers `>=5.6.0,<6.0.0`.
Do not upgrade a shared environment in place when another application pins an
incompatible version. `revision` passed to `gen_stt()` pins Hugging Face model
files only; it does not pin the separately installed helper.

For AMD, genflow accepts `device = "rocm"`/`"hip"` and maps it to PyTorch's
`cuda` device API, which is also how PyTorch represents HIP devices. Hardware,
OS, PyTorch wheel, and ROCm compatibility still need to match. Diagnostics
reject a CUDA wheel when ROCm is selected, but a real transcription remains
the end-to-end readiness check. Selecting ROCm does not convert a CUDA PyTorch
wheel. Native Vulkan inference uses `local-native` or a compatible server
through `local-openai` instead.

See [Local inference](inst/doc/local-inference.md) for configuration precedence,
dependencies, native engine/model compatibility, MOSS behavior, Vulkan, server
mode, security, and AMD diagnostics.

## Images, STT, and TTS

Image generation:

```r
image <- gen_img(
  prompt = "A clean isometric diagram of a reproducible data pipeline",
  service = "openai",
  model = "gpt-image-2",
  h = 1024,
  y = 1024
)
```

Cloud transcription:

```r
stt <- gen_stt(
  "audio.ogg",
  service = "openai",
  model = "whisper-1"
)
```

Speech synthesis:

```r
tts <- gen_tts(
  "The validation run completed successfully.",
  service = "openai",
  model = "gpt-4o-mini-tts",
  voice = "alloy"
)
```

Replicate accepts model-specific image input through `replicate_input` and an
optional `model_version`. Asynchronous Replicate and FAL calls have bounded
polling through `poll_interval` and `max_poll_seconds`.

## Reusable setups, content, and agents

```r
set_setup(
  sname = "reviewer",
  service = "openai",
  model = "gpt-5-mini",
  reasoning = "medium",
  type = "Chat"
)

set_content(
  cname = "release_notes",
  context = "Review the supplied release notes for ambiguity and missing risks."
)

agent <- set_agent(
  name = "release_reviewer",
  setup = "reviewer",
  content = "release_notes"
)

agent |> gen_txt()
agent |> gen_txt(context_override = "Review this replacement text.")
```

Agents can also drive other modalities. A saved content `context` becomes the
default prompt/text for image and TTS agents:

```r
agent |> gen_img(prompt_override = "A minimal release dashboard")
agent |> gen_tts(text_override = "The release is ready.")
```

Use `audio_override` for one-off STT input. Unknown overrides now raise an
error instead of being silently ignored.

Setups, content, and agents are stored below
`tools::R_user_dir("genflow", "cache")`. Filenames include a content hash to
avoid collisions caused by punctuation, case, or truncation. Renaming a setup
or content entry updates referencing agents; deleting a referenced entry is
blocked unless `force = TRUE`.

```r
list_setups()
list_content()
list_agents()
gen_list()
```

## Batch execution and checkpoints

```r
items <- list(
  list(topic = "catalog refresh"),
  list(topic = "credential storage"),
  list(topic = "local STT")
)

results <- agent |> gen_batch_agent(
  qty = length(items),
  one_item_each = items,
  workers = 2,
  persist = FALSE,
  checkpoint_each = file.path(
    "checkpoints",
    sprintf("task-%02d.rds", seq_along(items))
  )
)
```

`qty` is task count; `workers` is the concurrency limit. PSOCK is the default
backend for provider calls on every OS. Unix-like systems can explicitly choose
`backend = "fork"` for fork-safe work; interrupted fork batches force cleanup
of blocked child processes. Completed per-task checkpoints remain recoverable;
genflow does not automatically trust and resume arbitrary checkpoint files.

Pass one `genflow_agent` directly to `gen_batch_agent()`. It is serialized to
workers without creating temporary per-task objects in `.GlobalEnv`.

## App and viewer

```r
gen_interface()
gen_view(result)
```

The app manages setups, content, agents, credentials, model catalogs, custom
OpenAI-compatible providers, and local-inference settings. It binds to
`127.0.0.1` by default. A non-loopback host requires
`allow_remote = TRUE` and emits a warning because the app has no authentication
layer.

`gen_view()` renders text and media in the RStudio Viewer and falls back to the
console. Large media is copied into a bounded Viewer asset history instead of
being embedded as an unbounded Base64 string. Configure:

```r
options(
  genflow.viewer_inline_max_bytes = 1024^2,
  genflow.viewer_history = 10L
)
```

## Persistence and bundles

Daily statistics use an inter-process lock and atomic RDS replacement:

```r
options(genflow.log_dir = "/absolute/path/to/logs")
gen_stats()
gen_stats_rm(Sys.Date() - 30)
```

A corrupt existing log is reported and preserved rather than overwritten.
Generation results remain available if logging fails, with an explicit
warning.

Bundle import validates archive paths, entry counts, expanded sizes, allowed
file types, and serialized schemas before installing anything:

```r
bundle <- gen_export_bundle()
gen_import_bundle(bundle, overwrite = FALSE)
```

## Main functions

| Function | Purpose |
| --- | --- |
| `gen_txt()` | Cloud and local text generation |
| `gen_img()` | Image generation |
| `gen_stt()` | Cloud, local Python, native GGUF, and local-server transcription |
| `gen_tts()` | Speech synthesis |
| `gen_batch()` / `gen_batch_agent()` | Parallel, checkpointable workloads |
| `gen_local_config()` | Read or update non-secret local settings |
| `gen_local_diagnostics()` | Check Python, native backends, Vulkan, FFmpeg, and endpoints |
| `gen_update_models()` / `gen_show_models()` | Refresh and browse catalogs |
| `set_*()` / `get_*()` / `list_*()` | Persist setups, content, and agents |
| `mv_*()` / `rm_*()` | Rename or remove persisted entities |
| `gen_interface()` | Launch the management app |
| `gen_view()` | Render structured results |
| `gen_stats()` / `gen_stats_rm()` | Inspect or remove daily logs |
| `gen_export_bundle()` / `gen_import_bundle()` | Portable validated bundles |
| `gen_vote()` | Extract and rank structured vote markers |

## Validation

The repository test suite uses mocked provider HTTP/subprocess boundaries so it
does not spend API credits or download large models. Release validation should
include:

```r
devtools::document()
devtools::test()
```

and a tarball-based `R CMD check`. Real provider credentials, a running local
server, or a compatible ROCm environment are still required for their
respective end-to-end smoke tests.

## License

GPL (>= 3), as declared in `DESCRIPTION`.
