# Architecture and runtime contract

This document is the maintainer map for the current genflow checkout. It
describes active code, not a future roadmap.

## Runtime entrypoints

| Modality | Public entrypoint | Runtime implementation |
| --- | --- | --- |
| Text | `gen_txt()` | `R/text_functions.R` |
| Image | `gen_img()` | `R/image_functions.R` |
| Speech to text | `gen_stt()` | `R/stt_functions.R` |
| Text to speech | `gen_tts()` | `R/tts_functions.R` |

Each public entrypoint is an S3 generic with a default method and a
`genflow_agent` method. Agent methods are thin adapters: they resolve saved
setup/content, apply explicit overrides, and call the default method. Unknown
overrides are errors.

Provider dispatch belongs to the modality runtime, not to the Shiny app or
model catalog. Adding a provider name to the UI or a CSV is therefore not
sufficient to make it executable.

## Active provider matrix

| Provider/service | Text | Image | STT | TTS | Catalog |
| --- | :---: | :---: | :---: | :---: | :---: |
| OpenAI | yes | yes | yes | yes | yes |
| OpenRouter | yes |  |  |  | yes |
| Anthropic | yes |  |  |  | yes |
| Groq | yes |  | yes |  | yes |
| Cerebras | yes |  |  |  | yes |
| Together | yes |  |  |  | yes |
| SambaNova | yes |  |  |  | yes |
| Nebius | yes |  |  |  | yes |
| DeepSeek | yes |  |  |  | yes |
| Perplexity | yes |  |  |  | yes |
| Fireworks | yes |  |  |  | yes |
| DeepInfra | yes |  |  |  | yes |
| Hyperbolic | yes |  |  |  | yes |
| Gemini | yes |  |  |  | yes |
| Hugging Face API (`hf`) | yes | yes | yes |  | yes |
| Hugging Face local (`hf-local`) |  |  | yes |  | yes |
| FAL |  | yes |  |  | yes |
| Replicate |  | yes | yes | yes | yes |
| Ollama | yes |  |  |  | yes |
| llama.cpp | yes |  |  |  | yes |
| AssemblyAI |  |  | yes |  |  |
| Cloudflare |  |  | yes |  |  |
| Voicegain |  |  | yes |  |  |
| Local OpenAI-compatible STT (`local-openai`) |  |  | yes |  |  |
| Native STT engine (`local-native`) |  |  | yes |  |  |

Custom providers created with `set_provider_openai_compat()` extend the text
runtime and model catalog through an OpenAI-compatible contract.

Blank cells are deliberate. A catalog can serve more than one modality, but
the app must filter models by `type` before presenting them to an agent.

## Structured return contract

High-level generation functions return a list containing at least:

- `response_value`
- `service`
- `model`
- `duration`
- `status_api`, either `"SUCCESS"` or `"ERROR"`
- `status_msg`

Text adds token estimates and persistence labels. Media generators add a saved
file path or transcript metadata where applicable. Batch code treats
`status_api` as the authoritative success signal and preserves the provider
result instead of inferring success from console output.

Provider adapters may internally return:

- plain generated text;
- a structured tool/function-call payload;
- a provider result that the high-level method normalizes;
- a recognized error sentinel.

New sentinel prefixes must also be added to the high-level classifier. New
providers should prefer explicit errors and structured responses over inventing
additional sentinels.

## Agents and persisted entities

`R/agent_management.R` owns setup, content, and agent storage. Entity filenames
use a hash of the logical name, while the name inside the RDS remains
authoritative. Legacy name-derived cache files are read and migrated when
touched.

Rename/delete operations preserve references:

- renaming a setup or content item retargets referring agents;
- deleting a referenced setup or content item is blocked;
- corrupt persisted entries fail with an actionable path instead of
  disappearing from lists.

`R/agent_runtime.R` owns agent execution. Supported one-call aliases are:

- `context_override` for text;
- `prompt_override` for image;
- `audio_override` for STT;
- `text_override` for TTS.

## Batch and persistence ownership

`R/batch_functions.R` orchestrates serial, PSOCK, and opt-in fork execution.
`R/helper_functions.R` executes one task and owns generation/statistics
persistence.

Important invariants:

- `qty` is task count; `workers` is concurrency;
- automatic concurrency is capped by
  `options(genflow.batch_max_workers = 4L)` unless configured otherwise;
- retry workers never exceed the number of pending tasks;
- `gen_batch_agent()` passes one agent directly to every task;
- the `.GlobalEnv` lookup path exists only for compatibility with old
  `gen_batch()` callers;
- fork interruption performs hard child cleanup;
- completed caller-owned checkpoints remain recoverable;
- `persist = FALSE` prevents genflow from competing with caller-owned storage.

RDS statistics and entity writes use staging plus atomic promotion. Shared
statistics updates use an inter-process lock. A corrupt statistics file is
preserved for diagnosis rather than overwritten silently.

## Model catalogs

`R/update_functions.R` owns catalog acquisition and
`R/agent_interface.R` owns catalog display/filtering.

Every updater writes `<provider>.csv` with:

- `service` (or the accepted compatibility name `provider`);
- `model`;
- `type`;
- `pricing`;
- `description`.

`gen_update_models()` runs each updater in an isolated staging directory,
validates the result, takes a provider lock, and promotes the validated CSV
atomically. A failed updater or invalid schema leaves the previous catalog
byte-for-byte unchanged.

Hugging Face intentionally has two catalogs:

- `hf.csv`: models with a live Hugging Face Inference Provider mapping;
- `hf-local.csv`: Hub models discoverable for local execution.

This distinction prevents local-only repositories such as
`OpenMOSS-Team/MOSS-Transcribe-Diarize` from being advertised as remotely
callable through `service = "hf"`.

## Local inference boundary

`R/local_inference.R` owns non-secret local configuration and diagnostics.
The JSON file never stores provider tokens.

Text:

- Ollama is an external server at `/api/chat`;
- llama.cpp is an external OpenAI-compatible server.

STT:

- `hf-local` launches `inst/python/genflow_stt.py` as an isolated subprocess;
- `local-native` launches a configured external native CLI and separates the
  transport (`service`), engine, model backend, and model selector;
  `native_engine = "crispasr"` is the experimental/beta multi-architecture
  route, while `native_engine = "moss-transcribe"` is MOSS-specific;
- `local-openai` calls a user-managed `/v1/audio/transcriptions` endpoint.

The native model selector can be a local path, `auto`, or an engine-compatible
`hf://OWNER/REPO:FILE` reference. A slash before the required filename is
accepted at the public boundary and normalized to the colon form used by
CrispASR. This does not make arbitrary Hugging Face repositories executable:
the selected engine must implement the architecture and support that exact
packaging; genflow does not combine split model/projector GGUF artifacts. The
old `moss-cpp` service id is a pre-release compatibility alias, not the
canonical provider contract.

genflow does not install external engines, manage native model caches,
supervise servers, or silently change any local backend. Explicit `auto` or
`hf://` selectors may authorize the selected engine to download a compatible
model. Diagnostics reject a directory used as an executable and may inspect
the CrispASR cache read-only to distinguish a pending download from an existing
non-empty model file, including a source-origin sidecar when present. The
native cache is separate from the Python Hugging Face cache configuration.
Native `hf://` downloads follow the repository's mutable `main` revision, so
auditable deployments should use a verified local artifact. Only engine
execution proves architecture compatibility and actual accelerator use.
See
[Local inference](local-inference.md) for configuration, engine/backend/model
compatibility, MOSS, revision pinning, ROCm, Vulkan, and security details.

## Shiny application

`R/agent_interface.R` owns the UI and server behavior.
`R/interface_launcher.R` owns launch policy.

The launcher binds to loopback by default. Listening on a non-loopback address
requires explicit `allow_remote = TRUE`, because the app can edit credentials
and persisted agents.

The app has five ownership areas:

- setup/content/agent CRUD;
- model catalogs and favorites;
- credentials;
- local inference configuration and diagnostics;
- validated bundle import/export.

Values read from persisted entities and catalogs must be escaped before they
reach HTML. Shiny observers must tolerate their inputs being `NULL` during
initialization.

## Portable bundles

`R/import_export.R` validates archive paths before extraction, rejects
unsupported file types and schemas, and enforces entry/file/bundle size limits.
Imports never accept absolute paths, parent traversal, or ambiguous archive
roots.

Credentials and local-inference secrets are outside the bundle contract.

## Adding a provider

For a new provider or modality:

1. Add and test the provider adapter in the correct runtime file.
2. Add the dispatch branch and service alias normalization.
3. Return the common structured contract after high-level normalization.
4. Define explicit behavior for images, tools, reasoning, and plugins.
5. Add a transactional catalog updater when model discovery exists.
6. Add the provider label and credential specification.
7. Test direct execution, agent execution, batch execution, catalog failure,
   and app initialization.
8. Update README, roxygen documentation, and this matrix.

Do not use a selectable UI entry as a placeholder for an unimplemented
runtime.
