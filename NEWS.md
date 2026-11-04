# genflow 0.0.21

- `gen_tts@4` defaults to reference consent and marking responsibility enabled,
  with the spoken disclaimer disabled. Explicit choices and v2/v3 workflow
  behavior are preserved.
- Document that the native VoxCPM2 adapter uses reference audio rather than a
  named preset-voice catalog.

# genflow 0.0.20

- `gen_block_configuration()` exposes an offline projection of block contracts,
  pinned service/model schemas and adapter restrictions for editable forms.
  Existing snapshots and explicit invalid values are preserved.
- Share adapter argument restrictions and provider defaults between form
  projection and validation; keep connected inputs pending where appropriate.
- Validation diagnostics retain the actual block contract version, including
  direct calls and portable agent definitions.

# genflow 0.0.19

- `gen_load_credentials()` lets server processes reload registered credential assignments through the public API, without exposing values or changing unrelated environment variables.
- Compatible provider catalogs accept minimal model responses containing only an identifier column.

# genflow 0.0.18

- Expose ordinary R APIs for credentials, full model catalogs, favorites and
  native model files/download jobs. The Shiny manager consumes these APIs;
  existing provider and local-runtime APIs remain the shared source of truth.
- Imports recheck detected credential sources before writing. Favorite writes
  use atomic replacement; native removal protects saved references in the
  library, including unreadable-state failures. Public download status retains
  completion semantics and returns safe diagnostics instead of raw stderr.
- Consolidate catalog reading and remove duplicated Shiny storage/formatting
  helpers. Existing files and storage locations remain compatible. See
  `inst/doc/management-api.md` for API ownership and session semantics.

# genflow 0.0.16

- `gen_img()` uses contract v2 and accepts optional ordered `add_img` references
  in direct, agent and batch calls. OpenAI GPT Image edits and FAL FLUX.2 edit
  have documented mappings and limits; unsupported models fail before inference.
  Contract v1 remains readable. Provider overrides cannot replace canonical
  prompts, references or output counts in the current runtime.
- All image adapters share bounded URL/base64/data URI/byte/ZIP handling and
  FFmpeg decoding. PNG, JPEG and WebP retain their real format and dimensions.
  Invalid media and multiple outputs are errors, with recoverable receipts.
- `gen_img_recover()` retries only download, validation and publication. Completed
  inference survives output/storage failures; automatic batch recovery never
  submits those image calls again. FFmpeg and ffprobe are required for images.

# genflow 0.0.15

- Added the versioned diagnostic schema and pt-BR catalog, exported through
  `gen_error_catalog()`, `gen_error_message()`, `gen_diagnostic()` and
  `gen_error_summary()`. Schema errors carry rule parameters and original
  document paths, node labels and field titles; nested causes survive adapters.
- Generator, voting and validation boundaries share structured conditions and
  results. Provider failures preserve execution outcome independently of their
  translated message, optional HTTP status and a redacted log reference.
  Batch summaries retain diagnostics alongside compatible error strings.
- Unknown exceptions have safe public explanations. Persistence failures retain
  generated content and successful inference; translating errors does not change
  retry policy. See `inst/doc/diagnostics.md` for extension and compatibility rules.

# genflow 0.0.13

- Compare JSON objects and schemas by exact values and property names, with
  object ordering ignored. Call/port validators reject missing contract IDs.
- Preserve invalid explicit counts, prepare static graph metadata once, and
  separate incomplete root diagnostics from envelope/dependency failures.
- Export `gen_validate_agent_definition()` so consumers reuse agent preparation.
  Runtime-owned agent parameters use `x-owner`, independently of form visibility.
- Include shared portable examples for contract parity tests and verify pinned
  contract versions independently.

# genflow 0.0.12

- `gen_validate_structure()` now accepts complete `genflow-studio-definition` v1
  bundles and self-contained Studio workflow bodies using the existing contract
  and typed graph engine. It validates all resources offline, reports original
  document paths and returns a prepared copy plus resource dependency order.
- Added cross-resource checks for agent arguments, prompts, candidate schemas,
  subworkflow interfaces and asset manifests. Preserved v1 empty-object and
  plural-agent-reference compatibility without modifying stored documents.
- Added an R-side `check_capabilities` policy for isolated fixture validation;
  serialized documents cannot change this policy. No contracts or defaults changed.

# genflow 0.0.11

- Added versioned block contracts with shared defaults, typed connections and
  presentation metadata, exposed through `gen_block_contracts()`.
- Added offline `gen_validate_call()`, `gen_validate_structure()`,
  `gen_block_ports()`, `gen_check_schema()` and `gen_validate_schema()`. Validation
  accumulates independent errors and never executes the described operations.
- Generator and voting entry points now validate with the same contracts before
  provider or file work. Omitted arguments receive defaults; invalid explicit
  types/ranges and unknown arguments raise structured `genflow_validation_error`
  conditions. Silent scalar/list and numeric-string coercions are removed.
- Portable workflows pin contract versions, validate nested definitions and
  preserve connected values as pending until execution. See
  `inst/doc/block-contracts.md` for the bounded schema vocabulary and limits.

# genflow 0.0.10

- Added an explicit OpenAI `max_output_tokens` cap for direct, agent, and batch
  text calls, including reasoning tokens. Chat Completions uses
  `max_completion_tokens`; Responses uses `max_output_tokens`. Unsupported
  providers reject the parameter before inference.

# genflow 0.0.9

- Unified direct/agent/batch request preparation, complete-field overrides,
  and byte-sensitive batch input identity. PSOCK workers receive a frozen
  provider/local configuration, including custom option paths.
- Consolidated ten OpenAI-compatible providers and their catalogs around a
  shared registry; exposed `gen_capabilities()`. Shiny UI is constructed only
  when launched, while its public entrypoints remain available.
- Added typed internal text responses, normalized result/error/persistence
  fields, provider usage and request metadata, bounded text retries, and
  explicit refusal/truncation handling. Literal error-like generated text is
  no longer treated as transport failure.
- All generated artifacts use unique contained paths and staged publication.
  Inference success survives a storage failure without reporting overall
  success or automatically repeating paid work. Image/TTS support temporary
  output via `persist = FALSE`.
- Fixed vector voting, failed-response and candidate filtering; added quorum,
  explicit tie policies and structured decisions. The default tie policy now
  returns no winner; `ties = "first"` explicitly restores legacy tie selection.
- Saved/exported configurations reject executable values and credential
  fields. Provider registry edits lock the full read/modify/write transaction;
  failed exports preserve existing archives.
- Corrected Hugging Face chat/STT routes and catalog executability; omitted
  unsupported OpenAI sampling parameters. Removed legacy `*_dir_mkt` behavior.

## Changes carried forward from 0.0.8.9000

- Persistent STT run folders now identify their service, model, creation time,
  and a short code. Each completed chunk publishes a readable
  `part_NNNN.txt`; the structured resume envelope is retained as the hidden
  `.part_NNNN.checkpoint.rds`, and legacy opaque runs remain resumable.
- Simplified STT chunking to one explicit rule: when
  `chunk_segment_seconds` is set, Genflow prepares the audio and splits it into
  contiguous chunks of that duration. There is no overlap, adaptive byte-size
  planner, or implicit model-duration policy. Without that argument, the
  recording is sent as one model input.
- Removed the auxiliary Python/Pyannote/TitaNet speaker paths and all
  cross-chunk voice linking. Multi-chunk results retain model-provided labels
  under honest `C01:S01`, `C02:S01`, ... namespaces, rebase timestamps, and
  concatenate text in source order without boundary deduplication. The merge
  signature is now `sequential-chunks-v1`.
- MOSS Diarize now defaults its CrispASR KV cache to `q8_0` when
  `native_kv_quant` is `NULL`, reports the source as `model-default`, and still
  gives priority to explicit `f16`, `q8_0`, or `q4_0`. The new implicit default
  changes the STT signature/checkpoint key; explicit `f16` preserves the former
  policy.

# genflow 0.0.8
# genflow 0.0.7

- Added semantic `chunk_format = "auto"`, `"wav"`, or `"mp3"` selection.
  CrispASR can now receive compact native MP3 chunks; moss-transcribe.cpp
  rejects MP3 early because its CLI requires WAV.
- Added operational `checkpoint_retention = "results"` cleanup, which removes
  only safe Genflow-owned prepared/chunk audio after a successful result while
  retaining manifests and per-part transcript checkpoints. Its structured
  `checkpoint_media_cleanup_complete` result prevents downstream retention
  markers from treating partial cleanup as complete.

# genflow 0.0.6

- `gen_stt()` now owns large-audio preparation, chunk planning, overlap,
  checkpoint resume, retry, deduplication, and conservative speaker-label
  reconciliation.
- Added persistent checkpoint locking, validation, adaptive size reduction,
  safe per-recording retention, and a model-aware 60-minute MOSS Diarize
  policy.
- Added `gen_stt_signature()` so downstream caches can track the effective
  endpoint, native runtime, model, executable, and semantic STT configuration
  without storing credentials.
- Added the compact `output = "transcript"` projection while preserving common
  status, service, model, elapsed-time, and saved-artifact fields.
- Native executable paths remain independently configurable for CrispASR and
  moss-transcribe.cpp.
