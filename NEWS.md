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
