# genflow 0.0.7

- MOSS Diarize now defaults its CrispASR KV cache to `q8_0` when
  `native_kv_quant` is `NULL`, reports the source as `model-default`, and still
  gives priority to explicit `f16`, `q8_0`, or `q4_0`. The new implicit default
  changes the STT signature/checkpoint key; explicit `f16` preserves the former
  policy.
- Timestamped chunk overlaps can now reconcile two-speaker identity/swap
  permutations using duration support plus conservative purity and margin
  thresholds. Weak, conflicting, or larger-roster evidence still receives
  unresolved `U...` labels, and only exact normalized text overlap can delete
  duplicated content.
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
