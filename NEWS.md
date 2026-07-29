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
