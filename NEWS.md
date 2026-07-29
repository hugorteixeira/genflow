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
