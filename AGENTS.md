# AGENTS.md

## Scope
This file documents how to work inside `genflow` as a coding agent or maintainer.
Primary goal: keep provider integrations consistent across runtime, batch, model catalog, and UI.

## Git Workflow: Main Only
- This repository uses `main` as its only working and publication branch.
- Make changes, commits, and pushes directly on `main`; do not create
  `agent/*`, feature, release, or temporary branches.
- This repository rule takes precedence over generic workflows that recommend
  creating a branch or pull request before publishing.
- When the user requests publication, push the validated commits directly to
  `origin/main`.
- If work is ever found on another branch, integrate it into `main` first and
  remove the redundant branch only after verifying that `origin/main` contains
  the complete history.

## Architecture Map
- Text runtime entrypoint: `R/text_functions.R`
  - `gen_txt.default()` normalizes inputs, dispatches by `service`, saves output, returns structured status list.
  - Provider implementations currently in-tree: `.gen_txt_openai()`, `.gen_txt_openrouter()`, `.gen_txt_hf()`, `.gen_txt_ollama()`, `.gen_txt_llamacpp()`.
- Image runtime entrypoint: `R/image_functions.R` (`gen_img.default()` + provider-specific internals).
- STT runtime entrypoint: `R/stt_functions.R` (`gen_stt.default()` + provider-specific internals).
- TTS runtime entrypoint: `R/tts_functions.R` (`gen_tts.default()` + provider-specific internals).
- Batch/parallel runtime:
  - `R/batch_functions.R` orchestrates batches.
  - `R/helper_functions.R` contains `.execute_agent_task()` which calls `gen_txt()`/`gen_img()`.
  - `gen_batch_agent()` passes one `genflow_agent` directly to all tasks; do not
    reintroduce one temporary `.GlobalEnv` clone per task.
  - Keep task count (`qty`) separate from concurrency (`workers`). Vision
    callers may use `add_img_each`, and caller-owned caches should use
    `persist = FALSE` plus optional per-task checkpoints.
  - Forked Unix-like batches use hard child cleanup on interruption. Preserve
    that guarantee so blocked provider calls cannot prevent an R/RStudio
    restart; completed per-task checkpoints must remain recoverable.
- Agent/setup/content persistence and wiring:
  - `R/agent_management.R`, `R/agent_runtime.R`.
- Interactive UI + model catalog:
  - `R/agent_interface.R` provider labels, model source UI, setup/agent form behavior.
- Model catalog update/read:
  - `R/update_functions.R` (`gen_update_models()`, `.update_models_*()`, `gen_show_models()`).

## Runtime Contract
All high-level generators should return a list with this shape (or compatible superset):
- `response_value`
- `service`
- `model`
- `duration`
- `status_api` (`"SUCCESS"` or `"ERROR"`)
- `status_msg`

Text (`gen_txt`) also expects token estimates and persists responses through `.save_response()`.
Batch worker code expects list-like responses and uses `status_api` for error handling.

## Service Naming Rules
- Service ids are lowercase (examples: `openai`, `openrouter`, `hf`).
- New service support is not complete until all relevant switch/provider lists are updated.
- Model catalog CSV filenames should match provider id (`<provider>.csv`).

## Add New Text Provider Checklist
1. Add internal function in `R/text_functions.R`:
   - Suggested name: `.gen_txt_<provider>()`.
   - Handle auth/config, timeout, request, response parsing, and error sentinel strings.
2. Add switch branch in `gen_txt.default()` `.do_call`.
3. Keep compatibility with:
   - `tools` payload handling (or explicit warning when unsupported).
   - `add_img` handling (or explicit warning when unsupported).
   - `reasoning` / `plugins` behavior (pass-through or explicit no-op warning).
4. Ensure returned values are either:
   - plain text content,
   - tool/function object payload,
   - sentinel string consumed by `gen_txt.default()` status classification.
5. Add model catalog updater in `R/update_functions.R` when feasible.
6. Add provider label/options in `R/agent_interface.R` (`.MODEL_PROVIDER_LABELS`).
7. Update docs: `README.md`, roxygen comments, and generated man pages.

## Ollama Integration Blueprint (Local Models)
### Recommended MVP (text first)
- Add service id: `ollama`.
- Add `.gen_txt_ollama()` in `R/text_functions.R`.
- Endpoint: `POST {base_url}/api/chat` (default base URL: `http://127.0.0.1:11434`).
- Env/config:
  - `OLLAMA_BASE_URL` (optional; default local URL).
  - `OLLAMA_MODEL` (optional fallback model).
- Request shape:
  - `model`, `messages`, `options = list(temperature = temp_v)`, `stream = FALSE`.
  - For `add_img`, include base64 image in message content when Ollama multimodal model is used; otherwise warn and ignore.
- Response parsing:
  - Prefer `message$content`.
  - Return sentinel error strings on HTTP/parse errors.
- Reasoning/plugins/tools:
  - MVP: no-op with warning when unsupported.

### Model Catalog + UI
- Add `.update_models_ollama()` in `R/update_functions.R`:
  - `GET {base_url}/api/tags`.
  - Produce `ollama.csv` with columns: `service,model,type,pricing,description`.
  - Pricing should be empty/local.
- Include `ollama` in `gen_update_models()` provider list and mapping.
- Add `ollama` to `.MODEL_PROVIDER_LABELS` in `R/agent_interface.R`.

### Optional Phase 2
- Tool-calling mapping (if stable in chosen Ollama model family).
- Streaming support (`/api/chat` stream chunks).
- Optional embeddings and additional local backends.

## llama-cpp Integration Blueprint (Local Models)
### Current Support
- Service id: `llamacpp` (aliases accepted in runtime/update path: `llama-cpp`, `llama_cpp`).
- Runtime call: `.gen_txt_llamacpp()` in `R/text_functions.R`.
- Endpoint: `POST {base_url}/v1/chat/completions` using OpenAI-compatible payload.
- Model discovery/fallback:
  - explicit `model`, unless placeholder default
  - `LLAMACPP_MODEL` / `LLAMA_CPP_MODEL`
  - first id from `GET {base_url}/v1/models`
  - fallback `"local-model"`
- Env/config:
  - `LLAMACPP_BASE_URL` or `LLAMA_CPP_BASE_URL` (default `http://127.0.0.1:8080`)
  - `LLAMACPP_API_KEY` or `LLAMA_CPP_API_KEY` (optional)
- Catalog updater: `.update_models_llamacpp()` writes `llamacpp.csv`.
- UI model source: provider label added in `R/agent_interface.R`.
- Batch workers: the serialized task function resolves llama-cpp and other
  provider internals through the package namespace; PSOCK exports only current
  task state.

## Current Risks To Keep In Mind
- `gen_txt.default()` retry path currently calls `.do_call()` with mismatched arguments; be careful when touching retry logic.
- Error sentinel prefixes are not fully consistent across providers (`API_ERROR` vs `API_ERRORR`, timeout prefix variants). Keep new provider aligned with `gen_txt.default()` classifier.

## Validation Routine For Provider Changes
- Manual smoke tests (text):
  - direct call via `gen_txt()`
  - call via setup/agent (`set_setup` + `set_agent` + `gen_txt(agent)`)
  - batch call (`gen_batch_agent()`)
- UI checks:
  - provider appears in setup/agent selectors
  - model updater can fetch and display catalog rows
- Confirm no regressions in existing providers.
