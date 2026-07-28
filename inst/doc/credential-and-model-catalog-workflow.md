# Credential and Model Catalog Workflow

## Purpose

This document describes the supported workflow for provider credentials,
connection overrides, and model catalog refreshes in genflow.

## Runtime Contract

Provider runtime code continues to read configuration with `Sys.getenv()`.
The interface does not store secrets in genflow setup, agent, content, bundle,
or model catalog files.

Where a provider has supported aliases, the credential preflight treats them
as one requirement. For Gemini, `GOOGLE_API_KEY` takes precedence and
`GEMINI_API_KEY` remains accepted.

## Credentials Panel

Launch the interface:

```r
gen_interface()
```

In the **Models > Credentials** panel, choose a provider and use:

- **Add / edit** to write API keys, API tokens, account IDs, and optional base
  URL overrides to the user `.Renviron`.
- **Import detected** to scan the current R session, user `.Renviron`, project
  `.Renviron`, project `.env`, `~/.bashrc`, and `~/.zshrc` for simple
  `KEY=value` assignments. Values are masked before confirmation.
- **Delete** to remove that provider's managed variables from `.Renviron` and
  unset them in the current R session.

Every write takes an inter-process lock, stages a private replacement, backs up
the current `.Renviron`, commits atomically, and then loads the new values into
the active R session with `Sys.setenv()`. On Unix-like systems, managed files
and recovery copies use mode `0600`. Interrupted portable replacements retain
or recover the original instead of silently truncating it.

## Base URLs

Built-in provider base URLs stay hardcoded in provider runtime/update code.
The Credentials panel shows those defaults where an override is supported, so
users can see the effective connection target. Saving the default value is not
required. Save a base URL only when you intentionally want an environment
override.

## Models

The Credentials panel is not a model picker and does not manage `*_MODEL`
environment variables. The expected order is:

1. Set or import the provider credential.
2. Run **Update selected provider** or **Update all** in the Models tab.
3. Pick a model from the refreshed catalog in setup or agent configuration.

The same ownership rule applies to local adapters: Models selects the model
stored in a setup or agent, while Local configures runtimes and manages local
resources. Local does not maintain a second model selection for a setup or
agent. In particular, the STT server panel exposes only its URL; its model comes
from Models, the setup/agent, or an explicit runtime call.

## Model Update Preflight

The interface checks required credentials before calling a provider's model
catalog endpoint. If a selected provider is missing a key, genflow opens the
credential dialog instead of calling the API. For **Update all**, missing
providers can be configured or skipped while ready providers continue.

`gen_update_models()` also has a checked mode:

```r
gen_update_models(provider = "openai", fail_on_error = TRUE)
```

The default remains compatible with prior behavior, but UI calls use checked
mode so provider failures are visible instead of being reported as success.

## Hugging Face Catalog

Hugging Face is a remote provider in genflow. `provider = "hf"` writes
`hf.csv` with models that currently advertise at least one live Hugging Face
Inference Provider mapping.

```r
gen_update_models(
  provider = "hf",
  fail_on_error = TRUE
)
```

A local-only Hub model is deliberately excluded from `hf.csv`; otherwise the
app could offer it for a remote `service = "hf"` call that cannot execute it.
There is no `hf-local` provider/catalog and no bundled Python/Transformers
bridge. A user-managed Python transcription service remains supported through
the OpenAI-compatible STT server adapter described in
[Local inference](local-inference.md).

## Native STT Catalog

`provider = "local-native"` writes `local-native.csv` from the canonical
CrispASR cache:

```r
gen_update_models(
  provider = "local-native",
  fail_on_error = TRUE
)
```

Only downloaded files marked as managed by genflow are listed. External model
paths, symbolic links, and incomplete downloads do not become selectable
catalog entries. Models are identified by their cache filename and use
`type = "Audio"`. If the managed cache is empty, genflow publishes an empty
catalog instead of retaining stale rows.

Use **Local > Native STT** to choose the engine/device, edit the independently
saved CrispASR and moss-transcribe.cpp executable paths, and search, verify,
download, monitor, or delete CrispASR cache files. Use **Models**, setup, or
agent configuration to select which downloaded model is executed. The
OpenAI-compatible STT server remains available under Local for a server
operated by the user; its model choice belongs to the setup or agent rather
than a Local fallback setting.
