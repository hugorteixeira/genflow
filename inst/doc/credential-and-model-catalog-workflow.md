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

## Hugging Face Catalogs

Hugging Face has two different execution surfaces, so genflow keeps two
catalogs:

- `provider = "hf"` writes `hf.csv` with models that currently advertise at
  least one live Hugging Face Inference Provider mapping.
- `provider = "hf-local"` writes `hf-local.csv` with Hub models intended for
  local discovery, including compatible speech-to-text candidates.

```r
gen_update_models(
  provider = c("hf", "hf-local"),
  fail_on_error = TRUE
)
```

A local-only Hub model is deliberately excluded from `hf.csv`; otherwise the
app could offer it for a remote `service = "hf"` call that cannot execute it.
See [Local inference](local-inference.md) for local STT configuration.
