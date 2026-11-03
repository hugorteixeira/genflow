# Public management API

Management is an ordinary R API. It does not require a Shiny session or a new
HTTP server. The bundled Shiny app consumes these exported functions; other
applications can call the installed package through the same boundary.

## Operations and ownership

| Area | Exported functions | Responsibility |
|---|---|---|
| Provider definitions | `list_providers()`, `get_provider()`, `set_provider_openai_compat()`, `rm_provider()`, `test_provider()` | Existing provider registry and endpoint checks |
| Credentials | `gen_credentials()`, `gen_detect_credentials()`, `gen_save_credentials()`, `gen_import_credentials()`, `gen_delete_credentials()` | Field metadata, masked state and private Renviron persistence |
| Catalog | `gen_models()`, `gen_update_models()`, `gen_show_models()` | Full machine-readable data, explicit refresh and console presentation |
| Parameter schemas | `gen_parameter_schemas()`, `gen_save_parameter_schema()`, `gen_resolve_parameter_schema()`, `gen_validate_parameters()` | Versioned service/model definitions and portable validation; see [parameter schemas](parameter-schemas.md) |
| Favorites | `gen_model_favorites()` | Read, normalize or replace model selections |
| Local runtime | `gen_local_config()`, `gen_local_diagnostics()` | Existing configuration validation, persistence and diagnostics |
| Native files | `gen_native_models()`, `gen_native_model_reference()`, `gen_native_model_check()`, `gen_native_model_download()`, `gen_native_model_remove()` | Inventory, exact references, verification, verified publication and guarded removal |
| Background downloads | `gen_native_download_status()`, `gen_native_download_cancel()`, `gen_native_download_cleanup()` | Session-owned worker lifecycle |

`list_providers()$catalog_update` identifies providers supported by the catalog
updater. It is adapter metadata, not proof of connectivity, credentials or
model compatibility. `test_provider()`, `gen_local_diagnostics()`, catalog
updates and remote model checks may contact their configured endpoints.
Downloads transfer files. None of the new management operations runs inference.

## Credentials

```r
state <- genflow::gen_credentials("openai")
state$fields  # Registered fields, defaults, active/detected flags and masked values
state$missing

# Explicit mutation; use a value supplied securely by the caller.
genflow::gen_save_credentials(c(OPENAI_API_KEY = "example-only"))

detected <- genflow::gen_detect_credentials("openai")
selection <- detected[!duplicated(detected$env), , drop = FALSE]
if (nrow(selection)) genflow::gen_import_credentials(selection)

genflow::gen_delete_credentials("OPENAI_API_KEY")
```

Reads never return plaintext API keys. Detection returns source metadata and a
SHA-256 fingerprint, not a secret value. Import rereads every selected source
and rejects a changed value before any write. Sources are parsed as simple
assignments, never executed. Detection may inspect known shell configuration
files; use `paths = c(source = "/exact/file")` to narrow this or
`paths = character()` for the current R process only.

Writes accept only variables registered for built-in or custom providers.
Omitted values are preserved; an explicit empty string clears a value. The
Shiny password field keeps its existing behavior: leaving it blank preserves
the current credential. The file uses the existing lock, atomic replacement,
private backups and Unix mode `0600`. Credentials remain outside workflows,
agents, catalogs and portable bundles.

`path` defaults to `getOption("genflow.renviron_path")`, then `~/.Renviron`.
`set_session = TRUE` and `unset_session = TRUE` affect only the calling R
process. Saving does not restart services or reload other processes. A server
integrating this API owns authorization and how settings reach its workers.

## Catalog and favorites

```r
catalog <- genflow::gen_models(provider = "hf")
favorites <- genflow::gen_model_favorites()
candidate <- catalog[, c("service", "model", "type"), drop = FALSE]
normalized <- genflow::gen_model_favorites(candidate, catalog = catalog, save = FALSE)
```

`gen_models()` returns `service`, `model`, `type`, `pricing`, `description` and
`source_file`, including typed columns for an empty catalog. It makes no remote
request, prints nothing, normalizes aliases and keeps complete descriptions.
`gen_show_models()` retains its existing display-oriented result, including its
`provider` column and shortened descriptions; both use the same CSV reader.

Catalog reads, updates and favorites share `genflow.models_dir`, otherwise
`tools::R_user_dir("agent_models", "data")`. Favorites retain the existing
`favorites.rds` format. Reads never rewrite it; supplied selections are normalized
against the catalog and written with a lock and atomic replacement. No migration
or additional database is required.

## Native models and downloads

```r
files <- genflow::gen_native_models(managed_only = TRUE)
reference <- genflow::gen_native_model_reference("hf://OWNER/REPOSITORY:MODEL.gguf")

# These operations intentionally contact the model source.
artifact <- genflow::gen_native_model_check(reference)
job <- genflow::gen_native_model_download(reference, background = TRUE)
status <- genflow::gen_native_download_status(job)
# Poll status as needed. Cancel only when the caller requests cancellation:
# genflow::gen_native_download_cancel(job)
# Clean after the worker stops:
# genflow::gen_native_download_cleanup(job)
```

Check and download also accept existing local files or CrispASR `auto`
selectors. Local files are reused without copying or guessing remote provenance.
Remote verification reuses the existing exact artifact, revision, size and
checksum rules. Downloads keep the existing staging and publication gates.
Cancellation preserves completed results and never kills a worker while it
owns the publication gate. Cleanup removes the stopped job's temporary data,
not its published model. These operations do not qualify a model for inference.

Background downloads return an **opaque R handle**, owned by the calling session.
Keep it server-side and expose only its JSON-serializable status. Handles are
not durable across application restarts. The caller owns polling, cancellation
and cleanup; this step does not introduce a job database or recovery service.
Reported failures use `gen_diagnostic()` and do not expose raw worker stderr.

Inventory includes a `repository` column, empty when provenance is unknown.
Only regular files directly in the canonical CrispASR cache are manageable.
Removal protects active downloads, saved local configuration, saved R setups
and agents. A client with additional references must pass `protected_models`;
the library does not inspect external databases. Symlinks and external files
are not removable. Unreadable saved state blocks removal.

After a cache mutation, call `gen_update_models(provider = "local-native")`
when a refreshed catalog is needed. The Shiny app already does this. The API
keeps file mutation and catalog refresh explicit so a refresh failure never
causes an automatic repeat of a successful download.

## Implementation and errors

The public boundary lives in `R/management.R`, with domain operations in
`R/management_credentials.R`, `R/management_models.R` and
`R/management_native.R`; `R/parameter_schemas.R` owns versioned parameter schemas. Existing credential storage, provider registry,
runtime configuration, cache downloader and worker implementations remain
their respective owners. `R/agent_interface.R` handles forms and presentation.

The new functions signal `genflow_error` conditions. Use `gen_diagnostic(error)`
for the existing JSON-safe pt-BR contract: stable code, stage, outcome, path,
message and hint. Known validation failures retain their code; unclassified
failures have a safe message and a redacted log reference. Existing exported
provider/configuration functions keep their compatible behavior and can also
be normalized at the application boundary with `gen_diagnostic()`.


Native model inventory now includes `modality` (`stt`, `tts`, `unknown`). Catalog
reads normalize older native `Audio` rows without modifying their CSV files.
Saved `tts_native_model` is protected by the same removal gate as STT models.
