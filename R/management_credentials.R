#' Inspect provider credential fields and their current state
#'
#' These functions are the credential-management boundary shared by graphical
#' interfaces and ordinary R callers. Reads never return plaintext secrets.
#' Credentials remain in a private Renviron file, outside agents and workflows.
#'
#' @param providers Optional provider identifiers. NULL selects all providers.
#' @param path Credential file. Defaults to option `genflow.renviron_path`, then
#'   `~/.Renviron`. Only `gen_load_credentials()` explicitly loads assignments
#'   from the file into the process environment.
#' @return `gen_credentials()` returns `schema_version`, `path`, `fields` (field
#'   definitions plus active/detected/masked state) and `missing` (required
#'   credential groups missing from the current process for catalog updates).
#' @export
gen_credentials <- function(providers = NULL, path = NULL) .gen_management({
  providers <- .gen_management_providers(providers)
  path <- .genflow_credentials_path(.gen_management_path(path))
  list(schema_version = 1L, path = path,
    fields = .genflow_credential_status(providers, path = path),
    missing = .genflow_required_credentials_missing(providers))
})

.gen_credential_vars <- function(vars) {
  .gen_management_assert(is.character(vars) && length(vars) > 0L &&
    !anyNA(vars) && !anyDuplicated(vars) && all(nzchar(vars)), "vars")
  .gen_management_assert(all(vars %in% .genflow_credential_specs()$env), "vars")
  vars
}

.gen_credential_hash <- function(value) unclass(as.character(openssl::sha256(charToRaw(enc2utf8(value)))))

#' @rdname gen_credentials
#' @param paths Optional named character vector of source files for detection.
#'   NULL uses the configured credential file, project Renviron/env and user
#'   shell configuration files. `character()` scans only the current process.
#'   Files are parsed as simple assignments, never executed or sourced.
#' @return `gen_detect_credentials()` returns a data frame with `env`, `masked`,
#'   `source`, `path`, `line`, and `fingerprint`. Keep selected rows for
#'   [gen_import_credentials()]; their source is rechecked before import.
#' @export
gen_detect_credentials <- function(providers = NULL, paths = NULL) .gen_management({
  providers <- .gen_management_providers(providers)
  if (!is.null(paths)) {
    .gen_management_assert(is.character(paths) && !anyNA(paths) && all(nzchar(paths)) &&
      (!length(paths) || (!is.null(names(paths)) && !anyNA(names(paths)) &&
        all(nzchar(names(paths))) && !anyDuplicated(names(paths)) &&
        !"current_session" %in% names(paths))), "paths")
  }
  found <- .genflow_detect_credentials(providers, include_values = TRUE, paths = paths)
  found$fingerprint <- vapply(found$value, .gen_credential_hash, character(1))
  found$value <- NULL
  found
})

#' @rdname gen_credentials
#' @param values Named character vector or named list of scalar strings. Only
#'   variables registered for built-in or custom providers can be changed.
#'   Omitted variables are preserved; an explicit empty string clears a value.
#' @param backup Keep a private backup before replacing an existing file.
#' @param set_session Also apply saved values to the current R process.
#' @return Save/import return the target and backup paths and names added/updated,
#'   never the saved values. Saving uses the existing inter-process lock and
#'   atomic replacement. It does not reload other processes or restart services.
#' @export
gen_save_credentials <- function(values, path = NULL, backup = TRUE, set_session = TRUE) .gen_management({
  .gen_management_assert((is.character(values) || is.list(values)) && length(values) > 0L,
    "values")
  .gen_credential_vars(names(values))
  .gen_management_assert(all(vapply(as.list(values), function(value)
    .gen_scalar(value) && !grepl("[\r\n]", value), logical(1))), "values")
  .genflow_save_credentials(unlist(values, use.names = TRUE),
    path = .gen_management_path(path),
    backup = .gen_management_flag(backup, "backup"),
    set_session = .gen_management_flag(set_session, "set_session"))
})

#' @rdname gen_credentials
#' @param selection Rows from [gen_detect_credentials()], with one row per
#'   variable. Import rejects a selection whose source or value has changed.
#'   Secret values are read and written inside the library, not returned to UI.
#' @export
gen_import_credentials <- function(selection, path = NULL, backup = TRUE, set_session = TRUE) .gen_management({
  .gen_management_assert(is.data.frame(selection) && nrow(selection) > 0L &&
    all(c("env", "source", "path", "line", "fingerprint") %in% names(selection)), "selection")
  .gen_credential_vars(selection$env)
  values <- vapply(seq_len(nrow(selection)), function(i) {
    row <- selection[i, , drop = FALSE]
    .gen_management_string(row$source, "selection")
    .gen_management_string(row$fingerprint, "selection")
    if (identical(row$source, "current_session")) {
      value <- Sys.getenv(row$env, unset = "")
    } else {
      .gen_management_string(row$path, "selection")
      .gen_management_assert(is.numeric(row$line) && length(row$line) == 1L &&
        is.finite(row$line) && row$line >= 1 && row$line == floor(row$line), "selection")
      found <- .genflow_read_env_assignments(row$path, vars = row$env, source = row$source)
      value <- found$value[found$line == row$line]
    }
    if (length(value) != 1L || !nzchar(value) ||
        !identical(.gen_credential_hash(value), row$fingerprint)) {
      .gen_stop("management_source_changed", stage = "management", outcome = "not_started")
    }
    value
  }, character(1))
  gen_save_credentials(stats::setNames(values, selection$env), path, backup, set_session)
})

#' @rdname gen_credentials
#' @param vars Registered environment variable names to remove.
#' @param unset_session Also remove those variables from the current R process.
#' @return Delete returns target/backup paths and the names removed. Unrelated
#'   assignments and comments in the credential file are preserved.
#' @export
gen_delete_credentials <- function(vars, path = NULL, backup = TRUE, unset_session = TRUE) .gen_management({
  .genflow_delete_credentials(.gen_credential_vars(vars), path = .gen_management_path(path),
    backup = .gen_management_flag(backup, "backup"),
    unset_session = .gen_management_flag(unset_session, "unset_session"))
})


#' @rdname gen_credentials
#' @return `gen_load_credentials()` applies registered assignments from one
#'   credential file to the current process and returns only its path and loaded
#'   variable names. Empty assignments clear inherited values; absent assignments
#'   preserve the environment. It never scans other sources, evaluates expressions,
#'   writes files, or changes unregistered process variables.
#' @export
gen_load_credentials <- function(path = NULL) .gen_management({
  path <- .genflow_credentials_path(.gen_management_path(path))
  fields <- .genflow_credential_specs()$env
  found <- .genflow_read_env_assignments(path, vars = fields, literal = TRUE)
  found <- found[!duplicated(found$env, fromLast = TRUE), , drop = FALSE]
  if (nrow(found)) do.call(Sys.setenv, stats::setNames(as.list(found$value), found$env))
  list(path = path, loaded = found$env)
})
