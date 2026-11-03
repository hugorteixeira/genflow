#' Inspect and manage native model files
#'
#' Uses the existing CrispASR cache, verified downloader and lifecycle gates.
#' These functions never load a speech model or perform inference. Catalog
#' refresh remains an explicit [gen_update_models()] operation after mutations.
#'
#' @param config Optional local configuration, as in [gen_local_config()].
#' @param cache_dirs Optional read-only inventory directories. Only regular
#'   files directly in the canonical CrispASR cache are manageable.
#' @param managed_only Return only manageable files.
#' @return `gen_native_models()` returns a data frame with path, filename,
#'   quantization, size, source URL, managed/selected flags, `repository`, and
#'   `modality` (stt, tts or unknown). Modality describes recognized model
#'   families, not a guarantee that the installed executable can run them.
#'   Unknown repository provenance is an empty string, never guessed.
#' @export
gen_native_models <- function(config = NULL, cache_dirs = NULL, managed_only = FALSE) .gen_management({
  config <- .gen_native_config(config)
  if (!is.null(cache_dirs)) .gen_management_assert(is.character(cache_dirs) &&
    !anyNA(cache_dirs) && all(nzchar(cache_dirs)), "cache_dirs")
  .gen_management_flag(managed_only, "managed_only")
  rows <- .genflow_crispasr_inventory(config, cache_dirs)
  rows$repository <- .genflow_native_repository(rows$source_url, rows$filename)
  rows$modality <- tolower(vapply(seq_len(nrow(rows)), function(i)
    .gen_native_model_type(rows$path[[i]]), character(1)))
  tts_model <- config$tts_native_model %||% ""
  if (nzchar(tts_model)) rows$selected <- rows$selected |
    rows$path == normalizePath(path.expand(tts_model),mustWork=FALSE) |
    (identical(tts_model,basename(tts_model)) & rows$filename == tts_model)
  if (managed_only) rows <- rows[rows$managed, , drop = FALSE]
  rownames(rows) <- NULL
  rows
})

.gen_native_config <- function(config) {
  current <- gen_local_config()
  if (is.null(config)) return(current)
  .gen_management_assert(is.list(config) && !is.null(names(config)) &&
    !anyNA(names(config)) && all(nzchar(names(config))) && !anyDuplicated(names(config)), "config")
  .genflow_validate_local_config(utils::modifyList(current, config, keep.null = TRUE))
}

#' @rdname gen_native_models
#' @param selector Exact Hugging Face file reference, including `hf download`
#'   input and normal `/blob/` links. Check/download also accept CrispASR `auto`
#'   selectors or an existing local file; a local file is inspected/reused, not
#'   copied to the managed cache or attributed to a remote repository.
#' @return `gen_native_model_reference()` validates and normalizes an exact
#'   Hugging Face file reference without network access.
#' @export
gen_native_model_reference <- function(selector) .gen_management({
  .gen_management_string(selector, "selector")
  tryCatch(.genflow_native_hf_selector(selector), error = function(error)
    .gen_stop("native_reference_invalid", path = "/selector", stage = "management"))
})

.gen_native_selector <- function(selector) {
  .gen_management_string(selector, "selector")
  selector <- .genflow_native_reference_input(selector)
  if (file.exists(path.expand(selector)) && !dir.exists(path.expand(selector))) {
    # Keep the original path so the downloader can reject symlinks itself.
    return(path.expand(selector))
  }
  if (grepl("^auto(?::[A-Za-z0-9._-]+)?$", selector, perl = TRUE)) return(selector)
  gen_native_model_reference(selector)
}

.gen_native_options <- function(backend, quant, executable) {
  for (name in c("backend", "quant", "executable"))
    .gen_management_string(get(name), name, empty = TRUE)
  invisible(NULL)
}

#' @rdname gen_native_models
#' @param backend CrispASR backend for registry selectors.
#' @param quant Optional registry quantization preference.
#' @param executable Optional CrispASR executable for registry lookup.
#' @return Check returns the resolved artifact metadata. Remote checks verify
#'   exact filename, revision, size and hash metadata without downloading.
#' @export
gen_native_model_check <- function(selector, backend = "", quant = "", executable = "") .gen_management({
  selector <- .gen_native_selector(selector)
  .gen_native_options(backend, quant, executable)
  if (file.exists(selector)) {
    value <- .genflow_crispasr_download(selector, backend, quant, executable)
    value$sha256 <- .genflow_crispasr_file_sha256(value$path)
    return(value)
  }
  .genflow_crispasr_resolve_download(selector, backend, quant, executable)
})

#' @rdname gen_native_models
#' @param background Start a managed background R process. FALSE waits and
#'   returns the result; TRUE returns an opaque `genflow_native_download_job`.
#' @param progress Optional synchronous progress callback; not accepted for
#'   background jobs, whose progress is read with [gen_native_download_status()].
#' @return Download returns artifact metadata or an opaque job handle. The
#'   caller owns that handle, cancellation and cleanup. Keep it on the R side;
#'   expose only status data to a browser. Jobs are scoped to the owning R
#'   session, not durable across application restarts.
#' @export
gen_native_model_download <- function(selector, backend = "", quant = "", executable = "",
                                      background = FALSE, progress = NULL) .gen_management({
  selector <- .gen_native_selector(selector)
  .gen_native_options(backend, quant, executable)
  .gen_management_flag(background, "background")
  .gen_management_assert(is.null(progress) || (!background && is.function(progress)), "progress")
  if (background) .genflow_native_download_job_start(selector, backend, quant, executable)
  else .genflow_crispasr_download(selector, backend, quant, executable, progress = progress)
})

.gen_native_job <- function(job) {
  .gen_management_assert(inherits(job, "genflow_native_download_job") && is.list(job) &&
    .gen_scalar(job$id) && .gen_scalar(job$dir) && .gen_scalar(job$status_path), "job")
  invisible(job)
}

#' @rdname gen_native_models
#' @param job Opaque job returned by [gen_native_model_download()].
#' @return Status/cancel return JSON-serializable progress data, including
#'   `state`, `stage`, `running` and the result when complete. Errors use the
#'   diagnostic contract; process objects and raw stderr are not returned.
#' @export
gen_native_download_status <- function(job) .gen_management({
  .gen_native_job(job)
  status <- .genflow_native_download_job_read(job)
  running <- .genflow_native_download_job_alive(job)
  if (!running && !.genflow_native_download_state_terminal(status$state)) {
    # The worker may have completed between the first read and the liveness check.
    status <- .genflow_native_download_job_read(job)
  }
  status$running <- running
  if (identical(status$state, "error")) {
    status$error <- gen_diagnostic(simpleError(status$message %||% "Download failed"),
      message = gen_error_message("management_failed"), stage = "download", outcome = "failed", log = TRUE)
    status$message <- status$error$message
  }
  status
})

#' @rdname gen_native_models
#' @param publishing_wait_ms Time to wait for an ongoing atomic publication.
#'   Cancellation never kills a worker while it owns the publication gate.
#' @export
gen_native_download_cancel <- function(job, publishing_wait_ms = 5000L) .gen_management({
  .gen_native_job(job)
  .gen_management_assert(is.numeric(publishing_wait_ms) && length(publishing_wait_ms) == 1L &&
    is.finite(publishing_wait_ms) && publishing_wait_ms >= 0 &&
    publishing_wait_ms <= .Machine$integer.max && publishing_wait_ms == floor(publishing_wait_ms),
    "publishing_wait_ms")
  .genflow_native_download_job_cancel(job, publishing_wait_ms)
  gen_native_download_status(job)
})

#' @rdname gen_native_models
#' @return Cleanup returns TRUE after removing a stopped job's private temporary
#'   files; it never deletes the downloaded model or cleans a running job.
#' @export
gen_native_download_cleanup <- function(job) .gen_management({
  .gen_native_job(job)
  .genflow_native_download_job_cleanup(job)
})

#' @rdname gen_native_models
#' @param path Absolute path of one regular managed model file.
#' @param protected_models Additional references owned by the caller, for example
#'   client workflows or unsaved form selections. Saved R setups/agents and local
#'   configuration are protected automatically. External applications must pass
#'   their own references; the R library does not inspect client databases.
#' @return Remove returns TRUE after removal. Active downloads, referenced files,
#'   symlinks and files outside the managed cache cannot be removed.
#' @export
gen_native_model_remove <- function(path, protected_models = character(), config = NULL) .gen_management({
  path <- .gen_management_string(path, "path")
  .gen_management_assert(is.character(protected_models) && !anyNA(protected_models), "protected_models")
  config <- .gen_native_config(config)
  referenced <- .genflow_native_referenced_models(config)
  .genflow_crispasr_remove_model(path, active_model = unique(c(referenced, protected_models)))
})

# Native file metadata and saved references ---------------------------------

.genflow_native_repository <- function(source_url, filename) {
  source_url <- as.character(source_url %||% character())
  filename <- as.character(filename %||% character())
  length_out <- max(length(source_url), length(filename))
  if (!length_out) return(character())

  source_url <- if (length(source_url)) {
    rep_len(source_url, length_out)
  } else {
    rep("", length_out)
  }
  filename <- if (length(filename)) {
    rep_len(filename, length_out)
  } else {
    rep("", length_out)
  }

  vapply(seq_len(length_out), function(index) {
    source <- trimws(source_url[[index]])
    expected_filename <- trimws(filename[[index]])
    if (
      is.na(source) || !nzchar(source) ||
      is.na(expected_filename) || !nzchar(expected_filename)
    ) {
      return("")
    }

    parsed <- tryCatch(
      .genflow_crispasr_parse_hf_url(source),
      error = function(e) NULL
    )
    if (
      is.null(parsed) ||
      !identical(parsed$filename, expected_filename)
    ) {
      return("")
    }
    parsed$repository
  }, character(1), USE.NAMES = FALSE)
}

.genflow_native_reference_input <- function(value) {
  value <- trimws(as.character(value %||% "")[1])
  if (is.na(value)) return("")
  value <- sub(
    "^hf[[:space:]]+download[[:space:]]+",
    "",
    value,
    ignore.case = TRUE
  )
  value <- trimws(value)
  if (nchar(value) >= 2L) {
    first <- substr(value, 1L, 1L)
    last <- substr(value, nchar(value), nchar(value))
    if (first %in% c("\"", "'") && identical(first, last)) {
      value <- trimws(substr(value, 2L, nchar(value) - 1L))
    }
  }
  value
}

.genflow_native_hf_selector <- function(value) {
  value <- .genflow_native_reference_input(value)
  if (is.na(value) || !nzchar(value)) {
    stop("Enter a Hugging Face model reference first.", call. = FALSE)
  }
  if (!.stt_is_crispasr_hf_reference(value)) {
    stop(
      "Use an exact `hf://OWNER/REPOSITORY:FILE` reference or a ",
      "Hugging Face `/blob/main/FILE` URL.",
      call. = FALSE
    )
  }
  reference <- .stt_parse_crispasr_hf_reference(value)
  filename <- .genflow_crispasr_validate_filename(reference$file)
  paste0("hf://", reference$repository, ":", filename)
}

.genflow_native_referenced_models <- function(
  config = NULL,
  setup_names = NULL,
  agent_names = NULL,
  setup_reader = function(name) get_setup(name, assign = FALSE),
  agent_reader = function(name) get_agent(name, assign = FALSE)
) {
  saved_config <- gen_local_config()
  config <- config %||% saved_config
  referenced <- c(
    saved_config$stt_native_model %||% "",
    saved_config$tts_native_model %||% "",
    saved_config$moss_cpp_model %||% "",
    config$stt_native_model %||% "",
    config$tts_native_model %||% "",
    config$moss_cpp_model %||% ""
  )
  model_from_entry <- function(entry) {
    if (!is.list(entry)) return("")
    service <- tryCatch(
      .stt_normalize_service(entry$service %||% ""),
      error = function(e) ""
    )
    if (!identical(service, "local-native")) return("")
    model <- trimws(as.character(entry$model %||% "")[1])
    if (is.na(model)) "" else model
  }
  read_models <- function(names, reader, type) {
    if (is.null(names)) {
      paths <- list.files(.genflow_cache_subdir(paste0(type, "s")),
        pattern = "\\.rds$", full.names = TRUE)
      return(vapply(paths, function(path)
        model_from_entry(.genflow_load_object(path, type)), character(1), USE.NAMES = FALSE))
    }
    names <- unique(trimws(as.character(names %||% character())))
    names <- names[!is.na(names) & nzchar(names)]
    if (!length(names)) return(character())
    vapply(names, function(name) {
      entry <- reader(name)
      model_from_entry(entry)
    }, character(1), USE.NAMES = FALSE)
  }

  referenced <- c(
    referenced,
    read_models(setup_names, setup_reader, "setup"),
    read_models(agent_names, agent_reader, "agent")
  )
  referenced <- unique(trimws(as.character(referenced)))
  referenced[
    !is.na(referenced) &
      nzchar(referenced) &
      tolower(referenced) != "auto"
  ]
}
