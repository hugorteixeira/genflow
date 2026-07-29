# Large-audio STT orchestration ---------------------------------------------

# This file owns only media preparation, chunk persistence, retry/resume, and
# orchestration. Interpretation of speaker labels and overlap text belongs to
# `.stt_reconcile_chunk_results()` in the STT reconciliation module.

#' @keywords internal
#' @noRd
.stt_chunk_validate_options <- function(chunking = c("auto", "never"),
                                        chunk_max_mb = NULL,
                                        chunk_bitrate_kbps = 48,
                                        chunk_segment_seconds = NULL,
                                        chunk_overlap_seconds = 8,
                                        chunk_format = c("auto", "wav", "mp3"),
                                        checkpoint_dir = NULL,
                                        checkpoint_retention = c("all", "results"),
                                        resume = TRUE,
                                        chunk_retry_forever = TRUE,
                                        chunk_max_retries = 20,
                                        chunk_retry_wait_seconds = 2,
                                        output = c("full", "transcript")) {
  chunking <- match.arg(chunking)
  chunk_format <- match.arg(chunk_format)
  checkpoint_retention <- match.arg(checkpoint_retention)
  output <- match.arg(output)

  scalar_number <- function(value, arg, minimum = 0, strict = FALSE) {
    compatible <- (is.numeric(value) && !is.complex(value)) ||
      is.character(value)
    if (!compatible || length(value) != 1L || is.na(value)) {
      stop("`", arg, "` must be a finite numeric scalar.", call. = FALSE)
    }
    value <- suppressWarnings(as.numeric(value))
    invalid <- !is.finite(value) ||
      if (isTRUE(strict)) value <= minimum else value < minimum
    if (invalid) {
      qualifier <- if (isTRUE(strict)) "greater than" else "at least"
      stop(
        "`", arg, "` must be ", qualifier, " ", minimum, ".",
        call. = FALSE
      )
    }
    value
  }

  if (!is.null(chunk_max_mb)) {
    chunk_max_mb <- scalar_number(
      chunk_max_mb,
      "chunk_max_mb",
      minimum = 0,
      strict = TRUE
    )
  }
  chunk_bitrate_kbps <- scalar_number(
    chunk_bitrate_kbps,
    "chunk_bitrate_kbps",
    minimum = 8,
    strict = FALSE
  )
  if (!is.null(chunk_segment_seconds)) {
    chunk_segment_seconds <- scalar_number(
      chunk_segment_seconds,
      "chunk_segment_seconds",
      minimum = 0,
      strict = TRUE
    )
  }
  chunk_overlap_seconds <- scalar_number(
    chunk_overlap_seconds,
    "chunk_overlap_seconds",
    minimum = 0
  )
  if (!is.null(chunk_segment_seconds) &&
      chunk_overlap_seconds >= chunk_segment_seconds) {
    stop(
      "`chunk_overlap_seconds` must be smaller than ",
      "`chunk_segment_seconds`.",
      call. = FALSE
    )
  }

  resume <- .stt_validate_logical_scalar(resume, "resume")
  chunk_retry_forever <- .stt_validate_logical_scalar(
    chunk_retry_forever,
    "chunk_retry_forever"
  )
  chunk_max_retries <- scalar_number(
    chunk_max_retries,
    "chunk_max_retries",
    minimum = 0
  )
  if (chunk_max_retries != floor(chunk_max_retries)) {
    stop("`chunk_max_retries` must be a whole number.", call. = FALSE)
  }
  chunk_retry_wait_seconds <- scalar_number(
    chunk_retry_wait_seconds,
    "chunk_retry_wait_seconds",
    minimum = 0
  )

  if (!is.null(checkpoint_dir)) {
    if (!is.character(checkpoint_dir) || length(checkpoint_dir) != 1L ||
        is.na(checkpoint_dir) || !nzchar(trimws(checkpoint_dir))) {
      stop(
        "`checkpoint_dir` must be NULL or a non-empty directory path.",
        call. = FALSE
      )
    }
    checkpoint_dir <- path.expand(checkpoint_dir)
  }

  list(
    chunking = chunking,
    chunk_max_mb = chunk_max_mb,
    chunk_bitrate_kbps = as.integer(round(chunk_bitrate_kbps)),
    chunk_segment_seconds = chunk_segment_seconds,
    chunk_overlap_seconds = chunk_overlap_seconds,
    chunk_format = chunk_format,
    checkpoint_dir = checkpoint_dir,
    checkpoint_retention = checkpoint_retention,
    resume = resume,
    chunk_retry_forever = chunk_retry_forever,
    chunk_max_retries = as.integer(chunk_max_retries),
    chunk_retry_wait_seconds = chunk_retry_wait_seconds,
    output = output
  )
}

#' Resolve the prepared/chunk media format for an STT service
#'
#' @keywords internal
#' @noRd
.stt_chunk_resolve_format <- function(service,
                                      chunk_format = c("auto", "wav", "mp3")) {
  chunk_format <- match.arg(chunk_format)
  if (!identical(chunk_format, "auto")) return(chunk_format)
  if (identical(service, "local-native")) "wav" else "mp3"
}

#' Validate prepared/chunk media support for a native STT engine
#'
#' CrispASR decodes MP3 directly. The separate moss-transcribe.cpp CLI accepts
#' WAV input, so reject an explicit MP3 chunk request before any inference is
#' attempted instead of relying on an implicit per-part reconversion.
#'
#' @keywords internal
#' @noRd
.stt_chunk_validate_native_format <- function(service,
                                              format,
                                              engine = NULL) {
  if (!identical(service, "local-native") ||
      !identical(format, "mp3") ||
      !identical(engine, "moss-transcribe")) {
    return(invisible(format))
  }
  stop(
    '`chunk_format = "mp3"` is supported by the CrispASR native engine, ',
    "but moss-transcribe.cpp requires WAV input. Use ",
    '`chunk_format = "wav"` (or `"auto"`) or select ',
    '`native_engine = "crispasr"`.',
    call. = FALSE
  )
}

#' @keywords internal
#' @noRd
.stt_chunk_object_fingerprint <- function(value) {
  payload <- tryCatch(
    serialize(value, NULL, ascii = FALSE, version = 2),
    error = function(e) NULL
  )
  fingerprint <- .genflow_raw_md5(payload)
  if (is.null(fingerprint) || !nzchar(fingerprint)) {
    stop("Could not fingerprint the STT chunk configuration.", call. = FALSE)
  }
  fingerprint
}

#' @keywords internal
#' @noRd
.stt_chunk_artifact_signature <- function(path) {
  if (is.null(path) || !length(path) || is.na(path[[1]])) return(NULL)
  path <- path.expand(trimws(as.character(path[[1]])))
  if (!nzchar(path) || !file.exists(path) || dir.exists(path)) return(NULL)
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  info <- suppressWarnings(file.info(path))
  if (!nrow(info) || !is.finite(info$size[[1]]) || info$size[[1]] <= 0) {
    return(NULL)
  }
  signature <- list(
    path = path,
    size_bytes = as.numeric(info$size[[1]]),
    modified_at = format(
      as.POSIXct(info$mtime[[1]], tz = "UTC"),
      "%Y-%m-%dT%H:%M:%OS6Z"
    ),
    changed_at = format(
      as.POSIXct(info$ctime[[1]], tz = "UTC"),
      "%Y-%m-%dT%H:%M:%OS6Z"
    )
  )
  signature$fingerprint <- .stt_chunk_object_fingerprint(signature)
  signature
}

#' Resolve effective local-native artifacts for checkpoint invalidation
#'
#' This is deliberately best-effort: runtime validation remains owned by the
#' native adapter. Explicit files and files resolved from saved local settings
#' contribute path/size/mtime/ctime signatures without hashing multi-gigabyte
#' models for every call.
#'
#' @keywords internal
#' @noRd
.stt_chunk_runtime_artifacts <- function(service,
                                         model = NULL,
                                         executable = NULL,
                                         native_engine = NULL,
                                         native_backend = NULL) {
  if (!identical(service, "local-native")) {
    return(list(
      model = .stt_chunk_artifact_signature(model),
      executable = .stt_chunk_artifact_signature(executable)
    ))
  }

  config <- tryCatch(.genflow_read_local_config(), error = function(e) list())
  requested_model <- trimws(as.character(model %||% "")[1])
  if (is.na(requested_model)) requested_model <- ""
  backend_input <- if (!is.null(native_backend)) {
    native_backend
  } else if (nzchar(requested_model) &&
             !identical(tolower(requested_model), "auto")) {
    ""
  } else {
    NULL
  }
  engine <- tryCatch(
    .stt_resolve_native_engine(
      native_engine = native_engine,
      executable = executable,
      model = model,
      native_backend = backend_input,
      config = config
    ),
    error = function(e) ""
  )
  executable_path <- if (nzchar(engine)) {
    tryCatch(
      .stt_resolve_native_executable(
        engine,
        executable = executable,
        config = config
      ),
      error = function(e) ""
    )
  } else {
    as.character(executable %||% "")[1]
  }

  model_value <- tryCatch(
    .stt_native_setting(
      model,
      field = "stt_native_model",
      env = "GENFLOW_STT_NATIVE_MODEL",
      config = config
    ),
    error = function(e) as.character(model %||% "")[1]
  )
  model_path <- ""
  if (!is.na(model_value) && nzchar(model_value) &&
      !identical(tolower(model_value), "auto") &&
      !.stt_is_crispasr_hf_reference(model_value)) {
    catalog_filename <- identical(model_value, basename(model_value)) &&
      grepl("\\.(?:gguf|bin)$", model_value, ignore.case = TRUE, perl = TRUE)
    if (catalog_filename) {
      model_path <- tryCatch(
        .genflow_crispasr_managed_model(model_value),
        error = function(e) ""
      )
    } else {
      model_path <- path.expand(model_value)
    }
  }
  backend <- tryCatch(
    .stt_validate_native_backend(.stt_native_setting(
      backend_input,
      field = "stt_native_backend",
      env = "GENFLOW_STT_NATIVE_BACKEND",
      config = config
    )),
    error = function(e) ""
  )
  if (identical(engine, "moss-transcribe") ||
      backend %in% c("moss", "moss-transcribe")) {
    backend <- "moss-diarize"
  }
  source_hint <- if (nzchar(model_path) && file.exists(model_path)) {
    tryCatch(
      .genflow_crispasr_read_source(model_path),
      error = function(e) ""
    )
  } else {
    ""
  }
  inferred_backend <- tryCatch(
    .stt_crispasr_backend_from_model(
      if (nzchar(model_path)) model_path else model_value,
      source = source_hint
    ),
    error = function(e) ""
  )
  if (nzchar(inferred_backend)) backend <- inferred_backend

  list(
    engine = if (nzchar(engine)) engine else NULL,
    backend = if (nzchar(backend)) backend else NULL,
    model_value = if (!is.na(model_value) && nzchar(model_value)) {
      model_value
    } else {
      NULL
    },
    model = .stt_chunk_artifact_signature(model_path),
    executable = .stt_chunk_artifact_signature(executable_path)
  )
}

#' Resolve model-owned chunk duration limits
#'
#' Keep these limits separate from transport size limits. Unknown backends
#' deliberately retain whole-file behavior.
#'
#' @keywords internal
#' @noRd
.stt_chunk_model_policy <- function(service, runtime_artifacts) {
  backend <- tolower(trimws(as.character(
    runtime_artifacts$backend %||% ""
  )[1]))
  model_value <- as.character(runtime_artifacts$model_value %||% "")[1]
  model_backend <- tryCatch(
    .stt_crispasr_backend_from_model(model_value),
    error = function(e) ""
  )
  is_moss_diarize <- identical(service, "local-native") &&
    (identical(backend, "moss-diarize") ||
      identical(model_backend, "moss-diarize"))
  if (is_moss_diarize) {
    return(list(
      model_segment_seconds = 3600,
      decision_reason = "moss-diarize-context-window"
    ))
  }
  list(
    model_segment_seconds = NULL,
    decision_reason = NULL
  )
}

#' @keywords internal
#' @noRd
.stt_chunk_file_fingerprint <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path) ||
      !file.exists(path) || dir.exists(path)) {
    return("")
  }
  info <- suppressWarnings(file.info(path))
  if (!nrow(info) || !is.finite(info$size[[1]]) || info$size[[1]] <= 0) {
    return("")
  }
  unname(as.character(tools::md5sum(path)[[1]]))
}

#' @keywords internal
#' @noRd
.stt_chunk_nonempty_file <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path) ||
      !file.exists(path) || dir.exists(path)) {
    return(FALSE)
  }
  size <- suppressWarnings(as.numeric(file.info(path)$size[[1]]))
  is.finite(size) && size > 0
}

#' @keywords internal
#' @noRd
.stt_chunk_transport_limit <- function(service, chunk_max_mb = NULL) {
  if (!is.null(chunk_max_mb)) {
    return(as.numeric(chunk_max_mb) * 1024^2)
  }
  as.numeric(.stt_max_local_file_bytes(service))
}

#' @keywords internal
#' @noRd
.stt_chunk_ffmpeg <- function() {
  executable <- Sys.which("ffmpeg")
  if (!nzchar(executable)) {
    stop(
      "ffmpeg was not found on PATH; it is required for large-audio STT ",
      "preparation and chunking.",
      call. = FALSE
    )
  }
  executable
}

#' @keywords internal
#' @noRd
.stt_chunk_system2 <- function(command, args) {
  suppressWarnings(system2(
    command,
    args,
    stdout = TRUE,
    stderr = TRUE
  ))
}

#' @keywords internal
#' @noRd
.stt_chunk_run_ffmpeg <- function(args) {
  output <- .stt_chunk_system2(
    .stt_chunk_ffmpeg(),
    vapply(args, shQuote, character(1))
  )
  status <- attr(output, "status") %||% 0L
  if (!is.null(status) && !is.na(status) && status != 0L) {
    detail <- paste(utils::head(output, 8L), collapse = " | ")
    stop(
      "ffmpeg failed while preparing STT audio",
      if (nzchar(detail)) paste0(": ", detail) else "",
      call. = FALSE
    )
  }
  invisible(output)
}

#' @keywords internal
#' @noRd
.stt_chunk_prepare_media <- function(source,
                                     target,
                                     format = c("wav", "mp3"),
                                     bitrate_kbps = 48) {
  format <- match.arg(format)
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
  temporary <- file.path(
    dirname(target),
    paste0(
      tools::file_path_sans_ext(basename(target)),
      ".partial.",
      format
    )
  )
  on.exit(if (file.exists(temporary)) unlink(temporary, force = TRUE), add = TRUE)

  codec <- if (identical(format, "wav")) {
    c("-c:a", "pcm_s16le")
  } else {
    c("-c:a", "libmp3lame", "-b:a", paste0(bitrate_kbps, "k"))
  }
  .stt_chunk_run_ffmpeg(c(
    "-nostdin", "-y", "-loglevel", "error",
    "-i", source,
    "-vn", "-ac", "1", "-ar", "16000",
    codec,
    temporary
  ))
  if (!.stt_chunk_nonempty_file(temporary)) {
    stop("ffmpeg produced an empty prepared STT audio file.", call. = FALSE)
  }
  .stt_atomic_replace(temporary, target)
  invisible(target)
}

#' @keywords internal
#' @noRd
.stt_chunk_extract_media <- function(source,
                                     target,
                                     start_seconds,
                                     duration_seconds,
                                     format = c("wav", "mp3"),
                                     bitrate_kbps = 48) {
  format <- match.arg(format)
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
  temporary <- file.path(
    dirname(target),
    paste0(
      tools::file_path_sans_ext(basename(target)),
      ".partial.",
      format
    )
  )
  on.exit(if (file.exists(temporary)) unlink(temporary, force = TRUE), add = TRUE)
  output_args <- if (identical(format, "wav")) {
    c("-vn", "-ac", "1", "-ar", "16000", "-c:a", "pcm_s16le")
  } else {
    # Remote-service MP3 is already normalized to mono/16 kHz by
    # `.stt_chunk_prepare_media()`. Stream-copying it avoids a second lossy
    # encode for every chunk and preserves the configured bitrate exactly.
    c("-vn", "-c:a", "copy")
  }
  .stt_chunk_run_ffmpeg(c(
    "-nostdin", "-y", "-loglevel", "error",
    "-ss", base::format(start_seconds, scientific = FALSE, trim = TRUE),
    "-i", source,
    "-t", base::format(duration_seconds, scientific = FALSE, trim = TRUE),
    output_args,
    temporary
  ))
  if (!.stt_chunk_nonempty_file(temporary)) {
    stop("ffmpeg produced an empty STT audio chunk.", call. = FALSE)
  }
  .stt_atomic_replace(temporary, target)
  invisible(target)
}

#' @keywords internal
#' @noRd
.stt_chunk_starts <- function(duration_seconds,
                              segment_seconds,
                              overlap_seconds) {
  duration_seconds <- as.numeric(duration_seconds)
  segment_seconds <- as.numeric(segment_seconds)
  overlap_seconds <- as.numeric(overlap_seconds)
  if (!is.finite(duration_seconds) || duration_seconds <= 0 ||
      !is.finite(segment_seconds) || segment_seconds <= 0 ||
      !is.finite(overlap_seconds) || overlap_seconds < 0 ||
      overlap_seconds >= segment_seconds) {
    stop("Invalid duration or overlap for STT chunking.", call. = FALSE)
  }

  starts <- 0
  while (utils::tail(starts, 1L) + segment_seconds < duration_seconds) {
    next_start <- utils::tail(starts, 1L) +
      segment_seconds - overlap_seconds
    if (next_start <= utils::tail(starts, 1L)) {
      stop("STT chunk overlap does not advance the input.", call. = FALSE)
    }
    starts <- c(starts, next_start)
  }
  starts
}

#' @keywords internal
#' @noRd
.stt_chunk_manifest_path <- function(run_dir) {
  file.path(run_dir, "manifest.rds")
}

#' @keywords internal
#' @noRd
.stt_chunk_lock_path <- function(run_dir) {
  file.path(run_dir, ".stt-lock")
}

#' @keywords internal
#' @noRd
.stt_chunk_lock_host <- function() {
  host <- suppressWarnings(as.character(Sys.info()[["nodename"]])[1])
  if (is.na(host) || !nzchar(trimws(host))) host <- "unknown"
  host
}

#' @keywords internal
#' @noRd
.stt_chunk_process_exists <- function(pid) {
  pid <- suppressWarnings(as.integer(pid)[1])
  if (is.na(pid) || pid <= 0L) return(FALSE)
  if (identical(pid, as.integer(Sys.getpid()))) return(TRUE)

  exported <- "process_exists" %in% getNamespaceExports("processx")
  if (exported) {
    return(tryCatch(
      isTRUE(getExportedValue("processx", "process_exists")(pid)),
      error = function(e) NA
    ))
  }
  if (identical(.Platform$OS.type, "unix")) {
    return(tryCatch(
      isTRUE(tools::pskill(pid, signal = 0L)),
      error = function(e) NA
    ))
  }
  NA
}

#' @keywords internal
#' @noRd
.stt_chunk_lock_owner <- function(lock_path) {
  .stt_chunk_read_rds(file.path(lock_path, "owner.rds"))
}

#' @keywords internal
#' @noRd
.stt_chunk_lock_state <- function(lock_path,
                                  stale_after_seconds = 12 * 60 * 60) {
  if (!dir.exists(lock_path)) {
    return(list(state = "absent", owner = NULL, age_seconds = Inf))
  }
  if (nzchar(Sys.readlink(lock_path))) {
    return(list(state = "unsafe", owner = NULL, age_seconds = NA_real_))
  }

  owner <- .stt_chunk_lock_owner(lock_path)
  created_at <- suppressWarnings(as.numeric(owner$created_at %||% NA_real_)[1])
  if (!is.finite(created_at)) {
    info <- suppressWarnings(file.info(lock_path))
    created_at <- if (nrow(info)) {
      as.numeric(info$mtime[[1]])
    } else {
      NA_real_
    }
  }
  age <- if (is.finite(created_at)) {
    max(0, as.numeric(Sys.time()) - created_at)
  } else {
    0
  }

  owner_host <- trimws(as.character(owner$host %||% "")[1])
  owner_pid <- suppressWarnings(as.integer(owner$pid %||% NA_integer_)[1])
  same_host <- nzchar(owner_host) &&
    !identical(owner_host, "unknown") &&
    identical(owner_host, .stt_chunk_lock_host())
  if (same_host && !is.na(owner_pid) && owner_pid > 0L) {
    process_exists <- .stt_chunk_process_exists(owner_pid)
    if (!is.na(process_exists)) {
      return(list(
        state = if (isTRUE(process_exists)) "active" else "stale",
        owner = owner,
        age_seconds = age
      ))
    }
  }

  known_foreign_host <- nzchar(owner_host) &&
    !identical(owner_host, "unknown") &&
    !isTRUE(same_host)
  if (known_foreign_host) {
    return(list(
      state = "active",
      owner = owner,
      age_seconds = age
    ))
  }

  stale_after_seconds <- suppressWarnings(
    as.numeric(stale_after_seconds)[1]
  )
  if (!is.finite(stale_after_seconds) || stale_after_seconds <= 0) {
    stale_after_seconds <- 12 * 60 * 60
  }
  list(
    state = if (age >= stale_after_seconds) "stale" else "active",
    owner = owner,
    age_seconds = age
  )
}

#' Acquire exclusive ownership of one STT checkpoint run
#'
#' The lock is an atomically-created directory containing an owner PID, host,
#' creation time, and unguessable-enough per-call token. A same-host lock is
#' stale only when its process no longer exists; foreign-host or incomplete
#' locks are handled conservatively: a known foreign-host lock is never
#' reclaimed automatically, while an incomplete lock may be recovered after
#' the age threshold. Callers must retain the returned handle until the
#' complete transcription has finished and release it with
#' `.stt_chunk_release_lock()`.
#'
#' @keywords internal
#' @noRd
.stt_chunk_acquire_lock <- function(run_dir,
                                    stale_after_seconds = 12 * 60 * 60,
                                    max_race_attempts = 8L) {
  run_dir <- path.expand(as.character(run_dir)[1])
  if (!dir.exists(run_dir) || nzchar(Sys.readlink(run_dir))) {
    stop(
      "The STT checkpoint run directory is missing or is a symbolic link.",
      call. = FALSE
    )
  }
  lock_path <- .stt_chunk_lock_path(run_dir)
  max_race_attempts <- max(1L, as.integer(max_race_attempts)[1])

  for (attempt in seq_len(max_race_attempts)) {
    if (isTRUE(dir.create(
      lock_path,
      recursive = FALSE,
      showWarnings = FALSE
    ))) {
      token <- .stt_chunk_object_fingerprint(list(
        path = normalizePath(run_dir, winslash = "/", mustWork = TRUE),
        pid = Sys.getpid(),
        created_at = as.numeric(Sys.time()),
        elapsed = unname(proc.time()[["elapsed"]])
      ))
      owner <- list(
        schema_version = 1L,
        pid = as.integer(Sys.getpid()),
        host = .stt_chunk_lock_host(),
        created_at = as.numeric(Sys.time()),
        token = token
      )
      owner_path <- file.path(lock_path, "owner.rds")
      written <- tryCatch({
        .genflow_atomic_save_rds(owner, owner_path)
        TRUE
      }, error = function(e) FALSE)
      if (!written) {
        unlink(lock_path, recursive = TRUE, force = TRUE)
        stop("Could not persist STT checkpoint lock ownership.", call. = FALSE)
      }
      return(list(
        path = lock_path,
        token = token,
        pid = owner$pid,
        host = owner$host
      ))
    }

    state <- .stt_chunk_lock_state(
      lock_path,
      stale_after_seconds = stale_after_seconds
    )
    if (identical(state$state, "unsafe")) {
      stop(
        "Refusing to use an STT checkpoint lock that is a symbolic link.",
        call. = FALSE
      )
    }
    if (identical(state$state, "active")) {
      owner_pid <- suppressWarnings(
        as.integer(state$owner$pid %||% NA_integer_)[1]
      )
      detail <- if (!is.na(owner_pid) && owner_pid > 0L) {
        paste0(" (owner PID ", owner_pid, ")")
      } else {
        ""
      }
      stop(
        "This STT checkpoint run is already in use", detail, ".",
        call. = FALSE
      )
    }
    if (!identical(state$state, "stale")) next

    tombstone <- paste0(
      lock_path,
      ".stale-",
      .stt_chunk_object_fingerprint(list(
        pid = Sys.getpid(),
        attempt = attempt,
        time = as.numeric(Sys.time())
      ))
    )
    if (isTRUE(file.rename(lock_path, tombstone))) {
      unlink(tombstone, recursive = TRUE, force = TRUE)
    }
  }
  stop(
    "Could not acquire the STT checkpoint lock after concurrent retries.",
    call. = FALSE
  )
}

#' @keywords internal
#' @noRd
.stt_chunk_release_lock <- function(lock) {
  if (!is.list(lock)) return(invisible(FALSE))
  lock_path <- path.expand(as.character(lock$path %||% "")[1])
  token <- as.character(lock$token %||% "")[1]
  if (!nzchar(lock_path) || !nzchar(token) ||
      !dir.exists(lock_path) || nzchar(Sys.readlink(lock_path))) {
    return(invisible(FALSE))
  }
  owner <- .stt_chunk_lock_owner(lock_path)
  if (!is.list(owner) ||
      !identical(as.character(owner$token %||% ""), token)) {
    return(invisible(FALSE))
  }
  unlink(lock_path, recursive = TRUE, force = TRUE)
  invisible(!dir.exists(lock_path))
}

#' @keywords internal
#' @noRd
.stt_chunk_read_rds <- function(path) {
  if (!.stt_chunk_nonempty_file(path)) return(NULL)
  tryCatch(readRDS(path), error = function(e) NULL)
}

#' @keywords internal
#' @noRd
.stt_chunk_manifest_valid <- function(manifest, key) {
  is.list(manifest) &&
    identical(manifest$schema_version, 2L) &&
    identical(as.character(manifest$key %||% ""), as.character(key)) &&
    is.list(manifest$parts)
}

#' Prune superseded STT checkpoint runs after a successful transcription
#'
#' Only direct child directories named exactly `run-<hex>` with a matching,
#' valid manifest for the same source recording are eligible. The current run
#' and the requested number of newest previous runs are retained. Symbolic
#' links and actively locked runs are always skipped.
#'
#' @keywords internal
#' @noRd
.stt_chunk_prune_runs <- function(checkpoint_dir,
                                  current_run_dir,
                                  keep_previous = 1L) {
  checkpoint_input <- path.expand(as.character(checkpoint_dir)[1])
  current_input <- path.expand(as.character(current_run_dir)[1])
  keep_previous <- suppressWarnings(as.integer(keep_previous)[1])
  if (is.na(keep_previous) || keep_previous < 0L) {
    stop("`keep_previous` must be a non-negative integer.", call. = FALSE)
  }
  if (!dir.exists(checkpoint_input) || nzchar(Sys.readlink(checkpoint_input)) ||
      !dir.exists(current_input) || nzchar(Sys.readlink(current_input))) {
    return(invisible(character()))
  }

  checkpoint_dir <- normalizePath(
    checkpoint_input,
    winslash = "/",
    mustWork = TRUE
  )
  current_run_dir <- normalizePath(
    current_input,
    winslash = "/",
    mustWork = TRUE
  )
  if (!identical(dirname(current_run_dir), checkpoint_dir) ||
      !grepl("^run-[0-9a-f]+$", basename(current_run_dir), perl = TRUE)) {
    return(invisible(character()))
  }
  current_key <- sub("^run-", "", basename(current_run_dir))
  current_manifest <- .stt_chunk_read_rds(
    .stt_chunk_manifest_path(current_run_dir)
  )
  if (!.stt_chunk_manifest_valid(current_manifest, current_key)) {
    return(invisible(character()))
  }
  current_source <- as.character(
    current_manifest$source_fingerprint %||% ""
  )[1]
  if (!nzchar(current_source)) return(invisible(character()))

  entries <- list.files(
    checkpoint_dir,
    all.files = TRUE,
    full.names = TRUE,
    no.. = TRUE
  )
  records <- lapply(entries, function(path) {
    name <- basename(path)
    info <- suppressWarnings(file.info(path))
    if (!grepl("^run-[0-9a-f]+$", name, perl = TRUE) ||
        !nrow(info) || !isTRUE(info$isdir[[1]]) ||
        nzchar(Sys.readlink(path))) {
      return(NULL)
    }
    key <- sub("^run-", "", name)
    manifest <- .stt_chunk_read_rds(.stt_chunk_manifest_path(path))
    if (!.stt_chunk_manifest_valid(manifest, key)) return(NULL)
    source_fingerprint <- as.character(
      manifest$source_fingerprint %||% ""
    )[1]
    if (!identical(source_fingerprint, current_source)) return(NULL)
    updated <- tryCatch(
      suppressWarnings(as.numeric(as.POSIXct(
        manifest$updated_at %||% NA_character_,
        tz = "UTC"
      ))),
      error = function(e) NA_real_
    )
    if (!is.finite(updated)) updated <- as.numeric(info$mtime[[1]])
    list(
      path = normalizePath(path, winslash = "/", mustWork = TRUE),
      updated = updated,
      source_fingerprint = source_fingerprint
    )
  })
  records <- Filter(Negate(is.null), records)
  if (!length(records)) return(invisible(character()))

  paths <- vapply(records, `[[`, character(1), "path")
  updated <- vapply(records, `[[`, numeric(1), "updated")
  previous <- paths[paths != current_run_dir]
  if (length(previous)) {
    previous_updated <- updated[match(previous, paths)]
    previous <- previous[order(
      previous_updated,
      basename(previous),
      decreasing = TRUE,
      na.last = TRUE
    )]
  }
  keep <- unique(c(
    current_run_dir,
    utils::head(previous, keep_previous)
  ))
  candidates <- setdiff(paths, keep)
  deleted <- character()

  for (path in candidates) {
    lock <- tryCatch(
      .stt_chunk_acquire_lock(path),
      error = function(e) NULL
    )
    if (!is.list(lock)) next
    key <- sub("^run-", "", basename(path))
    manifest <- .stt_chunk_read_rds(.stt_chunk_manifest_path(path))
    safe <- !nzchar(Sys.readlink(path)) &&
      identical(dirname(path), checkpoint_dir) &&
      .stt_chunk_manifest_valid(manifest, key) &&
      identical(
        as.character(manifest$source_fingerprint %||% "")[1],
        current_source
      )
    if (safe) {
      unlink(path, recursive = TRUE, force = TRUE)
      if (!dir.exists(path)) {
        deleted <- c(deleted, basename(path))
        next
      }
    }
    .stt_chunk_release_lock(lock)
  }
  invisible(deleted)
}

#' Remove only checkpoint-owned prepared/chunk media after successful STT
#'
#' The current run identifies the source recording. Every eligible run must be
#' a direct, non-symlink child of the checkpoint root with a valid manifest for
#' that same source fingerprint. Only the exact manifest-owned `prepared.*`
#' and `part_NNNN.*` regular files are removed; manifests and result RDS files
#' remain reusable.
#'
#' @keywords internal
#' @noRd
.stt_chunk_cleanup_checkpoint_media <- function(checkpoint_dir,
                                                current_run_dir) {
  checkpoint_input <- as.character(checkpoint_dir)[1]
  current_input <- as.character(current_run_dir)[1]
  if (is.na(checkpoint_input) || !nzchar(checkpoint_input) ||
      is.na(current_input) || !nzchar(current_input)) {
    stop("STT checkpoint cleanup requires non-empty checkpoint/run paths.")
  }
  checkpoint_input <- path.expand(checkpoint_input)
  current_input <- path.expand(current_input)
  if (!dir.exists(checkpoint_input) ||
      nzchar(Sys.readlink(checkpoint_input))) {
    stop("The STT checkpoint root is missing or is a symbolic link.")
  }
  if (!dir.exists(current_input) ||
      nzchar(Sys.readlink(current_input))) {
    stop("The current STT checkpoint run is missing or is a symbolic link.")
  }

  checkpoint_dir <- normalizePath(
    checkpoint_input,
    winslash = "/",
    mustWork = TRUE
  )
  current_run_dir <- normalizePath(
    current_input,
    winslash = "/",
    mustWork = TRUE
  )
  if (!identical(dirname(current_run_dir), checkpoint_dir) ||
      !grepl("^run-[0-9a-f]+$", basename(current_run_dir), perl = TRUE)) {
    stop(
      "The current STT checkpoint run is not a safe direct run-* child."
    )
  }
  current_key <- sub("^run-", "", basename(current_run_dir))
  current_manifest <- .stt_chunk_read_rds(
    .stt_chunk_manifest_path(current_run_dir)
  )
  if (!.stt_chunk_manifest_valid(current_manifest, current_key)) {
    stop("The current STT checkpoint manifest is invalid.")
  }
  source_fingerprint <- as.character(
    current_manifest$source_fingerprint %||% ""
  )[1]
  if (!nzchar(source_fingerprint)) {
    stop("The current STT checkpoint manifest has no source fingerprint.")
  }

  entries <- list.files(
    checkpoint_dir,
    all.files = TRUE,
    full.names = TRUE,
    no.. = TRUE
  )
  runs <- Filter(Negate(is.null), lapply(entries, function(path) {
    name <- basename(path)
    info <- suppressWarnings(file.info(path))
    if (!grepl("^run-[0-9a-f]+$", name, perl = TRUE) ||
        !nrow(info) || !isTRUE(info$isdir[[1]]) ||
        nzchar(Sys.readlink(path))) {
      return(NULL)
    }
    normalized <- normalizePath(path, winslash = "/", mustWork = TRUE)
    if (!identical(dirname(normalized), checkpoint_dir)) return(NULL)
    key <- sub("^run-", "", name)
    manifest <- .stt_chunk_read_rds(.stt_chunk_manifest_path(normalized))
    if (!.stt_chunk_manifest_valid(manifest, key) ||
        !identical(
          as.character(manifest$source_fingerprint %||% "")[1],
          source_fingerprint
        )) {
      return(NULL)
    }
    normalized
  }))
  if (!(current_run_dir %in% unlist(runs, use.names = FALSE))) {
    stop("The current STT checkpoint run changed during media cleanup.")
  }

  cleanup_run <- function(run_dir, lock) {
    on.exit(.stt_chunk_release_lock(lock), add = TRUE)
    key <- sub("^run-", "", basename(run_dir))
    manifest <- .stt_chunk_read_rds(.stt_chunk_manifest_path(run_dir))
    valid <- !nzchar(Sys.readlink(run_dir)) &&
      identical(dirname(run_dir), checkpoint_dir) &&
      .stt_chunk_manifest_valid(manifest, key) &&
      identical(
        as.character(manifest$source_fingerprint %||% "")[1],
        source_fingerprint
      )
    if (!valid) {
      stop("STT checkpoint manifest changed during media cleanup.")
    }

    format <- as.character(manifest$prepared_format %||% "")[1]
    if (!format %in% c("wav", "mp3")) {
      stop("STT checkpoint manifest has an unsafe prepared media format.")
    }
    candidates <- list(list(
      path = manifest$prepared_path,
      basename = paste0("prepared.", format)
    ))
    if (length(manifest$parts)) {
      candidates <- c(candidates, lapply(
        seq_along(manifest$parts),
        function(index) {
          part <- manifest$parts[[index]]
          if (!is.list(part)) return(NULL)
          list(
            path = part$audio_path,
            basename = sprintf("part_%04d.%s", index, format)
          )
        }
      ))
      candidates <- Filter(Negate(is.null), candidates)
    }

    removed <- character()
    remaining <- character()
    for (candidate in candidates) {
      path <- candidate$path
      if (!is.character(path) || length(path) != 1L ||
          is.na(path) || !nzchar(path)) {
        next
      }
      path <- path.expand(path)
      link_target <- Sys.readlink(path)
      is_link <- !is.na(link_target) && nzchar(link_target)
      artifact_exists <- file.exists(path) || is_link
      if (!identical(basename(path), candidate$basename) ||
          !dir.exists(dirname(path)) || is_link) {
        if (artifact_exists) remaining <- c(remaining, path)
        next
      }
      parent <- normalizePath(
        dirname(path),
        winslash = "/",
        mustWork = TRUE
      )
      if (!identical(parent, run_dir)) {
        if (artifact_exists) remaining <- c(remaining, path)
        next
      }
      info <- suppressWarnings(file.info(path))
      if (!nrow(info) || !file.exists(path)) next
      if (!isTRUE(utils::file_test("-f", path)) || isTRUE(info$isdir[[1]])) {
        remaining <- c(remaining, path)
        next
      }
      .stt_chunk_remove_media_file(path)
      link_target <- Sys.readlink(path)
      still_exists <- file.exists(path) ||
        (!is.na(link_target) && nzchar(link_target))
      if (!still_exists) {
        removed <- c(removed, path)
      } else {
        remaining <- c(remaining, path)
      }
    }
    list(
      deleted = unique(removed),
      remaining = unique(remaining)
    )
  }

  deleted <- character()
  remaining <- character()
  skipped <- character()
  for (run_dir in runs) {
    lock <- tryCatch(
      .stt_chunk_acquire_lock(run_dir),
      error = function(e) NULL
    )
    if (!is.list(lock)) {
      skipped <- c(skipped, basename(run_dir))
      next
    }
    cleaned <- tryCatch(
      cleanup_run(run_dir, lock),
      error = function(e) {
        skipped <<- c(skipped, basename(run_dir))
        list(deleted = character(), remaining = character())
      }
    )
    deleted <- c(deleted, cleaned$deleted)
    remaining <- c(remaining, cleaned$remaining)
  }

  invisible(list(
    deleted = unique(deleted),
    remaining = unique(remaining),
    skipped_runs = unique(skipped)
  ))
}

#' @keywords internal
#' @noRd
.stt_chunk_remove_media_file <- function(path) {
  unlink(path, force = TRUE)
}

#' @keywords internal
#' @noRd
.stt_chunk_write_manifest <- function(manifest, path) {
  manifest$updated_at <- format(
    as.POSIXct(Sys.time(), tz = "UTC"),
    "%Y-%m-%dT%H:%M:%SZ"
  )
  .genflow_atomic_save_rds(manifest, path)
  invisible(manifest)
}

#' @keywords internal
#' @noRd
.stt_chunk_temp_root <- function() {
  tempfile("genflow-stt-chunks-")
}

#' @keywords internal
#' @noRd
.stt_chunk_duration_matches <- function(actual,
                                        expected,
                                        tolerance_seconds = 0.1) {
  actual <- suppressWarnings(as.numeric(actual)[1])
  expected <- suppressWarnings(as.numeric(expected)[1])
  is.finite(actual) &&
    is.finite(expected) &&
    abs(actual - expected) <= max(
      tolerance_seconds,
      abs(expected) * 1e-4
    )
}

#' @keywords internal
#' @noRd
.stt_chunk_prepared_reusable <- function(previous, path) {
  if (!is.list(previous) || !.stt_chunk_nonempty_file(path)) return(FALSE)
  size <- suppressWarnings(as.numeric(file.info(path)$size[[1]]))
  fingerprint <- .stt_chunk_file_fingerprint(path)
  duration <- .stt_audio_duration_seconds(path)
  identical(fingerprint, as.character(previous$prepared_fingerprint %||% "")) &&
    isTRUE(all.equal(
      size,
      suppressWarnings(as.numeric(previous$prepared_size_bytes)[1]),
      tolerance = 0
    )) &&
    .stt_chunk_duration_matches(
      duration,
      previous$input_duration_seconds
    )
}

#' @keywords internal
#' @noRd
.stt_chunk_part_reusable <- function(previous_part,
                                     path,
                                     start_seconds,
                                     requested_duration_seconds = NULL) {
  if (!is.list(previous_part) || !.stt_chunk_nonempty_file(path)) return(FALSE)
  size <- suppressWarnings(as.numeric(file.info(path)$size[[1]]))
  fingerprint <- .stt_chunk_file_fingerprint(path)
  duration <- .stt_audio_duration_seconds(path)
  identical(
    fingerprint,
    as.character(previous_part$audio_fingerprint %||% "")
  ) &&
    isTRUE(all.equal(
      size,
      suppressWarnings(as.numeric(previous_part$size_bytes)[1]),
      tolerance = 0
    )) &&
    .stt_chunk_duration_matches(
      duration,
      previous_part$duration_seconds
    ) &&
    .stt_chunk_duration_matches(
      start_seconds,
      previous_part$start_seconds,
      tolerance_seconds = 1e-6
    ) &&
    (is.null(requested_duration_seconds) ||
      .stt_chunk_duration_matches(
        requested_duration_seconds,
        previous_part$requested_duration_seconds %||%
          previous_part$duration_seconds,
        tolerance_seconds = 1e-6
      ))
}

#' @keywords internal
#' @noRd
.stt_chunk_plan_audio <- function(audio_path,
                                  service,
                                  config_fingerprint,
                                  options,
                                  model_segment_seconds = NULL,
                                  model_decision_reason = NULL,
                                  input_duration_seconds = NA_real_) {
  input_duration_seconds <- suppressWarnings(
    as.numeric(input_duration_seconds)[1]
  )
  if (!is.finite(input_duration_seconds) || input_duration_seconds <= 0) {
    input_duration_seconds <- NA_real_
  }
  if (!is.null(model_segment_seconds)) {
    model_segment_seconds <- suppressWarnings(
      as.numeric(model_segment_seconds)[1]
    )
    if (!is.finite(model_segment_seconds) || model_segment_seconds <= 0) {
      stop(
        "`model_segment_seconds` must be NULL or a positive finite number.",
        call. = FALSE
      )
    }
  }
  if (identical(options$chunking, "never")) {
    return(list(
      chunked = FALSE,
      audio_path = audio_path,
      cleanup_dir = NULL,
      prepared = FALSE,
      decision_reason = "chunking-disabled",
      model_segment_seconds = model_segment_seconds,
      input_duration_seconds = input_duration_seconds
    ))
  }
  if (.stt_is_url(audio_path)) {
    return(list(
      chunked = FALSE,
      audio_path = audio_path,
      cleanup_dir = NULL,
      prepared = FALSE
    ))
  }

  max_bytes <- .stt_chunk_transport_limit(
    service,
    options$chunk_max_mb
  )
  explicitly_segmented <- !is.null(options$chunk_segment_seconds)
  model_segmented <- !is.null(model_segment_seconds)
  only_model_policy <- !is.finite(max_bytes) &&
    !isTRUE(explicitly_segmented) &&
    isTRUE(model_segmented)
  if (only_model_policy && is.na(input_duration_seconds)) {
    stop(
      "Could not determine audio duration required to enforce the selected ",
      "model's safe segment limit. Install `ffprobe` and ensure the input ",
      "media is readable, or set `chunking = \"never\"` to accept the ",
      "model-context risk explicitly.",
      call. = FALSE
    )
  }
  if (only_model_policy &&
      input_duration_seconds <= model_segment_seconds) {
    return(list(
      chunked = FALSE,
      audio_path = audio_path,
      cleanup_dir = NULL,
      prepared = FALSE,
      decision_reason = "within-model-context-window",
      model_segment_seconds = model_segment_seconds,
      input_duration_seconds = input_duration_seconds
    ))
  }
  if (!is.finite(max_bytes) &&
      !isTRUE(explicitly_segmented) &&
      !isTRUE(model_segmented)) {
    return(list(
      chunked = FALSE,
      audio_path = audio_path,
      cleanup_dir = NULL,
      prepared = FALSE
    ))
  }

  source_fingerprint <- .stt_chunk_file_fingerprint(audio_path)
  if (!nzchar(source_fingerprint)) {
    stop("Could not fingerprint the input audio for chunking.", call. = FALSE)
  }
  format <- .stt_chunk_resolve_format(service, options$chunk_format)
  key <- .stt_chunk_object_fingerprint(list(
    schema_version = 2L,
    source_fingerprint = source_fingerprint,
    source_size = as.numeric(file.info(audio_path)$size[[1]]),
    service = service,
    config_fingerprint = config_fingerprint,
    format = format,
    max_bytes = max_bytes,
    bitrate_kbps = options$chunk_bitrate_kbps,
    segment_seconds = options$chunk_segment_seconds,
    overlap_seconds = options$chunk_overlap_seconds,
    model_segment_seconds = model_segment_seconds,
    model_decision_reason = model_decision_reason
  ))

  temporary_root <- is.null(options$checkpoint_dir)
  root <- if (temporary_root) {
    .stt_chunk_temp_root()
  } else {
    options$checkpoint_dir
  }
  plan_succeeded <- FALSE
  run_lock <- NULL
  on.exit({
    if (!plan_succeeded && is.list(run_lock)) {
      .stt_chunk_release_lock(run_lock)
    }
    if (temporary_root && !plan_succeeded && dir.exists(root)) {
      unlink(root, recursive = TRUE, force = TRUE)
    }
  }, add = TRUE)
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(root)) {
    stop("Could not create the STT checkpoint directory.", call. = FALSE)
  }
  run_dir <- if (temporary_root) root else file.path(root, paste0("run-", key))
  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(run_dir)) {
    stop("Could not create the STT checkpoint run directory.", call. = FALSE)
  }
  run_lock <- .stt_chunk_acquire_lock(run_dir)

  manifest_path <- .stt_chunk_manifest_path(run_dir)
  previous <- if (isTRUE(options$resume)) {
    .stt_chunk_read_rds(manifest_path)
  } else {
    NULL
  }
  if (!.stt_chunk_manifest_valid(previous, key)) previous <- NULL

  prepared_path <- file.path(run_dir, paste0("prepared.", format))
  reuse_prepared <- isTRUE(options$resume) &&
    .stt_chunk_prepared_reusable(previous, prepared_path)
  if (!reuse_prepared) {
    .stt_chunk_prepare_media(
      source = audio_path,
      target = prepared_path,
      format = format,
      bitrate_kbps = options$chunk_bitrate_kbps
    )
  }
  prepared_size <- as.numeric(file.info(prepared_path)$size[[1]])
  prepared_fingerprint <- .stt_chunk_file_fingerprint(prepared_path)
  duration_seconds <- .stt_audio_duration_seconds(prepared_path)
  if (!is.finite(prepared_size) || prepared_size <= 0 ||
      !nzchar(prepared_fingerprint) ||
      !is.finite(duration_seconds) || duration_seconds <= 0) {
    stop(
      "Could not validate prepared audio for STT chunking; install ffprobe ",
      "and ensure the prepared media is valid.",
      call. = FALSE
    )
  }

  size_requires_chunking <- is.finite(max_bytes) &&
    prepared_size > max_bytes
  duration_requires_chunking <- isTRUE(explicitly_segmented) &&
    duration_seconds > options$chunk_segment_seconds
  model_requires_chunking <- isTRUE(model_segmented) &&
    duration_seconds > model_segment_seconds
  needs_chunking <- size_requires_chunking ||
    duration_requires_chunking ||
    model_requires_chunking
  active_reasons <- c(
    if (size_requires_chunking) "transport-size-limit",
    if (duration_requires_chunking) "explicit-segment-limit",
    if (model_requires_chunking) {
      model_decision_reason %||% "model-segment-limit"
    }
  )
  decision_reason <- if (length(active_reasons)) {
    paste(unique(active_reasons), collapse = "+")
  } else if (isTRUE(model_segmented)) {
    "within-model-context-window"
  } else {
    "within-configured-limits"
  }
  if (!needs_chunking) {
    manifest <- list(
      schema_version = 2L,
      key = key,
      source_fingerprint = source_fingerprint,
      config_fingerprint = config_fingerprint,
      prepared_path = prepared_path,
      chunk_format = options$chunk_format,
      prepared_format = format,
      prepared_fingerprint = prepared_fingerprint,
      prepared_size_bytes = prepared_size,
      input_duration_seconds = duration_seconds,
      effective_max_bytes = max_bytes,
      requested_segment_seconds = options$chunk_segment_seconds,
      model_segment_seconds = model_segment_seconds,
      decision_reason = decision_reason,
      segment_seconds = NULL,
      overlap_seconds = options$chunk_overlap_seconds,
      parts = list()
    )
    .stt_chunk_write_manifest(manifest, manifest_path)
    plan_succeeded <- TRUE
    return(list(
      chunked = FALSE,
      audio_path = prepared_path,
      cleanup_dir = if (temporary_root) run_dir else NULL,
      prepared = TRUE,
      chunk_format = options$chunk_format,
      prepared_format = format,
      prepared_size_bytes = prepared_size,
      input_duration_seconds = duration_seconds,
      decision_reason = decision_reason,
      model_segment_seconds = model_segment_seconds,
      checkpoint_root = if (temporary_root) NULL else root,
      run_dir = run_dir,
      lock = run_lock
    ))
  }

  capacity_seconds <- if (is.finite(max_bytes)) {
    floor(duration_seconds * (max_bytes / prepared_size) * 0.92)
  } else {
    Inf
  }
  requested_segment <- options$chunk_segment_seconds %||% Inf
  model_segment <- model_segment_seconds %||% Inf
  calculated_segment <- min(
    requested_segment,
    capacity_seconds,
    model_segment
  )
  previous_segment <- suppressWarnings(
    as.numeric(previous$segment_seconds %||% NA_real_)[1]
  )
  previous_segment_valid <- is.finite(previous_segment) &&
    previous_segment > options$chunk_overlap_seconds &&
    previous_segment <= requested_segment &&
    previous_segment <= model_segment
  effective_segment <- if (isTRUE(options$resume) &&
      previous_segment_valid) {
    previous_segment
  } else {
    calculated_segment
  }
  if (!is.finite(effective_segment) ||
      effective_segment <= options$chunk_overlap_seconds) {
    stop(
      "The effective STT chunk duration is not larger than its overlap. ",
      "Increase `chunk_max_mb`, lower `chunk_overlap_seconds`, or lower ",
      "`chunk_bitrate_kbps`.",
      call. = FALSE
    )
  }

  extension <- if (identical(format, "wav")) "wav" else "mp3"
  previous_parts <- previous$parts %||% list()
  max_plan_attempts <- 6L
  planning_attempt <- 0L
  parts <- NULL

  repeat {
    planning_attempt <- planning_attempt + 1L
    starts <- .stt_chunk_starts(
      duration_seconds,
      effective_segment,
      options$chunk_overlap_seconds
    )
    durations <- pmin(effective_segment, duration_seconds - starts)
    part_paths <- file.path(
      run_dir,
      sprintf("part_%04d.%s", seq_along(starts), extension)
    )
    candidate_parts <- vector("list", length(starts))
    oversize <- NULL

    for (index in seq_along(starts)) {
      path <- part_paths[[index]]
      old <- if (length(previous_parts) >= index) {
        previous_parts[[index]]
      } else {
        NULL
      }
      reuse_part <- isTRUE(options$resume) &&
        .stt_chunk_part_reusable(
          previous_part = old,
          path = path,
          start_seconds = starts[[index]],
          requested_duration_seconds = durations[[index]]
        )
      if (!reuse_part) {
        .stt_chunk_extract_media(
          source = prepared_path,
          target = path,
          start_seconds = starts[[index]],
          duration_seconds = durations[[index]],
          format = format,
          bitrate_kbps = options$chunk_bitrate_kbps
        )
      }
      size <- as.numeric(file.info(path)$size[[1]])
      fingerprint <- .stt_chunk_file_fingerprint(path)
      actual_duration <- .stt_audio_duration_seconds(path)
      if (!is.finite(size) || size <= 0 || !nzchar(fingerprint) ||
          !is.finite(actual_duration) || actual_duration <= 0) {
        stop(
          sprintf("Prepared STT chunk %d could not be validated.", index),
          call. = FALSE
        )
      }
      if (is.finite(max_bytes) && size > max_bytes) {
        oversize <- list(
          index = index,
          size_bytes = size,
          requested_duration_seconds = durations[[index]]
        )
        break
      }
      reusable_result <- is.list(old) &&
        identical(old$audio_fingerprint, fingerprint)
      result_path <- file.path(
        run_dir,
        sprintf("part_%04d.result.rds", index)
      )
      candidate_parts[[index]] <- list(
        index = as.integer(index),
        audio_path = path,
        audio_fingerprint = fingerprint,
        start_seconds = as.numeric(starts[[index]]),
        duration_seconds = as.numeric(actual_duration),
        requested_duration_seconds = as.numeric(durations[[index]]),
        size_bytes = as.numeric(size),
        result_path = result_path,
        status = if (reusable_result) {
          old$status %||% "pending"
        } else {
          "pending"
        },
        attempts = if (reusable_result) {
          as.integer(old$attempts %||% 0L)
        } else {
          0L
        },
        last_error = if (reusable_result) old$last_error %||% "" else ""
      )
      if (!reusable_result && file.exists(result_path)) {
        unlink(result_path, force = TRUE)
      }
    }

    if (is.null(oversize)) {
      parts <- candidate_parts
      break
    }
    if (planning_attempt >= max_plan_attempts) {
      stop(
        sprintf(
          paste0(
            "Prepared STT chunks still exceed the effective %.2f MB limit ",
            "after %d adaptive planning attempts."
          ),
          max_bytes / 1024^2,
          planning_attempt
        ),
        call. = FALSE
      )
    }

    shrink_ratio <- max_bytes / oversize$size_bytes
    smaller_segment <- floor(
      oversize$requested_duration_seconds * shrink_ratio * 0.9
    )
    smaller_segment <- min(
      smaller_segment,
      effective_segment - max(1, effective_segment * 0.01)
    )
    if (!is.finite(smaller_segment) ||
        smaller_segment <= options$chunk_overlap_seconds) {
      stop(
        paste0(
          "Adaptive STT chunk shrinking cannot fit the effective size ",
          "limit while retaining the requested overlap."
        ),
        call. = FALSE
      )
    }
    effective_segment <- smaller_segment
  }

  manifest <- list(
    schema_version = 2L,
    key = key,
    source_fingerprint = source_fingerprint,
    config_fingerprint = config_fingerprint,
    prepared_path = prepared_path,
    chunk_format = options$chunk_format,
    prepared_format = format,
    prepared_fingerprint = prepared_fingerprint,
    prepared_size_bytes = prepared_size,
    input_duration_seconds = duration_seconds,
    effective_max_bytes = max_bytes,
    requested_segment_seconds = options$chunk_segment_seconds,
    model_segment_seconds = model_segment_seconds,
    decision_reason = decision_reason,
    segment_seconds = as.numeric(effective_segment),
    planning_attempts = as.integer(planning_attempt),
    overlap_seconds = options$chunk_overlap_seconds,
    parts = parts
  )
  .stt_chunk_write_manifest(manifest, manifest_path)

  plan_succeeded <- TRUE
  list(
    chunked = TRUE,
    audio_path = prepared_path,
    cleanup_dir = if (temporary_root) run_dir else NULL,
    prepared = TRUE,
    chunk_format = options$chunk_format,
    prepared_format = format,
    prepared_size_bytes = prepared_size,
    input_duration_seconds = duration_seconds,
    effective_max_bytes = max_bytes,
    segment_seconds = as.numeric(effective_segment),
    planning_attempts = as.integer(planning_attempt),
    overlap_seconds = options$chunk_overlap_seconds,
    decision_reason = decision_reason,
    model_segment_seconds = model_segment_seconds,
    parts = parts,
    manifest = manifest,
    manifest_path = manifest_path,
    checkpoint_dir = if (temporary_root) NULL else run_dir,
    checkpoint_root = if (temporary_root) NULL else root,
    run_dir = run_dir,
    lock = run_lock
  )
}

#' @keywords internal
#' @noRd
.stt_chunk_result_success <- function(result) {
  if (!is.list(result)) return(FALSE)
  status <- toupper(trimws(as.character(result$status_api %||% "")[1]))
  text <- result$response_value
  is.character(text) && length(text) > 0L && !is.na(text[[1]]) &&
    nzchar(trimws(text[[1]])) &&
    (!nzchar(status) || identical(status, "SUCCESS"))
}

#' @keywords internal
#' @noRd
.stt_chunk_result_empty_success <- function(result) {
  if (!is.list(result)) return(FALSE)
  status <- toupper(trimws(as.character(result$status_api %||% "")[1]))
  text <- result$response_value
  is.character(text) && length(text) == 1L && !is.na(text[[1]]) &&
    !nzchar(trimws(text[[1]])) &&
    identical(status, "SUCCESS")
}

#' @keywords internal
#' @noRd
.stt_chunk_result_checkpoint <- function(result,
                                         status,
                                         manifest,
                                         part,
                                         attempts) {
  list(
    schema_version = 1L,
    key = manifest$key,
    config_fingerprint = manifest$config_fingerprint,
    audio_fingerprint = part$audio_fingerprint,
    status = status,
    attempts = as.integer(attempts),
    result = result
  )
}

#' @keywords internal
#' @noRd
.stt_chunk_read_result_checkpoint <- function(path,
                                              manifest,
                                              part,
                                              allow_empty = FALSE) {
  checkpoint <- .stt_chunk_read_rds(path)
  if (!is.list(checkpoint) ||
      !identical(checkpoint$schema_version, 1L) ||
      !identical(
        as.character(checkpoint$key %||% ""),
        as.character(manifest$key %||% "")
      ) ||
      !identical(
        as.character(checkpoint$config_fingerprint %||% ""),
        as.character(manifest$config_fingerprint %||% "")
      ) ||
      !identical(
        as.character(checkpoint$audio_fingerprint %||% ""),
        as.character(part$audio_fingerprint %||% "")
      )) {
    return(NULL)
  }

  status <- as.character(checkpoint$status %||% "")[1]
  valid <- if (identical(status, "done")) {
    .stt_chunk_result_success(checkpoint$result)
  } else if (identical(status, "done_empty") && isTRUE(allow_empty)) {
    .stt_chunk_result_empty_success(checkpoint$result)
  } else {
    FALSE
  }
  if (!valid) return(NULL)
  checkpoint
}

#' @keywords internal
#' @noRd
.stt_chunk_is_tiny_tail <- function(part, index, part_count) {
  if (index != part_count) return(FALSE)
  duration <- suppressWarnings(as.numeric(part$duration_seconds %||% NA)[1])
  size <- suppressWarnings(as.numeric(part$size_bytes %||% NA)[1])
  (is.finite(duration) && duration <= 1) ||
    (is.finite(size) && size <= 8 * 1024)
}

#' @keywords internal
#' @noRd
.stt_chunk_recognized_empty <- function(result) {
  if (.stt_chunk_result_success(result)) return(FALSE)
  message <- tolower(.stt_chunk_error_message(result, fallback = ""))
  nzchar(message) && grepl(
    "empty transcri|without (?:a )?transcript|no transcript",
    message,
    perl = TRUE
  )
}

#' @keywords internal
#' @noRd
.stt_chunk_empty_tail_result <- function(result,
                                         part,
                                         call_arguments) {
  source <- if (is.list(result)) result else list()
  list(
    response_value = "",
    label = source$label %||% call_arguments$label %||%
      sprintf("chunk_%04d", part$index),
    label_cat = source$label_cat %||% call_arguments$label %||%
      sprintf("chunk_%04d", part$index),
    service = source$service %||% call_arguments$service %||% "unknown",
    model = source$model %||% call_arguments$model %||% "default",
    duration = suppressWarnings(as.numeric(source$duration %||% 0)[1]),
    status_api = "SUCCESS",
    status_msg = "OK (tiny empty tail)",
    saved_file = NA_character_,
    audio = part$audio_path,
    content_type = "text",
    metadata = list(
      segments = list(),
      input_duration_seconds = as.numeric(part$duration_seconds)
    )
  )
}

#' @keywords internal
#' @noRd
.stt_chunk_error_message <- function(result, fallback = "STT chunk failed.") {
  if (inherits(result, "error")) return(conditionMessage(result))
  if (!is.list(result)) return(fallback)
  candidates <- c(
    as.character(result$status_msg %||% ""),
    as.character(result$response_value %||% "")
  )
  candidates <- trimws(candidates)
  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
  if (length(candidates)) candidates[[1]] else fallback
}

#' @keywords internal
#' @noRd
.stt_chunk_error_retryable <- function(message) {
  message <- tolower(trimws(as.character(message %||% "")[1]))
  if (!nzchar(message)) return(FALSE)
  permanent <- paste(c(
    "invalid api key", "authentication", "unauthorized", "forbidden",
    "permission denied", "model not found", "unsupported model",
    "unsupported stt service", "unsupported native stt engine",
    "invalid gguf", "bad request", "file not found", "no such file",
    "executable not found", "not installed", "payload too large",
    "file too large", "unsupported audio", "unsupported format",
    "out of memory", "(^|[^a-z])oom([^a-z]|$)"
  ), collapse = "|")
  if (grepl(permanent, message, perl = TRUE)) return(FALSE)
  transient <- paste(c(
    "timeout", "timed out", "temporar", "try again", "rate limit",
    "too many requests", "connection", "network", "dns",
    "reset by peer", "service unavailable", "gateway", "overloaded",
    "http[^0-9]*429", "status[^0-9]*429",
    "http[^0-9]*5[0-9]{2}", "status[^0-9]*5[0-9]{2}"
  ), collapse = "|")
  grepl(transient, message, perl = TRUE)
}

#' @keywords internal
#' @noRd
.stt_chunk_retry_delay <- function(base_seconds, attempts, cap_seconds = 60) {
  if (!is.finite(base_seconds) || base_seconds <= 0) return(0)
  attempts <- max(1L, as.integer(attempts))
  min(cap_seconds, base_seconds * 2^(min(attempts - 1L, 10L)))
}

#' @keywords internal
#' @noRd
.stt_chunk_sleep <- function(seconds) {
  if (is.finite(seconds) && seconds > 0) Sys.sleep(seconds)
  invisible(NULL)
}

#' @keywords internal
#' @noRd
.stt_chunk_call_backend <- function(audio, arguments) {
  result <- NULL
  invisible(utils::capture.output(
    result <- do.call(gen_stt, c(list(audio = audio), arguments))
  ))
  result
}

#' @keywords internal
#' @noRd
.stt_chunk_transcribe_parts <- function(plan,
                                        call_arguments,
                                        options,
                                        timestamps = FALSE) {
  on.exit(.stt_chunk_release_lock(plan$lock), add = TRUE)
  manifest <- plan$manifest
  results <- vector("list", length(manifest$parts))
  resumed <- logical(length(manifest$parts))

  for (index in seq_along(manifest$parts)) {
    part <- manifest$parts[[index]]
    tiny_tail <- .stt_chunk_is_tiny_tail(
      part,
      index = index,
      part_count = length(manifest$parts)
    )
    cached <- if (isTRUE(options$resume)) {
      .stt_chunk_read_result_checkpoint(
        part$result_path,
        manifest = manifest,
        part = part,
        allow_empty = tiny_tail
      )
    } else {
      NULL
    }
    if (is.list(cached)) {
      results[[index]] <- cached$result
      resumed[[index]] <- TRUE
      manifest$parts[[index]]$status <- cached$status
      manifest$parts[[index]]$attempts <- as.integer(
        cached$attempts %||% part$attempts %||% 0L
      )
      manifest$parts[[index]]$last_error <- ""
      .stt_chunk_write_manifest(manifest, plan$manifest_path)
      next
    }

    attempts <- as.integer(part$attempts %||% 0L)
    repeat {
      attempts <- attempts + 1L
      arguments <- call_arguments
      arguments$label <- sprintf(
        "%s_chunk_%04d",
        call_arguments$label %||% "audio",
        index
      )
      arguments$save_txt <- FALSE
      arguments$chunking <- "never"
      arguments$checkpoint_dir <- NULL
      arguments$resume <- FALSE
      arguments$output <- "full"
      result <- tryCatch(
        .stt_chunk_call_backend(
          part$audio_path,
          arguments
        ),
        error = function(e) e
      )
      if (.stt_chunk_result_success(result)) {
        checkpoint <- .stt_chunk_result_checkpoint(
          result = result,
          status = "done",
          manifest = manifest,
          part = part,
          attempts = attempts
        )
        .genflow_atomic_save_rds(checkpoint, part$result_path)
        manifest$parts[[index]]$status <- "done"
        manifest$parts[[index]]$attempts <- attempts
        manifest$parts[[index]]$last_error <- ""
        .stt_chunk_write_manifest(manifest, plan$manifest_path)
        results[[index]] <- result
        break
      }

      if (tiny_tail && .stt_chunk_recognized_empty(result)) {
        empty_result <- .stt_chunk_empty_tail_result(
          result,
          part = part,
          call_arguments = call_arguments
        )
        checkpoint <- .stt_chunk_result_checkpoint(
          result = empty_result,
          status = "done_empty",
          manifest = manifest,
          part = part,
          attempts = attempts
        )
        .genflow_atomic_save_rds(checkpoint, part$result_path)
        manifest$parts[[index]]$status <- "done_empty"
        manifest$parts[[index]]$attempts <- attempts
        manifest$parts[[index]]$last_error <- ""
        .stt_chunk_write_manifest(manifest, plan$manifest_path)
        results[[index]] <- empty_result
        break
      }

      message <- .stt_chunk_error_message(
        result,
        sprintf("STT chunk %d failed.", index)
      )
      manifest$parts[[index]]$status <- "failed"
      manifest$parts[[index]]$attempts <- attempts
      manifest$parts[[index]]$last_error <- message
      .stt_chunk_write_manifest(manifest, plan$manifest_path)

      retryable <- .stt_chunk_error_retryable(message)
      exhausted <- !isTRUE(options$chunk_retry_forever) &&
        attempts > options$chunk_max_retries
      if (!retryable || exhausted) {
        stop(
          sprintf(
            "STT chunk %d failed after %d attempt(s): %s",
            index,
            attempts,
            message
          ),
          call. = FALSE
        )
      }
      .stt_chunk_sleep(.stt_chunk_retry_delay(
        options$chunk_retry_wait_seconds,
        attempts
      ))
    }
  }

  starts <- vapply(
    manifest$parts,
    function(part) as.numeric(part$start_seconds),
    numeric(1)
  )
  for (index in seq_along(results)) {
    if (!is.list(results[[index]]$metadata)) {
      results[[index]]$metadata <- list()
    }
    if (is.null(results[[index]]$metadata$input_duration_seconds)) {
      results[[index]]$metadata$input_duration_seconds <-
        as.numeric(manifest$parts[[index]]$duration_seconds)
    }
  }
  reconciled <- .stt_reconcile_chunk_results(
    results = results,
    chunk_starts_seconds = starts,
    chunk_overlap_seconds = plan$overlap_seconds,
    include_timestamps = timestamps
  )
  normalized <- .stt_normalize_result(reconciled)
  if (is.null(normalized$text) || !nzchar(trimws(normalized$text))) {
    stop("STT chunk reconciliation returned an empty transcript.", call. = FALSE)
  }
  metadata <- normalized$metadata %||% list()
  metadata$chunking <- list(
    schema_version = 1L,
    enabled = TRUE,
    part_count = as.integer(length(results)),
    resumed_part_count = as.integer(sum(resumed)),
    chunk_format = plan$chunk_format,
    prepared_format = plan$prepared_format,
    prepared_size_bytes = plan$prepared_size_bytes,
    input_duration_seconds = plan$input_duration_seconds,
    effective_max_bytes = plan$effective_max_bytes,
    segment_seconds = plan$segment_seconds,
    planning_attempts = plan$planning_attempts %||% 1L,
    overlap_seconds = plan$overlap_seconds,
    decision_reason = plan$decision_reason,
    model_segment_seconds = plan$model_segment_seconds,
    checkpoint_dir = plan$checkpoint_dir,
    checkpoint_retention = options$checkpoint_retention
  )
  list(text = normalized$text, metadata = metadata)
}

#' @keywords internal
#' @noRd
.stt_project_output <- function(result, output = c("full", "transcript")) {
  output <- match.arg(output)
  if (identical(output, "full")) return(result)
  metadata <- result$metadata %||% list()
  segments <- metadata$segments %||% list()
  reconciliation <- metadata$reconciliation %||%
    metadata$chunk_reconciliation %||% NULL
  if (!is.null(metadata$speaker_maps) || !is.null(metadata$boundaries)) {
    reconciliation <- list(
      method = reconciliation,
      speaker_maps = metadata$speaker_maps %||% list(),
      boundaries = metadata$boundaries %||% list()
    )
  }
  projected_metadata <- list(
    segments = segments,
    diarization = .stt_diarization_summary(segments),
    chunking = metadata$chunking %||% NULL,
    reconciliation = reconciliation
  )
  projected_metadata <- projected_metadata[
    !vapply(projected_metadata, is.null, logical(1))
  ]
  projected <- list(
    response_value = result$response_value,
    label = result$label,
    label_cat = result$label_cat,
    service = result$service,
    model = result$model,
    duration = result$duration,
    status_api = result$status_api,
    status_msg = result$status_msg,
    saved_file = result$saved_file,
    audio = result$audio,
    content_type = result$content_type,
    metadata = projected_metadata
  )
  if (!is.null(result$diarized_transcript)) {
    projected <- append(
      projected,
      list(diarized_transcript = result$diarized_transcript),
      after = 1L
    )
    projected <- append(
      projected,
      list(saved_metadata_file = result$saved_metadata_file),
      after = match("saved_file", names(projected))
    )
  }
  projected
}
