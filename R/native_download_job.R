# Background CrispASR model downloads ----------------------------------------

#' Return whether one native-download state is terminal
#'
#' @keywords internal
#' @noRd
.genflow_native_download_state_terminal <- function(state) {
  as.character(state %||% "")[1] %in% c("complete", "error", "cancelled")
}

#' Read one native-download status without lifecycle reconciliation
#'
#' @keywords internal
#' @noRd
.genflow_native_download_status_read <- function(path) {
  path <- path.expand(trimws(as.character(path %||% "")[1]))
  if (!nzchar(path) || !file.exists(path)) return(NULL)
  parsed <- tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.list(parsed)) parsed else NULL
}

#' Resolve the atomic terminal/publication gate for one job
#'
#' @keywords internal
#' @noRd
.genflow_native_download_job_gate_path <- function(job = NULL,
                                                   status_path = NULL) {
  explicit <- if (is.list(job)) job$gate_path else NULL
  explicit <- trimws(as.character(explicit %||% "")[1])
  if (nzchar(explicit)) return(path.expand(explicit))
  if (is.null(status_path) && is.list(job)) status_path <- job$status_path
  status_path <- path.expand(trimws(as.character(status_path %||% "")[1]))
  if (!nzchar(status_path)) {
    stop("A native download status path is required for its gate.", call. = FALSE)
  }
  file.path(dirname(status_path), "terminal.gate")
}

#' Acquire one cross-process native-download gate
#'
#' `dir.create()` is the atomic primitive. The returned opaque token must be
#' supplied when releasing the gate.
#'
#' @keywords internal
#' @noRd
.genflow_native_download_gate_acquire <- function(path,
                                                  timeout_ms = 0L,
                                                  poll_ms = 20L,
                                                  role = "unknown",
                                                  job_id = "") {
  path <- path.expand(trimws(as.character(path %||% "")[1]))
  timeout_ms <- suppressWarnings(as.numeric(timeout_ms)[1])
  poll_ms <- suppressWarnings(as.numeric(poll_ms)[1])
  role <- trimws(as.character(role %||% "unknown")[1])
  job_id <- trimws(as.character(job_id %||% "")[1])
  if (!nzchar(path) || !dir.exists(dirname(path))) {
    stop("Native download gate directory does not exist.", call. = FALSE)
  }
  if (is.na(timeout_ms) ||
      (!is.finite(timeout_ms) && !is.infinite(timeout_ms)) ||
      timeout_ms < 0) {
    stop("`timeout_ms` must be a non-negative number.", call. = FALSE)
  }
  if (!is.finite(poll_ms) || poll_ms <= 0) {
    stop("`poll_ms` must be a positive number.", call. = FALSE)
  }

  started <- unname(proc.time()[["elapsed"]])
  repeat {
    acquired <- isTRUE(dir.create(
      path,
      recursive = FALSE,
      showWarnings = FALSE,
      mode = "0700"
    ))
    if (acquired) {
      token <- paste0(
        Sys.getpid(),
        "-",
        format(Sys.time(), "%Y%m%d%H%M%OS6"),
        "-",
        basename(tempfile("gate-"))
      )
      owner_path <- file.path(path, "owner.rds")
      temporary <- tempfile(".owner.", tmpdir = path, fileext = ".rds")
      owner <- list(
        token = token,
        pid = Sys.getpid(),
        role = role,
        job_id = job_id
      )
      published <- tryCatch({
        saveRDS(owner, temporary, version = 3L)
        file.rename(temporary, owner_path)
      }, error = function(e) FALSE)
      unlink(temporary, force = TRUE)
      if (!isTRUE(published)) {
        unlink(path, recursive = TRUE, force = TRUE)
        stop("Could not publish native download gate ownership.", call. = FALSE)
      }
      return(token)
    }
    elapsed_ms <- (unname(proc.time()[["elapsed"]]) - started) * 1000
    if (is.finite(timeout_ms) && elapsed_ms >= timeout_ms) return(NULL)
    remaining_ms <- if (is.finite(timeout_ms)) {
      max(timeout_ms - elapsed_ms, 0)
    } else {
      poll_ms
    }
    Sys.sleep(min(poll_ms, remaining_ms) / 1000)
  }
}

#' Read validated ownership metadata for one native-download gate
#'
#' @keywords internal
#' @noRd
.genflow_native_download_gate_owner <- function(path) {
  owner_path <- file.path(path, "owner.rds")
  link <- tryCatch(Sys.readlink(owner_path), error = function(e) "")
  is_link <- length(link) == 1L && !is.na(link) && nzchar(link)
  if (is_link || !file.exists(owner_path) || dir.exists(owner_path)) return(NULL)
  owner <- tryCatch(readRDS(owner_path), error = function(e) NULL)
  if (!is.list(owner)) return(NULL)
  token <- trimws(as.character(owner$token %||% "")[1])
  pid <- suppressWarnings(as.integer(owner$pid %||% NA_integer_)[1])
  role <- trimws(as.character(owner$role %||% "")[1])
  if (!nzchar(token) || is.na(pid) || pid < 1L || !nzchar(role)) return(NULL)
  owner$token <- token
  owner$pid <- pid
  owner$role <- role
  owner
}

#' Release a native-download gate owned by the supplied token
#'
#' @keywords internal
#' @noRd
.genflow_native_download_gate_release <- function(path, token) {
  path <- path.expand(trimws(as.character(path %||% "")[1]))
  token <- trimws(as.character(token %||% "")[1])
  if (!nzchar(path) || !nzchar(token) || !dir.exists(path)) {
    return(invisible(FALSE))
  }
  link <- tryCatch(Sys.readlink(path), error = function(e) "")
  is_link <- length(link) == 1L && !is.na(link) && nzchar(link)
  if (is_link || !dir.exists(path)) {
    stop("Refusing to release an invalid native download gate.", call. = FALSE)
  }
  owner <- .genflow_native_download_gate_owner(path)
  if (is.null(owner) || !identical(owner$token, token)) {
    stop("Refusing to release a native download gate owned elsewhere.", call. = FALSE)
  }
  entries <- list.files(
    path,
    all.files = TRUE,
    full.names = TRUE,
    no.. = TRUE
  )
  owner_path <- file.path(path, "owner.rds")
  if (length(entries) != 1L || !identical(entries[[1]], owner_path)) {
    stop("Refusing to release a non-empty native download gate.", call. = FALSE)
  }
  if (unlink(owner_path, recursive = FALSE, force = FALSE) != 0L) {
    stop("Could not remove native download gate ownership.", call. = FALSE)
  }
  status <- unlink(path, recursive = TRUE, force = FALSE)
  if (status != 0L || dir.exists(path)) {
    stop("Could not release the native download gate.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Reclaim a gate abandoned by the confirmed-dead worker
#'
#' The gate must be the exact `terminal.gate` inside this job directory. A
#' cancel/reconcile owner is never reclaimed.
#'
#' @keywords internal
#' @noRd
.genflow_native_download_gate_reclaim_worker <- function(job,
                                                         path,
                                                         grace_ms = 100L) {
  if (.genflow_native_download_job_alive(job)) return(FALSE)
  job_dir <- path.expand(trimws(as.character(job$dir %||% "")[1]))
  if (!nzchar(job_dir) || !dir.exists(job_dir)) return(FALSE)
  job_dir <- normalizePath(job_dir, winslash = "/", mustWork = TRUE)
  path <- normalizePath(path.expand(path), winslash = "/", mustWork = FALSE)
  if (!identical(path, file.path(job_dir, "terminal.gate"))) return(FALSE)
  link <- tryCatch(Sys.readlink(path), error = function(e) "")
  is_link <- length(link) == 1L && !is.na(link) && nzchar(link)
  if (is_link || !dir.exists(path)) return(!dir.exists(path))

  grace_ms <- suppressWarnings(as.numeric(grace_ms)[1])
  if (!is.finite(grace_ms) || grace_ms < 0) grace_ms <- 100
  if (grace_ms > 0) Sys.sleep(grace_ms / 1000)
  if (.genflow_native_download_job_alive(job) || !dir.exists(path)) {
    return(!dir.exists(path))
  }

  entries <- list.files(
    path,
    all.files = TRUE,
    full.names = TRUE,
    no.. = TRUE
  )
  owner <- .genflow_native_download_gate_owner(path)
  if (!length(entries)) {
    return(unlink(path, recursive = TRUE, force = FALSE) == 0L)
  }
  pid <- suppressWarnings(as.integer(job$pid %||% NA_integer_)[1])
  owner_path <- file.path(path, "owner.rds")
  valid_worker <- !is.null(owner) &&
    identical(owner$role, "worker") &&
    !is.na(pid) &&
    identical(owner$pid, pid) &&
    length(entries) == 1L &&
    identical(entries[[1]], owner_path)
  if (!valid_worker) return(FALSE)
  if (unlink(owner_path, recursive = FALSE, force = FALSE) != 0L) return(FALSE)
  unlink(path, recursive = TRUE, force = FALSE) == 0L
}

#' Write one native-download job status atomically
#'
#' @keywords internal
#' @noRd
.genflow_native_download_job_write <- function(path, status) {
  path <- path.expand(trimws(as.character(path %||% "")[1]))
  if (!nzchar(path) || !dir.exists(dirname(path))) {
    stop("Native download job status directory does not exist.", call. = FALSE)
  }
  existing <- .genflow_native_download_status_read(path)
  if (is.list(existing) &&
      .genflow_native_download_state_terminal(existing$state)) {
    return(invisible(existing))
  }
  status$updated_at <- format(
    Sys.time(),
    "%Y-%m-%dT%H:%M:%OS3%z",
    tz = "UTC"
  )
  temporary <- tempfile(
    pattern = ".status.",
    tmpdir = dirname(path),
    fileext = ".json"
  )
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  jsonlite::write_json(
    status,
    temporary,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    pretty = FALSE
  )
  backup <- ""
  if (file.exists(path) && .Platform$OS.type == "windows") {
    backup <- tempfile(
      pattern = ".status-backup.",
      tmpdir = dirname(path),
      fileext = ".json"
    )
    if (!file.rename(path, backup)) {
      stop("Could not preserve the previous download status.", call. = FALSE)
    }
  }
  if (!file.rename(temporary, path)) {
    restored <- !nzchar(backup) ||
      (file.exists(backup) && file.rename(backup, path))
    if (!restored) {
      stop(
        "Could not publish or restore the native download job status.",
        call. = FALSE
      )
    }
    stop("Could not publish native download job status.", call. = FALSE)
  }
  if (nzchar(backup) && file.exists(backup)) unlink(backup, force = TRUE)
  invisible(status)
}

#' Read one native-download job status
#'
#' @keywords internal
#' @noRd
.genflow_native_download_job_read <- function(job) {
  status_path <- if (is.list(job)) job$status_path else job
  status_path <- path.expand(trimws(as.character(status_path %||% "")[1]))
  parsed <- .genflow_native_download_status_read(status_path)
  if (is.null(parsed)) {
    parsed <- list(
      state = "starting",
      stage = "starting",
      message = "Starting background download."
    )
  }

  terminal <- .genflow_native_download_state_terminal(parsed$state)
  has_process <- is.list(job) && inherits(job$process, "process")
  if (!terminal && has_process &&
      !isTRUE(tryCatch(job$process$is_alive(), error = function(e) FALSE))) {
    # A worker may atomically publish its terminal status immediately before
    # exiting. The second read is mandatory after observing the dead process.
    latest <- .genflow_native_download_status_read(status_path)
    if (is.list(latest)) parsed <- latest
    if (.genflow_native_download_state_terminal(parsed$state)) return(parsed)

    gate_path <- .genflow_native_download_job_gate_path(job, status_path)
    gate_token <- .genflow_native_download_gate_acquire(
      gate_path,
      timeout_ms = 250L,
      role = "reconcile",
      job_id = job$id %||% parsed$id %||% ""
    )
    if (is.null(gate_token) &&
        .genflow_native_download_gate_reclaim_worker(job, gate_path)) {
      gate_token <- .genflow_native_download_gate_acquire(
        gate_path,
        timeout_ms = 250L,
        role = "reconcile",
        job_id = job$id %||% parsed$id %||% ""
      )
    }
    if (is.null(gate_token)) {
      latest <- .genflow_native_download_status_read(status_path)
      if (is.list(latest)) parsed <- latest
      return(parsed)
    }
    on.exit(
      .genflow_native_download_gate_release(gate_path, gate_token),
      add = TRUE
    )

    # Re-read under the terminal gate as well. This closes the race with a
    # concurrent cancel operation that acquired the gate before us.
    latest <- .genflow_native_download_status_read(status_path)
    if (is.list(latest)) parsed <- latest
    if (.genflow_native_download_state_terminal(parsed$state)) return(parsed)

    parsed <- .genflow_native_download_job_worker_exit(job, parsed)
    if (nzchar(status_path) && dir.exists(dirname(status_path))) {
      parsed <- .genflow_native_download_job_write(status_path, parsed)
    }
  }
  parsed
}

#' Execute one native model download job
#'
#' This function runs in the background R process started by
#' `.genflow_native_download_job_start()`.
#'
#' @keywords internal
#' @noRd
.genflow_native_download_job_worker <- function(spec_path, status_path) {
  spec <- readRDS(spec_path)
  job_id <- as.character(spec$id %||% "")[1]
  gate_path <- trimws(as.character(spec$gate_path %||% "")[1])
  if (!nzchar(gate_path)) {
    gate_path <- .genflow_native_download_job_gate_path(
      status_path = status_path
    )
  }
  gate_token <- NULL
  release_gate <- function() {
    if (is.null(gate_token)) return(invisible(TRUE))
    .genflow_native_download_gate_release(gate_path, gate_token)
    gate_token <<- NULL
    invisible(TRUE)
  }
  on.exit(release_gate(), add = TRUE)
  ensure_gate <- function() {
    if (!is.null(gate_token)) return(invisible(TRUE))
    gate_token <<- .genflow_native_download_gate_acquire(
      gate_path,
      timeout_ms = Inf,
      role = "worker",
      job_id = job_id
    )
    invisible(TRUE)
  }
  last_status <- NULL
  write_status <- function(status) {
    status$id <- job_id
    written <- .genflow_native_download_job_write(status_path, status)
    if (!.genflow_native_download_state_terminal(written$state)) {
      last_status <<- written
    }
    invisible(written)
  }
  write_terminal <- function(status) {
    ensure_gate()
    written <- write_status(status)
    release_gate()
    invisible(written)
  }

  write_status(list(
    state = "running",
    stage = "resolving",
    message = "Resolving the exact model artifact.",
    bytes_received = 0,
    bytes_total = NULL,
    proportion = NULL
  ))

  progress <- function(update) {
    stage <- as.character(update$stage %||% "downloading")[1]
    if (identical(stage, "publishing")) ensure_gate()
    state <- if (!is.null(gate_token)) "publishing" else "running"
    filename <- as.character(update$filename %||% "")[1]
    received <- suppressWarnings(as.numeric(update$bytes_received)[1])
    total <- suppressWarnings(as.numeric(update$bytes_total)[1])
    proportion <- suppressWarnings(as.numeric(update$proportion)[1])
    if (!is.finite(received)) received <- 0
    if (!is.finite(total)) total <- NULL
    if (!is.finite(proportion)) proportion <- NULL
    write_status(list(
      state = state,
      stage = stage,
      filename = filename,
      message = switch(
        stage,
        resolving = "Resolving the exact model artifact.",
        downloading = "Downloading model data.",
        verifying = "Verifying SHA-256.",
        publishing = "Publishing the verified model and source sidecar.",
        complete = "Finalizing the downloaded model.",
        "Preparing the model."
      ),
      bytes_received = received,
      bytes_total = total,
      proportion = proportion
    ))
  }

  result <- tryCatch(
    .genflow_crispasr_download(
      selector = as.character(spec$selector %||% "")[1],
      backend = as.character(spec$backend %||% "")[1],
      quant = as.character(spec$quant %||% "")[1],
      executable = as.character(spec$executable %||% "")[1],
      progress = progress
    ),
    error = function(e) e
  )
  if (inherits(result, "error")) {
    error_status <- last_status %||% list(
      stage = "error",
      bytes_received = 0,
      bytes_total = NULL,
      proportion = NULL
    )
    error_status <- utils::modifyList(error_status, list(
      state = "error",
      message = conditionMessage(result),
      result = NULL,
      error = list(
        class = class(result)[[1]],
        message = conditionMessage(result)
      )
    ))
    write_terminal(error_status)
    return(invisible(FALSE))
  }

  write_terminal(list(
    state = "complete",
    stage = "complete",
    message = "Model ready.",
    filename = result$filename %||% basename(result$path %||% ""),
    bytes_received = result$size_bytes %||% NULL,
    bytes_total = result$size_bytes %||% NULL,
    proportion = 1,
    result = result
  ))
  invisible(TRUE)
}

#' Locate the background native-download worker script
#'
#' @keywords internal
#' @noRd
.genflow_native_download_job_script <- function() {
  script <- system.file(
    "scripts",
    "crispasr-download.R",
    package = "genflow"
  )
  if (!nzchar(script) || !file.exists(script)) {
    candidate <- file.path(
      getwd(),
      "inst",
      "scripts",
      "crispasr-download.R"
    )
    if (file.exists(candidate)) script <- candidate
  }
  if (!nzchar(script) || !file.exists(script)) {
    stop("The native model download worker script is missing.", call. = FALSE)
  }
  normalizePath(script, winslash = "/", mustWork = TRUE)
}

#' Find the source checkout when running through pkgload
#'
#' @keywords internal
#' @noRd
.genflow_native_download_source_root <- function(script) {
  candidate <- normalizePath(
    file.path(dirname(script), "..", ".."),
    winslash = "/",
    mustWork = FALSE
  )
  if (file.exists(file.path(candidate, "DESCRIPTION")) &&
      file.exists(file.path(candidate, "R", "native_model_cache.R"))) {
    candidate
  } else {
    ""
  }
}

#' Start one background CrispASR model download
#'
#' @keywords internal
#' @noRd
.genflow_native_download_job_start <- function(selector,
                                               backend = "",
                                               quant = "",
                                               executable = "") {
  selector <- trimws(as.character(selector %||% "")[1])
  if (!nzchar(selector)) {
    stop("A native model selector is required.", call. = FALSE)
  }
  script <- .genflow_native_download_job_script()
  job_dir <- tempfile("genflow-native-download-", tmpdir = tempdir())
  if (!dir.create(job_dir, recursive = FALSE, showWarnings = FALSE)) {
    stop("Could not create the native download job directory.", call. = FALSE)
  }
  cleanup_dir <- TRUE
  on.exit({
    if (isTRUE(cleanup_dir)) unlink(job_dir, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  job_id <- paste0(
    format(Sys.time(), "%Y%m%d%H%M%S"),
    "-",
    Sys.getpid(),
    "-",
    basename(job_dir)
  )
  spec_path <- file.path(job_dir, "spec.rds")
  status_path <- file.path(job_dir, "status.json")
  gate_path <- file.path(job_dir, "terminal.gate")
  stdout_path <- file.path(job_dir, "stdout.log")
  stderr_path <- file.path(job_dir, "stderr.log")
  source_root <- .genflow_native_download_source_root(script)
  spec <- list(
    id = job_id,
    selector = selector,
    backend = trimws(as.character(backend %||% "")[1]),
    quant = trimws(as.character(quant %||% "")[1]),
    executable = trimws(as.character(executable %||% "")[1]),
    source_root = source_root,
    library_paths = .libPaths(),
    gate_path = gate_path
  )
  saveRDS(spec, spec_path, version = 3L)
  .genflow_native_download_job_write(status_path, list(
    id = job_id,
    state = "queued",
    stage = "queued",
    message = "Background download queued.",
    bytes_received = 0,
    bytes_total = NULL,
    proportion = NULL
  ))

  rscript <- unname(Sys.which("Rscript"))
  if (!nzchar(rscript)) {
    rscript <- file.path(R.home("bin"), "Rscript")
  }
  if (!file.exists(rscript)) {
    stop("Rscript is required for background model downloads.", call. = FALSE)
  }
  process <- processx::process$new(
    command = rscript,
    args = c("--vanilla", script, spec_path, status_path),
    stdout = stdout_path,
    stderr = stderr_path,
    cleanup = FALSE,
    cleanup_tree = FALSE,
    windows_hide_window = TRUE
  )
  cleanup_dir <- FALSE
  structure(
    list(
      id = job_id,
      dir = normalizePath(job_dir, winslash = "/", mustWork = TRUE),
      pid = process$get_pid(),
      process = process,
      spec_path = spec_path,
      status_path = status_path,
      gate_path = gate_path,
      stdout_path = stdout_path,
      stderr_path = stderr_path
    ),
    class = "genflow_native_download_job"
  )
}

#' Return whether a native-download job is still running
#'
#' @keywords internal
#' @noRd
.genflow_native_download_job_alive <- function(job) {
  is.list(job) &&
    inherits(job$process, "process") &&
    isTRUE(tryCatch(job$process$is_alive(), error = function(e) FALSE))
}

#' Build a terminal worker-exit status without losing progress
#'
#' @keywords internal
#' @noRd
.genflow_native_download_job_worker_exit <- function(job, status) {
  status <- status %||% list()
  exit_status <- tryCatch(
    job$process$get_exit_status(),
    error = function(e) NULL
  )
  stderr <- if (!is.null(job$stderr_path) && file.exists(job$stderr_path)) {
    lines <- tryCatch(
      readLines(job$stderr_path, warn = FALSE, encoding = "UTF-8"),
      error = function(e) character()
    )
    trimws(paste(utils::tail(lines, 8L), collapse = "\n"))
  } else {
    ""
  }
  message <- "Native model download worker exited before completing."
  if (nzchar(stderr)) message <- paste(message, stderr)
  if (is.null(status$stage) || !nzchar(as.character(status$stage)[1])) {
    status$stage <- "error"
  }
  utils::modifyList(status, list(
    state = "error",
    message = message,
    result = NULL,
    error = list(
      class = "worker_exit",
      exit_status = if (is.null(exit_status) ||
          !length(exit_status) ||
          is.na(exit_status[[1]])) {
        NULL
      } else {
        as.integer(exit_status[[1]])
      },
      stderr = if (nzchar(stderr)) stderr else NULL
    )
  ))
}

#' Safely remove partial cache files created by one worker PID
#'
#' @keywords internal
#' @noRd
.genflow_native_download_job_cleanup_parts <- function(job, status = NULL) {
  pid <- suppressWarnings(as.integer(job$pid %||% NA_integer_)[1])
  status <- status %||% .genflow_native_download_job_read(job)
  filename <- trimws(as.character(status$filename %||% "")[1])
  if (is.na(pid) || pid < 1L || !nzchar(filename)) {
    return(invisible(character()))
  }
  filename <- tryCatch(
    .genflow_crispasr_validate_filename(filename),
    error = function(e) ""
  )
  if (!nzchar(filename)) return(invisible(character()))
  cache_dir <- tryCatch(
    .genflow_crispasr_canonical_cache_dir(create = FALSE),
    error = function(e) ""
  )
  if (!nzchar(cache_dir) || !dir.exists(cache_dir)) {
    return(invisible(character()))
  }
  prefixes <- c(
    paste0(".", filename, ".part.", pid, "."),
    paste0(".", filename, ".src.part.", pid, ".")
  )
  entries <- list.files(
    cache_dir,
    all.files = TRUE,
    full.names = TRUE,
    recursive = FALSE,
    no.. = TRUE
  )
  names <- basename(entries)
  entries <- entries[vapply(
    names,
    function(name) any(startsWith(name, prefixes)),
    logical(1)
  )]
  removed <- character()
  for (entry in entries) {
    link <- tryCatch(Sys.readlink(entry), error = function(e) "")
    is_link <- length(link) == 1L && !is.na(link) && nzchar(link)
    if (is_link) next
    info <- suppressWarnings(file.info(entry))
    regular <- file.exists(entry) &&
      nrow(info) == 1L &&
      !isTRUE(info$isdir[[1]])
    if (regular) {
      unlink(entry, recursive = FALSE, force = TRUE)
      if (!file.exists(entry)) removed <- c(removed, entry)
    }
  }
  invisible(removed)
}

#' Cancel one background native model download
#'
#' @keywords internal
#' @noRd
.genflow_native_download_job_cancel <- function(job,
                                                publishing_wait_ms = 5000L) {
  publishing_wait_ms <- suppressWarnings(
    as.integer(publishing_wait_ms)[1]
  )
  if (is.na(publishing_wait_ms) || publishing_wait_ms < 0L) {
    stop("`publishing_wait_ms` must be a non-negative integer.", call. = FALSE)
  }

  status <- .genflow_native_download_job_read(job)
  if (.genflow_native_download_state_terminal(status$state)) {
    return(invisible(status))
  }

  gate_path <- .genflow_native_download_job_gate_path(job)
  gate_token <- .genflow_native_download_gate_acquire(
    gate_path,
    timeout_ms = 0L,
    role = "cancel",
    job_id = job$id %||% status$id %||% ""
  )
  contended <- is.null(gate_token)
  if (contended) {
    gate_token <- .genflow_native_download_gate_acquire(
      gate_path,
      timeout_ms = publishing_wait_ms,
      role = "cancel",
      job_id = job$id %||% status$id %||% ""
    )
    if (is.null(gate_token) &&
        !.genflow_native_download_job_alive(job) &&
        .genflow_native_download_gate_reclaim_worker(job, gate_path)) {
      gate_token <- .genflow_native_download_gate_acquire(
        gate_path,
        timeout_ms = 250L,
        role = "cancel",
        job_id = job$id %||% status$id %||% ""
      )
    }
    if (is.null(gate_token)) {
      latest <- .genflow_native_download_status_read(job$status_path)
      if (is.list(latest) &&
          .genflow_native_download_state_terminal(latest$state)) {
        return(invisible(latest))
      }
      stop(
        "The native model lifecycle gate is busy; the worker was not killed. ",
        "Wait for publication to finish.",
        call. = FALSE
      )
    }
  }
  on.exit(
    .genflow_native_download_gate_release(gate_path, gate_token),
    add = TRUE
  )

  latest <- .genflow_native_download_status_read(job$status_path)
  if (is.list(latest)) status <- latest
  if (.genflow_native_download_state_terminal(status$state)) {
    return(invisible(status))
  }

  # Once another owner was observed, this invocation is wait-only. The worker
  # normally releases the gate only after publishing a terminal status.
  if (contended) {
    if (.genflow_native_download_job_alive(job)) {
      stop(
        "The native model lifecycle gate was busy; the worker was not killed. ",
        "Wait for the current lifecycle transition to finish.",
        call. = FALSE
      )
    }
    failed <- .genflow_native_download_job_worker_exit(job, status)
    failed <- .genflow_native_download_job_write(job$status_path, failed)
    .genflow_native_download_job_cleanup_parts(job, failed)
    return(invisible(failed))
  }

  state <- as.character(status$state %||% "")[1]
  stage <- as.character(status$stage %||% "")[1]
  if (identical(state, "publishing") || identical(stage, "publishing")) {
    stop(
      "The verified model is being published and cannot be cancelled safely.",
      call. = FALSE
    )
  }

  was_alive <- .genflow_native_download_job_alive(job)
  if (was_alive) {
    try(job$process$kill_tree(), silent = TRUE)
    try(job$process$wait(timeout = 5000), silent = TRUE)
    if (.genflow_native_download_job_alive(job)) {
      try(job$process$kill(), silent = TRUE)
      try(job$process$wait(timeout = 5000), silent = TRUE)
    }
    if (.genflow_native_download_job_alive(job)) {
      stop("Could not stop the native download worker.", call. = FALSE)
    }
  }

  latest <- .genflow_native_download_status_read(job$status_path)
  if (is.list(latest)) status <- latest
  if (.genflow_native_download_state_terminal(status$state)) {
    return(invisible(status))
  }
  if (!was_alive) {
    failed <- .genflow_native_download_job_worker_exit(job, status)
    failed <- .genflow_native_download_job_write(job$status_path, failed)
    .genflow_native_download_job_cleanup_parts(job, failed)
    return(invisible(failed))
  }

  .genflow_native_download_job_cleanup_parts(job, status)
  cancelled <- utils::modifyList(status, list(
    id = job$id %||% status$id,
    state = "cancelled",
    stage = "cancelled",
    message = "Download cancelled.",
    result = NULL,
    error = NULL
  ))
  cancelled <- .genflow_native_download_job_write(
    job$status_path,
    cancelled
  )
  invisible(cancelled)
}

#' Remove the private files for a completed native-download job
#'
#' @keywords internal
#' @noRd
.genflow_native_download_job_cleanup <- function(job) {
  job_dir <- normalizePath(
    as.character(job$dir %||% "")[1],
    winslash = "/",
    mustWork = FALSE
  )
  temp_root <- normalizePath(tempdir(), winslash = "/", mustWork = TRUE)
  if (!nzchar(job_dir) ||
      !identical(dirname(job_dir), temp_root) ||
      !startsWith(basename(job_dir), "genflow-native-download-")) {
    stop("Refusing to clean an invalid native download job path.", call. = FALSE)
  }
  if (.genflow_native_download_job_alive(job)) {
    stop("Cannot clean up a running native download job.", call. = FALSE)
  }
  status <- .genflow_native_download_job_read(job)
  if (!.genflow_native_download_state_terminal(status$state)) {
    stop("Cannot clean up a non-terminal native download job.", call. = FALSE)
  }
  .genflow_native_download_job_cleanup_parts(job, status)
  if (dir.exists(job_dir)) {
    unlink(job_dir, recursive = TRUE, force = TRUE)
  }
  invisible(!dir.exists(job_dir))
}
