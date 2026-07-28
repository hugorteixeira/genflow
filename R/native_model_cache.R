# CrispASR native model cache -------------------------------------------------

# These helpers deliberately keep model discovery and cache mutation separate
# from the STT runtime. The UI may call them, but gen_stt() remains responsible
# only for transcription.

#' Resolve CrispASR's writable cache directory
#'
#' @keywords internal
#' @noRd
.genflow_crispasr_canonical_cache_dir <- function(create = FALSE) {
  cache_dir <- trimws(Sys.getenv("CRISPASR_CACHE_DIR", unset = ""))
  models_dir <- trimws(Sys.getenv("CRISPASR_MODELS_DIR", unset = ""))
  selected <- if (nzchar(cache_dir)) {
    cache_dir
  } else if (nzchar(models_dir)) {
    models_dir
  } else {
    file.path(path.expand("~"), ".cache", "crispasr")
  }
  selected <- normalizePath(
    path.expand(selected),
    winslash = "/",
    mustWork = FALSE
  )

  home_dir <- normalizePath(
    path.expand("~"),
    winslash = "/",
    mustWork = TRUE
  )
  if (!nzchar(selected) ||
      identical(selected, "/") ||
      identical(selected, home_dir)) {
    stop(
      "The CrispASR cache directory cannot be the filesystem root or home directory.",
      call. = FALSE
    )
  }

  if (isTRUE(create) && !dir.exists(selected)) {
    created <- dir.create(selected, recursive = TRUE, showWarnings = FALSE)
    if (!isTRUE(created) && !dir.exists(selected)) {
      stop(
        "Could not create the CrispASR cache directory: ",
        selected,
        call. = FALSE
      )
    }
  }

  if (dir.exists(selected)) {
    selected <- normalizePath(selected, winslash = "/", mustWork = TRUE)
  } else {
    parent <- dirname(selected)
    if (dir.exists(parent)) {
      selected <- file.path(
        normalizePath(parent, winslash = "/", mustWork = TRUE),
        basename(selected)
      )
    }
  }

  if (!nzchar(selected) ||
      identical(selected, "/") ||
      identical(selected, home_dir)) {
    stop(
      "The CrispASR cache directory cannot be the filesystem root or home directory.",
      call. = FALSE
    )
  }
  selected
}

#' Normalize a cache directory without creating it
#'
#' @keywords internal
#' @noRd
.genflow_crispasr_normalize_cache_dir <- function(path) {
  path <- trimws(as.character(path %||% "")[1])
  if (is.na(path) || !nzchar(path)) return("")
  path <- path.expand(path)
  if (dir.exists(path)) {
    return(normalizePath(path, winslash = "/", mustWork = TRUE))
  }
  parent <- dirname(path)
  if (dir.exists(parent)) {
    return(file.path(
      normalizePath(parent, winslash = "/", mustWork = TRUE),
      basename(path)
    ))
  }
  path
}

#' Check whether a path itself is a symbolic link
#'
#' `Sys.readlink()` returns `NA` for a missing path on some platforms, and
#' `nzchar(NA_character_)` is true. Keep that sentinel out of safety checks.
#'
#' @keywords internal
#' @noRd
.genflow_crispasr_is_symlink <- function(path) {
  link <- Sys.readlink(path)
  length(link) == 1L && !is.na(link) && nzchar(link)
}

#' Read a small CrispASR source sidecar
#'
#' @keywords internal
#' @noRd
.genflow_crispasr_read_source <- function(path, max_bytes = 65536) {
  sidecar <- paste0(path, ".src")
  info <- suppressWarnings(file.info(sidecar))
  usable <- file.exists(sidecar) &&
    !dir.exists(sidecar) &&
    !.genflow_crispasr_is_symlink(sidecar) &&
    is.finite(info$size[[1]]) &&
    info$size[[1]] > 0 &&
    info$size[[1]] <= max_bytes
  if (!usable) return("")

  value <- tryCatch(
    suppressWarnings(
      readChar(sidecar, nchars = info$size[[1]], useBytes = TRUE)
    ),
    error = function(e) ""
  )
  sub("[\r\n\t ]+$", "", value, perl = TRUE)
}

#' Format a byte count for the native model inventory
#'
#' @keywords internal
#' @noRd
.genflow_crispasr_format_size <- function(bytes) {
  bytes <- suppressWarnings(as.numeric(bytes)[1])
  if (!is.finite(bytes) || bytes < 0) return("")
  units <- c("B", "KiB", "MiB", "GiB", "TiB")
  index <- if (bytes == 0) {
    1L
  } else {
    min(floor(log(bytes, base = 1024)) + 1L, length(units))
  }
  value <- bytes / (1024 ^ (index - 1L))
  digits <- if (index == 1L || value >= 100) 0L else 1L
  paste0(format(round(value, digits), trim = TRUE, nsmall = digits), " ", units[[index]])
}

#' Infer a quantization label from one concrete model filename
#'
#' @keywords internal
#' @noRd
.genflow_crispasr_model_quant <- function(filename) {
  filename <- tolower(basename(as.character(filename %||% "")[1]))
  match <- regexec(
    "-((?:q[2-8]_[a-z0-9_]+|f16|bf16)(?:-[a-z0-9_]+)*)\\.(?:gguf|bin)$",
    filename,
    perl = TRUE
  )
  parts <- regmatches(filename, match)[[1]]
  if (length(parts) >= 2L) parts[[2]] else ""
}

#' Validate a flat cache filename
#'
#' @keywords internal
#' @noRd
.genflow_crispasr_validate_filename <- function(filename) {
  filename <- trimws(as.character(filename %||% "")[1])
  valid <- !is.na(filename) &&
    nzchar(filename) &&
    identical(filename, basename(filename)) &&
    !filename %in% c(".", "..") &&
    !grepl("[/\\\\:?#[:space:][:cntrl:]]", filename, perl = TRUE) &&
    grepl("\\.(?:gguf|bin)$", filename, ignore.case = TRUE, perl = TRUE)
  if (!valid) {
    stop(
      "CrispASR model filenames must be flat .gguf or .bin filenames.",
      call. = FALSE
    )
  }
  filename
}

#' Resolve one model directly from the managed CrispASR cache
#'
#' @keywords internal
#' @noRd
.genflow_crispasr_managed_model <- function(filename) {
  filename <- .genflow_crispasr_validate_filename(filename)
  cache_dir <- .genflow_crispasr_canonical_cache_dir(create = FALSE)
  if (!dir.exists(cache_dir)) return("")
  cache_dir <- normalizePath(cache_dir, winslash = "/", mustWork = TRUE)
  candidate <- file.path(cache_dir, filename)
  info <- suppressWarnings(file.info(candidate))
  usable <- file.exists(candidate) &&
    !dir.exists(candidate) &&
    !.genflow_crispasr_is_symlink(candidate) &&
    identical(dirname(candidate), cache_dir) &&
    is.finite(info$size[[1]]) &&
    info$size[[1]] > 0
  if (!usable) return("")
  normalizePath(candidate, winslash = "/", mustWork = TRUE)
}

#' Return an empty native model inventory
#'
#' @keywords internal
#' @noRd
.genflow_crispasr_empty_inventory <- function() {
  data.frame(
    path = character(),
    filename = character(),
    quant = character(),
    size_bytes = numeric(),
    size = character(),
    source_url = character(),
    managed = logical(),
    selected = logical(),
    stringsAsFactors = FALSE
  )
}

#' List downloaded CrispASR models
#'
#' Scans only the immediate children of the requested cache directories.
#' Files from auxiliary or shared directories are visible but only regular,
#' non-symlink files directly inside CrispASR's canonical cache are marked as
#' managed. `.src` sidecars and in-progress `.part` files are never returned.
#'
#' @param config Optional local inference configuration.
#' @param cache_dirs Optional directories to scan. Defaults to CrispASR's
#'   well-known read-only search directories.
#'
#' @return A data frame with model paths, sizes, source metadata, and selection
#'   state.
#'
#' @keywords internal
#' @noRd
.genflow_crispasr_inventory <- function(config = NULL, cache_dirs = NULL) {
  config <- config %||% .genflow_read_local_config()
  canonical <- .genflow_crispasr_canonical_cache_dir(create = FALSE)
  canonical_existing <- if (dir.exists(canonical)) {
    normalizePath(canonical, winslash = "/", mustWork = TRUE)
  } else {
    canonical
  }

  if (is.null(cache_dirs)) {
    cache_dirs <- .genflow_crispasr_cache_dirs()
  }
  cache_dirs <- unique(vapply(
    as.character(cache_dirs %||% character()),
    .genflow_crispasr_normalize_cache_dir,
    character(1)
  ))
  cache_dirs <- cache_dirs[nzchar(cache_dirs) & dir.exists(cache_dirs)]
  if (!length(cache_dirs)) return(.genflow_crispasr_empty_inventory())

  selected_value <- trimws(as.character(config$stt_native_model %||% "")[1])
  if (is.na(selected_value)) selected_value <- ""
  selected_path <- ""
  selected_filename <- ""
  selected_source <- ""
  if (nzchar(selected_value) &&
      !identical(tolower(selected_value), "auto") &&
      !.stt_is_crispasr_hf_reference(selected_value)) {
    candidate <- path.expand(selected_value)
    if (file.exists(candidate) && !dir.exists(candidate)) {
      selected_path <- normalizePath(candidate, winslash = "/", mustWork = TRUE)
    } else {
      selected_path <- candidate
    }
  } else if (.stt_is_crispasr_hf_reference(selected_value)) {
    reference <- tryCatch(
      .stt_parse_crispasr_hf_reference(selected_value),
      error = function(e) NULL
    )
    if (!is.null(reference)) {
      selected_filename <- reference$file
      selected_source <- paste0(
        "https://huggingface.co/",
        reference$repository,
        "/resolve/main/",
        reference$file
      )
    }
  }

  rows <- list()
  for (cache_dir in cache_dirs) {
    files <- list.files(
      cache_dir,
      all.files = TRUE,
      full.names = TRUE,
      recursive = FALSE,
      no.. = TRUE
    )
    if (!length(files)) next
    names_only <- basename(files)
    keep <- grepl("\\.(?:gguf|bin)$", names_only, ignore.case = TRUE, perl = TRUE) &
      !grepl("\\.src$", names_only, ignore.case = TRUE, perl = TRUE) &
      !grepl("\\.part(?:\\.|$)", names_only, ignore.case = TRUE, perl = TRUE)
    files <- files[keep]
    if (!length(files)) next

    for (path in files) {
      filename <- tryCatch(
        .genflow_crispasr_validate_filename(basename(path)),
        error = function(e) ""
      )
      if (!nzchar(filename)) next
      info <- suppressWarnings(file.info(path))
      if (!file.exists(path) ||
          isTRUE(info$isdir[[1]]) ||
          !is.finite(info$size[[1]]) ||
          info$size[[1]] <= 0) {
        next
      }
      is_link <- .genflow_crispasr_is_symlink(path)
      visible_path <- file.path(cache_dir, filename)
      normalized_path <- if (!is_link) {
        normalizePath(visible_path, winslash = "/", mustWork = TRUE)
      } else {
        visible_path
      }
      source_url <- .genflow_crispasr_read_source(visible_path)
      managed <- !is_link &&
        identical(
          normalizePath(dirname(visible_path), winslash = "/", mustWork = TRUE),
          canonical_existing
        )
      selected <- if (nzchar(selected_path)) {
        identical(normalized_path, selected_path)
      } else if (nzchar(selected_filename)) {
        identical(basename(visible_path), selected_filename) &&
          (!nzchar(source_url) ||
            identical(source_url, selected_source) ||
            .genflow_crispasr_same_hf_artifact(source_url, selected_source))
      } else {
        FALSE
      }
      rows[[length(rows) + 1L]] <- data.frame(
        path = normalized_path,
        filename = filename,
        quant = .genflow_crispasr_model_quant(visible_path),
        size_bytes = as.numeric(info$size[[1]]),
        size = .genflow_crispasr_format_size(info$size[[1]]),
        source_url = source_url,
        managed = managed,
        selected = selected,
        stringsAsFactors = FALSE
      )
    }
  }

  if (!length(rows)) return(.genflow_crispasr_empty_inventory())
  result <- do.call(rbind, rows)
  result <- result[!duplicated(result$path), , drop = FALSE]
  result <- result[order(!result$managed, tolower(result$filename)), , drop = FALSE]
  rownames(result) <- NULL
  result
}

#' Hugging Face bearer-token request configuration
#'
#' @keywords internal
#' @noRd
.genflow_crispasr_hf_configs <- function(timeout = 30) {
  token <- .genflow_crispasr_hf_token()
  configs <- list(httr::timeout(timeout))
  if (nzchar(token)) {
    configs <- c(
      list(httr::add_headers(Authorization = paste("Bearer", token))),
      configs
    )
  }
  configs
}

#' Resolve a Hugging Face access token
#'
#' @keywords internal
#' @noRd
.genflow_crispasr_hf_token <- function() {
  variables <- c(
    "HF_TOKEN",
    "HUGGING_FACE_HUB_TOKEN",
    "HUGGINGFACE_API_TOKEN"
  )
  for (variable in variables) {
    token <- trimws(Sys.getenv(variable, unset = ""))
    if (nzchar(token)) return(token)
  }
  ""
}

#' Read public or authenticated Hugging Face repository metadata
#'
#' @keywords internal
#' @noRd
.genflow_crispasr_hf_metadata <- function(repository, timeout = 30) {
  repository <- trimws(as.character(repository %||% "")[1])
  if (!grepl(
    "^[A-Za-z0-9][A-Za-z0-9._-]*/[A-Za-z0-9][A-Za-z0-9._-]*$",
    repository,
    perl = TRUE
  )) {
    stop("Invalid Hugging Face repository id.", call. = FALSE)
  }
  url <- paste0(
    "https://huggingface.co/api/models/",
    repository,
    "?blobs=true"
  )
  response <- do.call(
    httr::GET,
    c(list(url = url), .genflow_crispasr_hf_configs(timeout))
  )
  status <- httr::status_code(response)
  if (status >= 400L) {
    stop(
      "Hugging Face repository metadata request failed with HTTP ",
      status,
      " for ",
      repository,
      ".",
      call. = FALSE
    )
  }
  payload <- tryCatch(
    jsonlite::fromJSON(
      httr::content(response, as = "text", encoding = "UTF-8"),
      simplifyVector = FALSE
    ),
    error = function(e) e
  )
  if (inherits(payload, "error") || !is.list(payload)) {
    stop("Hugging Face returned malformed repository metadata.", call. = FALSE)
  }
  if (isTRUE(payload$disabled)) {
    stop("The selected Hugging Face repository is disabled.", call. = FALSE)
  }
  payload
}

#' Select one real model artifact from Hugging Face metadata
#'
#' @keywords internal
#' @noRd
.genflow_crispasr_hf_artifact <- function(metadata,
                                          repository,
                                          filename,
                                          backend = "") {
  filename <- .genflow_crispasr_validate_filename(filename)
  siblings <- metadata$siblings %||% list()
  matches <- Filter(function(item) {
    is.list(item) &&
      identical(as.character(item$rfilename %||% "")[1], filename)
  }, siblings)
  if (length(matches) != 1L) {
    stop(
      "The model file `",
      filename,
      "` does not exist in Hugging Face repository `",
      repository,
      "`.",
      call. = FALSE
    )
  }
  sibling <- matches[[1]]
  size_bytes <- suppressWarnings(as.numeric(
    sibling$size %||% sibling$lfs$size %||% NA_real_
  )[1])
  if (!is.finite(size_bytes) || size_bytes <= 0) {
    stop(
      "Hugging Face did not report a usable size for `",
      filename,
      "`.",
      call. = FALSE
    )
  }

  architecture <- tolower(trimws(as.character(
    metadata$gguf$architecture %||% ""
  )[1]))
  backend <- tolower(trimws(as.character(backend %||% "")[1]))
  granite_backends <- c("granite", "granite-4.1", "granite-4.1-plus")
  if (backend %in% granite_backends &&
      nzchar(architecture) &&
      !architecture %in% c("granite_speech", "granite-speech", "granitespeech")) {
    stop(
      "The selected repository reports GGUF architecture `",
      architecture,
      "`, but CrispASR's ",
      backend,
      " backend requires a monolithic `granite_speech` model.",
      call. = FALSE
    )
  }
  if (identical(backend, "granite-4.1-nar") &&
      nzchar(architecture) &&
      !architecture %in% c("granite_nle", "granite-nle", "granitenle")) {
    stop(
      "The selected repository is not a CrispASR granite-4.1-nar model.",
      call. = FALSE
    )
  }

  revision <- tolower(trimws(as.character(metadata$sha %||% "")[1]))
  if (!grepl("^[0-9a-f]{40}$", revision, perl = TRUE)) {
    stop(
      "Hugging Face did not report a valid immutable repository revision.",
      call. = FALSE
    )
  }
  sha256 <- tolower(trimws(as.character(sibling$lfs$sha256 %||% "")[1]))
  if (!grepl("^[0-9a-f]{64}$", sha256, perl = TRUE)) {
    stop(
      "Hugging Face did not report a valid LFS SHA-256 for `",
      filename,
      "`.",
      call. = FALSE
    )
  }
  source_url <- paste0(
    "https://huggingface.co/",
    repository,
    "/resolve/",
    revision,
    "/",
    filename
  )
  list(
    repository = repository,
    filename = filename,
    source_url = source_url,
    size_bytes = size_bytes,
    sha256 = sha256,
    revision = revision,
    architecture = architecture
  )
}

#' Validate a Hugging Face artifact with a HEAD request
#'
#' @keywords internal
#' @noRd
.genflow_crispasr_hf_head <- function(url, expected_size = NA_real_, timeout = 30) {
  if (!grepl(
    "^https://huggingface\\.co/[A-Za-z0-9._-]+/[A-Za-z0-9._-]+/resolve/",
    url,
    perl = TRUE
  )) {
    stop("Refusing to validate a non-Hugging-Face model URL.", call. = FALSE)
  }
  response <- do.call(
    httr::HEAD,
    c(
      list(url = url),
      .genflow_crispasr_hf_configs(timeout),
      list(httr::config(followlocation = TRUE))
    )
  )
  status <- httr::status_code(response)
  if (!status %in% c(200L, 206L)) {
    stop(
      "The selected Hugging Face model file is unavailable (HTTP ",
      status,
      ").",
      call. = FALSE
    )
  }
  headers <- httr::headers(response)
  remote_size <- suppressWarnings(as.numeric(
    headers[["x-linked-size"]] %||%
      headers[["content-length"]] %||%
      NA_character_
  )[1])
  expected_size <- suppressWarnings(as.numeric(expected_size)[1])
  if (is.finite(expected_size) &&
      is.finite(remote_size) &&
      expected_size != remote_size) {
    stop(
      "Hugging Face model size changed between repository discovery and ",
      "download validation.",
      call. = FALSE
    )
  }
  list(status = status, size_bytes = remote_size)
}

#' Resolve the configured CrispASR executable for cache operations
#'
#' @keywords internal
#' @noRd
.genflow_crispasr_cache_executable <- function(executable = "") {
  executable <- trimws(as.character(executable %||% "")[1])
  if (!nzchar(executable)) {
    config <- .genflow_read_local_config()
    executable <- .stt_saved_native_executable("crispasr", config)
  }
  if (!nzchar(executable)) {
    executable <- unname(Sys.which("crispasr"))
  }
  resolved <- .genflow_resolve_executable(executable)
  if (!nzchar(resolved)) {
    stop(
      "A working CrispASR executable is required to resolve `auto`.",
      call. = FALSE
    )
  }
  resolved
}

#' Parse CrispASR's text dry-run preview
#'
#' @keywords internal
#' @noRd
.genflow_crispasr_parse_preview <- function(output) {
  lines <- as.character(output %||% character())
  result <- list()
  in_model <- FALSE
  pattern <- paste0(
    "^\\s*(requested|backend|registry|url|size|status|path):",
    "\\s*(.*?)\\s*$"
  )
  for (line in lines) {
    if (identical(trimws(line), "model:")) {
      in_model <- TRUE
      next
    }
    if (!in_model) next
    if (grepl("^\\S.*:\\s*$", line, perl = TRUE)) break
    match <- regmatches(line, regexec(pattern, line, perl = TRUE))[[1]]
    if (length(match) == 3L) result[[match[[2]]]] <- match[[3]]
  }
  required <- c("registry", "url", "path", "status")
  if (!all(required %in% names(result)) ||
      any(!nzchar(vapply(result[required], as.character, character(1))))) {
    stop("CrispASR returned an unrecognized dry-run model preview.", call. = FALSE)
  }
  result
}

#' Preview CrispASR registry resolution without downloading
#'
#' @keywords internal
#' @noRd
.genflow_crispasr_preview_auto <- function(selector,
                                           backend,
                                           quant = "",
                                           executable = "") {
  backend <- tolower(trimws(as.character(backend %||% "")[1]))
  if (!nzchar(backend) ||
      !grepl("^[a-z0-9][a-z0-9._-]*$", backend, perl = TRUE)) {
    stop("`backend` is required for CrispASR model `auto`.", call. = FALSE)
  }
  selector <- tolower(trimws(as.character(selector %||% "auto")[1]))
  embedded_quant <- ""
  if (grepl("^auto:[a-z0-9._-]+$", selector, perl = TRUE)) {
    embedded_quant <- sub("^auto:", "", selector)
    selector <- "auto"
  }
  if (!identical(selector, "auto")) {
    stop("Unsupported CrispASR auto selector.", call. = FALSE)
  }
  quant <- tolower(trimws(as.character(quant %||% "")[1]))
  if (nzchar(embedded_quant)) {
    if (nzchar(quant) && !identical(quant, embedded_quant)) {
      stop("Conflicting CrispASR quantization selections.", call. = FALSE)
    }
    quant <- embedded_quant
  }
  if (nzchar(quant) &&
      !grepl("^[a-z0-9][a-z0-9._-]*$", quant, perl = TRUE)) {
    stop("Invalid CrispASR quantization selector.", call. = FALSE)
  }

  executable <- .genflow_crispasr_cache_executable(executable)
  cache_dir <- .genflow_crispasr_canonical_cache_dir(create = FALSE)
  args <- c(
    "-m", "auto",
    "--backend", backend,
    "--cache-dir", cache_dir,
    if (nzchar(quant)) c("--model-quant", quant),
    "--dry-run-resolve"
  )
  process <- .stt_run_process(
    command = executable,
    args = args,
    timeout_secs = 30,
    environment = character()
  )
  status <- suppressWarnings(as.integer(process$status %||% 0L)[1])
  if (is.na(status) || status != 0L) {
    stop(
      "CrispASR could not resolve the requested model: ",
      .stt_process_detail(process$output),
      call. = FALSE
    )
  }
  preview <- .genflow_crispasr_parse_preview(process$output)
  preview$backend <- backend
  preview$quant <- quant
  preview
}

#' Parse one safe Hugging Face resolve URL
#'
#' @keywords internal
#' @noRd
.genflow_crispasr_parse_hf_url <- function(url) {
  pattern <- paste0(
    "^https://huggingface\\.co/",
    "([A-Za-z0-9][A-Za-z0-9._-]*/[A-Za-z0-9][A-Za-z0-9._-]*)/",
    "resolve/(main|[0-9A-Fa-f]{40})/([^/?#]+)$"
  )
  match <- regmatches(url, regexec(pattern, url, perl = TRUE))[[1]]
  if (length(match) != 4L) {
    stop(
      "CrispASR resolved an unsupported model URL.",
      call. = FALSE
    )
  }
  list(
    repository = match[[2]],
    revision = tolower(match[[3]]),
    filename = utils::URLdecode(match[[4]])
  )
}

#' Compare two Hugging Face URLs as repository artifacts
#'
#' A legacy `main` URL and an immutable revision URL may identify the same
#' repository file. This comparison deliberately ignores only that revision;
#' malformed URLs and different repositories or filenames never match.
#'
#' @keywords internal
#' @noRd
.genflow_crispasr_same_hf_artifact <- function(left, right) {
  left <- tryCatch(
    .genflow_crispasr_parse_hf_url(left),
    error = function(e) NULL
  )
  right <- tryCatch(
    .genflow_crispasr_parse_hf_url(right),
    error = function(e) NULL
  )
  !is.null(left) &&
    !is.null(right) &&
    identical(left$repository, right$repository) &&
    identical(left$filename, right$filename)
}

#' Resolve and validate one remote CrispASR artifact
#'
#' @keywords internal
#' @noRd
.genflow_crispasr_resolve_download <- function(selector,
                                               backend = "",
                                               quant = "",
                                               executable = "") {
  selector <- trimws(as.character(selector %||% "")[1])
  if (is.na(selector) || !nzchar(selector)) {
    stop("A CrispASR model selector is required.", call. = FALSE)
  }
  backend <- tolower(trimws(as.character(backend %||% "")[1]))

  if (.stt_is_crispasr_hf_reference(selector)) {
    reference <- .stt_parse_crispasr_hf_reference(selector)
    repository <- reference$repository
    filename <- reference$file
  } else if (grepl("^auto(?::[A-Za-z0-9._-]+)?$", selector, ignore.case = TRUE, perl = TRUE)) {
    preview <- .genflow_crispasr_preview_auto(
      selector = selector,
      backend = backend,
      quant = quant,
      executable = executable
    )
    parsed <- .genflow_crispasr_parse_hf_url(preview$url)
    repository <- parsed$repository
    filename <- parsed$filename
  } else {
    stop(
      "Download selectors must be `auto`, `auto:QUANT`, or ",
      "an exact `hf://OWNER/REPO:FILE` / Hugging Face `/blob/main/` URL.",
      call. = FALSE
    )
  }

  metadata <- .genflow_crispasr_hf_metadata(repository)
  artifact <- .genflow_crispasr_hf_artifact(
    metadata = metadata,
    repository = repository,
    filename = filename,
    backend = backend
  )
  .genflow_crispasr_hf_head(
    artifact$source_url,
    expected_size = artifact$size_bytes
  )
  artifact
}

#' Report native model download progress
#'
#' The callback receives one list with `stage`, `filename`, `bytes_received`,
#' `bytes_total`, and `proportion`.
#'
#' @keywords internal
#' @noRd
.genflow_crispasr_report_progress <- function(progress,
                                              stage,
                                              filename,
                                              bytes_received = 0,
                                              bytes_total = NA_real_) {
  if (is.null(progress)) return(invisible(NULL))
  if (!is.function(progress)) {
    stop("`progress` must be NULL or a function.", call. = FALSE)
  }
  proportion <- if (is.finite(bytes_total) && bytes_total > 0) {
    min(max(bytes_received / bytes_total, 0), 1)
  } else {
    NA_real_
  }
  progress(list(
    stage = stage,
    filename = filename,
    bytes_received = as.numeric(bytes_received),
    bytes_total = as.numeric(bytes_total),
    proportion = proportion
  ))
  invisible(NULL)
}

#' Create a throttled native model progress reporter
#'
#' @keywords internal
#' @noRd
.genflow_crispasr_progress_throttler <- function(progress,
                                                 filename,
                                                 bytes_total,
                                                 interval = 0.25,
                                                 byte_step = 8 * 1024^2,
                                                 clock = function() {
                                                   unname(proc.time()[["elapsed"]])
                                                 }) {
  if (is.null(progress)) return(function(...) invisible(NULL))
  if (!is.function(progress)) {
    stop("`progress` must be NULL or a function.", call. = FALSE)
  }
  last_time <- suppressWarnings(as.numeric(clock())[1])
  last_bytes <- 0
  last_reported <- NA_real_

  function(bytes_received, force = FALSE) {
    bytes_received <- suppressWarnings(as.numeric(bytes_received)[1])
    if (isTRUE(force) &&
        is.finite(bytes_received) &&
        is.finite(last_reported) &&
        identical(bytes_received, last_reported)) {
      return(invisible(NULL))
    }
    now <- suppressWarnings(as.numeric(clock())[1])
    due_by_time <- is.finite(now) &&
      is.finite(last_time) &&
      now - last_time >= interval
    due_by_bytes <- is.finite(bytes_received) &&
      bytes_received - last_bytes >= byte_step
    complete <- is.finite(bytes_total) &&
      bytes_total > 0 &&
      bytes_received >= bytes_total
    if (!isTRUE(force) && !due_by_time && !due_by_bytes && !complete) {
      return(invisible(NULL))
    }

    .genflow_crispasr_report_progress(
      progress,
      stage = "downloading",
      filename = filename,
      bytes_received = bytes_received,
      bytes_total = bytes_total
    )
    if (is.finite(now)) last_time <<- now
    if (is.finite(bytes_received)) {
      last_bytes <<- bytes_received
      last_reported <<- bytes_received
    }
    invisible(NULL)
  }
}

#' Calculate and validate a model file's SHA-256
#'
#' @keywords internal
#' @noRd
.genflow_crispasr_file_sha256 <- function(path) {
  info <- suppressWarnings(file.info(path))
  if (!file.exists(path) ||
      dir.exists(path) ||
      .genflow_crispasr_is_symlink(path) ||
      !is.finite(info$size[[1]]) ||
      info$size[[1]] <= 0) {
    stop("Cannot hash a missing, empty, or non-regular model file.", call. = FALSE)
  }
  connection <- file(path, open = "rb")
  on.exit(close(connection), add = TRUE)
  tolower(as.character(openssl::sha256(connection))[[1]])
}

#' Verify a model file against one Hugging Face LFS SHA-256
#'
#' @keywords internal
#' @noRd
.genflow_crispasr_verify_sha256 <- function(path, expected) {
  expected <- tolower(trimws(as.character(expected %||% "")[1]))
  if (!grepl("^[0-9a-f]{64}$", expected, perl = TRUE)) {
    stop("A valid Hugging Face LFS SHA-256 is required.", call. = FALSE)
  }
  actual <- .genflow_crispasr_file_sha256(path)
  if (!identical(actual, expected)) {
    stop(
      "CrispASR model SHA-256 does not match Hugging Face LFS metadata.",
      call. = FALSE
    )
  }
  invisible(actual)
}

#' Stream a Hugging Face model into one temporary file
#'
#' @keywords internal
#' @noRd
.genflow_crispasr_fetch <- function(url,
                                    destination,
                                    expected_size,
                                    filename,
                                    progress = NULL,
                                    timeout = 3600) {
  connection <- file(destination, open = "wb")
  connection_open <- TRUE
  on.exit({
    if (isTRUE(connection_open)) try(close(connection), silent = TRUE)
  }, add = TRUE)
  received <- 0
  report <- .genflow_crispasr_progress_throttler(
    progress = progress,
    filename = filename,
    bytes_total = expected_size
  )
  writer <- function(bytes) {
    writeBin(bytes, connection, useBytes = TRUE)
    received <<- received + length(bytes)
    report(received)
  }
  response <- do.call(
    httr::GET,
    c(
      list(url = url),
      .genflow_crispasr_hf_configs(timeout),
      list(
        httr::config(followlocation = TRUE),
        httr::write_stream(writer)
      )
    )
  )
  status <- httr::status_code(response)
  if (status >= 400L) {
    stop(
      "Hugging Face model download failed with HTTP ",
      status,
      ".",
      call. = FALSE
    )
  }
  close(connection)
  connection_open <- FALSE
  report(received, force = TRUE)
  invisible(received)
}

#' Write a source sidecar atomically
#'
#' @keywords internal
#' @noRd
.genflow_crispasr_write_source <- function(path, source_url) {
  sidecar <- paste0(path, ".src")
  temporary <- tempfile(
    pattern = paste0(
      ".",
      basename(sidecar),
      ".part.",
      Sys.getpid(),
      "."
    ),
    tmpdir = dirname(sidecar)
  )
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  writeChar(source_url, temporary, eos = NULL, useBytes = TRUE)
  if (file.exists(sidecar) && .Platform$OS.type == "windows") {
    unlink(sidecar, force = TRUE)
  }
  if (!file.rename(temporary, sidecar)) {
    stop("Could not install the CrispASR source sidecar.", call. = FALSE)
  }
  invisible(sidecar)
}

#' Download one validated CrispASR model into its managed cache
#'
#' @param selector `auto`, `auto:QUANT`, an explicit
#'   `hf://OWNER/REPO:FILE` reference, or the equivalent Hugging Face
#'   `/blob/main/FILE` URL. An existing regular local model path is returned
#'   unchanged.
#' @param backend CrispASR backend used for `auto` resolution and compatibility
#'   validation.
#' @param quant Optional registry quantization preference.
#' @param executable Optional CrispASR executable.
#' @param progress Optional callback receiving one structured progress list.
#'
#' @return A list with `path`, `filename`, `source_url`, `cached`, and
#'   `size_bytes`.
#'
#' @keywords internal
#' @noRd
.genflow_crispasr_download <- function(selector,
                                       backend = "",
                                       quant = "",
                                       executable = "",
                                       progress = NULL) {
  selector <- trimws(as.character(selector %||% "")[1])
  if (is.na(selector) || !nzchar(selector)) {
    stop("A CrispASR model selector is required.", call. = FALSE)
  }

  local_candidate <- path.expand(selector)
  if (!.stt_is_crispasr_hf_reference(selector) &&
      !grepl("^auto(?::[A-Za-z0-9._-]+)?$", selector, ignore.case = TRUE, perl = TRUE) &&
      file.exists(local_candidate)) {
    if (dir.exists(local_candidate) ||
        .genflow_crispasr_is_symlink(local_candidate)) {
      stop("Local CrispASR models must be regular, non-symlink files.", call. = FALSE)
    }
    filename <- .genflow_crispasr_validate_filename(basename(local_candidate))
    path <- normalizePath(local_candidate, winslash = "/", mustWork = TRUE)
    size_bytes <- as.numeric(file.info(path)$size[[1]])
    if (!is.finite(size_bytes) || size_bytes <= 0) {
      stop("The local CrispASR model is empty.", call. = FALSE)
    }
    return(list(
      path = path,
      filename = filename,
      source_url = .genflow_crispasr_read_source(path),
      cached = TRUE,
      size_bytes = size_bytes
    ))
  }

  .genflow_crispasr_report_progress(
    progress,
    stage = "resolving",
    filename = "",
    bytes_received = 0,
    bytes_total = NA_real_
  )
  artifact <- .genflow_crispasr_resolve_download(
    selector = selector,
    backend = backend,
    quant = quant,
    executable = executable
  )
  filename <- .genflow_crispasr_validate_filename(artifact$filename)
  cache_dir <- .genflow_crispasr_canonical_cache_dir(create = TRUE)
  target <- file.path(cache_dir, filename)
  if (!identical(dirname(target), cache_dir)) {
    stop("Refusing a CrispASR cache path traversal.", call. = FALSE)
  }
  if (.genflow_crispasr_is_symlink(target)) {
    stop("Refusing to replace a symlink in the CrispASR cache.", call. = FALSE)
  }
  sidecar <- paste0(target, ".src")
  parts <- .genflow_crispasr_cleanup_stale_parts(cache_dir, filename)
  if (length(parts$active)) {
    stop("The CrispASR model has an active download job.", call. = FALSE)
  }

  target_info <- suppressWarnings(file.info(target))
  target_present <- file.exists(target) &&
    !dir.exists(target) &&
    is.finite(target_info$size[[1]]) &&
    target_info$size[[1]] > 0
  if (target_present) {
    existing_source <- .genflow_crispasr_read_source(target)
    exact_source <- identical(existing_source, artifact$source_url)
    same_artifact <- nzchar(existing_source) &&
      .genflow_crispasr_same_hf_artifact(
        existing_source,
        artifact$source_url
      )
    if (nzchar(existing_source) && !exact_source && !same_artifact) {
      stop(
        "A same-named CrispASR cache file comes from a different source; ",
        "remove it explicitly before downloading this model.",
        call. = FALSE
      )
    }
    if (target_info$size[[1]] != artifact$size_bytes) {
      stop(
        "The existing CrispASR cache file has an unexpected size; remove it ",
        "explicitly before downloading it again.",
        call. = FALSE
      )
    }
    .genflow_crispasr_report_progress(
      progress,
      stage = "verifying",
      filename = filename,
      bytes_received = artifact$size_bytes,
      bytes_total = artifact$size_bytes
    )
    .genflow_crispasr_verify_sha256(target, artifact$sha256)
    if (!exact_source) {
      .genflow_crispasr_report_progress(
        progress,
        stage = "publishing",
        filename = filename,
        bytes_received = artifact$size_bytes,
        bytes_total = artifact$size_bytes
      )
      .genflow_crispasr_write_source(target, artifact$source_url)
    }
    path <- normalizePath(target, winslash = "/", mustWork = TRUE)
    .genflow_crispasr_report_progress(
      progress,
      stage = "complete",
      filename = filename,
      bytes_received = artifact$size_bytes,
      bytes_total = artifact$size_bytes
    )
    return(list(
      path = path,
      filename = filename,
      source_url = artifact$source_url,
      cached = TRUE,
      size_bytes = artifact$size_bytes
    ))
  }
  if (file.exists(target) && dir.exists(target)) {
    stop("The CrispASR model target is a directory.", call. = FALSE)
  }
  if (file.exists(sidecar) || .genflow_crispasr_is_symlink(sidecar)) {
    stop(
      "An orphan CrispASR source sidecar already exists for this model; ",
      "remove it explicitly before downloading.",
      call. = FALSE
    )
  }

  temporary <- tempfile(
    pattern = paste0(
      ".",
      filename,
      ".part.",
      Sys.getpid(),
      "."
    ),
    tmpdir = cache_dir
  )
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  .genflow_crispasr_report_progress(
    progress,
    stage = "downloading",
    filename = filename,
    bytes_received = 0,
    bytes_total = artifact$size_bytes
  )
  .genflow_crispasr_fetch(
    url = artifact$source_url,
    destination = temporary,
    expected_size = artifact$size_bytes,
    filename = filename,
    progress = progress
  )
  downloaded_size <- suppressWarnings(as.numeric(file.info(temporary)$size[[1]]))
  if (!is.finite(downloaded_size) ||
      downloaded_size <= 0 ||
      downloaded_size != artifact$size_bytes) {
    stop(
      "Downloaded CrispASR model size does not match Hugging Face metadata.",
      call. = FALSE
    )
  }
  .genflow_crispasr_report_progress(
    progress,
    stage = "verifying",
    filename = filename,
    bytes_received = downloaded_size,
    bytes_total = downloaded_size
  )
  .genflow_crispasr_verify_sha256(temporary, artifact$sha256)
  .genflow_crispasr_report_progress(
    progress,
    stage = "publishing",
    filename = filename,
    bytes_received = downloaded_size,
    bytes_total = downloaded_size
  )

  if (file.exists(target)) {
    target_info <- suppressWarnings(file.info(target))
    if (is.finite(target_info$size[[1]]) && target_info$size[[1]] > 0) {
      stop(
        "The CrispASR cache target changed while the download was running.",
        call. = FALSE
      )
    }
    unlink(target, force = TRUE)
  }
  if (!file.rename(temporary, target)) {
    stop("Could not atomically install the downloaded CrispASR model.", call. = FALSE)
  }
  sidecar_error <- tryCatch(
    {
      .genflow_crispasr_write_source(target, artifact$source_url)
      NULL
    },
    error = function(e) e
  )
  if (inherits(sidecar_error, "error")) {
    unlink(sidecar, force = FALSE)
    rollback_status <- unlink(target, force = FALSE)
    if (rollback_status != 0L || file.exists(target)) {
      stop(
        conditionMessage(sidecar_error),
        " The sidecar failed and the downloaded payload could not be rolled back.",
        call. = FALSE
      )
    }
    stop(
      conditionMessage(sidecar_error),
      " The downloaded payload was rolled back.",
      call. = FALSE
    )
  }
  path <- normalizePath(target, winslash = "/", mustWork = TRUE)
  .genflow_crispasr_report_progress(
    progress,
    stage = "complete",
    filename = filename,
    bytes_received = downloaded_size,
    bytes_total = downloaded_size
  )
  list(
    path = path,
    filename = filename,
    source_url = artifact$source_url,
    cached = FALSE,
    size_bytes = downloaded_size
  )
}

#' Check whether a process id still belongs to a live process
#'
#' @keywords internal
#' @noRd
.genflow_crispasr_pid_alive <- function(pid) {
  pid <- suppressWarnings(as.integer(pid)[1])
  if (is.na(pid) || pid <= 0L) return(FALSE)
  isTRUE(tryCatch(
    tools::pskill(pid, signal = 0L),
    error = function(e) TRUE,
    warning = function(w) TRUE
  ))
}

#' Remove stale temporary files for one exact CrispASR model
#'
#' PID-bearing files are retained while their owner is alive and removed once
#' that process is gone. Legacy files without a parseable PID are removed only
#' after the age threshold. Directories and symlinks are never removed
#' automatically and remain active blockers.
#'
#' @keywords internal
#' @noRd
.genflow_crispasr_cleanup_stale_parts <- function(cache_dir,
                                                  filename,
                                                  stale_after = 3600,
                                                  now = Sys.time()) {
  filename <- .genflow_crispasr_validate_filename(filename)
  stale_after <- suppressWarnings(as.numeric(stale_after)[1])
  if (!is.finite(stale_after) || stale_after < 0) {
    stop("`stale_after` must be a non-negative number of seconds.", call. = FALSE)
  }
  canonical <- .genflow_crispasr_canonical_cache_dir(create = FALSE)
  if (!dir.exists(canonical) || !dir.exists(cache_dir)) {
    return(list(active = character(), removed = character()))
  }
  canonical <- normalizePath(canonical, winslash = "/", mustWork = TRUE)
  cache_dir <- normalizePath(cache_dir, winslash = "/", mustWork = TRUE)
  if (!identical(cache_dir, canonical)) {
    stop(
      "Temporary CrispASR cleanup is confined to the canonical cache.",
      call. = FALSE
    )
  }

  prefixes <- c(
    paste0(".", filename, ".part."),
    paste0(filename, ".part."),
    paste0(".", filename, ".src.part."),
    paste0(filename, ".src.part.")
  )
  names <- list.files(
    cache_dir,
    all.files = TRUE,
    full.names = FALSE,
    no.. = TRUE
  )
  active <- character()
  removed <- character()
  now <- as.POSIXct(now)

  for (name in names) {
    matching <- prefixes[startsWith(name, prefixes)]
    if (!length(matching)) next
    prefix <- matching[[which.max(nchar(matching))]]
    suffix <- substring(name, nchar(prefix) + 1L)
    pid_match <- regmatches(
      suffix,
      regexec("^([0-9]+)(?:\\.|$)", suffix, perl = TRUE)
    )[[1]]
    pid <- if (length(pid_match) == 2L) {
      suppressWarnings(as.integer(pid_match[[2]]))
    } else {
      NA_integer_
    }

    path <- file.path(cache_dir, name)
    info <- suppressWarnings(file.info(path))
    regular <- file.exists(path) &&
      !isTRUE(info$isdir[[1]]) &&
      !.genflow_crispasr_is_symlink(path)
    age <- suppressWarnings(as.numeric(
      difftime(now, info$mtime[[1]], units = "secs")
    ))
    stale <- if (!is.na(pid) && pid > 0L) {
      !.genflow_crispasr_pid_alive(pid)
    } else {
      is.finite(age) && age >= stale_after
    }

    if (regular && isTRUE(stale) &&
        unlink(path, force = FALSE) == 0L &&
        !file.exists(path)) {
      removed <- c(removed, path)
    } else {
      active <- c(active, path)
    }
  }
  list(active = active, removed = removed)
}

#' Remove one managed CrispASR model and its exact source sidecar
#'
#' Confirmation is intentionally delegated to the UI. This helper accepts only
#' an absolute regular file directly inside the canonical CrispASR cache. It
#' never follows symlinks, expands globs, or removes companion files by name.
#'
#' @param path Exact model path.
#' @param active_model Optional configured/active path or `hf://` selector that
#'   must not be removed.
#'
#' @return Invisibly returns TRUE after both payload and sidecar are absent.
#'
#' @keywords internal
#' @noRd
.genflow_crispasr_remove_model <- function(path, active_model = "") {
  path <- trimws(as.character(path %||% "")[1])
  if (is.na(path) || !nzchar(path)) {
    stop("A managed CrispASR model path is required.", call. = FALSE)
  }
  absolute <- grepl(
    "^(?:/|[A-Za-z]:[/\\\\]|\\\\\\\\)",
    path,
    perl = TRUE
  )
  traversal <- grepl("(^|[/\\\\])\\.\\.([/\\\\]|$)", path, perl = TRUE)
  if (!absolute || traversal) {
    stop("CrispASR model removal requires an absolute flat path.", call. = FALSE)
  }

  canonical <- .genflow_crispasr_canonical_cache_dir(create = FALSE)
  if (!dir.exists(canonical)) {
    stop("The CrispASR cache directory does not exist.", call. = FALSE)
  }
  canonical <- normalizePath(canonical, winslash = "/", mustWork = TRUE)
  expanded <- path.expand(path)
  parent <- dirname(expanded)
  if (!dir.exists(parent) ||
      !identical(
        normalizePath(parent, winslash = "/", mustWork = TRUE),
        canonical
      )) {
    stop(
      "Only models directly inside the managed CrispASR cache can be removed.",
      call. = FALSE
    )
  }
  filename <- .genflow_crispasr_validate_filename(basename(expanded))
  candidate <- file.path(canonical, filename)
  if (!file.exists(candidate) ||
      dir.exists(candidate) ||
      .genflow_crispasr_is_symlink(candidate)) {
    stop(
      "The requested CrispASR model is not a removable regular cache file.",
      call. = FALSE
    )
  }

  parts <- .genflow_crispasr_cleanup_stale_parts(canonical, filename)
  if (length(parts$active)) {
    stop("The CrispASR model has an active download job.", call. = FALSE)
  }

  active_models <- trimws(as.character(active_model %||% character()))
  active_models <- unique(active_models[!is.na(active_models) & nzchar(active_models)])
  for (active_model in active_models) {
    is_hf_reference <- .stt_is_crispasr_hf_reference(active_model)
    if (is_hf_reference) {
      reference <- tryCatch(
        .stt_parse_crispasr_hf_reference(active_model),
        error = function(e) NULL
      )
      if (!is.null(reference) && identical(reference$file, filename)) {
        expected_source <- paste0(
          "https://huggingface.co/",
          reference$repository,
          "/resolve/main/",
          reference$file
        )
        current_source <- .genflow_crispasr_read_source(candidate)
        if (!nzchar(current_source) ||
            identical(current_source, expected_source) ||
            .genflow_crispasr_same_hf_artifact(
              current_source,
              expected_source
          )) {
          stop(
            "Refusing to remove the selected CrispASR model. Select another ",
            "model before deleting it.",
            call. = FALSE
          )
        }
      }
    } else if (!identical(tolower(active_model), "auto")) {
      active_filename <- tryCatch(
        .genflow_crispasr_validate_filename(active_model),
        error = function(e) ""
      )
      if (nzchar(active_filename) && identical(active_filename, filename)) {
        stop(
          "Refusing to remove the selected CrispASR model. Select another ",
          "model before deleting it.",
          call. = FALSE
        )
      }
      active_path <- path.expand(active_model)
      if (file.exists(active_path) && !dir.exists(active_path)) {
        active_path <- normalizePath(active_path, winslash = "/", mustWork = TRUE)
      }
      if (identical(active_path, candidate)) {
        stop(
          "Refusing to remove the selected CrispASR model. Select another ",
          "model before deleting it.",
          call. = FALSE
        )
      }
    }
  }

  sidecar <- paste0(candidate, ".src")
  if (unlink(candidate, force = FALSE) != 0L || file.exists(candidate)) {
    stop("Could not remove the CrispASR model file.", call. = FALSE)
  }
  if (file.exists(sidecar) || .genflow_crispasr_is_symlink(sidecar)) {
    if (unlink(sidecar, force = FALSE) != 0L || file.exists(sidecar)) {
      stop(
        "The model was removed, but its CrispASR source sidecar could not be removed.",
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}
