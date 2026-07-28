# Local inference configuration and diagnostics -----------------------------

.genflow_local_config_fields <- c(
  "ollama_base_url",
  "ollama_model",
  "llamacpp_base_url",
  "llamacpp_model",
  "stt_server_base_url",
  "stt_server_model",
  "stt_native_engine",
  "stt_native_executable",
  "stt_native_crispasr_executable",
  "stt_native_moss_transcribe_executable",
  "stt_native_model",
  "stt_native_backend",
  "stt_native_quant",
  "stt_native_device",
  "moss_cpp_executable",
  "moss_cpp_model",
  "moss_cpp_device"
)

.genflow_local_config_defaults <- function() {
  list(
    version = 5L,
    ollama_base_url = "http://127.0.0.1:11434",
    ollama_model = "",
    llamacpp_base_url = "http://127.0.0.1:8080",
    llamacpp_model = "",
    stt_server_base_url = "http://127.0.0.1:8000",
    stt_server_model = "",
    stt_native_engine = "auto",
    stt_native_executable = "",
    stt_native_crispasr_executable = "",
    stt_native_moss_transcribe_executable = "",
    stt_native_model = "",
    stt_native_backend = "",
    stt_native_quant = "",
    stt_native_device = "auto",
    moss_cpp_executable = "",
    moss_cpp_model = "",
    moss_cpp_device = "auto"
  )
}

.genflow_migrate_legacy_native_config <- function(config) {
  if (!is.list(config)) return(config)
  original_names <- names(config)
  canonical_fields <- c(
    "stt_native_engine",
    "stt_native_executable",
    "stt_native_model",
    "stt_native_backend",
    "stt_native_quant",
    "stt_native_device"
  )
  # A canonical key is an explicit source of truth, including an intentionally
  # empty value. Legacy MOSS-only objects are migrated before the v4 generic
  # executable is assigned to its engine-specific field.
  if (!any(canonical_fields %in% original_names)) {
    legacy_executable <- .genflow_local_scalar(
      config$moss_cpp_executable %||% "",
      "moss_cpp_executable"
    )
    legacy_model <- .genflow_local_scalar(
      config$moss_cpp_model %||% "",
      "moss_cpp_model"
    )
    legacy_device <- .genflow_local_scalar(
      config$moss_cpp_device %||% "auto",
      "moss_cpp_device"
    )
    config$stt_native_executable <- legacy_executable
    config$stt_native_model <- legacy_model
    config$stt_native_device <- legacy_device
    config$stt_native_backend <- ""
    legacy_configured <- nzchar(legacy_executable) ||
      nzchar(legacy_model) ||
      !identical(tolower(legacy_device), "auto")
    config$stt_native_engine <- if (legacy_configured) {
      "moss-transcribe"
    } else {
      "auto"
    }
  }

  has_crispasr_path <- "stt_native_crispasr_executable" %in% original_names
  has_moss_path <- "stt_native_moss_transcribe_executable" %in% original_names
  if (!has_crispasr_path) config$stt_native_crispasr_executable <- ""
  if (!has_moss_path) config$stt_native_moss_transcribe_executable <- ""

  generic_executable <- .genflow_local_scalar(
    config$stt_native_executable %||% "",
    "stt_native_executable"
  )
  configured_engine <- tryCatch(
    .stt_normalize_native_engine(config$stt_native_engine %||% "auto"),
    error = function(e) "auto"
  )
  inferred_engine <- .stt_native_engine_from_executable(generic_executable)
  target_engine <- if (!identical(configured_engine, "auto")) {
    configured_engine
  } else {
    inferred_engine
  }
  if (nzchar(generic_executable) && identical(target_engine, "crispasr") &&
      !nzchar(config$stt_native_crispasr_executable)) {
    config$stt_native_crispasr_executable <- generic_executable
  }
  if (nzchar(generic_executable) && identical(target_engine, "moss-transcribe") &&
      !nzchar(config$stt_native_moss_transcribe_executable)) {
    config$stt_native_moss_transcribe_executable <- generic_executable
  }
  legacy_moss_executable <- .genflow_local_scalar(
    config$moss_cpp_executable %||% "",
    "moss_cpp_executable"
  )
  if (nzchar(legacy_moss_executable) &&
      !nzchar(config$stt_native_moss_transcribe_executable)) {
    config$stt_native_moss_transcribe_executable <- legacy_moss_executable
  }
  # The v4 generic field is a one-way migration alias once its owner is known.
  # With engine=auto and a custom binary name, preserve it until the user picks
  # an engine instead of discarding a path that cannot yet be classified.
  if (!nzchar(generic_executable) ||
      target_engine %in% c("crispasr", "moss-transcribe")) {
    config$stt_native_executable <- ""
  }
  config
}

.genflow_local_config_path <- function(path = NULL) {
  configured <- path %||%
    getOption("genflow.local_config_path", NULL) %||%
    Sys.getenv("GENFLOW_LOCAL_CONFIG", unset = "")
  configured <- trimws(as.character(configured %||% "")[1])
  if (!nzchar(configured)) {
    configured <- file.path(
      tools::R_user_dir("genflow", which = "config"),
      "local-inference.json"
    )
  }
  path.expand(configured)
}

.genflow_local_scalar <- function(value, field) {
  if (is.null(value) || length(value) == 0L) {
    return("")
  }
  if (length(value) != 1L || is.list(value)) {
    stop("`", field, "` must be a single character value.", call. = FALSE)
  }
  value <- as.character(value)[1]
  if (is.na(value)) "" else trimws(value)
}

.genflow_validate_local_url <- function(value, field) {
  value <- .genflow_local_scalar(value, field)
  if (!nzchar(value)) {
    return("")
  }
  if (!grepl("^https?://[^[:space:]]+$", value, perl = TRUE)) {
    stop("`", field, "` must be an http(s) URL.", call. = FALSE)
  }
  sub("/+$", "", value)
}

.genflow_validate_local_config <- function(config) {
  defaults <- .genflow_local_config_defaults()
  unknown <- setdiff(names(config), names(defaults))
  if (length(unknown)) {
    stop(
      "Unknown local inference setting",
      if (length(unknown) > 1L) "s" else "",
      ": ",
      paste(unknown, collapse = ", "),
      call. = FALSE
    )
  }

  validated <- defaults
  for (field in .genflow_local_config_fields) {
    if (!is.null(config[[field]])) {
      validated[[field]] <- .genflow_local_scalar(config[[field]], field)
    }
  }

  validated$stt_native_engine <- tolower(validated$stt_native_engine)
  native_engine_aliases <- c(
    "moss-cpp" = "moss-transcribe",
    "moss_cpp" = "moss-transcribe",
    "mosscpp" = "moss-transcribe",
    "crisp-asr" = "crispasr",
    "crisp_asr" = "crispasr"
  )
  if (validated$stt_native_engine %in% names(native_engine_aliases)) {
    validated$stt_native_engine <- unname(
      native_engine_aliases[validated$stt_native_engine]
    )
  }
  if (!validated$stt_native_engine %in% c(
    "auto",
    "crispasr",
    "moss-transcribe"
  )) {
    stop(
      "`stt_native_engine` must be auto, crispasr, or moss-transcribe.",
      call. = FALSE
    )
  }

  validated$stt_native_backend <- tolower(validated$stt_native_backend)
  if (grepl(
    "[[:space:][:cntrl:]]",
    validated$stt_native_backend,
    perl = TRUE
  )) {
    stop(
      "`stt_native_backend` cannot contain whitespace or control characters.",
      call. = FALSE
    )
  }

  validated$stt_native_quant <- .stt_validate_native_quant(
    validated$stt_native_quant
  )

  validated$moss_cpp_device <- tolower(validated$moss_cpp_device)
  if (!validated$moss_cpp_device %in% c(
    "auto",
    "cpu",
    "vulkan",
    "hip",
    "cuda",
    "metal"
  )) {
    stop(
      "`moss_cpp_device` must be auto, cpu, vulkan, hip, cuda, or metal.",
      call. = FALSE
    )
  }

  validated$stt_native_device <- tolower(validated$stt_native_device)
  if (!validated$stt_native_device %in% c(
    "auto",
    "cpu",
    "vulkan",
    "hip",
    "cuda",
    "metal"
  )) {
    stop(
      "`stt_native_device` must be auto, cpu, vulkan, hip, cuda, or metal.",
      call. = FALSE
    )
  }

  for (field in c(
    "ollama_base_url",
    "llamacpp_base_url",
    "stt_server_base_url"
  )) {
    validated[[field]] <- .genflow_validate_local_url(validated[[field]], field)
  }

  for (field in c(
    "stt_native_executable",
    "stt_native_crispasr_executable",
    "stt_native_moss_transcribe_executable",
    "stt_native_model",
    "moss_cpp_executable",
    "moss_cpp_model"
  )) {
    if (nzchar(validated[[field]]) &&
        grepl("[/\\\\]", validated[[field]])) {
      validated[[field]] <- path.expand(validated[[field]])
    }
  }
  if (.stt_is_crispasr_hf_reference(validated$stt_native_model)) {
    reference <- tryCatch(
      .stt_parse_crispasr_hf_reference(validated$stt_native_model),
      error = function(e) NULL
    )
    if (!is.null(reference)) {
      validated$stt_native_model <- reference$reference
    }
  }
  # Legacy keys remain accepted on input but are never persisted as active
  # state. This makes migration one-way and lets users clear canonical values.
  validated$moss_cpp_executable <- ""
  validated$moss_cpp_model <- ""
  validated$moss_cpp_device <- "auto"
  validated$version <- 5L
  validated
}

.genflow_read_local_config <- function(path = NULL) {
  config_path <- .genflow_local_config_path(path)
  if (!file.exists(config_path)) {
    return(.genflow_local_config_defaults())
  }

  parsed <- tryCatch(
    jsonlite::fromJSON(config_path, simplifyVector = FALSE),
    error = function(e) {
      stop(
        "Could not read local inference config at ", config_path, ": ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
  if (!is.list(parsed) || is.null(names(parsed))) {
    stop(
      "Local inference config must contain a JSON object: ",
      config_path,
      call. = FALSE
    )
  }

  # Forward-compatible reads ignore fields created by a newer genflow.
  parsed <- parsed[intersect(names(parsed), names(.genflow_local_config_defaults()))]
  parsed <- .genflow_migrate_legacy_native_config(parsed)
  .genflow_validate_local_config(parsed)
}

.genflow_write_local_config <- function(config, path = NULL) {
  config_path <- .genflow_local_config_path(path)
  config <- .genflow_validate_local_config(config)
  omitted <- c(
    "moss_cpp_executable",
    "moss_cpp_model",
    "moss_cpp_device"
  )
  if (!nzchar(config$stt_native_executable %||% "")) {
    omitted <- c(omitted, "stt_native_executable")
  }
  serialized <- config[setdiff(names(config), omitted)]
  dir.create(dirname(config_path), recursive = TRUE, showWarnings = FALSE)

  temporary <- tempfile(
    pattern = paste0(".", basename(config_path), "-"),
    tmpdir = dirname(config_path)
  )
  on.exit(unlink(temporary), add = TRUE)
  jsonlite::write_json(
    serialized,
    path = temporary,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )

  replaced <- file.rename(temporary, config_path)
  if (!isTRUE(replaced)) {
    replaced <- file.copy(temporary, config_path, overwrite = TRUE)
  }
  if (!isTRUE(replaced)) {
    stop(
      "Could not save local inference config at ", config_path, ".",
      call. = FALSE
    )
  }
  invisible(config)
}

#' Configure local inference backends
#'
#' Reads or updates genflow's non-secret local inference settings. Calling the
#' function without `config` or named settings returns the saved configuration.
#' Tokens are deliberately not stored here; use the credential manager or
#' environment variables for secrets. CrispASR
#' `hf://OWNER/REPO/FILE` selectors and supported Hugging Face
#' `/blob/main/FILE` URLs are normalized to `hf://OWNER/REPO:FILE` when valid.
#'
#' @param config Optional named list of settings to update.
#' @param ... Named settings to update. Supported names include
#'   `ollama_base_url`, `ollama_model`, `llamacpp_base_url`,
#'   `llamacpp_model`, `stt_server_base_url`, `stt_server_model`,
#'   `stt_native_engine`, `stt_native_crispasr_executable`,
#'   `stt_native_moss_transcribe_executable`, `stt_native_executable`,
#'   `stt_native_model`,
#'   `stt_native_backend`, `stt_native_quant`, and `stt_native_device`.
#'   The two engine-specific executable fields are persisted independently, so
#'   changing `stt_native_engine` never erases either path.
#'   `stt_native_executable` remains an accepted one-way compatibility alias
#'   for older code and is copied into the selected engine's field.
#'   `stt_native_quant` is a CrispASR registry preference used only with
#'   `model = "auto"`; it does not assert that a remote artifact exists. The
#'   old `moss_cpp_*` names are accepted only as migration aliases.
#' @param path Optional JSON config path. Primarily useful for portable
#'   installations and tests.
#' @param save Logical; persist updates when TRUE.
#'
#' @return A named list containing the normalized configuration.
#' @export
gen_local_config <- function(config = NULL, ..., path = NULL, save = TRUE) {
  updates <- list(...)
  if (!is.null(config)) {
    if (!is.list(config) || is.null(names(config))) {
      stop("`config` must be a named list.", call. = FALSE)
    }
    duplicate_names <- intersect(names(config), names(updates))
    if (length(duplicate_names)) {
      stop(
        "Settings supplied twice: ",
        paste(duplicate_names, collapse = ", "),
        call. = FALSE
      )
    }
    updates <- c(config, updates)
  }

  current <- .genflow_read_local_config(path)
  if (!length(updates)) {
    return(current)
  }
  if (is.null(names(updates)) || any(!nzchar(names(updates)))) {
    stop("All local inference updates must be named.", call. = FALSE)
  }

  unknown <- setdiff(names(updates), .genflow_local_config_fields)
  if (length(unknown)) {
    stop(
      "Unknown local inference setting",
      if (length(unknown) > 1L) "s" else "",
      ": ",
      paste(unknown, collapse = ", "),
      call. = FALSE
    )
  }
  current[names(updates)] <- updates
  if ("stt_native_executable" %in% names(updates)) {
    target_engine <- .stt_normalize_native_engine(
      current$stt_native_engine %||% "auto"
    )
    if (identical(target_engine, "auto")) {
      target_engine <- .stt_native_engine_from_executable(
        updates$stt_native_executable
      )
    }
    target_field <- switch(
      target_engine,
      crispasr = "stt_native_crispasr_executable",
      `moss-transcribe` = "stt_native_moss_transcribe_executable",
      NULL
    )
    if (!is.null(target_field)) {
      supplied_target <- if (target_field %in% names(updates)) {
        .genflow_local_scalar(updates[[target_field]], target_field)
      } else {
        ""
      }
      if (!target_field %in% names(updates) || !nzchar(supplied_target)) {
        current[[target_field]] <- updates$stt_native_executable
      }
      current$stt_native_executable <- ""
    }
  } else if ("stt_native_engine" %in% names(updates) &&
             nzchar(current$stt_native_executable %||% "")) {
    target_engine <- .stt_normalize_native_engine(
      current$stt_native_engine %||% "auto"
    )
    target_field <- switch(
      target_engine,
      crispasr = "stt_native_crispasr_executable",
      `moss-transcribe` = "stt_native_moss_transcribe_executable",
      NULL
    )
    if (!is.null(target_field) && !nzchar(current[[target_field]] %||% "")) {
      current[[target_field]] <- current$stt_native_executable
      current$stt_native_executable <- ""
    }
  }
  legacy_to_native <- c(
    moss_cpp_executable = "stt_native_moss_transcribe_executable",
    moss_cpp_model = "stt_native_model",
    moss_cpp_device = "stt_native_device"
  )
  for (legacy_field in names(legacy_to_native)) {
    native_field <- unname(legacy_to_native[[legacy_field]])
    if (legacy_field %in% names(updates) &&
        !native_field %in% names(updates)) {
      current[[native_field]] <- updates[[legacy_field]]
    }
  }
  legacy_updates <- intersect(names(updates), names(legacy_to_native))
  if (length(legacy_updates) &&
      !"stt_native_engine" %in% names(updates)) {
    current$stt_native_engine <- "moss-transcribe"
  }
  result <- .genflow_validate_local_config(current)
  if (isTRUE(save)) {
    .genflow_write_local_config(result, path)
  }
  result
}

.genflow_first_env <- function(names) {
  for (name in names) {
    value <- trimws(Sys.getenv(name, unset = ""))
    if (nzchar(value)) {
      return(value)
    }
  }
  ""
}

.genflow_local_setting <- function(field,
                                   env = character(),
                                   default = "",
                                   config = NULL) {
  env_value <- .genflow_first_env(env)
  if (nzchar(env_value)) {
    return(env_value)
  }
  config <- config %||% .genflow_read_local_config()
  value <- .genflow_local_scalar(config[[field]] %||% "", field)
  if (nzchar(value)) value else default
}

.genflow_local_effective_config <- function(config = NULL) {
  config <- .genflow_validate_local_config(
    config %||% .genflow_read_local_config()
  )
  mapping <- list(
    ollama_base_url = c("OLLAMA_BASE_URL"),
    ollama_model = c("OLLAMA_MODEL"),
    llamacpp_base_url = c("LLAMACPP_BASE_URL", "LLAMA_CPP_BASE_URL"),
    llamacpp_model = c("LLAMACPP_MODEL", "LLAMA_CPP_MODEL"),
    stt_server_base_url = c("GENFLOW_STT_BASE_URL"),
    stt_server_model = c("GENFLOW_STT_MODEL"),
    stt_native_engine = c("GENFLOW_STT_NATIVE_ENGINE"),
    stt_native_executable = c("GENFLOW_STT_NATIVE_EXECUTABLE"),
    stt_native_model = c("GENFLOW_STT_NATIVE_MODEL"),
    stt_native_backend = c("GENFLOW_STT_NATIVE_BACKEND"),
    stt_native_quant = c("GENFLOW_STT_NATIVE_QUANT"),
    stt_native_device = c("GENFLOW_STT_NATIVE_DEVICE")
  )
  for (field in names(mapping)) {
    env_value <- .genflow_first_env(mapping[[field]])
    if (nzchar(env_value)) {
      config[[field]] <- env_value
    }
  }
  legacy_native_env <- any(nzchar(c(
    Sys.getenv("GENFLOW_MOSS_CPP_EXECUTABLE", unset = ""),
    Sys.getenv("GENFLOW_MOSS_CPP_MODEL", unset = ""),
    Sys.getenv("GENFLOW_MOSS_CPP_DEVICE", unset = "")
  )))
  if (legacy_native_env &&
      identical(tolower(config$stt_native_engine), "auto") &&
      !nzchar(Sys.getenv("GENFLOW_STT_NATIVE_ENGINE", unset = ""))) {
    config$stt_native_engine <- "moss-transcribe"
  }
  effective_native_engine <- .stt_normalize_native_engine(
    config$stt_native_engine
  )
  if (identical(effective_native_engine, "moss-transcribe")) {
    legacy_mapping <- c(
      stt_native_executable = "GENFLOW_MOSS_CPP_EXECUTABLE",
      stt_native_model = "GENFLOW_MOSS_CPP_MODEL",
      stt_native_device = "GENFLOW_MOSS_CPP_DEVICE"
    )
    canonical_mapping <- c(
      stt_native_executable = "GENFLOW_STT_NATIVE_EXECUTABLE",
      stt_native_model = "GENFLOW_STT_NATIVE_MODEL",
      stt_native_device = "GENFLOW_STT_NATIVE_DEVICE"
    )
    for (field in names(legacy_mapping)) {
      canonical_value <- trimws(Sys.getenv(
        canonical_mapping[[field]],
        unset = ""
      ))
      legacy_value <- trimws(Sys.getenv(
        legacy_mapping[[field]],
        unset = ""
      ))
      if (!nzchar(canonical_value) && nzchar(legacy_value)) {
        config[[field]] <- legacy_value
      }
    }
  }
  .genflow_validate_local_config(config)
}

.genflow_resolve_executable <- function(value, alternatives = character()) {
  candidates <- c(value, alternatives)
  candidates <- trimws(as.character(candidates))
  candidates <- unique(candidates[nzchar(candidates)])
  for (candidate in candidates) {
    resolved <- if (grepl("[/\\\\]", candidate)) {
      path.expand(candidate)
    } else {
      unname(Sys.which(candidate))
    }
    usable <- nzchar(resolved) &&
      file.exists(resolved) &&
      !dir.exists(resolved) &&
      (.Platform$OS.type == "windows" ||
        isTRUE(file.access(resolved, mode = 1L) == 0L))
    if (usable) {
      # Preserve the final symlink so wrapper executables keep their own
      # invocation semantics.
      directory <- normalizePath(
        dirname(resolved),
        winslash = "/",
        mustWork = TRUE
      )
      return(file.path(directory, basename(resolved)))
    }
  }
  ""
}

.genflow_diagnostic_row <- function(component, status, detail) {
  data.frame(
    component = as.character(component),
    status = as.character(status),
    detail = as.character(detail),
    stringsAsFactors = FALSE
  )
}

.genflow_crispasr_cache_dirs <- function() {
  cache_dir <- trimws(Sys.getenv("CRISPASR_CACHE_DIR", unset = ""))
  models_dir <- trimws(Sys.getenv("CRISPASR_MODELS_DIR", unset = ""))
  default_dir <- file.path(path.expand("~"), ".cache", "crispasr")
  canonical_dir <- if (nzchar(cache_dir)) {
    cache_dir
  } else if (nzchar(models_dir)) {
    models_dir
  } else {
    default_dir
  }

  directories <- c(
    canonical_dir,
    models_dir,
    "/mnt/storage/gguf-models",
    "/Volumes/backups/ai/crispasr-models",
    default_dir,
    file.path(path.expand("~"), ".cache", "crispasr-models"),
    file.path(path.expand("~"), ".cache", "huggingface", "hub")
  )
  unique(path.expand(directories[nzchar(directories)]))
}

.genflow_crispasr_cached_model <- function(filename, repository = "") {
  expected_source <- ""
  repository <- trimws(as.character(repository %||% "")[1])
  if (!is.na(repository) && nzchar(repository)) {
    expected_source <- paste0(
      "https://huggingface.co/",
      repository,
      "/resolve/main/",
      filename
    )
  }

  for (cache_dir in .genflow_crispasr_cache_dirs()) {
    candidate <- file.path(cache_dir, filename)
    info <- suppressWarnings(file.info(candidate))
    usable <- file.exists(candidate) &&
      !dir.exists(candidate) &&
      is.finite(info$size[[1]]) &&
      info$size[[1]] > 0
    if (!usable) next

    sidecar <- paste0(candidate, ".src")
    sidecar_info <- suppressWarnings(file.info(sidecar))
    has_sidecar <- file.exists(sidecar) &&
      !dir.exists(sidecar) &&
      is.finite(sidecar_info$size[[1]]) &&
      sidecar_info$size[[1]] > 0
    if (has_sidecar && nzchar(expected_source)) {
      sidecar_size <- sidecar_info$size[[1]]
      recorded_source <- if (sidecar_size <= 65536) {
        tryCatch(
          suppressWarnings(
            readChar(sidecar, nchars = sidecar_size, useBytes = TRUE)
          ),
          error = function(e) ""
        )
      } else {
        ""
      }
      recorded_source <- sub(
        "[\r\n ]+$",
        "",
        recorded_source,
        perl = TRUE
      )
      if (!identical(recorded_source, expected_source) &&
          !.genflow_crispasr_same_hf_artifact(
            recorded_source,
            expected_source
          )) {
        next
      }
    }

    return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
  }
  ""
}

.genflow_endpoint_url <- function(base_url, path) {
  base_url <- sub("/+$", "", trimws(as.character(base_url)[1]))
  path <- sub("^/+", "", trimws(as.character(path)[1]))
  if (!nzchar(path)) {
    return(base_url)
  }
  if (endsWith(tolower(base_url), paste0("/", tolower(path)))) {
    return(base_url)
  }
  if (grepl("/v1$", base_url, ignore.case = TRUE) &&
      startsWith(tolower(path), "v1/")) {
    return(paste0(base_url, "/", substring(path, 4L)))
  }
  if (grepl(
    "/v1/audio/transcriptions$",
    base_url,
    ignore.case = TRUE
  ) && identical(tolower(path), "v1/models")) {
    return(sub(
      "/audio/transcriptions$",
      "/models",
      base_url,
      ignore.case = TRUE
    ))
  }
  paste0(base_url, "/", path)
}

.genflow_endpoint_diagnostic <- function(component,
                                         base_url,
                                         path,
                                         timeout,
                                         headers = character()) {
  if (!nzchar(base_url)) {
    return(.genflow_diagnostic_row(component, "info", "Not configured."))
  }
  url <- .genflow_endpoint_url(base_url, path)
  header_config <- if (length(headers)) {
    httr::add_headers(.headers = headers)
  } else {
    NULL
  }
  response <- tryCatch(
    if (is.null(header_config)) {
      httr::GET(url, httr::timeout(timeout))
    } else {
      httr::GET(url, header_config, httr::timeout(timeout))
    },
    error = function(e) e
  )
  if (inherits(response, "error")) {
    return(.genflow_diagnostic_row(
      component,
      "warning",
      paste0(url, " is unavailable: ", conditionMessage(response))
    ))
  }
  status <- httr::status_code(response)
  .genflow_diagnostic_row(
    component,
    if (status >= 200L && status < 300L) "ok" else "warning",
    paste0(url, " returned HTTP ", status, ".")
  )
}

.genflow_native_stt_diagnostics <- function(config,
                                            timeout,
                                            check_remote = FALSE) {
  config <- .genflow_validate_local_config(config)
  engine_result <- tryCatch(
    .stt_resolve_native_engine(
      native_engine = config$stt_native_engine,
      executable = NULL,
      model = "",
      native_backend = "",
      config = config
    ),
    error = function(e) e
  )
  engine <- if (inherits(engine_result, "error")) {
    "auto"
  } else {
    engine_result
  }
  engine_error_message <- if (inherits(engine_result, "error")) {
    conditionMessage(engine_result)
  } else {
    ""
  }
  configured_engine_paths <- c(
    config$stt_native_crispasr_executable %||% "",
    config$stt_native_moss_transcribe_executable %||% ""
  )
  blocking_engine_error <- nzchar(engine_error_message) && (
    any(nzchar(trimws(as.character(configured_engine_paths)))) ||
      grepl("More than one native STT engine", engine_error_message, fixed = TRUE)
  )
  registry <- .stt_native_engine_registry()
  configured_executable <- if (!identical(engine, "auto")) {
    .stt_native_executable_candidate(
      engine,
      executable = NULL,
      config = config
    )
  } else {
    .genflow_local_scalar(
      config$stt_native_executable,
      "stt_native_executable"
    )
  }
  alternatives <- if (identical(engine, "auto")) {
    unlist(lapply(registry, `[[`, "executables"), use.names = FALSE)
  } else {
    registry[[engine]]$executables
  }
  executable <- if (nzchar(configured_executable)) {
    .genflow_resolve_executable(configured_executable)
  } else {
    .genflow_resolve_executable("", alternatives)
  }
  detected_engine <- engine
  if (identical(detected_engine, "auto") && nzchar(executable)) {
    detected_engine <- .stt_native_engine_from_executable(executable)
    if (!nzchar(detected_engine)) detected_engine <- "auto"
  }

  cli_row <- if (blocking_engine_error) {
    .genflow_diagnostic_row(
      "Native STT CLI",
      "error",
      engine_error_message
    )
  } else if (!nzchar(executable)) {
    configured_path <- path.expand(configured_executable)
    .genflow_diagnostic_row(
      "Native STT CLI",
      if (nzchar(configured_executable)) "error" else "info",
      if (nzchar(configured_executable)) {
        if (dir.exists(configured_path)) {
          expected_binary <- if (identical(engine, "moss-transcribe")) {
            "build/bin/moss-transcribe"
          } else {
            "build/bin/crispasr"
          }
          paste0(
            "Configured executable points to a directory: ",
            configured_executable,
            ". Select the executable file, normally ", expected_binary, "."
          )
        } else if (file.exists(configured_path)) {
          paste0(
            "Configured file is not executable: ",
            configured_executable
          )
        } else {
          paste0("Configured executable was not found: ", configured_executable)
        }
      } else if (inherits(engine_result, "error")) {
        conditionMessage(engine_result)
      } else {
        paste0(
          "No ", registry[[engine]]$label,
          " executable was found. Native STT is optional."
        )
      }
    )
  } else {
    output <- tryCatch(
      suppressWarnings(system2(
        executable,
        "--help",
        stdout = TRUE,
        stderr = TRUE,
        timeout = timeout
      )),
      error = function(e) e
    )
    if (inherits(output, "error")) {
      .genflow_diagnostic_row(
        "Native STT CLI",
        "error",
        paste0(executable, " could not run: ", conditionMessage(output))
      )
    } else {
      status <- as.integer(attr(output, "status") %||% 0L)
      detail <- trimws(paste(output, collapse = "\n"))
      pattern <- if (identical(detected_engine, "crispasr")) {
        "(crispasr|list-backends|ASR backends)"
      } else if (identical(detected_engine, "moss-transcribe")) {
        "(moss-transcribe|transcribe)"
      } else {
        "(crispasr|moss-transcribe|transcribe)"
      }
      recognized <- grepl(pattern, detail, ignore.case = TRUE, perl = TRUE)
      .genflow_diagnostic_row(
        "Native STT CLI",
        if (identical(status, 124L)) {
          "error"
        } else if (recognized) {
          "ok"
        } else if (!identical(detected_engine, "auto")) {
          "error"
        } else {
          "warning"
        },
        if (identical(status, 124L)) {
          paste0(executable, " timed out while running --help.")
        } else if (recognized) {
          paste0(
            executable,
            if (!identical(detected_engine, "auto")) {
              paste0(" (engine: ", detected_engine, ")")
            } else {
              ""
            }
          )
        } else {
          paste0(
            executable,
            " did not expose a recognizable ",
            if (!identical(detected_engine, "auto")) {
              paste0(registry[[detected_engine]]$label, " help response.")
            } else {
              "native STT CLI help response."
            }
          )
        }
      )
    }
  }

  inventory <- tryCatch(
    .genflow_crispasr_inventory(config),
    error = function(e) e
  )
  cache_row <- if (inherits(inventory, "error")) {
    .genflow_diagnostic_row(
      "Native STT cache",
      "warning",
      conditionMessage(inventory)
    )
  } else {
    managed <- inventory[inventory$managed, , drop = FALSE]
    total_bytes <- sum(managed$size_bytes, na.rm = TRUE)
    detail <- if (!nrow(managed)) {
      paste0(
        "No downloaded models in ",
        .genflow_crispasr_canonical_cache_dir(create = FALSE),
        "."
      )
    } else {
      paste0(
        nrow(managed),
        " downloaded model",
        if (nrow(managed) == 1L) "" else "s",
        " (",
        .genflow_crispasr_format_size(total_bytes),
        ") in ",
        .genflow_crispasr_canonical_cache_dir(create = FALSE),
        "."
      )
    }
    .genflow_diagnostic_row(
      "Native STT cache",
      if (nrow(managed)) "ok" else "info",
      detail
    )
  }

  list(cli_row, cache_row)
}

.genflow_moss_cpp_diagnostics <- function(config, timeout) {
  .genflow_native_stt_diagnostics(config, timeout)
}

.genflow_vulkan_diagnostic <- function(timeout, required = FALSE) {
  executable <- .genflow_resolve_executable("", "vulkaninfo")
  if (!nzchar(executable)) {
    return(.genflow_diagnostic_row(
      "Vulkan",
      if (isTRUE(required)) "error" else "info",
      "vulkaninfo was not found; Vulkan availability was not verified."
    ))
  }

  output <- tryCatch(
    suppressWarnings(system2(
      executable,
      "--summary",
      stdout = TRUE,
      stderr = TRUE,
      timeout = timeout
    )),
    error = function(e) e
  )
  if (inherits(output, "error")) {
    return(.genflow_diagnostic_row(
      "Vulkan",
      if (isTRUE(required)) "error" else "warning",
      paste0("vulkaninfo could not run: ", conditionMessage(output))
    ))
  }
  status <- as.integer(attr(output, "status") %||% 0L)
  detail <- trimws(paste(output, collapse = "\n"))
  device_lines <- grep("deviceName", output, value = TRUE, fixed = TRUE)
  if (length(device_lines)) {
    device_lines <- trimws(sub(
      "^.*deviceName[[:space:]]*=[[:space:]]*",
      "",
      device_lines
    ))
    device <- paste0(
      "Vulkan device: ",
      paste(unique(device_lines[nzchar(device_lines)]), collapse = "; ")
    )
  } else {
    device <- ""
  }
  if (nchar(device) > 500L) {
    device <- substr(device, 1L, 500L)
  }

  .genflow_diagnostic_row(
    "Vulkan",
    if (identical(status, 0L)) "ok" else if (isTRUE(required)) "error" else "warning",
    if (identical(status, 0L)) {
      if (nzchar(device)) device else "The Vulkan loader responded successfully."
    } else if (identical(status, 124L)) {
      paste0("vulkaninfo timed out after ", timeout, " seconds.")
    } else {
      paste0(
        "vulkaninfo exited with status ",
        status,
        if (nzchar(detail)) paste0(": ", substr(detail, 1L, 500L)) else "."
      )
    }
  )
}

#' Diagnose local inference readiness
#'
#' Performs read-only checks for FFmpeg, the optional native STT CLI and cache,
#' Vulkan, and configured local HTTP backends. A warning means the component is
#' optional or currently unavailable; it does not prevent unrelated genflow
#' providers from working. Diagnostics do not load a model; a real
#' transcription remains the compatibility check.
#'
#' @param config Optional configuration returned by [gen_local_config()].
#' @param check_endpoints Logical; probe configured local HTTP services.
#' @param timeout Numeric timeout in seconds for each subprocess or HTTP probe.
#' @param adapters Optional local adapter id or character vector to check.
#'   Supported ids are `"ollama"`, `"llamacpp"`, `"local-native"`, and
#'   `"local-openai"`. The default checks all adapters.
#'
#' @return A data frame with `component`, `status`, and `detail` columns.
#' @export
gen_local_diagnostics <- function(config = NULL,
                                  check_endpoints = TRUE,
                                  timeout = 5,
                                  adapters = NULL) {
  timeout <- suppressWarnings(as.numeric(timeout)[1])
  if (is.na(timeout) || !is.finite(timeout) || timeout <= 0) {
    stop("`timeout` must be a positive number.", call. = FALSE)
  }
  config <- .genflow_local_effective_config(config)
  supported_adapters <- c(
    "ollama",
    "llamacpp",
    "local-native",
    "local-openai"
  )
  adapter_aliases <- c(
    "native" = "local-native",
    "native-stt" = "local-native",
    "stt-server" = "local-openai",
    "openai-compatible" = "local-openai",
    "llama-cpp" = "llamacpp",
    "llama_cpp" = "llamacpp"
  )
  if (is.null(adapters) || !length(adapters) ||
      any(tolower(trimws(as.character(adapters))) == "all")) {
    adapters <- supported_adapters
  } else {
    adapters <- tolower(trimws(as.character(adapters)))
    mapped <- unname(adapter_aliases[adapters])
    replace <- !is.na(mapped)
    adapters[replace] <- mapped[replace]
    unknown <- setdiff(adapters, supported_adapters)
    if (length(unknown)) {
      stop(
        "Unsupported local diagnostic adapter",
        if (length(unknown) > 1L) "s" else "",
        ": ",
        paste(unique(unknown), collapse = ", "),
        call. = FALSE
      )
    }
    adapters <- unique(adapters)
  }
  wants <- function(adapter) adapter %in% adapters
  rows <- list()

  if (wants("local-native")) {
    ffmpeg <- .genflow_resolve_executable("", "ffmpeg")
    rows[[length(rows) + 1L]] <- .genflow_diagnostic_row(
      "FFmpeg",
      if (nzchar(ffmpeg)) "ok" else "warning",
      if (nzchar(ffmpeg)) ffmpeg else
        "FFmpeg was not found; some audio formats may not load locally."
    )
  }

  if (wants("local-native")) {
    rows <- c(rows, .genflow_native_stt_diagnostics(
      config,
      timeout,
      check_remote = isTRUE(check_endpoints)
    ))
    rows[[length(rows) + 1L]] <- .genflow_vulkan_diagnostic(
      timeout,
      required = identical(config$stt_native_device, "vulkan")
    )
  }

  if (isTRUE(check_endpoints)) {
    if (wants("ollama")) {
      rows[[length(rows) + 1L]] <- .genflow_endpoint_diagnostic(
        "Ollama",
        config$ollama_base_url,
        "api/tags",
        timeout
      )
    }
    if (wants("llamacpp")) {
      llama_key <- .llamacpp_api_key()
      rows[[length(rows) + 1L]] <- .genflow_endpoint_diagnostic(
        "llama.cpp",
        .llamacpp_base_url(config = config),
        "v1/models",
        timeout,
        headers = if (nzchar(llama_key)) {
          c(Authorization = paste("Bearer", llama_key))
        } else {
          character()
        }
      )
    }
    if (wants("local-openai")) {
      stt_key <- trimws(Sys.getenv("GENFLOW_STT_API_KEY", unset = ""))
      rows[[length(rows) + 1L]] <- .genflow_endpoint_diagnostic(
        "Local STT server",
        config$stt_server_base_url,
        "v1/models",
        timeout,
        headers = if (nzchar(stt_key)) {
          c(Authorization = paste("Bearer", stt_key))
        } else {
          character()
        }
      )
    }
  }

  if (!length(rows)) {
    return(data.frame(
      component = character(),
      status = character(),
      detail = character(),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, rows)
}
