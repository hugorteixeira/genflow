# Local inference configuration and diagnostics -----------------------------

.genflow_local_config_fields <- c(
  "python",
  "hf_cache_dir",
  "hf_stt_model",
  "hf_revision",
  "hf_stt_profile",
  "device",
  "dtype",
  "ollama_base_url",
  "ollama_model",
  "llamacpp_base_url",
  "llamacpp_model",
  "stt_server_base_url",
  "stt_server_model",
  "stt_native_engine",
  "stt_native_executable",
  "stt_native_model",
  "stt_native_backend",
  "stt_native_device",
  "moss_cpp_executable",
  "moss_cpp_model",
  "moss_cpp_device"
)

.genflow_local_config_defaults <- function() {
  list(
    version = 2L,
    python = "",
    hf_cache_dir = "",
    hf_stt_model = "openai/whisper-large-v3-turbo",
    hf_revision = "",
    hf_stt_profile = "auto",
    device = "auto",
    dtype = "auto",
    ollama_base_url = "http://127.0.0.1:11434",
    ollama_model = "",
    llamacpp_base_url = "http://127.0.0.1:8080",
    llamacpp_model = "",
    stt_server_base_url = "http://127.0.0.1:8000",
    stt_server_model = "",
    stt_native_engine = "auto",
    stt_native_executable = "",
    stt_native_model = "",
    stt_native_backend = "",
    stt_native_device = "auto",
    moss_cpp_executable = "",
    moss_cpp_model = "",
    moss_cpp_device = "auto"
  )
}

.genflow_migrate_legacy_native_config <- function(config) {
  if (!is.list(config)) return(config)
  canonical_fields <- c(
    "stt_native_engine",
    "stt_native_executable",
    "stt_native_model",
    "stt_native_backend",
    "stt_native_device"
  )
  # A v2 key is an explicit source of truth, including an intentionally empty
  # value. Legacy fields are copied only when reading a genuinely old object.
  if (any(canonical_fields %in% names(config))) return(config)

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

  validated$hf_stt_profile <- tolower(validated$hf_stt_profile)
  if (!validated$hf_stt_profile %in% c("auto", "transformers", "moss")) {
    stop(
      "`hf_stt_profile` must be \"auto\", \"transformers\", or \"moss\".",
      call. = FALSE
    )
  }

  if (grepl("[[:space:][:cntrl:]]", validated$hf_revision, perl = TRUE)) {
    stop(
      "`hf_revision` cannot contain whitespace or control characters.",
      call. = FALSE
    )
  }

  validated$device <- tolower(validated$device)
  if (!grepl("^(auto|cpu|mps|cuda(:[0-9]+)?|rocm|hip)$", validated$device)) {
    stop(
      "`device` must be auto, cpu, mps, cuda, cuda:N, rocm, or hip.",
      call. = FALSE
    )
  }

  dtype_aliases <- c(fp32 = "float32", fp16 = "float16", bf16 = "bfloat16")
  validated$dtype <- tolower(validated$dtype)
  if (validated$dtype %in% names(dtype_aliases)) {
    validated$dtype <- unname(dtype_aliases[validated$dtype])
  }
  if (!validated$dtype %in% c("auto", "float32", "float16", "bfloat16")) {
    stop(
      "`dtype` must be auto, float32, float16, or bfloat16.",
      call. = FALSE
    )
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

  if (nzchar(validated$hf_cache_dir)) {
    validated$hf_cache_dir <- path.expand(validated$hf_cache_dir)
  }
  for (field in c(
    "stt_native_executable",
    "stt_native_model",
    "moss_cpp_executable",
    "moss_cpp_model"
  )) {
    if (nzchar(validated[[field]]) &&
        grepl("[/\\\\]", validated[[field]])) {
      validated[[field]] <- path.expand(validated[[field]])
    }
  }
  if (startsWith(tolower(validated$stt_native_model), "hf://")) {
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
  validated$version <- 2L
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
  serialized <- config[setdiff(
    names(config),
    c("moss_cpp_executable", "moss_cpp_model", "moss_cpp_device")
  )]
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
#' `hf://OWNER/REPO/FILE` selectors are normalized to
#' `hf://OWNER/REPO:FILE` when valid.
#'
#' @param config Optional named list of settings to update.
#' @param ... Named settings to update. Supported names include `python`,
#'   `hf_cache_dir`, `hf_stt_model`, `hf_revision`, `hf_stt_profile`, `device`,
#'   `dtype`, `ollama_base_url`, `ollama_model`, `llamacpp_base_url`,
#'   `llamacpp_model`, `stt_server_base_url`, `stt_server_model`,
#'   `stt_native_engine`, `stt_native_executable`, `stt_native_model`,
#'   `stt_native_backend`, and `stt_native_device`. The old `moss_cpp_*`
#'   names are accepted only as migration aliases.
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
  legacy_to_native <- c(
    moss_cpp_executable = "stt_native_executable",
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
    python = c("GENFLOW_PYTHON"),
    hf_cache_dir = c("HF_HOME"),
    hf_stt_model = c("GENFLOW_HF_STT_MODEL"),
    hf_revision = c("GENFLOW_HF_REVISION"),
    hf_stt_profile = c("GENFLOW_HF_STT_PROFILE"),
    device = c("GENFLOW_LOCAL_DEVICE"),
    dtype = c("GENFLOW_LOCAL_DTYPE"),
    ollama_base_url = c("OLLAMA_BASE_URL"),
    ollama_model = c("OLLAMA_MODEL"),
    llamacpp_base_url = c("LLAMACPP_BASE_URL", "LLAMA_CPP_BASE_URL"),
    llamacpp_model = c("LLAMACPP_MODEL", "LLAMA_CPP_MODEL"),
    stt_server_base_url = c("GENFLOW_STT_BASE_URL"),
    stt_server_model = c("GENFLOW_STT_MODEL"),
    stt_native_engine = c("GENFLOW_STT_NATIVE_ENGINE"),
    stt_native_executable = c(
      "GENFLOW_STT_NATIVE_EXECUTABLE",
      "GENFLOW_MOSS_CPP_EXECUTABLE"
    ),
    stt_native_model = c(
      "GENFLOW_STT_NATIVE_MODEL",
      "GENFLOW_MOSS_CPP_MODEL"
    ),
    stt_native_backend = c("GENFLOW_STT_NATIVE_BACKEND"),
    stt_native_device = c(
      "GENFLOW_STT_NATIVE_DEVICE",
      "GENFLOW_MOSS_CPP_DEVICE"
    )
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
      # Preserve the final symlink. Virtual environments commonly expose
      # `bin/python` as a link to the base interpreter; dereferencing it makes
      # Python lose that environment's pyvenv.cfg and site-packages.
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
      if (!identical(recorded_source, expected_source)) next
    }

    return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
  }
  ""
}

.genflow_moss_helpers_revision <- paste0(
  "9990574e6ac62390a21bcce25a914d66",
  "ac92c25e"
)

.genflow_moss_helpers_url <- function() {
  paste0(
    "https://github.com/OpenMOSS/MOSS-Transcribe-Diarize/archive/",
    .genflow_moss_helpers_revision,
    ".zip"
  )
}

.genflow_moss_install_command <- function(python) {
  requirement <- paste0(
    "moss-transcribe-diarize @ ",
    .genflow_moss_helpers_url()
  )
  quote_type <- if (identical(.Platform$OS.type, "windows")) "cmd" else "sh"
  paste(
    shQuote(as.character(python)[1], type = quote_type),
    "-m pip install",
    shQuote(requirement, type = quote_type)
  )
}

.genflow_moss_transformers_supported <- function(version) {
  version <- trimws(as.character(version %||% "")[1])
  if (is.na(version) || !nzchar(version)) {
    return(FALSE)
  }
  version <- sub("\\+.*$", "", version)
  lower <- tryCatch(
    utils::compareVersion(version, "5.6.0") >= 0L,
    error = function(e) FALSE
  )
  upper <- tryCatch(
    utils::compareVersion(version, "6.0.0") < 0L,
    error = function(e) FALSE
  )
  isTRUE(lower) && isTRUE(upper)
}

.genflow_python_diagnostic_result <- function(python,
                                              payload,
                                              requested_device = "auto",
                                              require_moss = FALSE) {
  requested_device <- tolower(as.character(requested_device %||% "auto")[1])
  if (is.na(requested_device) || !nzchar(requested_device)) {
    requested_device <- "auto"
  }

  missing <- c(
    if (!is.null(payload$transformers_error)) "transformers",
    if (!is.null(payload$torch_error)) "torch"
  )
  if (length(missing)) {
    detail <- sprintf(
      "%s (Python %s); missing or broken: %s.",
      python,
      payload$python %||% "unknown",
      paste(missing, collapse = ", ")
    )
    if (isTRUE(require_moss)) {
      detail <- paste0(
        detail,
        " The MOSS profile also needs the official GitHub helper and ",
        "Transformers >=5.6.0,<6.0.0. Install the helper into this exact ",
        "interpreter with: ",
        .genflow_moss_install_command(python),
        ". The command can upgrade dependencies; do not run it in an ",
        "environment owned by another application with incompatible pins."
      )
    }
    return(.genflow_diagnostic_row(
      "Python",
      "error",
      detail
    ))
  }

  hip <- as.character(payload$hip %||% "")[1]
  cuda <- as.character(payload$cuda %||% "")[1]
  hip <- if (is.na(hip)) "" else hip
  cuda <- if (is.na(cuda)) "" else cuda
  accelerator <- isTRUE(payload$accelerator)
  mps <- isTRUE(payload$mps)
  device_count <- suppressWarnings(as.integer(payload$device_count %||% 0L)[1])
  if (is.na(device_count)) {
    device_count <- 0L
  }
  device_name <- as.character(payload$device %||% "")[1]
  if (is.na(device_name)) {
    device_name <- ""
  }

  build <- if (nzchar(hip)) {
    paste0("ROCm/HIP ", hip)
  } else if (nzchar(cuda)) {
    paste0("CUDA ", cuda)
  } else {
    "CPU-only"
  }
  active <- if (accelerator) {
    if (nzchar(device_name)) {
      paste0("available (", device_name, ")")
    } else {
      "available"
    }
  } else if (mps) {
    "MPS available"
  } else {
    "unavailable"
  }
  detail <- sprintf(
    "%s; Python %s, torch %s, transformers %s; build %s; accelerator %s.",
    python,
    payload$python %||% "unknown",
    payload$torch %||% "unknown",
    payload$transformers %||% "unknown",
    build,
    active
  )
  moss_missing <- isTRUE(require_moss) &&
    !is.null(payload$moss_transcribe_diarize_error)
  transformers_version <- as.character(payload$transformers %||% "")[1]
  moss_transformers_incompatible <- isTRUE(require_moss) &&
    !.genflow_moss_transformers_supported(transformers_version)
  if (isTRUE(require_moss) && !moss_missing) {
    detail <- paste0(
      detail,
      " moss_transcribe_diarize ",
      payload$moss_transcribe_diarize %||% "installed",
      "."
    )
  }

  status <- "ok"
  issue <- ""
  if (requested_device %in% c("rocm", "hip")) {
    if (!nzchar(hip)) {
      status <- "error"
      issue <- paste0(
        "ROCm/HIP was requested, but this is a ", build,
        " PyTorch build. The packages are installed, but this wheel cannot use ",
        "an AMD GPU. Choose CPU, use a ROCm PyTorch build, or use ",
        "service = \"local-native\" with a Vulkan-enabled engine."
      )
    } else if (!accelerator) {
      status <- "error"
      issue <- paste0(
        "A ROCm PyTorch build is installed, but torch.cuda.is_available() ",
        "is FALSE; verify the ROCm runtime, GPU support, and permissions."
      )
    }
  } else if (grepl("^cuda(?::[0-9]+)?$", requested_device)) {
    requested_index <- if (grepl(":", requested_device, fixed = TRUE)) {
      as.integer(sub("^cuda:", "", requested_device))
    } else {
      0L
    }
    if (!nzchar(cuda)) {
      status <- "error"
      issue <- paste0(
        "CUDA was requested, but this is a ", build,
        " PyTorch build. Install a CUDA PyTorch build."
      )
    } else if (!accelerator) {
      status <- "error"
      issue <- paste0(
        "A CUDA PyTorch build is installed, but torch.cuda.is_available() ",
        "is FALSE; verify the NVIDIA driver and device access."
      )
    } else if (requested_index >= device_count) {
      status <- "error"
      issue <- sprintf(
        "CUDA device %d was requested, but PyTorch reports %d device(s).",
        requested_index,
        device_count
      )
    }
  } else if (identical(requested_device, "mps")) {
    if (!mps) {
      status <- "error"
      issue <- "MPS was requested, but PyTorch reports that MPS is unavailable."
    }
  } else if (identical(requested_device, "auto") &&
             !accelerator &&
             !mps) {
    status <- "warning"
    issue <- "No supported accelerator is currently available; inference will use CPU."
  }

  if (nzchar(issue)) {
    detail <- paste(detail, issue)
  }
  if (moss_missing) {
    status <- "error"
    detail <- paste0(
      detail,
      " The selected MOSS profile needs helper code that is distributed from ",
      "the official GitHub repository, separately from the Hugging Face model ",
      "files. Its import failed: ",
      as.character(payload$moss_transcribe_diarize_error)[1],
      ". Install the pinned helper into this exact interpreter with: ",
      .genflow_moss_install_command(python),
      ". This is not a bare `pip install moss-transcribe-diarize` command."
    )
  }
  if (moss_transformers_incompatible) {
    status <- "error"
    installed_version <- if (
      is.na(transformers_version) || !nzchar(transformers_version)
    ) {
      "an unknown version"
    } else {
      transformers_version
    }
    detail <- paste0(
      detail,
      " The pinned MOSS helper requires Transformers >=5.6.0,<6.0.0, but ",
      installed_version,
      " is installed. Do not upgrade this shared environment in place if ",
      "another application pins a conflicting Transformers version."
    )
  }
  .genflow_diagnostic_row("Python", status, detail)
}

.genflow_parse_python_probe_output <- function(output) {
  if (!length(output)) {
    return(NULL)
  }

  for (line in rev(as.character(output))) {
    line <- trimws(line)
    if (!startsWith(line, "{")) {
      next
    }
    payload <- tryCatch(
      jsonlite::fromJSON(line, simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (is.list(payload) && isTRUE(payload$probe_complete)) {
      return(payload)
    }
  }
  NULL
}

.genflow_python_diagnostic <- function(python,
                                       timeout,
                                       requested_device = "auto",
                                       require_moss = FALSE) {
  if (!nzchar(python)) {
    return(.genflow_diagnostic_row(
      "Python",
      "error",
      "Python was not found. Configure a dedicated Python 3 environment."
    ))
  }

  moss_probe <- if (isTRUE(require_moss)) {
    paste0(
      "\ntry:\n from importlib import metadata as _metadata; ",
      "import moss_transcribe_diarize as mtd; ",
      "from moss_transcribe_diarize import parse_transcript; ",
      "from moss_transcribe_diarize.inference_utils import ",
      "build_transcription_messages,generate_transcription; ",
      "out['moss_transcribe_diarize']=",
      "_metadata.version('moss-transcribe-diarize')",
      "\nexcept Exception as e: ",
      "out['moss_transcribe_diarize_error']=str(e)"
    )
  } else {
    ""
  }
  probe <- paste0(
    "import json,sys; out={'python':sys.version.split()[0],",
    "'executable':sys.executable}; ",
    "\ntry:\n import transformers; out['transformers']=transformers.__version__",
    "\nexcept Exception as e: out['transformers_error']=str(e)",
    "\ntry:\n import torch; out['torch']=torch.__version__; ",
    "out['hip']=getattr(torch.version,'hip',None); ",
    "out['cuda']=getattr(torch.version,'cuda',None); ",
    "out['accelerator']=bool(torch.cuda.is_available()); ",
    "out['device_count']=torch.cuda.device_count(); ",
    "out['device']=torch.cuda.get_device_name(0) if torch.cuda.is_available() else None; ",
    "out['mps']=bool(hasattr(torch.backends,'mps') and torch.backends.mps.is_available())",
    "\nexcept Exception as e: out['torch_error']=str(e)",
    moss_probe,
    "\nout['probe_complete']=True",
    "\nprint(json.dumps(out))"
  )
  output <- suppressWarnings(system2(
    python,
    c("-c", shQuote(probe)),
    stdout = TRUE,
    stderr = TRUE,
    timeout = timeout
  ))
  status_code <- as.integer(attr(output, "status") %||% 0L)
  text <- paste(output, collapse = "\n")
  payload <- .genflow_parse_python_probe_output(output)
  if (is.null(payload)) {
    reason <- if (identical(status_code, 124L)) {
      sprintf(" timed out after %s seconds", format(timeout, trim = TRUE))
    } else if (!identical(status_code, 0L)) {
      paste0(" exited with status ", status_code)
    } else {
      " returned an unreadable result"
    }
    return(.genflow_diagnostic_row(
      "Python",
      "error",
      paste0(
        python,
        " could not complete the dependency probe because it",
        reason,
        if (nzchar(text)) paste0(": ", substr(text, 1L, 500L)) else "."
      )
    ))
  }

  .genflow_python_diagnostic_result(
    python = python,
    payload = payload,
    requested_device = requested_device,
    require_moss = require_moss
  )
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

.genflow_native_stt_diagnostics <- function(config, timeout) {
  config <- .genflow_validate_local_config(config)
  engine_result <- tryCatch(
    .stt_resolve_native_engine(
      native_engine = config$stt_native_engine,
      executable = config$stt_native_executable,
      model = config$stt_native_model,
      native_backend = config$stt_native_backend,
      config = config
    ),
    error = function(e) e
  )
  engine <- if (inherits(engine_result, "error")) {
    "auto"
  } else {
    engine_result
  }
  registry <- .stt_native_engine_registry()
  configured_executable <- .genflow_local_scalar(
    config$stt_native_executable,
    "stt_native_executable"
  )
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
    executable_name <- tolower(basename(executable))
    detected_engine <- if (grepl("crispasr", executable_name, fixed = TRUE)) {
      "crispasr"
    } else if (grepl("moss-transcribe", executable_name, fixed = TRUE)) {
      "moss-transcribe"
    } else {
      "auto"
    }
  }

  cli_row <- if (!nzchar(executable)) {
    configured_path <- path.expand(configured_executable)
    .genflow_diagnostic_row(
      "Native STT CLI",
      if (nzchar(configured_executable)) "error" else "info",
      if (nzchar(configured_executable)) {
        if (dir.exists(configured_path)) {
          paste0(
            "Configured executable points to a directory: ",
            configured_executable,
            ". Select the CrispASR binary, normally build/bin/crispasr."
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
        } else if (identical(status, 0L) || recognized) {
          "ok"
        } else {
          "warning"
        },
        if (identical(status, 124L)) {
          paste0(executable, " timed out while running --help.")
        } else if (identical(status, 0L) || recognized) {
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
            " did not expose a recognizable native STT CLI help response."
          )
        }
      )
    }
  }

  model <- .genflow_local_scalar(
    config$stt_native_model,
    "stt_native_model"
  )
  backend <- .genflow_local_scalar(
    config$stt_native_backend,
    "stt_native_backend"
  )
  model_row <- if (!nzchar(model)) {
    .genflow_diagnostic_row(
      "Native STT model",
      "info",
      "No model is configured for the optional native STT engine."
    )
  } else if (identical(tolower(model), "auto")) {
    compatible <- identical(detected_engine, "crispasr") && nzchar(backend)
    .genflow_diagnostic_row(
      "Native STT model",
      if (compatible) "info" else "error",
      if (compatible) {
        paste0(
          "CrispASR will download and cache the default model for backend ",
          backend,
          " on first use."
        )
      } else {
        "`model = \"auto\"` requires engine crispasr and a native backend."
      }
    )
  } else if (startsWith(tolower(model), "hf://")) {
    reference <- tryCatch(
      .stt_parse_crispasr_hf_reference(model),
      error = function(e) e
    )
    compatible <- identical(detected_engine, "crispasr") &&
      !inherits(reference, "error")
    cached <- if (compatible) {
      .genflow_crispasr_cached_model(
        reference$file,
        reference$repository
      )
    } else {
      ""
    }
    .genflow_diagnostic_row(
      "Native STT model",
      if (!compatible) {
        "error"
      } else if (nzchar(cached)) {
        "ok"
      } else {
        "info"
      },
      if (compatible) {
        if (nzchar(cached)) {
          paste0(
            "Cached model: ",
            cached,
            ". CrispASR validates its architecture when loaded."
          )
        } else {
          paste0(
            reference$file,
            " will be downloaded from ",
            reference$repository,
            " by CrispASR on first use; it must be a ",
            "CrispASR-compatible model."
          )
        }
      } else if (!identical(detected_engine, "crispasr")) {
        "Remote native models require engine crispasr."
      } else {
        conditionMessage(reference)
      }
    )
  } else {
    model_path <- path.expand(model)
    info <- suppressWarnings(file.info(model_path))
    usable <- file.exists(model_path) &&
      !isTRUE(info$isdir[[1]]) &&
      is.finite(info$size[[1]]) &&
      info$size[[1]] > 0
    .genflow_diagnostic_row(
      "Native STT model",
      if (usable) "ok" else "error",
      if (usable) {
        normalizePath(model_path, winslash = "/", mustWork = TRUE)
      } else {
        paste0("Configured native model was not found or is empty: ", model_path)
      }
    )
  }

  list(cli_row, model_row)
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
#' Performs read-only checks for Python, PyTorch/Transformers, FFmpeg, the
#' optional native STT CLI and model, Vulkan, and configured local HTTP
#' backends. A warning means the component is optional or currently
#' unavailable; it does not prevent unrelated genflow providers from working.
#' Native executable directories are rejected, and an explicitly selected
#' CrispASR filename already present in its cache is reported as available.
#' Diagnostics do not load the model; a real transcription remains the
#' compatibility check.
#'
#' @param config Optional configuration returned by [gen_local_config()].
#' @param check_endpoints Logical; probe configured local HTTP services.
#' @param timeout Numeric timeout in seconds for each subprocess or HTTP probe.
#' @param adapters Optional local adapter id or character vector to check.
#'   Supported ids are `"ollama"`, `"llamacpp"`, `"hf-local"`,
#'   `"local-native"`, and `"local-openai"`. The default checks all adapters.
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
    "hf-local",
    "local-native",
    "local-openai"
  )
  adapter_aliases <- c(
    "hf" = "hf-local",
    "huggingface" = "hf-local",
    "python" = "hf-local",
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

  if (wants("hf-local")) {
    python <- .genflow_resolve_executable(
      config$python,
      c("python3", "python")
    )
    rows[[length(rows) + 1L]] <- .genflow_python_diagnostic(
      python,
      timeout,
      requested_device = config$device,
      require_moss = identical(config$hf_stt_profile, "moss") ||
        (
          identical(config$hf_stt_profile, "auto") &&
          grepl(
            "moss-transcribe-diarize",
            tolower(config$hf_stt_model),
            fixed = TRUE
          )
        )
    )
  }

  if (wants("hf-local") || wants("local-native")) {
    ffmpeg <- .genflow_resolve_executable("", "ffmpeg")
    rows[[length(rows) + 1L]] <- .genflow_diagnostic_row(
      "FFmpeg",
      if (nzchar(ffmpeg)) "ok" else "warning",
      if (nzchar(ffmpeg)) ffmpeg else
        "FFmpeg was not found; some audio formats may not load locally."
    )
  }

  if (wants("hf-local")) {
    cache_dir <- config$hf_cache_dir
    rows[[length(rows) + 1L]] <- .genflow_diagnostic_row(
      "Hugging Face cache",
      if (!nzchar(cache_dir) || dir.exists(cache_dir)) "ok" else "info",
      if (!nzchar(cache_dir)) {
        "Using the Hugging Face default cache."
      } else if (dir.exists(cache_dir)) {
        cache_dir
      } else {
        paste0(cache_dir, " will be created by Hugging Face on first use.")
      }
    )
  }

  if (wants("local-native")) {
    rows <- c(rows, .genflow_native_stt_diagnostics(config, timeout))
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
