# STT configuration signatures ---------------------------------------------

#' Fingerprint an effective STT configuration
#'
#' Builds a deterministic, secret-free fingerprint for the effective
#' speech-to-text configuration that [gen_stt()] would use. The signature is
#' intended for downstream cache validation: it changes when semantic
#' transcription settings, the effective endpoint, a resolved local model or
#' executable, their lightweight artifact signatures, or semantic final
#' post-processing change. Genflow keeps the provider/runtime checkpoint
#' fingerprint separate internally so post-processing upgrades can reuse
#' successful chunk inference.
#'
#' Credentials and operational controls do not affect the fingerprint.
#' Excluded controls include timeouts, polling/retry settings, checkpoint and
#' output directories, persistence, and output projection. Fixed-duration
#' chunking controls that can change the transcript (duration, bitrate, and
#' format) remain part of the signature and use [gen_stt()] validation.
#'
#' @param service STT provider identifier accepted by [gen_stt()].
#' @param model Optional provider model identifier. `NULL` resolves the same
#'   provider or saved-local default used by [gen_stt()].
#' @param language Optional language code. Empty values normalize to `NULL`.
#' @param stt_args A fully named list of additional [gen_stt()] arguments.
#'   Names must be unique. Do not repeat `audio`, `service`, `model`, or
#'   `language`; those values are owned by the explicit arguments above.
#'
#' @return A lowercase hexadecimal character scalar. The fingerprint contains
#'   no credential values and is suitable for equality comparison and cache
#'   metadata; it is not a reversible configuration report.
#'
#' @examples
#' a <- gen_stt_signature(
#'   service = "local-openai",
#'   model = "whisper-large-v3",
#'   stt_args = list(base_url = "http://127.0.0.1:8000")
#' )
#' b <- gen_stt_signature(
#'   service = "local-openai",
#'   model = "whisper-large-v3",
#'   stt_args = list(
#'     base_url = "http://127.0.0.1:8000",
#'     timeout_api = 900
#'   )
#' )
#' identical(a, b)
#'
#' @export
gen_stt_signature <- function(service = "openai",
                              model = NULL,
                              language = NULL,
                              stt_args = list()) {
  .stt_signature_build(
    service = service,
    model = model,
    language = language,
    stt_args = stt_args,
    include_postprocessing = TRUE
  )
}

#' Build the shared STT signature payload
#'
#' Public cache consumers include semantic post-processing revisions, while
#' Genflow's opaque chunk checkpoints intentionally fingerprint only the
#' provider/runtime work. This lets a merge-only upgrade rebuild the
#' final transcript from successful part checkpoints without retranscribing
#' their audio.
#'
#' @keywords internal
#' @noRd
.stt_signature_build <- function(service = "openai",
                                 model = NULL,
                                 language = NULL,
                                 stt_args = list(),
                                 include_postprocessing = TRUE) {
  stt_args <- .stt_signature_validate_args(stt_args)
  removed <- intersect(
    names(stt_args),
    c(
      "chunk_max_mb", "chunk_overlap_seconds", "chunk_speaker_linking",
      "diarize_speakers", "diarize_embedder"
    )
  )
  if (length(removed)) {
    stop(
      "Removed STT argument(s): ", paste(removed, collapse = ", "), ".",
      call. = FALSE
    )
  }
  legacy_moss_service <- .stt_is_legacy_moss_service(service)
  service <- .stt_normalize_service(service)
  if (!service %in% .stt_supported_services()) {
    stop("Unsupported STT service: ", service, call. = FALSE)
  }

  model <- .stt_signature_optional_scalar(model)
  language <- .stt_signature_optional_scalar(language)
  get_arg <- function(name, default = NULL) {
    if (name %in% names(stt_args)) stt_args[[name]] else default
  }

  if (legacy_moss_service) {
    requested_engine <- get_arg("native_engine")
    if (is.null(requested_engine)) {
      stt_args$native_engine <- "moss-transcribe"
    } else if (!identical(
      .stt_normalize_native_engine(requested_engine),
      "moss-transcribe"
    )) {
      stop(
        '`service = "moss-cpp"` is a compatibility alias for ',
        '`service = "local-native"` with ',
        '`native_engine = "moss-transcribe"`; it cannot select another engine.',
        call. = FALSE
      )
    }
  }

  chunk_options <- .stt_chunk_validate_options(
    chunking = get_arg("chunking", "auto"),
    chunk_bitrate_kbps = get_arg("chunk_bitrate_kbps", 48),
    chunk_segment_seconds = get_arg("chunk_segment_seconds"),
    chunk_format = get_arg("chunk_format", "auto"),
    checkpoint_dir = get_arg("checkpoint_dir"),
    checkpoint_retention = get_arg("checkpoint_retention", "all"),
    resume = get_arg("resume", TRUE),
    chunk_retry_forever = get_arg("chunk_retry_forever", TRUE),
    chunk_max_retries = get_arg("chunk_max_retries", 20),
    chunk_retry_wait_seconds = get_arg("chunk_retry_wait_seconds", 2),
    output = get_arg("output", "full")
  )

  semantic <- list(
    prompt = .stt_signature_optional_scalar(get_arg("prompt")),
    convert = .stt_validate_logical_scalar(
      get_arg("convert", TRUE),
      "convert"
    ),
    diarize = .stt_validate_logical_scalar(
      get_arg("diarize", TRUE),
      "diarize"
    ),
    timestamps = .stt_validate_logical_scalar(
      get_arg("timestamps", FALSE),
      "timestamps"
    ),
    max_new_tokens = .stt_validate_max_new_tokens(
      get_arg("max_new_tokens")
    ),
    chunking = chunk_options$chunking,
    chunk_bitrate_kbps = chunk_options$chunk_bitrate_kbps,
    chunk_segment_seconds = chunk_options$chunk_segment_seconds,
    chunk_format = .stt_chunk_resolve_format(
      service,
      chunk_options$chunk_format
    )
  )
  consumed <- c(
    "prompt", "convert", "diarize", "timestamps", "max_new_tokens",
    "chunking", "chunk_bitrate_kbps", "chunk_segment_seconds", "chunk_format",
    "checkpoint_retention",
    "executable", "native_engine",
    "native_backend", "native_quant", "native_kv_quant", "native_device",
    "base_url", "response_format"
  )
  ignored <- vapply(
    names(stt_args),
    function(name) {
      .stt_signature_is_credential_name(name) ||
        .stt_signature_is_operational_name(name)
    },
    logical(1)
  )
  extra_names <- setdiff(names(stt_args)[!ignored], consumed)
  extras <- stt_args[sort(extra_names)]
  extras <- .stt_signature_strip_credentials(extras)

  endpoint <- .stt_signature_endpoint(
    service,
    base_url = get_arg("base_url"),
    model = model
  )
  effective_model <- model %||% .stt_default_model(service)
  effective_model <- .stt_signature_optional_scalar(effective_model)
  runtime <- NULL

  if (identical(service, "local-openai")) {
    response_format <- tolower(trimws(as.character(
      get_arg("response_format", "json") %||% "json"
    )[1]))
    if (is.na(response_format) || !nzchar(response_format)) {
      response_format <- "json"
    }
    semantic$response_format <- response_format
  }

  if (identical(service, "local-native")) {
    config <- tryCatch(
      .genflow_read_local_config(),
      error = function(e) list()
    )
    runtime <- .stt_chunk_runtime_artifacts(
      service = service,
      model = model,
      executable = get_arg("executable"),
      native_engine = get_arg("native_engine"),
      native_backend = get_arg("native_backend")
    )
    effective_model <- .stt_signature_optional_scalar(
      runtime$model_value %||% effective_model
    )
    engine <- runtime$engine %||% .stt_resolve_native_engine(
      native_engine = get_arg("native_engine"),
      executable = get_arg("executable"),
      model = effective_model,
      native_backend = get_arg("native_backend"),
      config = config
    )
    .stt_chunk_validate_native_format(
      service = service,
      format = semantic$chunk_format,
      engine = engine
    )
    backend <- runtime$backend %||% .stt_validate_native_backend(
      .stt_native_setting(
        get_arg("native_backend"),
        field = "stt_native_backend",
        env = "GENFLOW_STT_NATIVE_BACKEND",
        config = config
      )
    )
    device <- .stt_validate_native_device(.stt_native_setting(
      get_arg("native_device"),
      field = "stt_native_device",
      env = "GENFLOW_STT_NATIVE_DEVICE",
      config = config,
      default = "auto",
      legacy_field = if (identical(engine, "moss-transcribe")) {
        "moss_cpp_device"
      } else {
        NULL
      },
      legacy_env = if (identical(engine, "moss-transcribe")) {
        "GENFLOW_MOSS_CPP_DEVICE"
      } else {
        character()
      }
    ))
    quant <- if (identical(engine, "crispasr") &&
        identical(tolower(effective_model %||% ""), "auto")) {
      .stt_validate_native_quant(.stt_native_setting(
        get_arg("native_quant"),
        field = "stt_native_quant",
        env = "GENFLOW_STT_NATIVE_QUANT",
        config = config
      ))
    } else {
      ""
    }
    kv_policy <- .stt_resolve_native_kv_quant(
      requested = get_arg("native_kv_quant"),
      engine = engine,
      backend = backend
    )
    semantic$native <- list(
      engine = engine,
      backend = backend,
      quant = if (nzchar(quant)) quant else NULL,
      kv_quant = kv_policy$signature_value,
      device = device
    )
    endpoint <- paste0("local-native://", engine)
  }

  payload <- list(
    schema_version = 1L,
    service = service,
    model = effective_model %||% "default",
    language = language,
    endpoint = .stt_signature_sanitize_endpoint(endpoint),
    parameters = semantic,
    extras = extras,
    runtime = runtime
  )
  if (isTRUE(include_postprocessing)) {
    payload$postprocessing <- list(
      chunk_merge = .stt_reconciliation_version()
    )
  }
  .stt_chunk_object_fingerprint(.stt_signature_canonicalize(payload))
}

#' Fingerprint only provider/runtime work stored in chunk checkpoints
#'
#' @keywords internal
#' @noRd
.stt_checkpoint_signature <- function(service = "openai",
                                      model = NULL,
                                      language = NULL,
                                      stt_args = list()) {
  .stt_signature_build(
    service = service,
    model = model,
    language = language,
    stt_args = stt_args,
    include_postprocessing = FALSE
  )
}

#' @keywords internal
#' @noRd
.stt_signature_validate_args <- function(stt_args) {
  if (!is.list(stt_args) || inherits(stt_args, "data.frame")) {
    stop("`stt_args` must be a list.", call. = FALSE)
  }
  if (!length(stt_args)) return(list())
  arg_names <- names(stt_args)
  if (is.null(arg_names) || length(arg_names) != length(stt_args) ||
      anyNA(arg_names) || any(!nzchar(arg_names))) {
    stop("`stt_args` must be fully named.", call. = FALSE)
  }
  duplicated_names <- unique(arg_names[duplicated(arg_names)])
  if (length(duplicated_names)) {
    stop(
      "`stt_args` names must be unique; duplicated: ",
      paste(duplicated_names, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  owned <- intersect(arg_names, c("audio", "service", "model", "language"))
  if (length(owned)) {
    stop(
      "`stt_args` must not contain arguments owned by ",
      "`gen_stt_signature()`: ",
      paste(owned, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  stt_args
}

#' @keywords internal
#' @noRd
.stt_signature_optional_scalar <- function(value) {
  if (is.null(value) || !length(value)) return(NULL)
  if (is.list(value)) {
    value <- value[[1]]
  }
  value <- as.character(value)[1]
  if (is.na(value) || !nzchar(value)) return(NULL)
  value
}

#' @keywords internal
#' @noRd
.stt_signature_is_credential_name <- function(name) {
  name <- tolower(as.character(name)[1])
  grepl(
    paste0(
      "(^|[._-])(?:api[._-]?key|access[._-]?token|auth[._-]?token|",
      "token|secret|password|credential|authorization|bearer|headers?)",
      "([._-]|$)"
    ),
    name,
    perl = TRUE
  )
}

#' @keywords internal
#' @noRd
.stt_signature_is_operational_name <- function(name) {
  name <- tolower(as.character(name)[1])
  name %in% c(
    "directory", "label", "save_txt", "output", "checkpoint_dir",
    "checkpoint_retention", "resume",
    "timeout_api", "timeout_per_audio_minute", "poll_interval",
    "max_poll_seconds", "chunk_retry_forever", "chunk_max_retries",
    "chunk_retry_wait_seconds", "quiet", "verbose", "progress", "workers"
  ) || grepl(
    "(^|[._-])(?:timeout|retry|retries|checkpoint|directory)([._-]|$)",
    name,
    perl = TRUE
  ) || grepl("^(?:save|output)(?:[._-]|$)", name, perl = TRUE)
}

#' @keywords internal
#' @noRd
.stt_signature_strip_credentials <- function(value) {
  if (is.list(value)) {
    value_names <- names(value)
    if (!is.null(value_names)) {
      keep <- !vapply(
        value_names,
        .stt_signature_is_credential_name,
        logical(1)
      )
      value <- value[keep]
    }
    return(lapply(value, .stt_signature_strip_credentials))
  }
  if (!is.null(names(value))) {
    keep <- !vapply(
      names(value),
      .stt_signature_is_credential_name,
      logical(1)
    )
    value <- value[keep]
  }
  value
}

#' @keywords internal
#' @noRd
.stt_signature_endpoint <- function(service, base_url = NULL, model = NULL) {
  switch(service,
    "openai" = "https://api.openai.com/v1/audio/transcriptions",
    "groq" = "https://api.groq.com/openai/v1/audio/transcriptions",
    "assemblyai" = c(
      "https://api.assemblyai.com/v2/upload",
      "https://api.assemblyai.com/v2/transcript"
    ),
    "cloudflare" = paste0(
      "https://api.cloudflare.com/client/v4/accounts/{account}/",
      "ai/run/@cf/openai/whisper"
    ),
    "voicegain" = "https://api.voicegain.ai/v1/asr/transcribe/async",
    "hf" = paste0(
      "https://api-inference.hf.co/models/",
      model %||% .stt_default_model("hf")
    ),
    "replicate" = "https://api.replicate.com/v1/predictions",
    "local-openai" = .stt_local_transcriptions_url(base_url),
    "local-native" = "local-native://auto"
  )
}

#' @keywords internal
#' @noRd
.stt_signature_sanitize_endpoint <- function(endpoint) {
  vapply(as.character(endpoint), function(value) {
    parsed <- tryCatch(httr::parse_url(value), error = function(e) NULL)
    if (!is.list(parsed) || is.null(parsed$scheme)) return(value)
    parsed$username <- NULL
    parsed$password <- NULL
    if (is.list(parsed$query) && length(parsed$query)) {
      keep <- !vapply(
        names(parsed$query),
        .stt_signature_is_credential_name,
        logical(1)
      )
      parsed$query <- parsed$query[keep]
    }
    tryCatch(httr::build_url(parsed), error = function(e) value)
  }, character(1), USE.NAMES = FALSE)
}

#' @keywords internal
#' @noRd
.stt_signature_canonicalize <- function(value) {
  if (is.list(value)) {
    if (!length(value)) return(list())
    value <- lapply(value, .stt_signature_canonicalize)
    value_names <- names(value)
    if (!is.null(value_names) && length(value_names)) {
      value <- value[order(value_names)]
    }
    return(value)
  }
  if (!is.null(names(value)) && length(value)) {
    value <- value[order(names(value))]
  }
  value
}
