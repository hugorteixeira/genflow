#' Generate speech audio from text
#'
#' High-level text-to-speech (TTS) wrapper that dispatches to provider-specific
#' implementations (currently OpenAI and Replicate). Returns the saved audio file path.
#'
#' @param text Character. The text to synthesize.
#' @param add Optional character appended to `text`.
#' @param directory Optional output directory. Defaults to `getwd()/audios` if NULL.
#' @param label Optional short label used for filenames. If NULL, derived from text.
#' @param service Provider identifier (e.g., "openai", "replicate").
#' @param model Provider model identifier. Defaults to `gpt-4o-mini-tts`.
#' @param voice Optional voice identifier supported by the provider. If NULL,
#'   the first available voice is selected automatically.
#' @param format Requested output audio format. Defaults to `"mp3"`. Providers
#'   may return another audio container; the saved filename and returned
#'   `format` always describe the detected bytes instead of the request label.
#' @param speed Numeric speech speed (0.25 to 4.0). Defaults to 1.
#' @param instructions Optional instructions to style the voice (OpenAI only).
#' @param preview Logical; attempt to open audio after save if interactive.
#' @param timeout_api Numeric; request timeout in seconds.
#' @param ... Reserved for future provider-specific arguments.
#'
#' @return Invisibly returns a list with `response_value` (saved file path),
#'   `status_api`, `status_msg`, `service`, `model`, `duration`, `saved_file`,
#'   and metadata such as `voice`, detected `format`, `requested_format`, and
#'   canonical `mime_type`. The existing `content_type = "audio"` category is
#'   preserved.
#'
#' @details Replicate model metadata is fetched at most once per `gen_tts()`
#'   call and reused for voice validation plus prediction construction.
#'   Provider temporaries are removed after the final file is copied, including
#'   error paths.
#'
#' @examples
#' # Minimal example (requires OpenAI API key)
#' # tts <- gen_tts("Hello world", service = "openai")
#' # tts$response_value
#'
#' @export
gen_tts <- function(text, ...) {
  UseMethod("gen_tts")
}

#' @rdname gen_tts
#' @method gen_tts default
#' @export
gen_tts.default <- function(
  text,
  add = NULL,
  directory = NULL,
  label = NULL,
  service = "openai",
  model = "gpt-4o-mini-tts",
  voice = NULL,
  format = "mp3",
  speed = 1,
  instructions = NULL,
  preview = FALSE,
  timeout_api = 240,
  ...
) {
  start_time <- Sys.time()

  if (is.null(text) || length(text) == 0) {
    stop("`text` must be a non-empty character string.")
  }
  if (!is.character(text)) text <- as.character(text)
  text <- text[[1]]
  if (!nzchar(text)) stop("`text` must be a non-empty character string.")

  if (!is.null(add)) {
    if (!is.character(add)) add <- as.character(add)
    add <- add[[1]]
    if (nzchar(add)) text <- paste(text, add)
  }

  # Normalize inputs
  if (is.list(service)) service <- as.character(service$service %||% service[[1]]) else if (is.vector(service)) service <- as.character(service[1])
  if (is.list(model)) model <- as.character(model$model %||% model[[1]]) else if (is.vector(model)) model <- as.character(model[1])
  if (is.list(voice)) voice <- as.character(voice$voice %||% voice[[1]]) else if (is.vector(voice)) voice <- as.character(voice[1])
  if (is.list(format)) format <- as.character(format$format %||% format[[1]]) else if (is.vector(format)) format <- as.character(format[1])

  service <- tolower(as.character(service)[1])
  model <- if (!is.null(model)) as.character(model)[1] else "gpt-4o-mini-tts"
  if (is.na(model) || !nzchar(model)) model <- "gpt-4o-mini-tts"
  voice <- if (!is.null(voice)) as.character(voice)[1] else NULL
  if (is.null(voice) || is.na(voice) || !nzchar(voice)) voice <- NULL
  format <- if (!is.null(format)) tolower(as.character(format)[1]) else "mp3"
  if (is.na(format) || !nzchar(format)) format <- "mp3"

  if (service == "replicate" && (is.null(model) || !nzchar(model) || model == "gpt-4o-mini-tts")) {
    model <- "qwen/qwen3-tts"
  }

  if (!is.null(speed)) {
    if (!is.numeric(speed) || length(speed) != 1 || is.na(speed)) {
      stop("`speed` must be a single numeric value.")
    }
    if (speed < 0.25 || speed > 4.0) {
      stop("`speed` must be between 0.25 and 4.0.")
    }
  }

  valid_formats <- c("mp3", "opus", "aac", "flac", "wav", "pcm")
  if (!format %in% valid_formats) {
    stop("Invalid `format`. Choose one of: ", paste(valid_formats, collapse = ", "))
  }
  requested_format <- format

  replicate_model_info <- NULL
  replicate_model_info_error <- NULL
  if (identical(service, "replicate")) {
    replicate_model_info <- tryCatch(
      {
        replicate_token <- Sys.getenv("REPLICATE_API_TOKEN")
        if (!nzchar(replicate_token)) {
          stop("REPLICATE_API_TOKEN must be set.", call. = FALSE)
        }
        model_parts <- .tts_replicate_model_parts(model)
        .tts_replicate_model_info(
          model_parts$owner,
          model_parts$name,
          replicate_token,
          timeout_api
        )
      },
      error = function(e) {
        replicate_model_info_error <<- conditionMessage(e)
        NULL
      }
    )
  }

  available_voices <- tryCatch(
    {
      if (identical(service, "replicate")) {
        if (!is.null(replicate_model_info_error)) {
          stop(replicate_model_info_error, call. = FALSE)
        }
        .tts_replicate_voices_from_info(model, replicate_model_info)
      } else {
        gen_tts_voices(service = service, model = model, timeout_api = timeout_api)
      }
    },
    error = function(e) {
      warning("Could not fetch voices: ", conditionMessage(e))
      character(0)
    }
  )
  if (length(available_voices) > 0) {
    cat(
      "\nAvailable voices for ", service, " / ", model, ":\n  - ",
      paste(available_voices, collapse = ", "),
      "\n",
      sep = ""
    )
  }
  if (is.null(voice) && length(available_voices) > 0) {
    voice <- available_voices[[1]]
    cat("Using voice: ", voice, "\n", sep = "")
  }

  if (is.null(directory)) {
    directory <- .genflow_default_dir("audios")
  }
  if (!dir.exists(directory)) dir.create(directory, recursive = TRUE, showWarnings = FALSE)

  label_processed <- label
  if (is.null(label_processed) || length(label_processed) == 0 || !nzchar(as.character(label_processed[[1]]))) {
    words <- strsplit(text, "[[:space:]]+")[[1]]
    label_processed <- paste(head(words, 5), collapse = "_")
  }
  label_processed <- substr(as.character(label_processed[[1]]), 1, 36)
  if (!nzchar(label_processed)) label_processed <- "audio"
  label_sanitized <- .sanitize_filename(label_processed)
  model_sanitized <- .sanitize_filename(model)

  audio_tmp <- NULL
  error_message <- NULL
  on.exit(
    {
      tmp_path <- as.character(audio_tmp %||% "")[1]
      if (!is.na(tmp_path) && nzchar(tmp_path) && file.exists(tmp_path)) {
        try(unlink(tmp_path), silent = TRUE)
      }
    },
    add = TRUE
  )

  audio_tmp <- tryCatch({
    switch(service,
      "openai" = .tts_openai(text, model, voice %||% "alloy", format, speed, instructions, timeout_api),
      "replicate" = {
        if (!is.null(replicate_model_info_error)) {
          stop(replicate_model_info_error, call. = FALSE)
        }
        .tts_replicate(
          text,
          model,
          voice,
          format,
          speed,
          instructions,
          timeout_api,
          model_info = replicate_model_info
        )
      },
      stop("Unsupported TTS service: ", service)
    )
  }, error = function(e) {
    error_message <<- conditionMessage(e)
    NULL
  })

  final_status <- "SUCCESS"
  final_msg <- "OK"
  final_path <- NA_character_
  actual_format <- requested_format
  actual_content_type <- NA_character_

  if (is.null(audio_tmp) || !file.exists(audio_tmp) || file.info(audio_tmp)$size == 0) {
    final_status <- "ERROR"
    final_msg <- if (!is.null(error_message)) error_message else "Failed to generate audio."
  } else {
    audio_meta <- tryCatch(
      .tts_audio_metadata(
        audio_tmp,
        content_type = attr(audio_tmp, "tts_content_type", exact = TRUE),
        fallback_format = attr(audio_tmp, "tts_format", exact = TRUE) %||% requested_format
      ),
      error = function(e) {
        error_message <<- conditionMessage(e)
        NULL
      }
    )
    if (is.null(audio_meta)) {
      final_status <- "ERROR"
      final_msg <- error_message %||% "Could not determine the generated audio format."
    } else {
      actual_format <- audio_meta$format
      actual_content_type <- audio_meta$content_type
      dt <- format(Sys.time(), "%Y%m%d_%H%M%S")
      filename <- sprintf(
        "%s_%s_%s_%s.%s",
        label_sanitized,
        service,
        model_sanitized,
        dt,
        actual_format
      )
      final_path <- file.path(directory, filename)
      ok <- file.copy(audio_tmp, final_path, overwrite = TRUE)
      if (!ok || !file.exists(final_path)) {
        final_status <- "ERROR"
        final_msg <- sprintf("Failed to copy audio to '%s'", final_path)
        if (file.exists(final_path)) {
          try(unlink(final_path), silent = TRUE)
        }
        final_path <- NA_character_
      }
    }
  }

  if (preview && final_status == "SUCCESS") {
    if (interactive()) browseURL(final_path)
    else message("Preview not available in non-interactive mode.")
  }

  duration_response <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  result <- list(
    response_value = if (final_status == "SUCCESS") final_path else NULL,
    label = label_processed,
    label_cat = label_sanitized,
    service = service,
    model = model,
    voice = voice,
    format = actual_format,
    requested_format = requested_format,
    speed = speed,
    duration = duration_response,
    status_api = final_status,
    status_msg = final_msg,
    saved_file = if (final_status == "SUCCESS") final_path else NA_character_,
    content_type = "audio",
    mime_type = if (final_status == "SUCCESS") actual_content_type else NA_character_
  )

  return(invisible(result))
}

#' @rdname gen_tts
#' @method gen_tts genflow_agent
#' @details For a `genflow_agent`, a saved `context` is used as speech text when
#'   the agent has no explicit `text`. Supply `text_override` through `...` to
#'   replace the saved text for one call.
#' @export
gen_tts.genflow_agent <- function(text, ...) {
  agent <- text
  if (is.null(agent$text) && !is.null(agent$context)) {
    agent$text <- agent$context
  }
  overrides <- list(...)
  formals_default <- formals(gen_tts.default)
  agent_args <- .genflow_prepare_agent_args(
    agent = agent,
    overrides = overrides,
    target_formals = formals_default,
    required = "text",
    override_aliases = c(text_override = "text"),
    override_label = "gen_tts()"
  )
  do.call(gen_tts.default, agent_args, quote = TRUE)
}

# --- Internal helpers -------------------------------------------------------

#' @keywords internal
#' @noRd
.tts_scalar_text <- function(value) {
  if (is.null(value) || !length(value)) {
    return("")
  }
  value <- as.character(value)[1]
  if (is.na(value)) "" else trimws(value)
}

#' @keywords internal
#' @noRd
.tts_normalize_audio_format <- function(value) {
  value <- tolower(.tts_scalar_text(value))
  aliases <- c(
    mpeg = "mp3",
    mpga = "mp3",
    wave = "wav",
    x_wav = "wav",
    oga = "ogg",
    mp4 = "m4a"
  )
  if (value %in% names(aliases)) {
    value <- unname(aliases[[value]])
  }
  known <- c("mp3", "opus", "ogg", "aac", "flac", "wav", "pcm", "webm", "m4a")
  if (nzchar(value) && value %in% known) value else NULL
}

#' @keywords internal
#' @noRd
.tts_content_type_format <- function(content_type) {
  content_type <- tolower(.tts_scalar_text(content_type))
  content_type <- sub(";.*$", "", content_type)
  mapping <- c(
    "audio/mpeg" = "mp3",
    "audio/mp3" = "mp3",
    "audio/x-mpeg" = "mp3",
    "audio/wav" = "wav",
    "audio/wave" = "wav",
    "audio/x-wav" = "wav",
    "audio/ogg" = "ogg",
    "application/ogg" = "ogg",
    "audio/opus" = "opus",
    "audio/aac" = "aac",
    "audio/x-aac" = "aac",
    "audio/flac" = "flac",
    "audio/x-flac" = "flac",
    "audio/webm" = "webm",
    "audio/mp4" = "m4a",
    "audio/x-m4a" = "m4a",
    "audio/pcm" = "pcm",
    "audio/x-pcm" = "pcm",
    "audio/l16" = "pcm"
  )
  if (content_type %in% names(mapping)) unname(mapping[[content_type]]) else NULL
}

#' @keywords internal
#' @noRd
.tts_format_content_type <- function(format) {
  format <- .tts_normalize_audio_format(format)
  mapping <- c(
    mp3 = "audio/mpeg",
    wav = "audio/wav",
    ogg = "audio/ogg",
    opus = "audio/ogg",
    aac = "audio/aac",
    flac = "audio/flac",
    webm = "audio/webm",
    m4a = "audio/mp4",
    pcm = "audio/pcm"
  )
  if (!is.null(format) && format %in% names(mapping)) {
    unname(mapping[[format]])
  } else {
    "application/octet-stream"
  }
}

#' @keywords internal
#' @noRd
.tts_response_content_type <- function(response) {
  response_headers <- tryCatch(
    httr::headers(response),
    error = function(e) response$headers %||% list()
  )
  header_names <- names(response_headers)
  if (is.null(header_names)) {
    return(NULL)
  }
  idx <- match("content-type", tolower(header_names))
  if (is.na(idx)) {
    return(NULL)
  }
  value <- tolower(sub(";.*$", "", .tts_scalar_text(response_headers[[idx]])))
  if (nzchar(value)) value else NULL
}

#' @keywords internal
#' @noRd
.tts_http_status <- function(response) {
  status <- tryCatch(
    httr::status_code(response),
    error = function(e) NA_integer_
  )
  status <- suppressWarnings(as.integer(status)[1])
  if (is.na(status)) {
    stop("TTS provider returned a malformed HTTP response without a status code.", call. = FALSE)
  }
  status
}

#' @keywords internal
#' @noRd
.tts_http_detail <- function(response) {
  detail <- tryCatch(
    httr::content(response, as = "text", encoding = "UTF-8"),
    error = function(e) ""
  )
  if (is.list(detail)) {
    detail <- tryCatch(
      jsonlite::toJSON(detail, auto_unbox = TRUE, null = "null"),
      error = function(e) ""
    )
  }
  detail <- trimws(paste(as.character(detail %||% ""), collapse = " "))
  if (nzchar(detail)) substr(detail, 1L, 500L) else ""
}

#' @keywords internal
#' @noRd
.tts_expect_http_status <- function(response, expected, action) {
  status <- .tts_http_status(response)
  if (!status %in% expected) {
    detail <- .tts_http_detail(response)
    detail_suffix <- if (nzchar(detail)) paste0(": ", detail) else ""
    stop(
      action,
      " failed (HTTP ",
      status,
      ")",
      detail_suffix,
      ".",
      call. = FALSE
    )
  }
  invisible(status)
}

#' @keywords internal
#' @noRd
.tts_raw_starts_with <- function(bytes, signature) {
  signature <- as.raw(signature)
  length(bytes) >= length(signature) &&
    identical(bytes[seq_along(signature)], signature)
}

#' @keywords internal
#' @noRd
.tts_raw_contains <- function(bytes, signature) {
  signature <- as.raw(signature)
  if (!length(signature) || length(bytes) < length(signature)) {
    return(FALSE)
  }
  last_start <- length(bytes) - length(signature) + 1L
  any(vapply(
    seq_len(last_start),
    function(index) {
      identical(
        bytes[index:(index + length(signature) - 1L)],
        signature
      )
    },
    logical(1)
  ))
}

#' @keywords internal
#' @noRd
.tts_audio_magic_format <- function(path) {
  bytes <- tryCatch(
    readBin(path, what = "raw", n = 256L),
    error = function(e) raw()
  )
  if (!length(bytes)) {
    return(NULL)
  }

  if (.tts_raw_starts_with(bytes, charToRaw("RIFF")) &&
      length(bytes) >= 12L &&
      identical(bytes[9:12], charToRaw("WAVE"))) {
    return("wav")
  }
  if (.tts_raw_starts_with(bytes, charToRaw("fLaC"))) {
    return("flac")
  }
  if (.tts_raw_starts_with(bytes, charToRaw("OggS"))) {
    if (.tts_raw_contains(bytes, charToRaw("OpusHead"))) "opus" else "ogg"
  } else if (.tts_raw_starts_with(bytes, charToRaw("ID3"))) {
    "mp3"
  } else if (length(bytes) >= 8L &&
      identical(bytes[5:8], charToRaw("ftyp"))) {
    "m4a"
  } else if (.tts_raw_starts_with(bytes, as.raw(c(0x1a, 0x45, 0xdf, 0xa3)))) {
    "webm"
  } else if (.tts_raw_contains(bytes, charToRaw("OpusHead"))) {
    "opus"
  } else if (length(bytes) >= 2L &&
      as.integer(bytes[[1]]) == 0xffL &&
      bitwAnd(as.integer(bytes[[2]]), 0xf6L) == 0xf0L) {
    "aac"
  } else if (length(bytes) >= 2L &&
      as.integer(bytes[[1]]) == 0xffL &&
      bitwAnd(as.integer(bytes[[2]]), 0xe0L) == 0xe0L &&
      bitwAnd(as.integer(bytes[[2]]), 0x06L) != 0L) {
    "mp3"
  } else {
    NULL
  }
}

#' @keywords internal
#' @noRd
.tts_payload_looks_textual <- function(path) {
  bytes <- tryCatch(
    readBin(path, what = "raw", n = 64L),
    error = function(e) raw()
  )
  if (!length(bytes)) {
    return(FALSE)
  }
  chars <- tryCatch(rawToChar(bytes, multiple = TRUE), error = function(e) character())
  chars <- chars[nzchar(chars)]
  if (!length(chars)) {
    return(FALSE)
  }
  text <- trimws(paste(chars, collapse = ""))
  grepl("^(\\{|\\[|<|error\\b)", text, ignore.case = TRUE, perl = TRUE)
}

#' @keywords internal
#' @noRd
.tts_audio_metadata <- function(path, content_type = NULL, fallback_format = NULL) {
  path <- as.character(path %||% "")[1]
  if (is.na(path) || !nzchar(path) || !file.exists(path)) {
    stop("Generated audio file does not exist.", call. = FALSE)
  }
  size <- file.info(path)$size
  if (is.na(size) || size <= 0) {
    stop("Generated audio file is empty.", call. = FALSE)
  }

  content_type_value <- tolower(.tts_scalar_text(content_type))
  content_type_value <- sub(";.*$", "", content_type_value)
  declared_format <- .tts_content_type_format(content_type_value)
  allowed_unknown_types <- c("", "application/octet-stream", "binary/octet-stream")
  if (is.null(declared_format) && !content_type_value %in% allowed_unknown_types) {
    stop(
      "TTS provider returned non-audio content type `",
      content_type_value,
      "`.",
      call. = FALSE
    )
  }

  magic_format <- .tts_audio_magic_format(path)
  if (is.null(magic_format) && .tts_payload_looks_textual(path)) {
    stop("TTS provider returned a textual payload instead of audio bytes.", call. = FALSE)
  }
  extension_format <- .tts_normalize_audio_format(tools::file_ext(path))
  fallback_format <- .tts_normalize_audio_format(fallback_format)
  raw_pcm_format <- c(declared_format, extension_format, fallback_format)
  raw_pcm_format <- raw_pcm_format[raw_pcm_format %in% "pcm"]
  actual_format <- magic_format %||%
    if (length(raw_pcm_format)) raw_pcm_format[[1]] else NULL
  if (is.null(actual_format)) {
    stop(
      "Could not determine the generated audio format from its bytes or metadata.",
      call. = FALSE
    )
  }

  list(
    path = path,
    format = actual_format,
    content_type = .tts_format_content_type(actual_format),
    declared_content_type = if (nzchar(content_type_value)) content_type_value else NULL
  )
}

#' @keywords internal
#' @noRd
.tts_tag_audio_path <- function(path, content_type = NULL, fallback_format = NULL) {
  metadata <- .tts_audio_metadata(
    path,
    content_type = content_type,
    fallback_format = fallback_format
  )
  structure(
    metadata$path,
    tts_format = metadata$format,
    tts_content_type = metadata$content_type
  )
}

#' @keywords internal
#' @noRd
.tts_openai <- function(text, model, voice, format, speed, instructions, timeout_secs) {
  api_key <- Sys.getenv("OPENAI_API_KEY")
  if (!nzchar(api_key)) stop("OPENAI_API_KEY must be set.")

  body <- list(
    model = model,
    input = text,
    voice = voice,
    response_format = format,
    speed = speed
  )
  if (!is.null(instructions)) {
    instructions <- as.character(instructions)[1]
    if (nzchar(instructions)) body$instructions <- instructions
  }

  tmp <- tempfile(fileext = paste0(".", format))
  keep_tmp <- FALSE
  on.exit(
    {
      if (!keep_tmp && file.exists(tmp)) {
        try(unlink(tmp), silent = TRUE)
      }
    },
    add = TRUE
  )

  response <- httr::POST(
    url = "https://api.openai.com/v1/audio/speech",
    httr::add_headers(
      Authorization = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ),
    body = body,
    encode = "json",
    httr::write_disk(tmp, overwrite = TRUE),
    httr::timeout(timeout_secs)
  )

  .tts_expect_http_status(response, 200L, "OpenAI TTS request")
  tagged_path <- .tts_tag_audio_path(
    tmp,
    content_type = .tts_response_content_type(response),
    fallback_format = format
  )
  keep_tmp <- TRUE
  tagged_path
}

#' @keywords internal
#' @noRd
.tts_replicate_model_parts <- function(model) {
  model_id <- .tts_scalar_text(model)
  if (!nzchar(model_id)) {
    model_id <- "qwen/qwen3-tts"
  }
  parts <- strsplit(model_id, "/", fixed = TRUE)[[1]]
  if (length(parts) != 2L || any(!nzchar(parts))) {
    stop(
      "Replicate TTS expects model in the form 'owner/name' (e.g., qwen/qwen3-tts).",
      call. = FALSE
    )
  }
  list(model_id = model_id, owner = parts[[1]], name = parts[[2]])
}

#' @keywords internal
#' @noRd
.tts_replicate <- function(text, model, voice, format, speed, instructions, timeout_secs,
                           poll_interval = 5, max_poll_seconds = 600,
                           model_info = NULL) {
  replicate_token <- Sys.getenv("REPLICATE_API_TOKEN")
  if (!nzchar(replicate_token)) stop("REPLICATE_API_TOKEN must be set.")

  model_parts <- .tts_replicate_model_parts(model)
  model_info <- model_info %||% .tts_replicate_model_info(
    model_parts$owner,
    model_parts$name,
    replicate_token,
    timeout_secs
  )
  version_id <- if (is.list(model_info)) {
    .tts_scalar_text(model_info$version)
  } else {
    ""
  }
  if (!is.list(model_info) || !nzchar(version_id)) {
    stop("Replicate model metadata is missing a usable version id.", call. = FALSE)
  }
  props <- model_info$properties %||% list()

  text_field <- .tts_replicate_pick_text_field(props) %||% "text"
  input <- setNames(list(text), text_field)

  voice_field <- .tts_replicate_pick_voice_field(props)
  if (!is.null(voice_field) && !is.null(voice) && nzchar(voice)) {
    allowed_voices <- .tts_replicate_enum(props, voice_field)
    if (length(allowed_voices) > 0 && !(voice %in% allowed_voices)) {
      stop(
        "Voice must be one of: ",
        paste(allowed_voices, collapse = ", "),
        ". Set `voice` accordingly or call gen_tts_voices() to list them."
      )
    }
    input[[voice_field]] <- voice
  }

  format_field <- .tts_replicate_pick_format_field(props)
  if (!is.null(format_field) && !is.null(format) && nzchar(format)) {
    input[[format_field]] <- format
  }

  speed_field <- .tts_replicate_pick_speed_field(props)
  if (!is.null(speed_field) && !is.null(speed) && !is.na(speed)) {
    input[[speed_field]] <- speed
  }

  instructions_field <- .tts_replicate_pick_instructions_field(props)
  if (!is.null(instructions_field) && !is.null(instructions)) {
    inst <- as.character(instructions)[1]
    if (nzchar(inst)) input[[instructions_field]] <- inst
  }

  body <- list(
    version = version_id,
    input = input
  )

  response <- httr::POST(
    url = "https://api.replicate.com/v1/predictions",
    httr::add_headers(
      "Content-Type" = "application/json",
      "Authorization" = paste("Token", replicate_token)
    ),
    body = jsonlite::toJSON(body, auto_unbox = TRUE, null = "null"),
    encode = "raw",
    httr::timeout(timeout_secs)
  )

  .tts_expect_http_status(response, c(200L, 201L), "Replicate prediction creation")

  content <- httr::content(response, as = "parsed", simplifyVector = TRUE)
  if (!is.list(content)) {
    stop("Replicate prediction creation returned malformed JSON.", call. = FALSE)
  }
  prediction_urls <- content$urls
  get_url <- if (is.list(prediction_urls)) {
    .tts_scalar_text(prediction_urls$get)
  } else {
    ""
  }
  prediction_id <- .tts_scalar_text(content$id)
  if (!nzchar(prediction_id)) {
    prediction_id <- "unknown"
  }
  if (!nzchar(get_url)) {
    stop("Replicate did not return a polling URL (prediction id: ", prediction_id, ").")
  }

  status <- .tts_scalar_text(content$status)
  if (!nzchar(status)) {
    stop("Replicate prediction creation returned no status.", call. = FALSE)
  }
  poll_content <- content
  prediction_error <- .tts_scalar_text(poll_content$error)
  if (nzchar(prediction_error)) {
    stop(
      "Replicate prediction failed (ID: ",
      prediction_id,
      "): ",
      prediction_error,
      call. = FALSE
    )
  }
  started <- Sys.time()
  while (status %in% c("starting", "processing")) {
    elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
    if (elapsed > max_poll_seconds) {
      stop("Replicate TTS timed out after ", max_poll_seconds, " seconds.")
    }
    Sys.sleep(poll_interval)
    poll_response <- httr::GET(
      get_url,
      httr::add_headers("Authorization" = paste("Token", replicate_token)),
      httr::timeout(timeout_secs)
    )
    .tts_expect_http_status(poll_response, 200L, "Replicate prediction polling")
    poll_content <- httr::content(poll_response, as = "parsed", simplifyVector = TRUE)
    if (!is.list(poll_content)) {
      stop("Replicate prediction polling returned malformed JSON.", call. = FALSE)
    }
    status <- .tts_scalar_text(poll_content$status)
    if (!nzchar(status)) {
      stop("Replicate prediction polling returned no status.", call. = FALSE)
    }
    prediction_error <- .tts_scalar_text(poll_content$error)
    if (nzchar(prediction_error)) {
      stop(
        "Replicate prediction failed (ID: ",
        prediction_id,
        "): ",
        prediction_error,
        call. = FALSE
      )
    }
  }

  if (status != "succeeded") {
    stop("Replicate prediction did not succeed. Final status: ", status)
  }

  output_ref <- .tts_replicate_extract_output(poll_content$output)
  if (is.null(output_ref) || !nzchar(output_ref)) {
    stop("Replicate returned no audio output.")
  }

  tmp <- .tts_replicate_fetch_audio(output_ref, format, timeout_secs)
  keep_tmp <- FALSE
  on.exit(
    {
      tmp_path <- as.character(tmp %||% "")[1]
      if (!keep_tmp && !is.na(tmp_path) && nzchar(tmp_path) && file.exists(tmp_path)) {
        try(unlink(tmp_path), silent = TRUE)
      }
    },
    add = TRUE
  )
  output_path <- tmp
  if (format == "mp3") {
    output_path <- .tts_ensure_mp3(tmp)
  }
  if (identical(as.character(output_path)[1], as.character(tmp)[1])) {
    keep_tmp <- TRUE
  }
  output_path
}

#' @keywords internal
#' @noRd
.tts_replicate_model_info <- function(owner, name, token, timeout_secs) {
  url <- paste0("https://api.replicate.com/v1/models/", owner, "/", name)
  response <- httr::GET(
    url,
    httr::add_headers("Authorization" = paste("Token", token)),
    httr::timeout(timeout_secs)
  )
  .tts_expect_http_status(response, 200L, "Replicate model metadata request")
  info <- httr::content(response, as = "parsed", simplifyVector = TRUE)
  if (!is.list(info)) {
    stop("Replicate model metadata returned malformed JSON.", call. = FALSE)
  }
  latest_version <- info$latest_version
  if (!is.list(latest_version)) {
    stop("Replicate model metadata has no latest version object.", call. = FALSE)
  }
  version_id <- .tts_scalar_text(latest_version$id)
  schema <- latest_version$openapi_schema
  props <- .tts_replicate_collect_properties(schema)
  if (!nzchar(version_id)) {
    stop("Replicate model version id not found.", call. = FALSE)
  }
  if (is.null(props)) props <- list()
  list(version = version_id, properties = props)
}

#' @keywords internal
#' @noRd
.tts_replicate_collect_properties <- function(node, depth = 0, max_depth = 8) {
  if (is.null(node) || depth > max_depth) return(list())
  if (!is.list(node)) return(list())

  props <- list()
  if (!is.null(node$properties) && is.list(node$properties)) {
    props <- node$properties
  }

  for (item in node) {
    if (is.list(item)) {
      nested <- .tts_replicate_collect_properties(item, depth + 1, max_depth)
      if (length(nested) > 0) {
        for (nm in names(nested)) {
          if (is.null(props[[nm]])) {
            props[[nm]] <- nested[[nm]]
          }
        }
      }
    }
  }

  props
}

#' @keywords internal
#' @noRd
.tts_replicate_pick_text_field <- function(props) {
  if (is.null(props) || length(props) == 0) return(NULL)
  prop_names <- names(props)
  preferred <- c("text", "prompt", "input", "message", "script", "sentence")
  hit <- preferred[preferred %in% prop_names]
  if (length(hit) > 0) return(hit[[1]])
  if (length(prop_names) == 1) return(prop_names[[1]])
  for (nm in prop_names) {
    desc <- tolower(props[[nm]]$description %||% "")
    if (grepl("text|prompt|script|sentence", desc)) return(nm)
  }
  prop_names[[1]]
}

#' @keywords internal
#' @noRd
.tts_replicate_pick_voice_field <- function(props) {
  if (is.null(props) || length(props) == 0) return(NULL)
  prop_names <- names(props)
  preferred <- c("voice", "speaker", "speaker_id", "voice_id")
  hit <- preferred[preferred %in% prop_names]
  if (length(hit) > 0) return(hit[[1]])
  NULL
}

#' @keywords internal
#' @noRd
.tts_replicate_pick_format_field <- function(props) {
  if (is.null(props) || length(props) == 0) return(NULL)
  prop_names <- names(props)
  preferred <- c("format", "audio_format", "output_format")
  hit <- preferred[preferred %in% prop_names]
  if (length(hit) > 0) return(hit[[1]])
  NULL
}

#' @keywords internal
#' @noRd
.tts_replicate_pick_speed_field <- function(props) {
  if (is.null(props) || length(props) == 0) return(NULL)
  prop_names <- names(props)
  preferred <- c("speed", "rate")
  hit <- preferred[preferred %in% prop_names]
  if (length(hit) > 0) return(hit[[1]])
  NULL
}

#' @keywords internal
#' @noRd
.tts_replicate_pick_instructions_field <- function(props) {
  if (is.null(props) || length(props) == 0) return(NULL)
  prop_names <- names(props)
  preferred <- c("instruction", "instructions", "style", "prompt")
  hit <- preferred[preferred %in% prop_names]
  if (length(hit) > 0) return(hit[[1]])
  NULL
}

#' @keywords internal
#' @noRd
.tts_replicate_enum <- function(props, field) {
  if (is.null(props) || length(props) == 0 || is.null(field)) return(character(0))
  if (is.null(props[[field]])) return(character(0))
  enums <- props[[field]]$enum
  if (is.null(enums)) return(character(0))
  as.character(enums)
}

#' @keywords internal
#' @noRd
.tts_replicate_voices_from_info <- function(model, model_info) {
  model_parts <- .tts_replicate_model_parts(model)
  props <- if (is.list(model_info)) {
    model_info$properties %||% list()
  } else {
    list()
  }
  voice_field <- .tts_replicate_pick_voice_field(props)
  fallback_voices <- c(
    "Aiden", "Dylan", "Eric", "Ono_anna", "Ryan",
    "Serena", "Sohee", "Uncle_fu", "Vivian"
  )
  if (is.null(voice_field)) {
    if (identical(model_parts$model_id, "qwen/qwen3-tts")) {
      return(fallback_voices)
    }
    return(character(0))
  }
  voices <- .tts_replicate_enum(props, voice_field)
  if (!length(voices) && identical(model_parts$model_id, "qwen/qwen3-tts")) {
    return(fallback_voices)
  }
  voices
}

#' @keywords internal
#' @noRd
.tts_replicate_extract_output <- function(output) {
  if (is.null(output)) return(NULL)
  if (is.character(output) && length(output) >= 1) return(output[[1]])

  if (is.list(output)) {
    # Common keys
    for (key in c("audio", "audio_url", "url", "output", "result", "file")) {
      if (!is.null(output[[key]])) {
        if (is.character(output[[key]]) && length(output[[key]]) >= 1) return(output[[key]][[1]])
        if (is.list(output[[key]]) && !is.null(output[[key]]$url)) return(output[[key]]$url)
      }
    }
    # Search any string for URL/data
    flat <- unlist(output, use.names = FALSE)
    flat <- flat[is.character(flat)]
    if (length(flat) > 0) {
      hit <- flat[grepl("^https?://", flat) | grepl("^data:audio", flat)]
      if (length(hit) > 0) return(hit[[1]])
    }
  }
  NULL
}

#' @keywords internal
#' @noRd
.tts_replicate_fetch_audio <- function(ref, format, timeout_secs) {
  ref <- .tts_scalar_text(ref)
  if (!nzchar(ref)) {
    stop("Replicate returned an empty audio output reference.", call. = FALSE)
  }
  if (grepl("^data:audio", ref, ignore.case = TRUE)) {
    if (!requireNamespace("base64enc", quietly = TRUE)) {
      stop("Package 'base64enc' is required to decode data URLs.")
    }
    if (!grepl("^data:audio/[^;,]+;base64,", ref, ignore.case = TRUE)) {
      stop("Replicate audio data URI must use base64 encoding.", call. = FALSE)
    }
    mime <- tolower(sub("^data:([^;]+);base64,.*$", "\\1", ref, ignore.case = TRUE))
    b64 <- sub("^data:[^;]+;base64,", "", ref, ignore.case = TRUE)
    ext <- .tts_content_type_format(mime) %||%
      .tts_normalize_audio_format(format) %||%
      "bin"
    tmp <- tempfile(fileext = paste0(".", ext))
    keep_tmp <- FALSE
    on.exit(
      {
        if (!keep_tmp && file.exists(tmp)) {
          try(unlink(tmp), silent = TRUE)
        }
      },
      add = TRUE
    )
    raw <- tryCatch(
      base64enc::base64decode(b64),
      error = function(e) {
        stop(
          "Could not decode Replicate audio data URI: ",
          conditionMessage(e),
          call. = FALSE
        )
      }
    )
    writeBin(raw, tmp)
    tagged_path <- .tts_tag_audio_path(
      tmp,
      content_type = mime,
      fallback_format = format
    )
    keep_tmp <- TRUE
    return(tagged_path)
  }

  if (!grepl("^https?://", ref)) {
    stop("Replicate output is not a URL or data URI.")
  }

  ref_without_query <- sub("[?#].*$", "", ref)
  ext <- .tts_normalize_audio_format(tools::file_ext(ref_without_query)) %||%
    .tts_normalize_audio_format(format) %||%
    "bin"
  tmp <- tempfile(fileext = paste0(".", ext))
  keep_tmp <- FALSE
  on.exit(
    {
      if (!keep_tmp && file.exists(tmp)) {
        try(unlink(tmp), silent = TRUE)
      }
    },
    add = TRUE
  )
  resp <- httr::GET(
    ref,
    httr::write_disk(tmp, overwrite = TRUE),
    httr::timeout(timeout_secs)
  )
  .tts_expect_http_status(resp, 200L, "Replicate audio download")
  tagged_path <- .tts_tag_audio_path(
    tmp,
    content_type = .tts_response_content_type(resp),
    fallback_format = format
  )
  keep_tmp <- TRUE
  tagged_path
}

#' @keywords internal
#' @noRd
.tts_ensure_mp3 <- function(path) {
  source_metadata <- .tts_audio_metadata(
    path,
    content_type = attr(path, "tts_content_type", exact = TRUE),
    fallback_format = attr(path, "tts_format", exact = TRUE)
  )
  if (identical(source_metadata$format, "mp3")) {
    return(.tts_tag_audio_path(
      path,
      content_type = source_metadata$content_type,
      fallback_format = "mp3"
    ))
  }
  ffmpeg <- Sys.which("ffmpeg")
  if (!nzchar(ffmpeg)) {
    stop("Output is not mp3 and ffmpeg is not available to convert.")
  }
  tmp <- tempfile(fileext = ".mp3")
  keep_tmp <- FALSE
  on.exit(
    {
      if (!keep_tmp && file.exists(tmp)) {
        try(unlink(tmp), silent = TRUE)
      }
    },
    add = TRUE
  )
  args <- c("-y", "-i", path, tmp)
  output <- suppressWarnings(system2(ffmpeg, args, stdout = TRUE, stderr = TRUE))
  status <- attr(output, "status")
  if (!is.null(status) && status != 0) {
    stop("ffmpeg conversion failed: ", paste(output, collapse = "\n"))
  }
  if (!file.exists(tmp) || file.info(tmp)$size == 0) {
    stop("ffmpeg conversion produced an empty mp3.")
  }
  tagged_path <- .tts_tag_audio_path(
    tmp,
    content_type = "audio/mpeg",
    fallback_format = "mp3"
  )
  keep_tmp <- TRUE
  tagged_path
}

#' List available voices for a TTS model
#'
#' For Replicate, this inspects the model schema to return the allowed
#' voice/speaker options when provided by the model.
#'
#' @param service Provider identifier (e.g., "replicate", "openai").
#' @param model Provider model identifier.
#' @param timeout_api Numeric; request timeout in seconds.
#'
#' @return Character vector of available voices (empty if not declared).
#'
#' @examples
#' # Requires REPLICATE_API_TOKEN for replicate models
#' # gen_tts_voices(service = "replicate", model = "qwen/qwen3-tts")
#'
#' @export
gen_tts_voices <- function(service = "replicate", model = NULL, timeout_api = 30) {
  if (is.list(service)) service <- as.character(service$service %||% service[[1]]) else if (is.vector(service)) service <- as.character(service[1])
  if (is.list(model)) model <- as.character(model$model %||% model[[1]]) else if (is.vector(model)) model <- as.character(model[1])

  service <- tolower(as.character(service)[1])
  model <- if (!is.null(model)) as.character(model)[1] else NULL

  if (service == "openai") {
    return(c("alloy", "ash", "ballad", "coral", "echo", "fable", "nova", "onyx", "sage", "shimmer"))
  }

  if (service != "replicate") {
    stop("Unsupported service for voice listing: ", service)
  }

  replicate_token <- Sys.getenv("REPLICATE_API_TOKEN")
  if (!nzchar(replicate_token)) stop("REPLICATE_API_TOKEN must be set.")

  model_parts <- .tts_replicate_model_parts(model)
  info <- .tts_replicate_model_info(
    model_parts$owner,
    model_parts$name,
    replicate_token,
    timeout_api
  )
  .tts_replicate_voices_from_info(model_parts$model_id, info)
}
