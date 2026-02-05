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
#' @param format Output audio format. Defaults to "mp3".
#' @param speed Numeric speech speed (0.25 to 4.0). Defaults to 1.
#' @param instructions Optional instructions to style the voice (OpenAI only).
#' @param preview Logical; attempt to open audio after save if interactive.
#' @param timeout_api Numeric; request timeout in seconds.
#' @param ... Reserved for future provider-specific arguments.
#'
#' @return Invisibly returns a list with `response_value` (saved file path),
#'   `status_api`, `status_msg`, `service`, `model`, `duration`, `saved_file`,
#'   and metadata such as `voice`, `format`.
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

  available_voices <- tryCatch(
    gen_tts_voices(service = service, model = model, timeout_api = timeout_api),
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

  audio_tmp <- tryCatch({
    switch(service,
      "openai" = .tts_openai(text, model, voice %||% "alloy", format, speed, instructions, timeout_api),
      "replicate" = .tts_replicate(text, model, voice, format, speed, instructions, timeout_api),
      stop("Unsupported TTS service: ", service)
    )
  }, error = function(e) {
    error_message <<- conditionMessage(e)
    NULL
  })

  final_status <- "SUCCESS"
  final_msg <- "OK"
  final_path <- NA_character_

  if (is.null(audio_tmp) || !file.exists(audio_tmp) || file.info(audio_tmp)$size == 0) {
    final_status <- "ERROR"
    final_msg <- if (!is.null(error_message)) error_message else "Failed to generate audio."
  } else {
    dt <- format(Sys.time(), "%Y%m%d_%H%M%S")
    filename <- sprintf("%s_%s_%s_%s.%s", label_sanitized, service, model_sanitized, dt, format)
    final_path <- file.path(directory, filename)
    ok <- file.copy(audio_tmp, final_path, overwrite = TRUE)
    if (!ok || !file.exists(final_path)) {
      final_status <- "ERROR"
      final_msg <- sprintf("Failed to copy audio to '%s'", final_path)
      final_path <- NA_character_
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
    format = format,
    speed = speed,
    duration = duration_response,
    status_api = final_status,
    status_msg = final_msg,
    saved_file = if (final_status == "SUCCESS") final_path else NA_character_,
    content_type = "audio"
  )

  return(invisible(result))
}

#' @rdname gen_tts
#' @method gen_tts genflow_agent
#' @export
gen_tts.genflow_agent <- function(text, ...) {
  agent <- text
  overrides <- list(...)
  formals_default <- formals(gen_tts.default)
  agent_args <- .genflow_prepare_agent_args(
    agent = agent,
    overrides = overrides,
    target_formals = formals_default,
    required = "text"
  )
  do.call(gen_tts.default, agent_args, quote = TRUE)
}

# --- Internal helpers -------------------------------------------------------

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

  if (httr::status_code(response) != 200) {
    try(unlink(tmp), silent = TRUE)
    stop("OpenAI TTS error: ", httr::content(response, "text", encoding = "UTF-8"))
  }

  if (!file.exists(tmp) || file.info(tmp)$size == 0) {
    stop("OpenAI TTS returned an empty audio file.")
  }

  tmp
}

#' @keywords internal
#' @noRd
.tts_replicate <- function(text, model, voice, format, speed, instructions, timeout_secs,
                           poll_interval = 5, max_poll_seconds = 600) {
  replicate_token <- Sys.getenv("REPLICATE_API_TOKEN")
  if (!nzchar(replicate_token)) stop("REPLICATE_API_TOKEN must be set.")

  model_id <- model %||% "qwen/qwen3-tts"
  if (!grepl("/", model_id, fixed = TRUE)) {
    stop("Replicate TTS expects model in the form 'owner/name' (e.g., qwen/qwen3-tts).")
  }
  parts <- strsplit(model_id, "/", fixed = TRUE)[[1]]
  owner <- parts[1]
  name <- parts[2]

  model_info <- .tts_replicate_model_info(owner, name, replicate_token, timeout_secs)
  version_id <- model_info$version
  props <- model_info$properties

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

  if (!(httr::status_code(response) %in% c(200, 201))) {
    stop("Replicate TTS error: ", httr::content(response, "text", encoding = "UTF-8"))
  }

  content <- httr::content(response, as = "parsed", simplifyVector = TRUE)
  get_url <- content$urls$get
  prediction_id <- content$id
  if (is.null(get_url)) {
    stop("Replicate did not return a polling URL (prediction id: ", prediction_id, ").")
  }

  status <- content$status
  poll_content <- content
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
    if (httr::status_code(poll_response) != 200) {
      warning("Replicate polling returned status ", httr::status_code(poll_response), ". Retrying.")
      next
    }
    poll_content <- httr::content(poll_response, as = "parsed", simplifyVector = TRUE)
    status <- poll_content$status
    if (!is.null(poll_content$error)) {
      stop("Replicate prediction failed (ID: ", prediction_id, "): ", poll_content$error)
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
  if (format == "mp3") {
    tmp <- .tts_ensure_mp3(tmp)
  }
  tmp
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
  if (httr::status_code(response) != 200) {
    stop("Failed to fetch Replicate model info: ", httr::content(response, "text", encoding = "UTF-8"))
  }
  info <- httr::content(response, as = "parsed", simplifyVector = TRUE)
  version_id <- info$latest_version$id
  schema <- info$latest_version$openapi_schema
  props <- .tts_replicate_collect_properties(schema)
  if (is.null(version_id) || !nzchar(version_id)) stop("Replicate model version id not found.")
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
  if (grepl("^data:audio", ref)) {
    if (!requireNamespace("base64enc", quietly = TRUE)) {
      stop("Package 'base64enc' is required to decode data URLs.")
    }
    mime <- sub("^data:([^;]+);base64,.*$", "\\1", ref)
    b64 <- sub("^data:[^;]+;base64,", "", ref)
    ext <- switch(mime,
      "audio/mpeg" = "mp3",
      "audio/mp3" = "mp3",
      "audio/wav" = "wav",
      "audio/ogg" = "ogg",
      "audio/aac" = "aac",
      "audio/flac" = "flac",
      "audio/webm" = "webm",
      "audio/mp4" = "m4a",
      format %||% "mp3"
    )
    tmp <- tempfile(fileext = paste0(".", ext))
    raw <- base64enc::base64decode(b64)
    writeBin(raw, tmp)
    return(tmp)
  }

  if (!grepl("^https?://", ref)) {
    stop("Replicate output is not a URL or data URI.")
  }

  ext <- tolower(tools::file_ext(ref))
  if (!nzchar(ext)) ext <- format %||% "mp3"
  tmp <- tempfile(fileext = paste0(".", ext))
  resp <- httr::GET(
    ref,
    httr::write_disk(tmp, overwrite = TRUE),
    httr::timeout(timeout_secs)
  )
  if (httr::status_code(resp) != 200 || !file.exists(tmp) || file.info(tmp)$size == 0) {
    stop("Failed to download Replicate audio output.")
  }
  tmp
}

#' @keywords internal
#' @noRd
.tts_ensure_mp3 <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "mp3") return(path)
  ffmpeg <- Sys.which("ffmpeg")
  if (!nzchar(ffmpeg)) {
    stop("Output is not mp3 and ffmpeg is not available to convert.")
  }
  tmp <- tempfile(fileext = ".mp3")
  args <- c("-y", "-i", path, tmp)
  output <- suppressWarnings(system2(ffmpeg, args, stdout = TRUE, stderr = TRUE))
  status <- attr(output, "status")
  if (!is.null(status) && status != 0) {
    stop("ffmpeg conversion failed: ", paste(output, collapse = "\n"))
  }
  if (!file.exists(tmp) || file.info(tmp)$size == 0) {
    stop("ffmpeg conversion produced an empty mp3.")
  }
  tmp
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

  model_id <- model %||% "qwen/qwen3-tts"
  if (!grepl("/", model_id, fixed = TRUE)) {
    stop("Replicate model must be in the form 'owner/name'.")
  }
  parts <- strsplit(model_id, "/", fixed = TRUE)[[1]]
  owner <- parts[1]
  name <- parts[2]

  info <- .tts_replicate_model_info(owner, name, replicate_token, timeout_api)
  props <- info$properties
  voice_field <- .tts_replicate_pick_voice_field(props)
  if (is.null(voice_field)) {
    if (identical(model_id, "qwen/qwen3-tts")) {
      return(c("Aiden", "Dylan", "Eric", "Ono_anna", "Ryan", "Serena", "Sohee", "Uncle_fu", "Vivian"))
    }
    return(character(0))
  }
  voices <- .tts_replicate_enum(props, voice_field)
  if (length(voices) == 0 && identical(model_id, "qwen/qwen3-tts")) {
    return(c("Aiden", "Dylan", "Eric", "Ono_anna", "Ryan", "Serena", "Sohee", "Uncle_fu", "Vivian"))
  }
  voices
}
