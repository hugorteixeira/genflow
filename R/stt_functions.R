#' Transcribe speech from an audio file
#'
#' High-level speech-to-text (STT) wrapper that dispatches to provider-specific
#' implementations (OpenAI, Groq, AssemblyAI, Cloudflare, Voicegain, Hugging Face).
#' Returns the transcribed text and optionally saves a `.txt` file.
#'
#' @param audio Character path or URL to an audio file (e.g., .mp3, .ogg, .wav).
#' @param service Provider identifier (e.g., "openai", "groq", "assemblyai",
#'   "cloudflare", "voicegain", "hf", "replicate").
#' @param model Provider model identifier. If NULL, a sensible default is used
#'   per provider.
#' @param language Optional language code (e.g., "en", "pt"). If NULL, provider
#'   auto-detection is used when supported.
#' @param prompt Optional prompt to guide transcription (provider-specific).
#' @param directory Optional output directory for saved transcripts when
#'   `save_txt = TRUE`. Defaults to `~/.genflow/transcripts` when NULL.
#' @param label Optional short label used for saved filenames.
#' @param save_txt Logical; save transcript to disk if TRUE.
#' @param convert Logical; if TRUE, attempt ffmpeg conversion for unsupported
#'   audio formats.
#' @param timeout_api Numeric; request timeout in seconds.
#' @param poll_interval Numeric; polling interval (seconds) for async providers.
#' @param max_poll_seconds Numeric; max polling time for async providers.
#' @param ... Reserved for future provider-specific arguments.
#'
#' @return Invisibly returns a list with `response_value` (transcribed text),
#'   `status_api`, `status_msg`, `service`, `model`, `duration`, `saved_file`,
#'   and metadata such as `audio`.
#'
#' @examples
#' # Minimal example (requires a provider API key)
#' # res <- gen_stt("audio.ogg", service = "openai")
#' # res$response_value
#'
#' @export
gen_stt <- function(audio, ...) {
  UseMethod("gen_stt")
}

#' @rdname gen_stt
#' @method gen_stt default
#' @export
gen_stt.default <- function(
  audio,
  service = "openai",
  model = NULL,
  language = NULL,
  prompt = NULL,
  directory = NULL,
  label = NULL,
  save_txt = TRUE,
  convert = TRUE,
  timeout_api = 240,
  poll_interval = 5,
  max_poll_seconds = 600,
  ...
) {
  start_time <- Sys.time()

  # Normalize inputs
  if (is.list(service)) service <- as.character(service$service %||% service[[1]]) else if (is.vector(service)) service <- as.character(service[1])
  if (is.list(model)) model <- as.character(model$model %||% model[[1]]) else if (is.vector(model)) model <- as.character(model[1])
  if (is.list(language)) language <- as.character(language$language %||% language[[1]]) else if (is.vector(language)) language <- as.character(language[1])

  service <- tolower(as.character(service)[1])
  model <- if (!is.null(model)) as.character(model)[1] else NULL
  if (is.na(model) || !nzchar(model)) model <- NULL
  if (!is.null(language)) {
    language <- as.character(language)[1]
    if (is.na(language) || !nzchar(language)) language <- NULL
  }
  if (!is.null(prompt)) {
    prompt <- as.character(prompt)[1]
    if (is.na(prompt) || !nzchar(prompt)) prompt <- NULL
  }

  label_source <- audio
  prep <- .stt_prepare_audio(audio, convert = convert)
  if (!is.null(prep$tmp)) {
    on.exit(try(unlink(prep$tmp), silent = TRUE), add = TRUE)
  }

  if (prep$is_url && !service %in% c("voicegain", "replicate")) {
    downloaded <- .stt_download_audio(prep$path)
    if (!is.null(downloaded$tmp)) {
      on.exit(try(unlink(downloaded$tmp), silent = TRUE), add = TRUE)
    }
    prep <- downloaded
  }

  label_base <- label
  if (is.null(label_base) || length(label_base) == 0) {
    label_base <- ""
  } else {
    label_base <- as.character(label_base[1])
  }
  if (is.na(label_base) || !nzchar(label_base)) {
    label_base <- tools::file_path_sans_ext(basename(label_source))
  }
  if (is.na(label_base) || !nzchar(label_base)) label_base <- "audio"
  label_base <- substr(label_base, 1, 36)
  label_sanitized <- .sanitize_filename(label_base)

  transcribed_text <- NULL
  error_message <- NULL

  transcribed_text <- tryCatch({
    switch(service,
      "openai" = .stt_openai(prep$path, model, language, prompt, timeout_api),
      "groq" = .stt_groq(prep$path, model, language, prompt, timeout_api),
      "assemblyai" = .stt_assemblyai(prep$path, language, poll_interval, max_poll_seconds, timeout_api),
      "cloudflare" = .stt_cloudflare(prep$path, timeout_api),
      "voicegain" = .stt_voicegain(prep$path, language, poll_interval, max_poll_seconds, timeout_api),
      "hf" = .stt_hf(prep$path, model, timeout_api),
      "replicate" = .stt_replicate(prep$path, model, timeout_api, poll_interval, max_poll_seconds),
      stop("Unsupported STT service: ", service)
    )
  }, error = function(e) {
    error_message <<- conditionMessage(e)
    NULL
  })

  final_status <- "SUCCESS"
  final_msg <- "OK"
  if (is.null(transcribed_text) || !is.character(transcribed_text) || !nzchar(transcribed_text)) {
    final_status <- "ERROR"
    final_msg <- if (!is.null(error_message)) error_message else "Empty transcription."
  }

  saved_file <- NA_character_
  if (isTRUE(save_txt) && final_status == "SUCCESS") {
    if (is.null(directory) || is.na(directory)) {
      directory <- .genflow_default_dir("transcripts")
    }
    if (!dir.exists(directory)) dir.create(directory, recursive = TRUE, showWarnings = FALSE)
    dt <- format(Sys.time(), "%Y%m%d_%H%M%S")
    model_tag <- .sanitize_filename(model %||% "default")
    filename <- sprintf("%s_%s_%s_%s.txt", label_sanitized, service, model_tag, dt)
    saved_file <- file.path(directory, filename)
    try(writeLines(transcribed_text, saved_file, useBytes = TRUE), silent = TRUE)
    if (!file.exists(saved_file)) saved_file <- NA_character_
  }

  duration_response <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  result <- list(
    response_value = transcribed_text,
    label = label_base,
    label_cat = label_sanitized,
    service = service,
    model = model %||% .stt_default_model(service),
    duration = duration_response,
    status_api = final_status,
    status_msg = final_msg,
    saved_file = saved_file,
    audio = prep$path,
    content_type = "text"
  )

  return(invisible(result))
}

#' @rdname gen_stt
#' @method gen_stt genflow_agent
#' @export
gen_stt.genflow_agent <- function(audio, ...) {
  agent <- audio
  overrides <- list(...)
  formals_default <- formals(gen_stt.default)
  agent_args <- .genflow_prepare_agent_args(
    agent = agent,
    overrides = overrides,
    target_formals = formals_default,
    required = "audio"
  )
  do.call(gen_stt.default, agent_args, quote = TRUE)
}

# --- Internal helpers -------------------------------------------------------

#' @keywords internal
#' @noRd
.stt_is_url <- function(x) {
  is.character(x) && length(x) == 1 && grepl("^https?://", x)
}

#' @keywords internal
#' @noRd
.stt_default_model <- function(service) {
  switch(tolower(service),
    "openai" = "whisper-1",
    "groq" = "whisper-large-v3-turbo",
    "hf" = "openai/whisper-large-v3-turbo",
    "replicate" = "openai/whisper",
    "assemblyai" = "default",
    "cloudflare" = "whisper",
    "voicegain" = "default",
    "default"
  )
}

#' @keywords internal
#' @noRd
.stt_prepare_audio <- function(audio, convert = TRUE) {
  if (!is.character(audio) || length(audio) != 1 || !nzchar(audio)) {
    stop("`audio` must be a non-empty character path or URL.")
  }

  if (.stt_is_url(audio)) {
    return(list(path = audio, is_url = TRUE, tmp = NULL, ext = tolower(tools::file_ext(audio))))
  }

  if (!file.exists(audio)) {
    stop("Audio file not found: ", audio)
  }

  audio <- normalizePath(audio, winslash = "/", mustWork = TRUE)
  ext <- tolower(tools::file_ext(audio))
  supported <- c("mp3", "wav", "m4a", "ogg", "oga", "flac", "webm", "mp4", "mpga", "mpeg", "aac", "wma")

  if (nzchar(ext) && ext %in% supported) {
    return(list(path = audio, is_url = FALSE, tmp = NULL, ext = ext))
  }

  if (!isTRUE(convert)) {
    stop("Unsupported audio format: .", ext, ". Set `convert = TRUE` to try ffmpeg.")
  }

  ffmpeg <- Sys.which("ffmpeg")
  if (!nzchar(ffmpeg)) {
    stop("Unsupported audio format and ffmpeg not found in PATH.")
  }

  tmp <- tempfile(fileext = ".wav")
  args <- c("-y", "-i", audio, "-ac", "1", "-ar", "16000", tmp)
  output <- suppressWarnings(system2(ffmpeg, args, stdout = TRUE, stderr = TRUE))
  status <- attr(output, "status")
  if (!is.null(status) && status != 0) {
    stop("ffmpeg conversion failed: ", paste(output, collapse = "\n"))
  }
  if (!file.exists(tmp) || file.info(tmp)$size == 0) {
    stop("ffmpeg conversion produced an empty file.")
  }

  list(path = tmp, is_url = FALSE, tmp = tmp, ext = "wav")
}

#' @keywords internal
#' @noRd
.stt_download_audio <- function(url) {
  if (!.stt_is_url(url)) {
    stop("`url` must be a valid http(s) URL.")
  }
  ext <- tolower(tools::file_ext(url))
  if (!nzchar(ext)) ext <- "audio"
  tmp <- tempfile(fileext = paste0(".", ext))
  ok <- try(utils::download.file(url, tmp, mode = "wb", quiet = TRUE), silent = TRUE)
  if (inherits(ok, "try-error") || !file.exists(tmp) || file.info(tmp)$size == 0) {
    stop("Failed to download audio from URL.")
  }
  list(path = tmp, is_url = FALSE, tmp = tmp, ext = ext)
}

#' @keywords internal
#' @noRd
.stt_openai <- function(audio_path, model, language, prompt, timeout_secs) {
  api_key <- Sys.getenv("OPENAI_API_KEY")
  if (!nzchar(api_key)) stop("OPENAI_API_KEY must be set.")

  audio_file <- httr::upload_file(audio_path)
  body <- list(
    file = audio_file,
    model = model %||% .stt_default_model("openai"),
    response_format = "json"
  )
  if (!is.null(language) && !is.na(language) && nzchar(language)) body$language <- language
  if (!is.null(prompt) && !is.na(prompt) && nzchar(prompt)) body$prompt <- prompt

  response <- httr::POST(
    url = "https://api.openai.com/v1/audio/transcriptions",
    httr::add_headers(Authorization = paste("Bearer", api_key)),
    body = body,
    encode = "multipart",
    httr::timeout(timeout_secs)
  )

  if (httr::status_code(response) != 200) {
    stop("OpenAI STT error: ", httr::content(response, "text", encoding = "UTF-8"))
  }

  result <- httr::content(response, as = "parsed", type = "application/json", encoding = "UTF-8")
  text <- result$text %||% result$transcription
  if (is.null(text) || !nzchar(text)) stop("OpenAI returned an empty transcript.")
  text
}

#' @keywords internal
#' @noRd
.stt_groq <- function(audio_path, model, language, prompt, timeout_secs) {
  api_key <- Sys.getenv("GROQ_API_KEY")
  if (!nzchar(api_key)) stop("GROQ_API_KEY must be set.")

  audio_file <- httr::upload_file(audio_path)
  body <- list(
    file = audio_file,
    model = model %||% .stt_default_model("groq"),
    response_format = "json"
  )
  if (!is.null(language) && !is.na(language) && nzchar(language)) body$language <- language
  if (!is.null(prompt) && !is.na(prompt) && nzchar(prompt)) body$prompt <- prompt

  response <- httr::POST(
    url = "https://api.groq.com/openai/v1/audio/transcriptions",
    httr::add_headers(Authorization = paste("Bearer", api_key)),
    body = body,
    encode = "multipart",
    httr::timeout(timeout_secs)
  )

  if (httr::status_code(response) != 200) {
    stop("Groq STT error: ", httr::content(response, "text", encoding = "UTF-8"))
  }

  result <- httr::content(response, as = "parsed", type = "application/json", encoding = "UTF-8")
  text <- result$text %||% result$transcription
  if (is.null(text) || !nzchar(text)) stop("Groq returned an empty transcript.")
  text
}

#' @keywords internal
#' @noRd
.stt_assemblyai <- function(audio_path, language, poll_interval, max_poll_seconds, timeout_secs) {
  api_key <- Sys.getenv("ASSEMBLYAI_API_KEY")
  if (!nzchar(api_key)) stop("ASSEMBLYAI_API_KEY must be set.")

  upload_response <- httr::POST(
    url = "https://api.assemblyai.com/v2/upload",
    httr::add_headers(Authorization = api_key),
    body = httr::upload_file(audio_path),
    httr::timeout(timeout_secs)
  )
  if (httr::status_code(upload_response) != 200) {
    stop("AssemblyAI upload error: ", httr::content(upload_response, "text", encoding = "UTF-8"))
  }

  upload_result <- httr::content(upload_response, as = "parsed", type = "application/json", encoding = "UTF-8")
  audio_url <- upload_result$upload_url
  if (is.null(audio_url) || !nzchar(audio_url)) stop("AssemblyAI upload returned no URL.")

  body <- list(audio_url = audio_url)
  if (!is.null(language) && !is.na(language) && nzchar(language)) body$language_code <- language

  transcript_response <- httr::POST(
    url = "https://api.assemblyai.com/v2/transcript",
    httr::add_headers(Authorization = api_key, `Content-Type` = "application/json"),
    body = body,
    encode = "json",
    httr::timeout(timeout_secs)
  )
  if (httr::status_code(transcript_response) != 200) {
    stop("AssemblyAI transcript error: ", httr::content(transcript_response, "text", encoding = "UTF-8"))
  }

  transcript_result <- httr::content(transcript_response, as = "parsed", type = "application/json", encoding = "UTF-8")
  transcript_id <- transcript_result$id
  if (is.null(transcript_id) || !nzchar(transcript_id)) stop("AssemblyAI did not return a transcript ID.")

  poll_url <- paste0("https://api.assemblyai.com/v2/transcript/", transcript_id)
  started <- Sys.time()

  repeat {
    poll_response <- httr::GET(
      url = poll_url,
      httr::add_headers(Authorization = api_key),
      httr::timeout(timeout_secs)
    )
    poll_result <- httr::content(poll_response, as = "parsed", type = "application/json", encoding = "UTF-8")
    status <- poll_result$status

    if (identical(status, "completed")) {
      text <- poll_result$text
      if (is.null(text) || !nzchar(text)) stop("AssemblyAI returned an empty transcript.")
      return(text)
    }
    if (identical(status, "error")) {
      stop("AssemblyAI transcription failed: ", poll_result$error %||% "unknown error")
    }

    elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
    if (elapsed > max_poll_seconds) {
      stop("AssemblyAI transcription timed out after ", max_poll_seconds, " seconds.")
    }
    Sys.sleep(poll_interval)
  }
}

#' @keywords internal
#' @noRd
.stt_cloudflare <- function(audio_path, timeout_secs) {
  account_id <- Sys.getenv("CLOUDFLARE_ACCOUNT_ID")
  api_token <- Sys.getenv("CLOUDFLARE_API_TOKEN")
  if (!nzchar(account_id) || !nzchar(api_token)) {
    stop("CLOUDFLARE_ACCOUNT_ID and CLOUDFLARE_API_TOKEN must be set.")
  }

  url <- paste0(
    "https://api.cloudflare.com/client/v4/accounts/",
    account_id,
    "/ai/run/@cf/openai/whisper"
  )

  binary_data <- readBin(audio_path, what = "raw", n = file.info(audio_path)$size)
  response <- httr::POST(
    url = url,
    httr::add_headers(
      Authorization = paste("Bearer", api_token),
      `Content-Type` = "application/octet-stream"
    ),
    body = binary_data,
    encode = "raw",
    httr::timeout(timeout_secs)
  )

  if (httr::status_code(response) != 200) {
    stop("Cloudflare STT error: ", httr::content(response, "text", encoding = "UTF-8"))
  }

  result <- httr::content(response, as = "parsed", type = "application/json", encoding = "UTF-8")
  text <- result$result$text %||% result$text
  if (is.null(text) || !nzchar(text)) stop("Cloudflare returned an empty transcript.")
  text
}

#' @keywords internal
#' @noRd
.stt_voicegain <- function(audio_url, language, poll_interval, max_poll_seconds, timeout_secs) {
  if (!.stt_is_url(audio_url)) {
    stop("Voicegain requires `audio` to be a URL (https://...).")
  }

  api_key <- Sys.getenv("VOICEGAIN_API_KEY")
  if (!nzchar(api_key)) stop("VOICEGAIN_API_KEY must be set.")

  payload <- list(
    sessions = list(
      list(
        asyncMode = "OFF-LINE",
        poll = list(persist = max_poll_seconds * 1000),
        content = list(
          incremental = list("progress"),
          full = list("transcript", "words")
        )
      )
    ),
    audio = list(
      source = list(
        fromUrl = list(url = audio_url)
      )
    ),
    settings = list(
      asr = list(languages = list(if (!is.null(language) && !is.na(language) && nzchar(language)) language else "en"))
    )
  )

  response <- httr::POST(
    url = "https://api.voicegain.ai/v1/asr/transcribe/async",
    httr::add_headers(
      "Authorization" = paste("Bearer", api_key),
      "Content-Type" = "application/json",
      "Accept" = "application/json"
    ),
    body = jsonlite::toJSON(payload, auto_unbox = TRUE),
    httr::timeout(timeout_secs)
  )

  if (httr::status_code(response) != 202) {
    stop("Voicegain STT error: ", httr::content(response, "text", encoding = "UTF-8"))
  }

  init_response <- httr::content(response, as = "parsed", type = "application/json", encoding = "UTF-8")
  poll_url <- init_response$sessions[[1]]$poll$url
  if (is.null(poll_url) || !nzchar(poll_url)) stop("Voicegain did not return a poll URL.")

  started <- Sys.time()
  repeat {
    Sys.sleep(poll_interval)
    poll_response <- httr::GET(
      url = paste0(poll_url, "?full=false"),
      httr::add_headers(
        "Authorization" = paste("Bearer", api_key),
        "Accept" = "application/json"
      ),
      httr::timeout(timeout_secs)
    )
    poll_content <- httr::content(poll_response, as = "parsed", type = "application/json", encoding = "UTF-8")
    if (isTRUE(poll_content$result$final)) break

    elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
    if (elapsed > max_poll_seconds) {
      stop("Voicegain transcription timed out after ", max_poll_seconds, " seconds.")
    }
  }

  final_response <- httr::GET(
    url = paste0(poll_url, "?full=true"),
    httr::add_headers(
      "Authorization" = paste("Bearer", api_key),
      "Accept" = "application/json"
    ),
    httr::timeout(timeout_secs)
  )
  final_content <- httr::content(final_response, as = "parsed", type = "application/json", encoding = "UTF-8")

  if (!identical(final_content$result$status, "MATCH")) {
    stop("Voicegain transcription failed with status: ", final_content$result$status)
  }
  text <- final_content$result$transcript
  if (is.null(text) || !nzchar(text)) stop("Voicegain returned an empty transcript.")
  text
}

#' @keywords internal
#' @noRd
.stt_hf <- function(audio_path, model, timeout_secs) {
  token <- Sys.getenv("HUGGINGFACE_API_TOKEN")
  if (!nzchar(token)) stop("HUGGINGFACE_API_TOKEN must be set.")

  model_id <- model %||% .stt_default_model("hf")
  url <- paste0("https://api-inference.hf.co/models/", model_id)
  binary_data <- readBin(audio_path, what = "raw", n = file.info(audio_path)$size)

  response <- httr::POST(
    url = url,
    httr::add_headers(
      Authorization = paste("Bearer", token),
      `Content-Type` = "application/octet-stream"
    ),
    body = binary_data,
    encode = "raw",
    httr::timeout(timeout_secs)
  )

  if (httr::status_code(response) != 200) {
    stop("Hugging Face STT error: ", httr::content(response, "text", encoding = "UTF-8"))
  }

  result <- httr::content(response, as = "parsed", type = "application/json", encoding = "UTF-8")
  if (is.list(result) && !is.null(result$error)) {
    stop("Hugging Face STT error: ", result$error)
  }

  text <- result$text %||% result$transcription
  if (is.null(text) && is.list(result) && length(result) > 0 && !is.null(result[[1]]$text)) {
    text <- result[[1]]$text
  }
  if (is.null(text) || !nzchar(text)) stop("Hugging Face returned an empty transcript.")
  text
}

#' @keywords internal
#' @noRd
.stt_replicate <- function(audio_path, model, timeout_secs, poll_interval, max_poll_seconds) {
  replicate_token <- Sys.getenv("REPLICATE_API_TOKEN")
  if (!nzchar(replicate_token)) stop("REPLICATE_API_TOKEN must be set.")

  model_id <- model %||% .stt_default_model("replicate")
  if (!grepl("/", model_id, fixed = TRUE)) {
    stop("Replicate STT expects model in the form 'owner/name' (e.g., openai/whisper).")
  }
  parts <- strsplit(model_id, "/", fixed = TRUE)[[1]]
  owner <- parts[1]
  name <- parts[2]

  model_info <- .stt_replicate_model_info(owner, name, replicate_token, timeout_secs)
  version_id <- model_info$version
  input_field <- model_info$input_field

  input_value <- .stt_replicate_prepare_input(audio_path)
  body <- list(
    version = version_id,
    input = setNames(list(input_value), input_field)
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
    stop("Replicate STT error: ", httr::content(response, "text", encoding = "UTF-8"))
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
      stop("Replicate transcription timed out after ", max_poll_seconds, " seconds.")
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

  output <- poll_content$output
  text <- NULL
  if (is.character(output) && length(output) >= 1) {
    text <- output[[1]]
  } else if (is.list(output)) {
    text <- output$text %||% output$transcription
    if (is.null(text) && length(output) == 1 && is.character(output[[1]])) {
      text <- output[[1]]
    }
  }
  if (is.null(text) || !nzchar(text)) stop("Replicate returned an empty transcript.")
  text
}

#' @keywords internal
#' @noRd
.stt_replicate_model_info <- function(owner, name, token, timeout_secs) {
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
  input_field <- .stt_replicate_pick_input_field(schema)
  if (is.null(version_id) || !nzchar(version_id)) stop("Replicate model version id not found.")
  if (is.null(input_field) || !nzchar(input_field)) {
    warning("Replicate input field could not be determined; falling back to 'audio'.")
    input_field <- "audio"
  }
  list(version = version_id, input_field = input_field)
}

#' @keywords internal
#' @noRd
.stt_replicate_pick_input_field <- function(schema) {
  if (is.null(schema$components$schemas$Input$properties)) {
    return(NULL)
  }
  props <- schema$components$schemas$Input$properties
  prop_names <- names(props)
  if (is.null(prop_names) || length(prop_names) == 0) return(NULL)

  preferred <- c("audio", "file", "input_audio", "sound", "path", "url")
  hit <- preferred[preferred %in% prop_names]
  if (length(hit) > 0) return(hit[[1]])

  if (length(prop_names) == 1) return(prop_names[[1]])

  # Look for likely file/audio fields by metadata
  for (nm in prop_names) {
    p <- props[[nm]]
    desc <- tolower(p$description %||% "")
    fmt <- tolower(p$format %||% "")
    media <- tolower(p$contentMediaType %||% "")
    if (grepl("audio", desc) || grepl("audio", media) || fmt %in% c("uri", "binary")) {
      return(nm)
    }
    if (!is.null(p[["x-replicate-file"]])) {
      return(nm)
    }
  }

  prop_names[[1]]
}

#' @keywords internal
#' @noRd
.stt_replicate_prepare_input <- function(audio_path, max_data_url_bytes = 256 * 1024) {
  if (.stt_is_url(audio_path)) {
    return(audio_path)
  }
  if (!file.exists(audio_path)) {
    stop("Replicate STT expects a local file path or a URL.")
  }

  file_size <- file.info(audio_path)$size
  if (is.na(file_size)) stop("Could not read audio file size.")

  if (file_size > max_data_url_bytes) {
    stop(
      "Audio file is too large for data URL upload (",
      round(file_size / 1024, 1), " KB). ",
      "Provide a public URL or a smaller file."
    )
  }

  if (!requireNamespace("base64enc", quietly = TRUE)) {
    stop("Package 'base64enc' is required to send local files to Replicate.")
  }

  ext <- tolower(tools::file_ext(audio_path))
  mime <- switch(ext,
    "mp3" = "audio/mpeg",
    "wav" = "audio/wav",
    "ogg" = "audio/ogg",
    "oga" = "audio/ogg",
    "m4a" = "audio/mp4",
    "mp4" = "audio/mp4",
    "webm" = "audio/webm",
    "flac" = "audio/flac",
    "aac" = "audio/aac",
    "wma" = "audio/x-ms-wma",
    "application/octet-stream"
  )

  encoded <- base64enc::base64encode(audio_path)
  paste0("data:", mime, ";base64,", encoded)
}
