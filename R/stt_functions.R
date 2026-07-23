#' Transcribe speech from an audio file
#'
#' High-level speech-to-text (STT) wrapper that dispatches to provider-specific
#' implementations (OpenAI, Groq, AssemblyAI, Cloudflare, Voicegain, Hugging
#' Face, local Transformers, registered native engines, and local
#' OpenAI-compatible servers). Returns the transcribed text and optionally
#' saves a `.txt` file.
#'
#' @param audio Character path or URL to an audio file (e.g., .mp3, .ogg, .wav).
#' @param service Provider identifier (e.g., "openai", "groq", "assemblyai",
#'   "cloudflare", "voicegain", "hf", "replicate", "hf-local",
#'   `"local-native"`, or `"local-openai"`).
#' @param model Provider model identifier. If NULL, a sensible default is used
#'   per provider. For `service = "local-native"`, this is normally a local
#'   model file. The CrispASR engine also accepts `"auto"` or an explicit
#'   `hf://OWNER/REPO:FILE` reference. For copy-and-paste convenience,
#'   `hf://OWNER/REPO/FILE` is accepted and normalized to the same form.
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
#' @param local_profile Local inference profile. `"auto"` selects the dedicated
#'   MOSS-Transcribe-Diarize integration for its official model id and the
#'   generic Transformers ASR pipeline otherwise. Explicit alternatives are
#'   `"transformers"` and `"moss"`.
#' @param device Device for local Python inference: `"auto"`, `"cpu"`,
#'   `"cuda"`, `"cuda:N"`, `"rocm"`, `"hip"`, or `"mps"`. PyTorch uses the
#'   `"cuda"` device name for both CUDA and ROCm; `"rocm"` and `"hip"` are
#'   accepted aliases.
#' @param dtype Numeric precision for local inference: `"auto"`, `"float32"`,
#'   `"float16"`, or `"bfloat16"`.
#' @param python Optional Python executable for `service = "hf-local"`.
#'   Defaults to `GENFLOW_PYTHON`, then `python3`/`python` on `PATH`.
#' @param executable Optional executable for `service = "local-native"`.
#'   Resolution uses `GENFLOW_STT_NATIVE_EXECUTABLE`, the saved local inference
#'   configuration, and finally the selected engine's executable on `PATH`.
#' @param native_engine Native runtime for `service = "local-native"`:
#'   `"auto"`, `"crispasr"`, or `"moss-transcribe"`. Engines support model
#'   architectures explicitly; no native runtime can execute every model in
#'   the Hugging Face catalog.
#' @param native_backend Optional model architecture/backend understood by the
#'   selected engine, such as `"whisper"`, `"parakeet"`, `"canary"`,
#'   `"granite-4.1"`, or `"moss-diarize"` for CrispASR. Leave NULL to use
#'   model auto-detection for an explicit local or `hf://` model.
#'   CrispASR `model = "auto"` requires a backend.
#' @param native_device Native accelerator for `service = "local-native"`:
#'   `"auto"`, `"cpu"`, `"vulkan"`, `"hip"`, `"cuda"`, or `"metal"`.
#'   Support is engine-specific; CrispASR does not expose HIP and should use a
#'   Vulkan-enabled build on AMD hardware.
#' @param hf_cache_dir Optional Hugging Face cache root for local inference.
#'   Defaults to `HF_HOME`, then the saved local inference configuration.
#' @param revision Optional Hugging Face model revision for `service =
#'   "hf-local"`, such as a branch, tag, or preferably an immutable commit SHA.
#'   Defaults to `GENFLOW_HF_REVISION`, then the saved `hf_revision` setting.
#'   Use `revision = ""` to explicitly disable a configured pin for one call.
#' @param trust_remote_code Logical or NULL. Controls whether Transformers may
#'   execute model-repository code. NULL enables it only for genflow's known
#'   MOSS profile; generic local models default to FALSE.
#' @param chunk_length_s Optional chunk length in seconds for the generic
#'   Transformers ASR pipeline.
#' @param return_timestamps Logical or `"word"` for generic Transformers ASR.
#'   MOSS always emits its native timestamped, speaker-aware transcript.
#' @param max_new_tokens Optional generation limit. This is especially useful
#'   for long MOSS transcriptions and compatible local servers.
#' @param base_url Optional base URL for `service = "local-openai"`. Defaults
#'   to `GENFLOW_STT_BASE_URL`, then `http://127.0.0.1:8000`.
#' @param api_key Optional bearer token for `service = "local-openai"`.
#'   Defaults to `GENFLOW_STT_API_KEY`; no authorization header is sent when
#'   both are empty.
#' @param response_format Response format requested from a local
#'   OpenAI-compatible server, typically `"json"` or `"verbose_json"`.
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
  local_profile = NULL,
  device = NULL,
  dtype = NULL,
  python = NULL,
  executable = NULL,
  native_engine = NULL,
  native_backend = NULL,
  native_device = NULL,
  hf_cache_dir = NULL,
  trust_remote_code = NULL,
  chunk_length_s = NULL,
  return_timestamps = FALSE,
  max_new_tokens = NULL,
  base_url = NULL,
  api_key = NULL,
  response_format = "json",
  revision = NULL,
  ...
) {
  start_time <- Sys.time()
  save_txt <- .stt_validate_logical_scalar(save_txt, "save_txt")
  convert <- .stt_validate_logical_scalar(convert, "convert")
  timeout_api <- .stt_validate_positive_number(timeout_api, "timeout_api")
  poll_interval <- .stt_validate_positive_number(
    poll_interval,
    "poll_interval"
  )
  max_poll_seconds <- .stt_validate_positive_number(
    max_poll_seconds,
    "max_poll_seconds"
  )
  revision <- .stt_validate_revision(revision)

  # Normalize inputs. Keep track of the temporary model-specific service name
  # so callers of the pre-release adapter still select the same engine while
  # the public result uses the durable, transport-level service id.
  if (is.list(service)) {
    service <- service$service %||% if (length(service)) service[[1]] else NULL
  }
  if (is.vector(service)) service <- as.character(service[1])
  legacy_moss_service <- .stt_is_legacy_moss_service(service)
  if (legacy_moss_service && is.null(native_engine)) {
    native_engine <- "moss-transcribe"
  }
  if (is.list(model)) {
    model <- model$model %||% if (length(model)) model[[1]] else NULL
  }
  if (is.vector(model)) model <- as.character(model[1])
  if (is.list(language)) {
    language <- language$language %||% if (length(language)) language[[1]] else NULL
  }
  if (is.vector(language)) language <- as.character(language[1])

  service <- .stt_normalize_service(service)
  model <- if (!is.null(model)) as.character(model)[1] else NULL
  if (is.null(model) || length(model) == 0L || is.na(model) || !nzchar(model)) {
    model <- NULL
  }
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

  raw_transcription <- NULL
  transcribed_text <- NULL
  provider_metadata <- list()
  error_message <- NULL

  raw_transcription <- tryCatch({
    switch(service,
      "openai" = .stt_openai(prep$path, model, language, prompt, timeout_api),
      "groq" = .stt_groq(prep$path, model, language, prompt, timeout_api),
      "assemblyai" = .stt_assemblyai(prep$path, language, poll_interval, max_poll_seconds, timeout_api),
      "cloudflare" = .stt_cloudflare(prep$path, timeout_api),
      "voicegain" = .stt_voicegain(prep$path, language, poll_interval, max_poll_seconds, timeout_api),
      "hf" = .stt_hf(prep$path, model, timeout_api),
      "replicate" = .stt_replicate(prep$path, model, timeout_api, poll_interval, max_poll_seconds),
      "hf-local" = .stt_local_hf(
        audio_path = prep$path,
        model = model,
        language = language,
        prompt = prompt,
        timeout_secs = timeout_api,
        profile = local_profile,
        device = device,
        dtype = dtype,
        python = python,
        hf_cache_dir = hf_cache_dir,
        revision = revision,
        trust_remote_code = trust_remote_code,
        chunk_length_s = chunk_length_s,
        return_timestamps = return_timestamps,
        max_new_tokens = max_new_tokens
      ),
      "local-openai" = .stt_local_openai(
        audio_path = prep$path,
        model = model,
        language = language,
        prompt = prompt,
        timeout_secs = timeout_api,
        base_url = base_url,
        api_key = api_key,
        response_format = response_format,
        max_new_tokens = max_new_tokens
      ),
      "local-native" = .stt_local_native(
        audio_path = prep$path,
        model = model,
        language = language,
        prompt = prompt,
        timeout_secs = timeout_api,
        executable = executable,
        native_engine = native_engine,
        native_backend = native_backend,
        native_device = native_device,
        convert = convert,
        max_new_tokens = max_new_tokens,
        legacy_service = legacy_moss_service
      ),
      stop("Unsupported STT service: ", service)
    )
  }, error = function(e) {
    error_message <<- conditionMessage(e)
    NULL
  })

  normalized <- .stt_normalize_result(raw_transcription)
  transcribed_text <- normalized$text
  provider_metadata <- normalized$metadata
  effective_model <- provider_metadata$model %||% model %||%
    .stt_default_model(service)
  if (length(effective_model) == 0L || is.null(effective_model) ||
      is.na(effective_model[[1]]) || !nzchar(as.character(effective_model[[1]]))) {
    effective_model <- "default"
  } else {
    effective_model <- as.character(effective_model[[1]])
  }

  final_status <- "SUCCESS"
  final_msg <- "OK"
  if (is.null(transcribed_text) || !is.character(transcribed_text) ||
      length(transcribed_text) == 0L || is.na(transcribed_text[[1]]) ||
      !nzchar(transcribed_text[[1]])) {
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
    model_tag <- .sanitize_filename(effective_model)
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
    model = effective_model,
    duration = duration_response,
    status_api = final_status,
    status_msg = final_msg,
    saved_file = saved_file,
    audio = prep$path,
    content_type = "text",
    metadata = provider_metadata
  )

  return(invisible(result))
}

#' @rdname gen_stt
#' @method gen_stt genflow_agent
#' @details For a `genflow_agent`, supply `audio_override` through `...` to
#'   transcribe a different input file without changing the saved agent.
#' @export
gen_stt.genflow_agent <- function(audio, ...) {
  agent <- audio
  overrides <- list(...)
  formals_default <- formals(gen_stt.default)
  agent_args <- .genflow_prepare_agent_args(
    agent = agent,
    overrides = overrides,
    target_formals = formals_default,
    required = "audio",
    override_aliases = c(audio_override = "audio"),
    override_label = "gen_stt()"
  )
  do.call(gen_stt.default, agent_args, quote = TRUE)
}

# --- Internal helpers -------------------------------------------------------

#' @keywords internal
#' @noRd
.stt_validate_logical_scalar <- function(value, arg) {
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    stop("`", arg, "` must be TRUE or FALSE.", call. = FALSE)
  }
  value
}

#' @keywords internal
#' @noRd
.stt_validate_positive_number <- function(value, arg) {
  compatible_type <- (is.numeric(value) && !is.complex(value)) ||
    is.character(value)
  if (!compatible_type || length(value) != 1L || is.na(value)) {
    stop("`", arg, "` must be a positive finite number.", call. = FALSE)
  }
  number <- suppressWarnings(as.numeric(value))
  if (length(number) != 1L || is.na(number) || !is.finite(number) ||
      number <= 0) {
    stop("`", arg, "` must be a positive finite number.", call. = FALSE)
  }
  number
}

#' @keywords internal
#' @noRd
.stt_validate_revision <- function(value) {
  if (is.null(value)) return(NULL)
  if (!is.character(value) || length(value) != 1L || is.na(value)) {
    stop("`revision` must be NULL or one character value.", call. = FALSE)
  }
  value <- trimws(value)
  if (grepl("[[:space:][:cntrl:]]", value, perl = TRUE)) {
    stop("`revision` cannot contain whitespace or control characters.", call. = FALSE)
  }
  value
}

#' @keywords internal
#' @noRd
.stt_normalize_service <- function(service) {
  service_id <- tolower(trimws(as.character(service %||% "")[1]))
  if (is.na(service_id) || !nzchar(service_id)) {
    stop("`service` must be a non-empty provider identifier.", call. = FALSE)
  }

  aliases <- c(
    "hf_local" = "hf-local",
    "huggingface-local" = "hf-local",
    "huggingface_local" = "hf-local",
    "transformers" = "hf-local",
    "local-transformers" = "hf-local",
    "local_transformers" = "hf-local",
    "local_openai" = "local-openai",
    "openai-local" = "local-openai",
    "openai_local" = "local-openai",
    "openai-compatible" = "local-openai",
    "openai_compatible" = "local-openai",
    "stt-local-server" = "local-openai",
    "local_native" = "local-native",
    "native-stt" = "local-native",
    "native_stt" = "local-native",
    "local-cli" = "local-native",
    "local_cli" = "local-native",
    "moss-cpp" = "local-native",
    "mosscpp" = "local-native",
    "moss_cpp" = "local-native",
    "moss-transcribe-cpp" = "local-native",
    "moss_transcribe_cpp" = "local-native"
  )

  mapped <- unname(aliases[service_id])
  if (length(mapped) == 1L && !is.na(mapped)) mapped else service_id
}

#' @keywords internal
#' @noRd
.stt_is_legacy_moss_service <- function(service) {
  service_id <- tolower(trimws(as.character(service %||% "")[1]))
  !is.na(service_id) && service_id %in% c(
    "moss-cpp",
    "mosscpp",
    "moss_cpp",
    "moss-transcribe-cpp",
    "moss_transcribe_cpp"
  )
}

#' @keywords internal
#' @noRd
.stt_normalize_result <- function(value) {
  if (is.character(value) && length(value) > 0L &&
      !is.na(value[[1]]) && nzchar(value[[1]])) {
    return(list(text = as.character(value[[1]]), metadata = list()))
  }

  if (is.list(value)) {
    text <- value$text %||% value$transcription %||% value$response_value
    if (is.character(text) && length(text) > 0L &&
        !is.na(text[[1]]) && nzchar(text[[1]])) {
      metadata <- value$metadata
      if (is.null(metadata)) {
        metadata <- value[setdiff(names(value), c("text", "transcription", "response_value"))]
      }
      return(list(text = as.character(text[[1]]), metadata = metadata %||% list()))
    }
  }

  list(text = NULL, metadata = list())
}

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
    "hf-local" = .genflow_local_setting(
      "hf_stt_model",
      env = "GENFLOW_HF_STT_MODEL",
      default = "openai/whisper-large-v3-turbo"
    ),
    "local-openai" = .genflow_local_setting(
      "stt_server_model",
      env = "GENFLOW_STT_MODEL",
      default = "local-model"
    ),
    "local-native" = .genflow_local_setting(
      "stt_native_model",
      env = "GENFLOW_STT_NATIVE_MODEL",
      default = ""
    ),
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

#' Run speech recognition locally with a Hugging Face model
#'
#' Uses the package's optional Python bridge instead of embedding a Python
#' runtime in the R process. This keeps PyTorch, Transformers, ROCm, and
#' model-specific packages optional.
#'
#' @keywords internal
#' @noRd
.stt_local_hf <- function(audio_path,
                          model,
                          language,
                          prompt,
                          timeout_secs,
                          profile = NULL,
                          device = NULL,
                          dtype = NULL,
                          python = NULL,
                          hf_cache_dir = NULL,
                          trust_remote_code = NULL,
                          chunk_length_s = NULL,
                          return_timestamps = FALSE,
                          max_new_tokens = NULL,
                          runner = .stt_run_process,
                          revision = NULL) {
  if (!file.exists(audio_path)) {
    stop("Local Hugging Face STT requires a local audio file.", call. = FALSE)
  }

  local_config <- .genflow_read_local_config()
  model_id <- .stt_argument_or_local_setting(
    model,
    field = "hf_stt_model",
    env = "GENFLOW_HF_STT_MODEL",
    default = "openai/whisper-large-v3-turbo",
    config = local_config
  )
  profile_id <- .stt_resolve_local_profile(
    .stt_argument_or_local_setting(
      profile,
      field = "hf_stt_profile",
      env = "GENFLOW_HF_STT_PROFILE",
      default = "auto",
      config = local_config
    ),
    model_id
  )
  device_id <- .stt_validate_device(.stt_argument_or_local_setting(
    device,
    field = "device",
    env = "GENFLOW_LOCAL_DEVICE",
    default = "auto",
    config = local_config
  ))
  dtype_id <- .stt_validate_dtype(.stt_argument_or_local_setting(
    dtype,
    field = "dtype",
    env = "GENFLOW_LOCAL_DTYPE",
    default = "auto",
    config = local_config
  ))
  cache_dir <- .stt_argument_or_local_setting(
    hf_cache_dir,
    field = "hf_cache_dir",
    env = "HF_HOME",
    default = "",
    config = local_config,
    allow_empty = TRUE
  )
  revision_id <- .stt_validate_revision(.stt_argument_or_local_setting(
    revision,
    field = "hf_revision",
    env = "GENFLOW_HF_REVISION",
    default = "",
    config = local_config,
    allow_empty = TRUE
  ))
  timestamps_id <- .stt_normalize_timestamps(return_timestamps)

  if (!is.null(chunk_length_s)) {
    chunk_length_s <- suppressWarnings(as.numeric(chunk_length_s)[1])
    if (is.na(chunk_length_s) || !is.finite(chunk_length_s) || chunk_length_s <= 0) {
      stop("`chunk_length_s` must be NULL or a positive number.", call. = FALSE)
    }
  }
  if (!is.null(max_new_tokens)) {
    max_new_tokens <- suppressWarnings(as.numeric(max_new_tokens)[1])
    if (is.na(max_new_tokens) || !is.finite(max_new_tokens) ||
        max_new_tokens < 1 || max_new_tokens != as.integer(max_new_tokens)) {
      stop("`max_new_tokens` must be NULL or a positive integer.", call. = FALSE)
    }
    max_new_tokens <- as.integer(max_new_tokens)
  }

  if (is.null(trust_remote_code)) {
    trust_remote_code <- identical(profile_id, "moss")
  } else if (!is.logical(trust_remote_code) || length(trust_remote_code) != 1L ||
             is.na(trust_remote_code)) {
    stop("`trust_remote_code` must be TRUE, FALSE, or NULL.", call. = FALSE)
  }
  if (identical(profile_id, "moss") && !isTRUE(trust_remote_code)) {
    stop(
      "The MOSS profile requires `trust_remote_code = TRUE` because the official ",
      "model repository contains its Transformers implementation.",
      call. = FALSE
    )
  }

  python_bin <- .stt_resolve_python(python, config = local_config)
  bridge_script <- .stt_bridge_script()
  output_file <- tempfile("genflow-stt-", fileext = ".json")
  on.exit(unlink(output_file), add = TRUE)

  args <- c(
    bridge_script,
    "--audio", normalizePath(audio_path, winslash = "/", mustWork = TRUE),
    "--model", model_id,
    "--profile", profile_id,
    "--device", device_id,
    "--dtype", dtype_id,
    "--return-timestamps", timestamps_id,
    "--output", output_file
  )
  if (!is.null(language)) args <- c(args, "--language", language)
  if (!is.null(prompt)) args <- c(args, "--prompt", prompt)
  if (nzchar(revision_id)) args <- c(args, "--revision", revision_id)
  if (isTRUE(trust_remote_code)) args <- c(args, "--trust-remote-code")
  if (!is.null(chunk_length_s)) {
    args <- c(args, "--chunk-length-s", format(chunk_length_s, scientific = FALSE, trim = TRUE))
  }
  if (!is.null(max_new_tokens)) {
    args <- c(args, "--max-new-tokens", as.character(max_new_tokens))
  }

  process <- runner(
    command = python_bin,
    args = args,
    timeout_secs = timeout_secs,
    environment = if (nzchar(cache_dir)) {
      c(HF_HOME = path.expand(cache_dir))
    } else {
      character()
    }
  )
  status <- suppressWarnings(as.integer(process$status %||% 0L)[1])
  if (length(status) == 0L || is.na(status)) status <- -1L
  process_output <- as.character(process$output %||% character())

  payload <- NULL
  if (file.exists(output_file) && isTRUE(file.info(output_file)$size > 0)) {
    payload <- tryCatch(
      jsonlite::fromJSON(output_file, simplifyVector = FALSE),
      error = function(e) {
        stop(
          "Local STT bridge returned invalid JSON: ", conditionMessage(e),
          call. = FALSE
        )
      }
    )
  }

  if (is.null(payload)) {
    detail <- .stt_process_detail(process_output)
    if (identical(status, 124L)) {
      stop(
        "Local Hugging Face STT timed out after ", timeout_secs,
        " seconds.", if (nzchar(detail)) paste0(" Python output: ", detail) else "",
        call. = FALSE
      )
    }
    stop(
      "Local Hugging Face STT bridge exited with status ", status, ".",
      if (nzchar(detail)) paste0(" Python output: ", detail) else
        " No structured diagnostic was produced.",
      call. = FALSE
    )
  }

  if (!isTRUE(payload$ok)) {
    error_type <- as.character(payload$error_type %||% "PythonError")[1]
    error_text <- as.character(payload$error %||% "unknown local inference error")[1]
    hint <- as.character(payload$hint %||% "")[1]
    detail <- .stt_process_detail(process_output)
    stop(
      "Local Hugging Face STT failed (", error_type, "): ", error_text,
      if (nzchar(hint)) paste0(" ", hint) else "",
      if (nzchar(detail)) paste0(" Python output: ", detail) else "",
      call. = FALSE
    )
  }

  text <- as.character(payload$text %||% "")[1]
  if (is.na(text) || !nzchar(text)) {
    stop("Local Hugging Face STT returned an empty transcript.", call. = FALSE)
  }

  metadata <- payload[setdiff(
    names(payload),
    c("ok", "text", "error", "error_type", "hint", "traceback")
  )]
  list(text = text, metadata = metadata)
}

#' Registered native speech-to-text engines
#'
#' The public service id describes the transport (`local-native`). Engine ids
#' describe a concrete executable contract. Keeping those dimensions separate
#' lets genflow add native runtimes without adding one service per model family.
#'
#' @keywords internal
#' @noRd
.stt_native_engine_registry <- function() {
  list(
    crispasr = list(
      id = "crispasr",
      label = "CrispASR",
      aliases = c("crisp-asr", "crisp_asr"),
      executables = "crispasr",
      devices = c("auto", "cpu", "vulkan", "cuda", "metal"),
      model_kinds = c("file", "auto", "hf")
    ),
    `moss-transcribe` = list(
      id = "moss-transcribe",
      label = "moss-transcribe.cpp",
      aliases = c("moss", "moss-cpp", "moss_cpp", "mosscpp"),
      executables = "moss-transcribe",
      devices = c("auto", "cpu", "vulkan", "hip", "cuda", "metal"),
      model_kinds = "file"
    )
  )
}

#' @keywords internal
#' @noRd
.stt_normalize_native_engine <- function(engine, allow_auto = TRUE) {
  value <- tolower(trimws(as.character(engine %||% "auto")[1]))
  if (is.na(value) || !nzchar(value)) value <- "auto"
  registry <- .stt_native_engine_registry()
  aliases <- unlist(unname(lapply(registry, function(spec) {
    stats::setNames(rep(spec$id, length(spec$aliases)), spec$aliases)
  })), use.names = TRUE)
  if (value %in% names(aliases)) value <- unname(aliases[[value]])
  valid <- c(if (isTRUE(allow_auto)) "auto", names(registry))
  if (!value %in% valid) {
    stop(
      "`native_engine` must be one of ",
      paste0("\"", valid, "\"", collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  value
}

#' @keywords internal
#' @noRd
.stt_validate_native_backend <- function(value) {
  backend <- tolower(trimws(as.character(value %||% "")[1]))
  if (is.na(backend) || identical(backend, "auto")) return("")
  if (!nzchar(backend)) return("")
  if (!grepl("^[a-z0-9][a-z0-9._-]*$", backend, perl = TRUE)) {
    stop(
      "`native_backend` must be empty or a backend identifier containing only ",
      "letters, numbers, dots, underscores, and hyphens.",
      call. = FALSE
    )
  }
  backend
}

#' Parse an explicit CrispASR Hugging Face model reference
#'
#' `hf://` is genflow's opt-in marker, not a CrispASR URI scheme. CrispASR
#' receives `--hf-repo OWNER/REPO:FILE` and cannot resolve a repository without
#' a filename when `-m auto` is used. Accept a slash before the filename as a
#' copy-and-paste convenience, but always return CrispASR's canonical form.
#'
#' @keywords internal
#' @noRd
.stt_parse_crispasr_hf_reference <- function(value) {
  value <- trimws(as.character(value %||% "")[1])
  if (is.na(value) || !startsWith(tolower(value), "hf://")) {
    stop(
      "CrispASR Hugging Face references must start with `hf://`.",
      call. = FALSE
    )
  }

  body <- sub("^hf://", "", value, ignore.case = TRUE)
  component <- "[A-Za-z0-9][A-Za-z0-9._-]*"
  filename <- "[^/\\\\:?#[:space:]]+"
  patterns <- c(
    paste0("^(", component, ")/(", component, "):(", filename, ")$"),
    paste0("^(", component, ")/(", component, ")/(", filename, ")$")
  )
  matched <- NULL
  style <- NULL
  for (i in seq_along(patterns)) {
    candidate <- regmatches(body, regexec(patterns[[i]], body, perl = TRUE))[[1]]
    if (length(candidate) == 4L) {
      matched <- candidate
      style <- if (i == 1L) "colon" else "slash"
      break
    }
  }

  if (is.null(matched) || matched[[4]] %in% c(".", "..")) {
    stop(
      "CrispASR Hugging Face references must include one model filename as ",
      "`hf://OWNER/REPO:FILE` or `hf://OWNER/REPO/FILE`.",
      call. = FALSE
    )
  }

  repository <- paste0(matched[[2]], "/", matched[[3]])
  file <- matched[[4]]
  argument <- paste0(repository, ":", file)
  list(
    repository = repository,
    file = file,
    argument = argument,
    reference = paste0("hf://", argument),
    input_style = style
  )
}

#' @keywords internal
#' @noRd
.stt_validate_native_device <- function(value) {
  device <- tolower(trimws(as.character(value %||% "auto")[1]))
  if (is.na(device) || !nzchar(device) ||
      !device %in% c("auto", "cpu", "vulkan", "hip", "cuda", "metal")) {
    stop(
      "`native_device` must be \"auto\", \"cpu\", \"vulkan\", \"hip\", ",
      "\"cuda\", or \"metal\".",
      call. = FALSE
    )
  }
  device
}

#' @keywords internal
#' @noRd
.stt_validate_max_new_tokens <- function(value) {
  if (is.null(value)) return(NULL)
  number <- suppressWarnings(as.numeric(value)[1])
  if (is.na(number) || !is.finite(number) ||
      number < 1 || number != as.integer(number)) {
    stop("`max_new_tokens` must be NULL or a positive integer.", call. = FALSE)
  }
  as.integer(number)
}

#' @keywords internal
#' @noRd
.stt_native_setting <- function(value,
                                field,
                                env,
                                config,
                                default = "",
                                legacy_field = NULL,
                                legacy_env = character()) {
  resolved <- .stt_argument_or_local_setting(
    value,
    field = field,
    env = env,
    default = "",
    config = config,
    allow_empty = TRUE
  )
  if (nzchar(resolved) || is.null(legacy_field)) {
    return(if (nzchar(resolved)) resolved else default)
  }
  legacy <- .genflow_local_setting(
    legacy_field,
    env = legacy_env,
    default = "",
    config = config
  )
  if (nzchar(legacy)) legacy else default
}

#' @keywords internal
#' @noRd
.stt_resolve_native_engine <- function(native_engine = NULL,
                                       executable = NULL,
                                       model = NULL,
                                       native_backend = NULL,
                                       config = NULL) {
  config <- config %||% .genflow_read_local_config()
  requested <- .stt_native_setting(
    native_engine,
    field = "stt_native_engine",
    env = "GENFLOW_STT_NATIVE_ENGINE",
    config = config,
    default = "auto"
  )
  engine <- .stt_normalize_native_engine(requested)
  if (!identical(engine, "auto")) return(engine)

  executable_value <- .stt_native_setting(
    executable,
    field = "stt_native_executable",
    env = "GENFLOW_STT_NATIVE_EXECUTABLE",
    config = config
  )
  executable_name <- tolower(basename(executable_value))
  if (grepl("crispasr", executable_name, fixed = TRUE)) return("crispasr")
  if (grepl("moss-transcribe", executable_name, fixed = TRUE)) {
    return("moss-transcribe")
  }

  backend <- .stt_validate_native_backend(.stt_native_setting(
    native_backend,
    field = "stt_native_backend",
    env = "GENFLOW_STT_NATIVE_BACKEND",
    config = config
  ))
  model_value <- .stt_native_setting(
    model,
    field = "stt_native_model",
    env = "GENFLOW_STT_NATIVE_MODEL",
    config = config
  )
  if (nzchar(backend) ||
      identical(tolower(model_value), "auto") ||
      startsWith(tolower(model_value), "hf://")) {
    return("crispasr")
  }

  registry <- .stt_native_engine_registry()
  installed <- names(Filter(function(spec) {
    nzchar(.genflow_resolve_executable("", spec$executables))
  }, registry))
  if (length(installed) == 1L) return(installed[[1]])
  if (!length(installed)) {
    stop(
      "Could not select a native STT engine automatically. Configure ",
      "`native_engine` as \"crispasr\" or \"moss-transcribe\" and install ",
      "that executable.",
      call. = FALSE
    )
  }
  stop(
    "More than one native STT engine is installed (",
    paste(installed, collapse = ", "),
    "). Set `native_engine` explicitly.",
    call. = FALSE
  )
}

#' @keywords internal
#' @noRd
.stt_resolve_native_executable <- function(engine,
                                           executable = NULL,
                                           config = NULL) {
  config <- config %||% .genflow_read_local_config()
  registry <- .stt_native_engine_registry()
  spec <- registry[[engine]]
  if (is.null(spec)) {
    stop("Unsupported native STT engine: ", engine, call. = FALSE)
  }
  candidate <- .stt_native_setting(
    executable,
    field = "stt_native_executable",
    env = "GENFLOW_STT_NATIVE_EXECUTABLE",
    config = config,
    legacy_field = if (identical(engine, "moss-transcribe")) {
      "moss_cpp_executable"
    } else {
      NULL
    },
    legacy_env = if (identical(engine, "moss-transcribe")) {
      "GENFLOW_MOSS_CPP_EXECUTABLE"
    } else {
      character()
    }
  )
  resolved <- if (nzchar(candidate)) {
    .genflow_resolve_executable(candidate)
  } else {
    .genflow_resolve_executable("", spec$executables)
  }
  if (!nzchar(resolved)) {
    stop(
      spec$label,
      " executable not found",
      if (nzchar(candidate)) paste0(": ", candidate) else "",
      ". Pass `executable`, set GENFLOW_STT_NATIVE_EXECUTABLE, or install ",
      spec$executables[[1]],
      " on PATH.",
      call. = FALSE
    )
  }
  if (.Platform$OS.type != "windows" &&
      file.access(resolved, mode = 1L) != 0L) {
    stop(spec$label, " executable is not executable: ", resolved, call. = FALSE)
  }
  resolved
}

#' Dispatch speech recognition to a registered native engine
#'
#' @keywords internal
#' @noRd
.stt_local_native <- function(audio_path,
                              model,
                              language,
                              prompt,
                              timeout_secs,
                              executable = NULL,
                              native_engine = NULL,
                              native_backend = NULL,
                              native_device = NULL,
                              convert = TRUE,
                              max_new_tokens = NULL,
                              legacy_service = FALSE,
                              runner = .stt_run_process) {
  config <- .genflow_read_local_config()
  engine <- .stt_resolve_native_engine(
    native_engine = native_engine,
    executable = executable,
    model = model,
    native_backend = native_backend,
    config = config
  )
  backend <- .stt_validate_native_backend(.stt_native_setting(
    native_backend,
    field = "stt_native_backend",
    env = "GENFLOW_STT_NATIVE_BACKEND",
    config = config
  ))
  if (identical(engine, "moss-transcribe") && nzchar(backend) &&
      !backend %in% c("moss", "moss-diarize", "moss-transcribe")) {
    stop(
      "The moss-transcribe engine only supports the MOSS architecture; ",
      "`native_backend` must be empty or \"moss-diarize\".",
      call. = FALSE
    )
  }
  if (identical(engine, "moss-transcribe") &&
      backend %in% c("moss", "moss-transcribe")) {
    backend <- "moss-diarize"
  }
  model_value <- .stt_native_setting(
    model,
    field = "stt_native_model",
    env = "GENFLOW_STT_NATIVE_MODEL",
    config = config,
    legacy_field = if (identical(engine, "moss-transcribe")) {
      "moss_cpp_model"
    } else {
      NULL
    },
    legacy_env = if (identical(engine, "moss-transcribe")) {
      "GENFLOW_MOSS_CPP_MODEL"
    } else {
      character()
    }
  )
  device <- .stt_validate_native_device(.stt_native_setting(
    native_device,
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
  executable_path <- .stt_resolve_native_executable(
    engine,
    executable = executable,
    config = config
  )

  result <- switch(engine,
    crispasr = .stt_native_crispasr(
      audio_path = audio_path,
      model = model_value,
      language = language,
      prompt = prompt,
      timeout_secs = timeout_secs,
      executable = executable_path,
      native_backend = backend,
      native_device = device,
      max_new_tokens = max_new_tokens,
      runner = runner
    ),
    `moss-transcribe` = .stt_local_moss_cpp(
      audio_path = audio_path,
      model = model_value,
      language = language,
      prompt = prompt,
      timeout_secs = timeout_secs,
      executable = executable_path,
      native_device = device,
      convert = convert,
      max_new_tokens = max_new_tokens,
      runner = runner
    ),
    stop("Unsupported native STT engine: ", engine, call. = FALSE)
  )
  engine_metadata <- list(
    canonical_service = "local-native",
    transport = "process",
    engine = engine,
    backend = if (nzchar(backend)) {
      backend
    } else {
      result$metadata$backend %||% ""
    },
    executable = executable_path,
    native_device = device,
    model = result$metadata$model %||% model_value
  )
  if (isTRUE(legacy_service)) engine_metadata$service_alias <- "moss-cpp"
  result$metadata <- utils::modifyList(result$metadata %||% list(), engine_metadata)
  result
}

#' Run speech recognition with the multi-architecture CrispASR CLI
#'
#' CrispASR writes structured JSON to `<output-base>.json`. A local GGUF path
#' is the safest default. `auto` and `hf://` references are deliberately
#' explicit because they may download model files into CrispASR's own cache.
#'
#' @keywords internal
#' @noRd
.stt_native_crispasr <- function(audio_path,
                                 model,
                                 language,
                                 prompt,
                                 timeout_secs,
                                 executable,
                                 native_backend = NULL,
                                 native_device = "auto",
                                 max_new_tokens = NULL,
                                 runner = .stt_run_process) {
  if (!file.exists(audio_path) || dir.exists(audio_path)) {
    stop("CrispASR requires a local audio file.", call. = FALSE)
  }
  audio_path <- normalizePath(audio_path, winslash = "/", mustWork = TRUE)
  timeout_secs <- .stt_validate_positive_number(timeout_secs, "timeout_secs")
  backend <- .stt_validate_native_backend(native_backend)
  device <- .stt_validate_native_device(native_device)
  max_new_tokens <- .stt_validate_max_new_tokens(max_new_tokens)

  model_value <- trimws(as.character(model %||% "")[1])
  if (is.na(model_value) || !nzchar(model_value)) {
    stop(
      "A CrispASR model is required. Pass a local model path, \"auto\", or ",
      "an explicit hf://OWNER/REPO:FILE or hf://OWNER/REPO/FILE reference.",
      call. = FALSE
    )
  }

  model_kind <- "file"
  model_args <- character()
  if (identical(tolower(model_value), "auto")) {
    if (!nzchar(backend)) {
      stop(
        "CrispASR `model = \"auto\"` requires `native_backend` so the ",
        "downloaded architecture is unambiguous.",
        call. = FALSE
      )
    }
    model_kind <- "auto"
    model_value <- "auto"
    model_args <- c("-m", "auto")
  } else if (startsWith(tolower(model_value), "hf://")) {
    reference <- .stt_parse_crispasr_hf_reference(model_value)
    model_kind <- "hf"
    model_value <- reference$reference
    model_args <- c("-m", "auto", "--hf-repo", reference$argument)
  } else {
    model_path <- path.expand(model_value)
    info <- suppressWarnings(file.info(model_path))
    if (!file.exists(model_path) ||
        dir.exists(model_path) ||
        !is.finite(info$size[[1]]) ||
        info$size[[1]] <= 0) {
      stop(
        "CrispASR model file not found or empty: ",
        model_value,
        ". Prefix a compatible remote GGUF repository with `hf://`.",
        call. = FALSE
      )
    }
    model_value <- normalizePath(model_path, winslash = "/", mustWork = TRUE)
    model_args <- c("-m", model_value)
  }

  if (identical(device, "hip")) {
    stop(
      "CrispASR does not expose HIP as a runtime selector. On AMD GPUs use a ",
      "Vulkan-enabled build with `native_device = \"vulkan\"`.",
      call. = FALSE
    )
  }
  device_args <- switch(device,
    auto = character(),
    cpu = "--no-gpu",
    c("--gpu-backend", device)
  )

  ignored_arguments <- character()
  prompt_args <- character()
  if (!is.null(prompt)) {
    if (nzchar(backend) && !identical(backend, "whisper")) {
      ignored_arguments <- c(ignored_arguments, "prompt")
      warning(
        "CrispASR `prompt` is currently a Whisper-only control and was ",
        "ignored for backend \"", backend, "\".",
        call. = FALSE
      )
    } else {
      prompt_args <- c("--prompt", prompt)
    }
  }

  output_base <- tempfile("genflow-crispasr-")
  output_file <- paste0(output_base, ".json")
  on.exit(try(unlink(c(output_base, output_file)), silent = TRUE), add = TRUE)
  args <- c(
    model_args,
    if (nzchar(backend)) c("--backend", backend),
    "-f", audio_path,
    "-of", output_base,
    "-ojf",
    "-np",
    device_args,
    if (!is.null(language)) c("-l", language),
    prompt_args,
    if (!is.null(max_new_tokens)) {
      c("--max-new-tokens", as.character(max_new_tokens))
    }
  )
  process <- runner(
    command = executable,
    args = args,
    timeout_secs = timeout_secs,
    environment = character()
  )
  status <- suppressWarnings(as.integer(process$status %||% 0L)[1])
  if (length(status) == 0L || is.na(status)) status <- -1L
  process_output <- as.character(process$output %||% character())
  payload <- if (file.exists(output_file) && !dir.exists(output_file)) {
    tryCatch(
      jsonlite::fromJSON(output_file, simplifyVector = FALSE),
      error = function(e) e
    )
  } else {
    NULL
  }
  normalized <- if (is.list(payload) && !inherits(payload, "error")) {
    tryCatch(.stt_normalize_native_payload(payload), error = function(e) e)
  } else {
    NULL
  }
  complete_payload <- is.list(normalized) &&
    !inherits(normalized, "error") &&
    is.character(normalized$text) &&
    length(normalized$text) == 1L &&
    !is.na(normalized$text) &&
    nzchar(normalized$text)

  if (identical(status, 124L) && !complete_payload) {
    detail <- .stt_process_detail(process_output)
    stop(
      "CrispASR timed out after ", timeout_secs, " seconds.",
      if (nzchar(detail)) paste0(" Process output: ", detail) else "",
      call. = FALSE
    )
  }
  if (!identical(status, 0L) && !identical(status, 124L)) {
    detail <- .stt_process_detail(process_output)
    stop(
      "CrispASR exited with status ", status, ".",
      if (nzchar(detail)) paste0(" Process output: ", detail) else "",
      call. = FALSE
    )
  }
  if (inherits(payload, "error")) {
    stop(
      "CrispASR returned malformed JSON: ",
      conditionMessage(payload),
      call. = FALSE
    )
  }
  if (is.null(payload)) {
    detail <- .stt_process_detail(process_output)
    stop(
      "CrispASR did not create its expected JSON result: ",
      output_file,
      ".",
      if (nzchar(detail)) paste0(" Process output: ", detail) else "",
      call. = FALSE
    )
  }
  if (inherits(normalized, "error")) {
    stop(conditionMessage(normalized), call. = FALSE)
  }

  crisp_metadata <- if (is.list(payload$crispasr)) {
    payload$crispasr
  } else {
    list()
  }
  metadata <- utils::modifyList(
    normalized$metadata %||% list(),
    list(
      engine = "crispasr",
      backend = crisp_metadata$backend %||% backend,
      executable = executable,
      native_device = device,
      model = model_value,
      model_kind = model_kind,
      resolved_model = crisp_metadata$model %||% NULL
    )
  )
  if (length(ignored_arguments)) {
    metadata$ignored_arguments <- ignored_arguments
  }
  list(text = normalized$text, metadata = metadata)
}

#' Run speech recognition with the native MOSS C++ CLI
#'
#' The executable contract is:
#' `moss-transcribe transcribe <model.gguf> <audio.wav> --format json`.
#' Accelerator selection belongs to the native binary and is passed through
#' `MTD_DEVICE`; no Python environment is involved.
#'
#' @keywords internal
#' @noRd
.stt_local_moss_cpp <- function(audio_path,
                                model,
                                language,
                                prompt,
                                timeout_secs,
                                executable = NULL,
                                native_device = NULL,
                                convert = TRUE,
                                max_new_tokens = NULL,
                                runner = .stt_run_process) {
  if (!file.exists(audio_path) || dir.exists(audio_path)) {
    stop("Native MOSS C++ STT requires a local audio file.", call. = FALSE)
  }
  timeout_secs <- .stt_validate_positive_number(timeout_secs, "timeout_secs")
  convert <- .stt_validate_logical_scalar(convert, "convert")

  local_config <- .genflow_read_local_config()
  model_value <- .stt_native_setting(
    model,
    field = "stt_native_model",
    env = "GENFLOW_STT_NATIVE_MODEL",
    config = local_config,
    legacy_field = "moss_cpp_model",
    legacy_env = "GENFLOW_MOSS_CPP_MODEL"
  )
  if (!nzchar(model_value)) {
    stop(
      "A local MOSS GGUF model is required. Pass `model`, set ",
      "GENFLOW_STT_NATIVE_MODEL, or configure `stt_native_model`.",
      call. = FALSE
    )
  }
  model_path <- path.expand(model_value)
  model_info <- suppressWarnings(file.info(model_path))
  if (!file.exists(model_path) ||
      dir.exists(model_path) ||
      !is.finite(model_info$size[[1]]) ||
      model_info$size[[1]] <= 0) {
    stop(
      "MOSS C++ model file not found or empty: ",
      model_value,
      call. = FALSE
    )
  }
  model_path <- normalizePath(model_path, winslash = "/", mustWork = TRUE)

  executable_path <- .stt_resolve_native_executable(
    "moss-transcribe",
    executable = executable,
    config = local_config
  )
  device_id <- .stt_validate_native_device(.stt_native_setting(
    native_device,
    field = "stt_native_device",
    env = "GENFLOW_STT_NATIVE_DEVICE",
    config = local_config,
    default = "auto",
    legacy_field = "moss_cpp_device",
    legacy_env = "GENFLOW_MOSS_CPP_DEVICE"
  ))
  max_new_tokens <- .stt_validate_max_new_tokens(max_new_tokens)

  prepared <- .stt_moss_cpp_prepare_wav(
    audio_path,
    convert = convert,
    timeout_secs = timeout_secs
  )
  if (!is.null(prepared$tmp)) {
    on.exit(try(unlink(prepared$tmp), silent = TRUE), add = TRUE)
  }

  ignored_arguments <- character()
  if (!is.null(language)) ignored_arguments <- c(ignored_arguments, "language")
  if (!is.null(prompt)) ignored_arguments <- c(ignored_arguments, "prompt")
  if (length(ignored_arguments)) {
    warning(
      "The moss-transcribe engine does not currently support ",
      paste0("`", ignored_arguments, "`", collapse = " or "),
      "; the argument was ignored.",
      call. = FALSE
    )
  }

  process <- runner(
    command = executable_path,
    args = c(
      "transcribe",
      model_path,
      prepared$path,
      "--format",
      "json",
      if (!is.null(max_new_tokens)) {
        c("--max-new", as.character(max_new_tokens))
      }
    ),
    timeout_secs = timeout_secs,
    environment = if (identical(device_id, "auto")) {
      character()
    } else {
      c(MTD_DEVICE = device_id)
    }
  )
  status <- suppressWarnings(as.integer(process$status %||% 0L)[1])
  if (length(status) == 0L || is.na(status)) status <- -1L
  process_output <- as.character(process$output %||% character())
  payload <- .stt_parse_moss_cpp_json(process_output)
  normalized <- if (is.null(payload)) {
    NULL
  } else {
    tryCatch(
      .stt_normalize_moss_cpp_payload(payload),
      error = function(e) e
    )
  }

  # system2() can report its timeout status at the same boundary where a
  # process has already emitted a complete result. A valid, non-empty JSON
  # transcript is stronger completion evidence than that racy status.
  complete_payload <- is.list(normalized) &&
    is.character(normalized$text) &&
    length(normalized$text) == 1L &&
    !is.na(normalized$text) &&
    nzchar(normalized$text)
  if (identical(status, 124L) && !complete_payload) {
    detail <- .stt_process_detail(process_output)
    stop(
      "Native MOSS C++ STT timed out after ", timeout_secs, " seconds.",
      if (nzchar(detail)) paste0(" Process output: ", detail) else "",
      call. = FALSE
    )
  }
  if (!identical(status, 0L) && !identical(status, 124L)) {
    native_error <- .stt_moss_cpp_payload_error(payload)
    detail <- .stt_process_detail(process_output)
    stop(
      "Native MOSS C++ STT exited with status ", status, ".",
      if (nzchar(native_error)) paste0(" ", native_error) else "",
      if (!nzchar(native_error) && nzchar(detail)) {
        paste0(" Process output: ", detail)
      } else {
        ""
      },
      call. = FALSE
    )
  }
  if (is.null(payload)) {
    detail <- .stt_process_detail(process_output)
    stop(
      "Native MOSS C++ STT returned malformed or missing JSON.",
      if (nzchar(detail)) paste0(" Process output: ", detail) else "",
      call. = FALSE
    )
  }
  if (inherits(normalized, "error")) {
    native_error <- .stt_moss_cpp_payload_error(payload)
    if (nzchar(native_error)) {
      stop("Native MOSS C++ STT failed: ", native_error, call. = FALSE)
    }
    stop(conditionMessage(normalized), call. = FALSE)
  }

  metadata <- utils::modifyList(
    normalized$metadata %||% list(),
    list(
      engine = "moss-transcribe",
      backend = "moss-diarize",
      executable = executable_path,
      native_device = device_id,
      model = model_path
    )
  )
  if (length(ignored_arguments)) {
    metadata$ignored_arguments <- ignored_arguments
  }
  list(text = normalized$text, metadata = metadata)
}

#' @keywords internal
#' @noRd
.stt_moss_cpp_prepare_wav <- function(audio_path,
                                      convert,
                                      timeout_secs,
                                      runner = .stt_run_process) {
  audio_path <- normalizePath(audio_path, winslash = "/", mustWork = TRUE)
  if (identical(tolower(tools::file_ext(audio_path)), "wav")) {
    return(list(path = audio_path, tmp = NULL))
  }
  if (!isTRUE(convert)) {
    stop(
      "The moss-transcribe engine requires WAV input. Set `convert = TRUE` or provide ",
      "a .wav file.",
      call. = FALSE
    )
  }

  ffmpeg <- .genflow_resolve_executable("", "ffmpeg")
  if (!nzchar(ffmpeg)) {
    stop(
      "The moss-transcribe engine requires WAV input and ffmpeg was not found for ",
      "conversion.",
      call. = FALSE
    )
  }
  output_path <- tempfile("genflow-moss-transcribe-", fileext = ".wav")
  completed <- FALSE
  on.exit({
    if (!completed) try(unlink(output_path), silent = TRUE)
  }, add = TRUE)
  process <- runner(
    command = ffmpeg,
    args = c(
      "-y",
      "-i", audio_path,
      "-ac", "1",
      "-ar", "16000",
      "-c:a", "pcm_s16le",
      output_path
    ),
    timeout_secs = timeout_secs,
    environment = character()
  )
  status <- suppressWarnings(as.integer(process$status %||% 0L)[1])
  if (length(status) == 0L || is.na(status)) status <- -1L
  detail <- .stt_process_detail(process$output)
  if (identical(status, 124L)) {
    stop(
      "Audio conversion for moss-transcribe timed out after ", timeout_secs,
      " seconds.",
      call. = FALSE
    )
  }
  if (!identical(status, 0L) ||
      !file.exists(output_path) ||
      !isTRUE(file.info(output_path)$size > 0)) {
    stop(
      "Audio conversion for moss-transcribe failed.",
      if (nzchar(detail)) paste0(" ffmpeg output: ", detail) else "",
      call. = FALSE
    )
  }
  completed <- TRUE
  list(
    path = normalizePath(output_path, winslash = "/", mustWork = TRUE),
    tmp = output_path
  )
}

#' @keywords internal
#' @noRd
.stt_parse_moss_cpp_json <- function(output) {
  lines <- as.character(output %||% character())
  text <- trimws(paste(lines, collapse = "\n"))
  if (!nzchar(text)) return(NULL)

  parse_candidate <- function(candidate) {
    candidate <- trimws(candidate)
    if (!nzchar(candidate)) return(NULL)
    tryCatch(
      jsonlite::fromJSON(candidate, simplifyVector = FALSE),
      error = function(e) NULL
    )
  }

  candidate_score <- function(payload) {
    normalized <- tryCatch(
      .stt_normalize_moss_cpp_payload(payload),
      error = function(e) NULL
    )
    if (is.list(normalized) && nzchar(normalized$text %||% "")) return(3L)
    if (nzchar(.stt_moss_cpp_payload_error(payload))) return(2L)
    1L
  }

  parsed <- parse_candidate(text)
  if (!is.null(parsed) && candidate_score(parsed) == 3L) return(parsed)
  best_payload <- parsed
  best_score <- if (is.null(parsed)) 0L else candidate_score(parsed)

  chars <- strsplit(text, "", fixed = TRUE)[[1]]
  opening_positions <- sort(unique(c(
    gregexpr("{", text, fixed = TRUE)[[1]],
    gregexpr("[", text, fixed = TRUE)[[1]]
  )))
  opening_positions <- opening_positions[opening_positions > 0L]
  for (start in opening_positions) {
    stack <- character()
    in_string <- FALSE
    escaped <- FALSE
    end <- NA_integer_
    for (position in seq.int(start, length(chars))) {
      char <- chars[[position]]
      if (in_string) {
        if (escaped) {
          escaped <- FALSE
        } else if (identical(char, "\\")) {
          escaped <- TRUE
        } else if (identical(char, "\"")) {
          in_string <- FALSE
        }
        next
      }
      if (identical(char, "\"")) {
        in_string <- TRUE
      } else if (identical(char, "{")) {
        stack <- c(stack, "}")
      } else if (identical(char, "[")) {
        stack <- c(stack, "]")
      } else if (char %in% c("}", "]")) {
        if (!length(stack) || !identical(char, stack[[length(stack)]])) {
          break
        }
        stack <- stack[-length(stack)]
        if (!length(stack)) {
          end <- position
          break
        }
      }
    }
    if (is.na(end)) next
    parsed <- parse_candidate(paste0(chars[start:end], collapse = ""))
    if (is.null(parsed)) next
    score <- candidate_score(parsed)
    if (score == 3L) return(parsed)
    if (score > best_score) {
      best_payload <- parsed
      best_score <- score
    }
  }
  best_payload
}

#' @keywords internal
#' @noRd
.stt_native_scalar_text <- function(value) {
  if (!is.character(value) || length(value) == 0L || is.na(value[[1]])) {
    return("")
  }
  trimws(as.character(value[[1]]))
}

#' @keywords internal
#' @noRd
.stt_native_is_segment <- function(value) {
  if (!is.list(value) || is.null(names(value))) return(FALSE)
  has_text <- any(c("text", "transcript", "transcription") %in% names(value))
  has_boundary <- any(c(
    "start", "end", "start_time", "end_time", "speaker", "speaker_id",
    "offsets", "timestamps"
  ) %in% names(value))
  has_text && has_boundary
}

#' @keywords internal
#' @noRd
.stt_native_normalize_segment <- function(value) {
  if (!.stt_native_is_segment(value)) return(NULL)
  text <- .stt_native_scalar_text(
    value$text %||% value$transcript %||% value$transcription
  )
  if (!nzchar(text)) return(NULL)
  value$text <- text
  offsets <- if (is.list(value$offsets)) value$offsets else list()
  offset_start <- suppressWarnings(as.numeric(offsets$from %||% NA_real_)[1])
  offset_end <- suppressWarnings(as.numeric(offsets$to %||% NA_real_)[1])
  value$start <- value$start %||% value$start_time %||%
    if (is.finite(offset_start)) offset_start / 1000 else NULL
  value$end <- value$end %||% value$end_time %||%
    if (is.finite(offset_end)) offset_end / 1000 else NULL
  value$speaker <- value$speaker %||% value$speaker_id %||%
    value$speaker_label
  value
}

#' @keywords internal
#' @noRd
.stt_native_as_segments <- function(value) {
  if (.stt_native_is_segment(value)) {
    segment <- .stt_native_normalize_segment(value)
    return(if (is.null(segment)) list() else list(segment))
  }
  if (!is.list(value) || length(value) == 0L) return(list())
  valid <- vapply(value, .stt_native_is_segment, logical(1))
  if (!all(valid)) return(list())
  Filter(Negate(is.null), lapply(value, .stt_native_normalize_segment))
}

#' @keywords internal
#' @noRd
.stt_native_segments <- function(payload) {
  if (!is.list(payload)) return(list())
  nested <- function(value, name) {
    if (is.list(value)) value[[name]] else NULL
  }
  candidates <- list(
    nested(payload, "segments"),
    if (is.list(payload$transcription)) payload$transcription else NULL,
    nested(payload$result, "segments"),
    nested(payload$output, "segments"),
    nested(payload$data, "segments"),
    payload$result,
    payload$output,
    payload$data,
    payload
  )
  for (candidate in candidates) {
    segments <- .stt_native_as_segments(candidate)
    if (length(segments)) return(segments)
  }
  list()
}

#' @keywords internal
#' @noRd
.stt_native_direct_text <- function(payload) {
  if (is.character(payload)) return(.stt_native_scalar_text(payload))
  if (!is.list(payload)) return("")
  character_candidates <- list(
    payload$text,
    payload$transcript,
    if (is.character(payload$transcription)) payload$transcription else NULL,
    if (is.list(payload$result)) payload$result$text else NULL,
    if (is.list(payload$result)) payload$result$transcript else NULL,
    if (is.list(payload$output)) payload$output$text else NULL,
    if (is.list(payload$data)) payload$data$text else NULL
  )
  for (candidate in character_candidates) {
    text <- .stt_native_scalar_text(candidate)
    if (nzchar(text)) return(text)
  }
  ""
}

#' @keywords internal
#' @noRd
.stt_normalize_native_payload <- function(payload) {
  segments <- .stt_native_segments(payload)
  text <- .stt_native_direct_text(payload)
  if (!nzchar(text) && length(segments)) {
    segment_text <- vapply(
      segments,
      function(segment) .stt_native_scalar_text(segment$text),
      character(1)
    )
    text <- trimws(paste(segment_text[nzchar(segment_text)], collapse = " "))
  }
  if (!nzchar(text)) {
    stop(
      "Native STT engine returned JSON without a transcript.",
      call. = FALSE
    )
  }

  metadata <- if (is.list(payload) &&
      !is.null(names(payload)) &&
      any(nzchar(names(payload)))) {
    payload[setdiff(
      names(payload),
      c("text", "transcript", "transcription", "segments")
    )]
  } else {
    list()
  }
  if (length(segments)) metadata$segments <- segments
  list(text = text, metadata = metadata)
}

#' @keywords internal
#' @noRd
.stt_normalize_moss_cpp_payload <- function(payload) {
  .stt_normalize_native_payload(payload)
}

#' @keywords internal
#' @noRd
.stt_moss_cpp_payload_error <- function(payload) {
  if (!is.list(payload)) return("")
  candidates <- list(
    payload$error,
    payload$message,
    if (is.list(payload$error)) payload$error$message else NULL,
    if (is.list(payload$result)) payload$result$error else NULL,
    if (is.list(payload$result)) payload$result$message else NULL
  )
  for (candidate in candidates) {
    text <- .stt_native_scalar_text(candidate)
    if (nzchar(text)) return(text)
  }
  ""
}

#' Call a local OpenAI-compatible transcription server
#'
#' @keywords internal
#' @noRd
.stt_local_openai <- function(audio_path,
                              model,
                              language,
                              prompt,
                              timeout_secs,
                              base_url = NULL,
                              api_key = NULL,
                              response_format = "json",
                              max_new_tokens = NULL,
                              request = .stt_local_openai_request) {
  if (!file.exists(audio_path)) {
    stop("Local OpenAI-compatible STT requires a local audio file.", call. = FALSE)
  }

  endpoint <- .stt_local_transcriptions_url(base_url)
  token <- as.character(api_key %||% Sys.getenv("GENFLOW_STT_API_KEY", unset = ""))[1]
  format_id <- tolower(trimws(as.character(response_format %||% "json")[1]))
  if (is.na(format_id) || !nzchar(format_id)) format_id <- "json"

  body <- list(
    file = httr::upload_file(audio_path),
    model = model %||% .stt_default_model("local-openai"),
    response_format = format_id
  )
  if (!is.null(language)) body$language <- language
  if (!is.null(prompt)) body$prompt <- prompt
  if (!is.null(max_new_tokens)) {
    max_new_tokens <- suppressWarnings(as.numeric(max_new_tokens)[1])
    if (is.na(max_new_tokens) || !is.finite(max_new_tokens) ||
        max_new_tokens < 1 || max_new_tokens != as.integer(max_new_tokens)) {
      stop("`max_new_tokens` must be NULL or a positive integer.", call. = FALSE)
    }
    body$max_new_tokens <- as.integer(max_new_tokens)
  }

  headers <- character()
  if (!is.na(token) && nzchar(token)) {
    headers <- c(Authorization = paste("Bearer", token))
  }
  response <- request(
    endpoint = endpoint,
    headers = headers,
    body = body,
    timeout_secs = timeout_secs
  )
  status <- as.integer(response$status)
  response_text <- as.character(response$text %||% "")[1]
  if (status < 200L || status >= 300L) {
    stop(
      "Local OpenAI-compatible STT error (HTTP ", status, "): ",
      response_text,
      call. = FALSE
    )
  }

  payload <- tryCatch(
    jsonlite::fromJSON(response_text, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(payload)) {
    if (identical(format_id, "text") && nzchar(response_text)) {
      return(list(
        text = response_text,
        metadata = list(backend = "openai-compatible", endpoint = endpoint)
      ))
    }
    stop(
      "Local OpenAI-compatible STT returned invalid JSON: ",
      substr(response_text, 1L, 1000L),
      call. = FALSE
    )
  }

  text <- if (is.character(payload)) payload[[1]] else
    payload$text %||% payload$transcription
  if (is.null(text) || !is.character(text) || !nzchar(text[[1]])) {
    stop("Local OpenAI-compatible STT returned an empty transcript.", call. = FALSE)
  }

  metadata <- if (is.list(payload)) {
    payload[setdiff(names(payload), c("text", "transcription"))]
  } else {
    list()
  }
  metadata <- c(
    list(backend = "openai-compatible", endpoint = endpoint),
    metadata
  )
  list(text = text[[1]], metadata = metadata)
}

#' @keywords internal
#' @noRd
.stt_local_openai_request <- function(endpoint, headers, body, timeout_secs) {
  response <- httr::POST(
    url = endpoint,
    httr::add_headers(.headers = headers),
    body = body,
    encode = "multipart",
    httr::timeout(timeout_secs)
  )
  list(
    status = httr::status_code(response),
    text = httr::content(response, as = "text", encoding = "UTF-8")
  )
}

#' @keywords internal
#' @noRd
.stt_resolve_local_profile <- function(profile, model) {
  profile_id <- tolower(trimws(as.character(profile %||% "auto")[1]))
  aliases <- c(
    "generic" = "transformers",
    "asr" = "transformers",
    "transformers-asr" = "transformers",
    "moss-transcribe-diarize" = "moss",
    "moss_transcribe_diarize" = "moss"
  )
  mapped <- unname(aliases[profile_id])
  if (length(mapped) == 1L && !is.na(mapped)) profile_id <- mapped
  if (is.na(profile_id) || !nzchar(profile_id)) {
    stop(
      "`local_profile` must be one of \"auto\", \"transformers\", or \"moss\".",
      call. = FALSE
    )
  }
  if (!profile_id %in% c("auto", "transformers", "moss")) {
    stop(
      "`local_profile` must be one of \"auto\", \"transformers\", or \"moss\".",
      call. = FALSE
    )
  }
  if (!identical(profile_id, "auto")) return(profile_id)

  model_id <- tolower(gsub("\\\\", "/", as.character(model)[1]))
  if (grepl("(^|/)moss-transcribe-diarize/?$", model_id)) "moss" else "transformers"
}

#' @keywords internal
#' @noRd
.stt_validate_device <- function(device) {
  device_id <- tolower(trimws(as.character(device %||% "auto")[1]))
  if (is.na(device_id) || !nzchar(device_id)) {
    stop("`device` must be a non-empty device identifier.", call. = FALSE)
  }
  if (device_id %in% c("rocm", "hip")) return(device_id)
  if (!grepl("^(auto|cpu|mps|cuda(:[0-9]+)?)$", device_id)) {
    stop(
      "`device` must be \"auto\", \"cpu\", \"cuda\", \"cuda:N\", ",
      "\"rocm\", \"hip\", or \"mps\".",
      call. = FALSE
    )
  }
  device_id
}

#' @keywords internal
#' @noRd
.stt_validate_dtype <- function(dtype) {
  dtype_id <- tolower(trimws(as.character(dtype %||% "auto")[1]))
  aliases <- c(
    "fp32" = "float32",
    "fp16" = "float16",
    "bf16" = "bfloat16"
  )
  mapped <- unname(aliases[dtype_id])
  if (length(mapped) == 1L && !is.na(mapped)) dtype_id <- mapped
  if (is.na(dtype_id) || !nzchar(dtype_id)) {
    stop(
      "`dtype` must be \"auto\", \"float32\", \"float16\", or \"bfloat16\".",
      call. = FALSE
    )
  }
  if (!dtype_id %in% c("auto", "float32", "float16", "bfloat16")) {
    stop(
      "`dtype` must be \"auto\", \"float32\", \"float16\", or \"bfloat16\".",
      call. = FALSE
    )
  }
  dtype_id
}

#' @keywords internal
#' @noRd
.stt_normalize_timestamps <- function(return_timestamps) {
  if (is.logical(return_timestamps) && length(return_timestamps) == 1L &&
      !is.na(return_timestamps)) {
    return(if (isTRUE(return_timestamps)) "true" else "none")
  }

  value <- tolower(trimws(as.character(return_timestamps %||% "none")[1]))
  aliases <- c("false" = "none", "no" = "none", "yes" = "true")
  mapped <- unname(aliases[value])
  if (length(mapped) == 1L && !is.na(mapped)) value <- mapped
  if (is.na(value) || !nzchar(value)) {
    stop("`return_timestamps` must be TRUE, FALSE, or \"word\".", call. = FALSE)
  }
  if (!value %in% c("none", "true", "word")) {
    stop("`return_timestamps` must be TRUE, FALSE, or \"word\".", call. = FALSE)
  }
  value
}

#' @keywords internal
#' @noRd
.stt_argument_or_local_setting <- function(value,
                                           field,
                                           env,
                                           default,
                                           config,
                                           allow_empty = FALSE) {
  if (!is.null(value) && length(value) > 0L) {
    candidate <- as.character(value)[1]
    if (!is.na(candidate)) {
      candidate <- trimws(candidate)
      if (nzchar(candidate) || isTRUE(allow_empty)) return(candidate)
    }
  }
  .genflow_local_setting(
    field = field,
    env = env,
    default = default,
    config = config
  )
}

#' @keywords internal
#' @noRd
.stt_resolve_python <- function(python = NULL, config = NULL) {
  candidate <- .stt_argument_or_local_setting(
    python,
    field = "python",
    env = "GENFLOW_PYTHON",
    default = "",
    config = config %||% .genflow_read_local_config(),
    allow_empty = TRUE
  )
  if (!is.na(candidate) && nzchar(trimws(candidate))) {
    candidate <- trimws(candidate)
    resolved <- .genflow_resolve_executable(candidate)
    if (nzchar(resolved)) {
      return(resolved)
    }
    stop(
      "Python executable not found: ", candidate,
      ". Set `python` or GENFLOW_PYTHON to a valid environment.",
      call. = FALSE
    )
  }

  discovered <- .genflow_resolve_executable("", c("python3", "python"))
  if (!nzchar(discovered)) {
    stop(
      "Python was not found. Install Python and set GENFLOW_PYTHON (or pass ",
      "`python`) to use local Hugging Face STT.",
      call. = FALSE
    )
  }
  discovered
}

#' @keywords internal
#' @noRd
.stt_bridge_script <- function() {
  override <- getOption("genflow.stt_bridge_script", NULL)
  candidates <- c(
    as.character(override %||% character()),
    system.file("python", "genflow_stt.py", package = "genflow")
  )
  candidates <- unique(candidates[nzchar(candidates)])
  existing <- candidates[file.exists(candidates)]
  if (length(existing) == 0L) {
    stop(
      "The genflow local STT Python bridge was not found. Reinstall genflow ",
      "from a complete package build.",
      call. = FALSE
    )
  }
  normalizePath(existing[[1]], winslash = "/", mustWork = TRUE)
}

#' @keywords internal
#' @noRd
.stt_run_process <- function(command,
                             args,
                             timeout_secs,
                             environment = character()) {
  environment <- environment %||% character()
  environment <- stats::setNames(as.character(environment), names(environment))
  if (length(environment)) {
    if (is.null(names(environment)) || any(!nzchar(names(environment)))) {
      stop("Process environment overrides must be named.", call. = FALSE)
    }
    environment <- paste0(
      names(environment),
      "=",
      vapply(unname(environment), shQuote, character(1))
    )
  }
  output <- suppressWarnings(system2(
    command = command,
    args = vapply(args, shQuote, character(1)),
    stdout = TRUE,
    stderr = TRUE,
    env = environment,
    timeout = timeout_secs
  ))
  list(
    status = as.integer(attr(output, "status") %||% 0L),
    output = as.character(output)
  )
}

#' @keywords internal
#' @noRd
.stt_process_detail <- function(output, max_chars = 4000L) {
  text <- trimws(paste(as.character(output %||% character()), collapse = "\n"))
  if (!nzchar(text)) return("")
  substr(text, 1L, max_chars)
}

#' @keywords internal
#' @noRd
.stt_local_transcriptions_url <- function(base_url = NULL) {
  value <- .stt_argument_or_local_setting(
    base_url,
    field = "stt_server_base_url",
    env = "GENFLOW_STT_BASE_URL",
    default = "http://127.0.0.1:8000",
    config = .genflow_read_local_config()
  )
  value <- sub("/+$", "", trimws(value))
  if (is.na(value) || !nzchar(value) || !grepl("^https?://", value)) {
    stop("`base_url` must be an http(s) URL.", call. = FALSE)
  }
  if (grepl("/v1/audio/transcriptions$", value)) return(value)
  if (grepl("/v1$", value)) return(paste0(value, "/audio/transcriptions"))
  paste0(value, "/v1/audio/transcriptions")
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
