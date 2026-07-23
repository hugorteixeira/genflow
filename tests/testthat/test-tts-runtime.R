tts_test_envvar <- function(values) {
  names_values <- names(values)
  old <- setNames(
    Sys.getenv(names_values, unset = NA_character_),
    names_values
  )
  do.call(Sys.setenv, as.list(values))
  function() {
    missing <- names_values[is.na(old)]
    if (length(missing)) Sys.unsetenv(missing)
    present_names <- names_values[!is.na(old)]
    if (length(present_names)) {
      present <- as.list(unname(old[present_names]))
      names(present) <- present_names
      do.call(Sys.setenv, present)
    }
  }
}

tts_wav_bytes <- function() {
  c(
    charToRaw("RIFF"),
    as.raw(rep(0, 4)),
    charToRaw("WAVE"),
    charToRaw("fmt "),
    as.raw(rep(0, 32))
  )
}

tts_mp3_bytes <- function() {
  c(charToRaw("ID3"), as.raw(rep(0, 32)))
}

tts_write_disk_path <- function(arguments) {
  for (argument in arguments) {
    if (inherits(argument, "request") &&
        inherits(argument$output, "write_disk")) {
      return(argument$output$path)
    }
  }
  stop("Mock request did not receive httr::write_disk().")
}

test_that("OpenAI TTS normalizes HTTP errors and cleans provider temporaries", {
  restore_env <- tts_test_envvar(c(OPENAI_API_KEY = "test-openai-key"))
  on.exit(restore_env(), add = TRUE)
  temporary_path <- NULL

  testthat::local_mocked_bindings(
    POST = function(url, ...) {
      temporary_path <<- tts_write_disk_path(list(...))
      writeBin(charToRaw('{"error":"rate limited"}'), temporary_path)
      list(status = 429L, headers = list("content-type" = "application/json"))
    },
    status_code = function(response) response$status,
    headers = function(response) response$headers,
    content = function(response, ...) "rate limited",
    .package = "httr"
  )

  expect_error(
    genflow:::.tts_openai(
      text = "hello",
      model = "gpt-4o-mini-tts",
      voice = "alloy",
      format = "mp3",
      speed = 1,
      instructions = NULL,
      timeout_secs = 30
    ),
    "OpenAI TTS request failed (HTTP 429): rate limited.",
    fixed = TRUE
  )
  expect_false(is.null(temporary_path))
  expect_false(file.exists(temporary_path))
})

test_that("OpenAI TTS validates response bytes and records their real format", {
  restore_env <- tts_test_envvar(c(OPENAI_API_KEY = "test-openai-key"))
  on.exit(restore_env(), add = TRUE)

  testthat::local_mocked_bindings(
    POST = function(url, ...) {
      path <- tts_write_disk_path(list(...))
      writeBin(tts_wav_bytes(), path)
      list(status = 200L, headers = list("Content-Type" = "audio/wav"))
    },
    status_code = function(response) response$status,
    headers = function(response) response$headers,
    .package = "httr"
  )

  path <- genflow:::.tts_openai(
    text = "hello",
    model = "gpt-4o-mini-tts",
    voice = "alloy",
    format = "mp3",
    speed = 1,
    instructions = NULL,
    timeout_secs = 30
  )
  on.exit(unlink(as.character(path)), add = TRUE)

  expect_true(file.exists(path))
  expect_identical(attr(path, "tts_format"), "wav")
  expect_identical(attr(path, "tts_content_type"), "audio/wav")
})

test_that("gen_tts reuses Replicate metadata and removes its source temporary", {
  restore_env <- tts_test_envvar(c(REPLICATE_API_TOKEN = "test-replicate-token"))
  on.exit(restore_env(), add = TRUE)
  output_dir <- tempfile("genflow-tts-output-")
  dir.create(output_dir)
  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)

  schema_calls <- 0L
  source_path <- NULL
  model_info <- list(
    version = "version-1",
    properties = list(
      text = list(type = "string"),
      voice = list(type = "string", enum = c("Alpha", "Beta"))
    )
  )

  testthat::local_mocked_bindings(
    .tts_replicate_model_info = function(owner, name, token, timeout_secs) {
      schema_calls <<- schema_calls + 1L
      model_info
    },
    .tts_replicate_fetch_audio = function(ref, format, timeout_secs) {
      source_path <<- tempfile(fileext = ".mp3")
      writeBin(tts_mp3_bytes(), source_path)
      genflow:::.tts_tag_audio_path(
        source_path,
        content_type = "audio/mpeg",
        fallback_format = "mp3"
      )
    },
    .package = "genflow"
  )
  testthat::local_mocked_bindings(
    POST = function(...) {
      list(status = 201L)
    },
    status_code = function(response) response$status,
    content = function(response, as, ...) {
      if (identical(as, "parsed")) {
        list(
          id = "prediction-1",
          status = "succeeded",
          urls = list(get = "https://api.replicate.example/prediction-1"),
          output = "https://files.example/audio.mp3"
        )
      } else {
        ""
      }
    },
    .package = "httr"
  )

  result <- gen_tts(
    "hello",
    service = "replicate",
    model = "owner/model",
    directory = output_dir,
    preview = FALSE
  )

  expect_identical(schema_calls, 1L)
  expect_identical(result$status_api, "SUCCESS")
  expect_identical(result$voice, "Alpha")
  expect_identical(result$format, "mp3")
  expect_identical(result$requested_format, "mp3")
  expect_identical(result$content_type, "audio")
  expect_identical(result$mime_type, "audio/mpeg")
  expect_true(file.exists(result$saved_file))
  expect_false(file.exists(source_path))
})

test_that("saved extension follows audio bytes instead of the requested label", {
  restore_env <- tts_test_envvar(c(REPLICATE_API_TOKEN = "test-replicate-token"))
  on.exit(restore_env(), add = TRUE)
  output_dir <- tempfile("genflow-tts-format-")
  dir.create(output_dir)
  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)

  testthat::local_mocked_bindings(
    .tts_replicate_model_info = function(...) {
      list(version = "version-1", properties = list())
    },
    .tts_replicate = function(...) {
      path <- tempfile(fileext = ".wav")
      writeBin(tts_mp3_bytes(), path)
      path
    },
    .package = "genflow"
  )

  result <- gen_tts(
    "hello",
    service = "replicate",
    model = "owner/model",
    format = "wav",
    directory = output_dir
  )

  expect_identical(result$status_api, "SUCCESS")
  expect_identical(result$requested_format, "wav")
  expect_identical(result$format, "mp3")
  expect_identical(result$content_type, "audio")
  expect_identical(result$mime_type, "audio/mpeg")
  expect_match(result$saved_file, "\\.mp3$")
  expect_identical(readBin(result$saved_file, "raw", n = 3L), charToRaw("ID3"))
})

test_that("gen_tts cleans malformed provider output on structured errors", {
  output_dir <- tempfile("genflow-tts-error-")
  dir.create(output_dir)
  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)
  source_path <- NULL

  testthat::local_mocked_bindings(
    .tts_openai = function(...) {
      source_path <<- tempfile(fileext = ".mp3")
      writeBin(charToRaw('{"error":"not audio"}'), source_path)
      source_path
    },
    .package = "genflow"
  )

  result <- gen_tts(
    "hello",
    service = "openai",
    model = "gpt-4o-mini-tts",
    directory = output_dir
  )

  expect_identical(result$status_api, "ERROR")
  expect_match(result$status_msg, "textual payload instead of audio bytes")
  expect_false(file.exists(source_path))
  expect_length(list.files(output_dir), 0L)
})

test_that("audio metadata trusts signatures and rejects textual payloads", {
  disguised_wav <- tempfile(fileext = ".mp3")
  on.exit(unlink(disguised_wav), add = TRUE)
  writeBin(tts_wav_bytes(), disguised_wav)

  metadata <- genflow:::.tts_audio_metadata(
    disguised_wav,
    content_type = "audio/mpeg",
    fallback_format = "mp3"
  )
  expect_identical(metadata$format, "wav")
  expect_identical(metadata$content_type, "audio/wav")

  fake_audio <- tempfile(fileext = ".mp3")
  on.exit(unlink(fake_audio), add = TRUE)
  writeBin(charToRaw('{"error":"not audio"}'), fake_audio)
  expect_error(
    genflow:::.tts_audio_metadata(fake_audio, fallback_format = "mp3"),
    "textual payload instead of audio bytes"
  )
})

test_that("Replicate data URIs use byte signatures over a wrong MIME label", {
  skip_if_not_installed("base64enc")
  encoded <- base64enc::base64encode(tts_wav_bytes())
  path <- genflow:::.tts_replicate_fetch_audio(
    paste0("data:audio/mpeg;base64,", encoded),
    format = "wav",
    timeout_secs = 30
  )
  on.exit(unlink(as.character(path)), add = TRUE)

  expect_true(file.exists(path))
  expect_identical(attr(path, "tts_format"), "wav")
  expect_identical(attr(path, "tts_content_type"), "audio/wav")
})

test_that("Replicate download HTTP failures are normalized and cleaned", {
  temporary_path <- NULL
  testthat::local_mocked_bindings(
    GET = function(url, ...) {
      temporary_path <<- tts_write_disk_path(list(...))
      writeBin(charToRaw("bad gateway"), temporary_path)
      list(status = 502L, headers = list("content-type" = "text/plain"))
    },
    status_code = function(response) response$status,
    headers = function(response) response$headers,
    content = function(response, ...) "bad gateway",
    .package = "httr"
  )

  expect_error(
    genflow:::.tts_replicate_fetch_audio(
      "https://files.example/audio.mp3?token=secret",
      format = "mp3",
      timeout_secs = 30
    ),
    "Replicate audio download failed (HTTP 502): bad gateway.",
    fixed = TRUE
  )
  expect_false(is.null(temporary_path))
  expect_false(file.exists(temporary_path))
})
