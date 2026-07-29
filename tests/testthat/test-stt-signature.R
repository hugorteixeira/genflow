test_that("STT signatures validate their forwarding list", {
  expect_error(
    gen_stt_signature(stt_args = c(prompt = "context")),
    "`stt_args` must be a list"
  )
  expect_error(
    gen_stt_signature(stt_args = list("context")),
    "fully named"
  )
  duplicated <- structure(
    list("first", "second"),
    names = c("prompt", "prompt")
  )
  expect_error(
    gen_stt_signature(stt_args = duplicated),
    "must be unique"
  )
  for (owned in c("audio", "service", "model", "language")) {
    args <- stats::setNames(list("owned"), owned)
    expect_error(
      gen_stt_signature(stt_args = args),
      "must not contain arguments owned"
    )
  }
})

test_that("STT signatures normalize semantic defaults and argument order", {
  implicit <- gen_stt_signature(
    service = "openai",
    model = "whisper-1"
  )
  explicit <- gen_stt_signature(
    service = "openai",
    model = "whisper-1",
    stt_args = list(
      timestamps = FALSE,
      chunk_overlap_seconds = 8,
      diarize = TRUE,
      chunking = "auto",
      chunk_bitrate_kbps = 48,
      convert = TRUE,
      diarize_speakers = FALSE,
      diarize_embedder = TRUE
    )
  )
  reordered <- gen_stt_signature(
    service = "openai",
    model = "whisper-1",
    stt_args = list(custom_beta = 2, custom_alpha = 1)
  )
  reordered_again <- gen_stt_signature(
    service = "openai",
    model = "whisper-1",
    stt_args = list(custom_alpha = 1, custom_beta = 2)
  )

  expect_match(implicit, "^[0-9a-f]{32}$")
  expect_identical(implicit, explicit)
  expect_identical(reordered, reordered_again)
  expect_false(identical(
    implicit,
    gen_stt_signature(
      service = "openai",
      model = "whisper-1",
      stt_args = list(prompt = "domain terminology")
    )
  ))
})

test_that("STT signatures preserve runtime-significant scalar whitespace", {
  expect_false(identical(
    gen_stt_signature(
      service = "openai",
      model = "whisper-1",
      language = "en",
      stt_args = list(prompt = "context")
    ),
    gen_stt_signature(
      service = "openai",
      model = " whisper-1 ",
      language = " en ",
      stt_args = list(prompt = " context ")
    )
  ))
  expect_identical(
    gen_stt_signature(service = "openai", model = ""),
    gen_stt_signature(service = "openai", model = NULL)
  )
})

test_that("chunk controls use the regular STT validation", {
  expect_error(
    gen_stt_signature(
      stt_args = list(
        chunk_segment_seconds = 8,
        chunk_overlap_seconds = 8
      )
    ),
    "must be smaller"
  )
  expect_error(
    gen_stt_signature(stt_args = list(chunk_max_retries = 1.5)),
    "whole number"
  )
  expect_error(
    gen_stt_signature(stt_args = list(resume = NA)),
    "must be TRUE or FALSE"
  )
  expect_false(identical(
    gen_stt_signature(stt_args = list(chunk_overlap_seconds = 8)),
    gen_stt_signature(stt_args = list(chunk_overlap_seconds = 4))
  ))
  expect_false(identical(
    gen_stt_signature(stt_args = list(chunk_format = "wav")),
    gen_stt_signature(stt_args = list(chunk_format = "mp3"))
  ))
  expect_identical(
    gen_stt_signature(
      service = "local-native",
      model = "mock.gguf",
      stt_args = list(
        native_engine = "crispasr",
        chunk_format = "auto"
      )
    ),
    gen_stt_signature(
      service = "local-native",
      model = "mock.gguf",
      stt_args = list(
        native_engine = "crispasr",
        chunk_format = "wav"
      )
    )
  )
})

test_that("credentials and operational controls never change the signature", {
  first <- gen_stt_signature(
    service = "local-openai",
    model = "local-whisper",
    stt_args = list(
      base_url = "http://127.0.0.1:8090",
      api_key = "first-secret",
      timeout_api = 30,
      timeout_per_audio_minute = 10,
      checkpoint_dir = tempfile("signature-checkpoint-a-"),
      checkpoint_retention = "all",
      resume = FALSE,
      chunk_retry_forever = FALSE,
      chunk_max_retries = 2,
      chunk_retry_wait_seconds = 0,
      output = "full",
      headers = list(Authorization = "Bearer first-secret")
    )
  )
  second <- gen_stt_signature(
    service = "local-openai",
    model = "local-whisper",
    stt_args = list(
      base_url = "http://127.0.0.1:8090",
      api_key = "second-secret",
      timeout_api = 900,
      timeout_per_audio_minute = 120,
      checkpoint_dir = tempfile("signature-checkpoint-b-"),
      checkpoint_retention = "results",
      resume = TRUE,
      chunk_retry_forever = TRUE,
      chunk_max_retries = 99,
      chunk_retry_wait_seconds = 20,
      output = "transcript",
      headers = list(Authorization = "Bearer second-secret")
    )
  )
  expect_identical(first, second)
})

test_that("Moss native signatures reject MP3 chunk media", {
  expect_error(
    gen_stt_signature(
      service = "local-native",
      model = "moss-transcribe-diarize-0.9b-q8_0.gguf",
      stt_args = list(
        native_engine = "moss-transcribe",
        chunk_format = "mp3"
      )
    ),
    "requires WAV"
  )
})

test_that("the effective local endpoint changes the STT signature", {
  first <- gen_stt_signature(
    service = "local-openai",
    model = "local-whisper",
    stt_args = list(base_url = "http://127.0.0.1:8090")
  )
  second <- gen_stt_signature(
    service = "local-openai",
    model = "local-whisper",
    stt_args = list(base_url = "http://127.0.0.1:8091")
  )
  expect_false(identical(first, second))
})

test_that("resolved native model and executable artifacts affect signatures", {
  directory <- tempfile("genflow-signature-native-")
  dir.create(directory, recursive = TRUE)
  model <- file.path(directory, "whisper-test.gguf")
  executable <- file.path(directory, "crispasr")
  writeBin(as.raw(c(1, 2, 3)), model)
  writeBin(as.raw(c(4, 5, 6)), executable)
  Sys.chmod(executable, mode = "0755")
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)

  signature <- function() {
    gen_stt_signature(
      service = "local-native",
      model = model,
      language = "en",
      stt_args = list(
        executable = executable,
        native_engine = "crispasr",
        native_backend = "whisper",
        native_device = "cpu"
      )
    )
  }
  original <- signature()

  writeBin(c(readBin(model, "raw", n = 100), as.raw(9)), model)
  model_changed <- signature()
  expect_false(identical(original, model_changed))

  writeBin(c(readBin(executable, "raw", n = 100), as.raw(8)), executable)
  executable_changed <- signature()
  expect_false(identical(model_changed, executable_changed))
})
