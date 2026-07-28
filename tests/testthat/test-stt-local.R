local_stt_audio <- function() {
  path <- tempfile("genflow-stt-", fileext = ".wav")
  writeBin(as.raw(c(82, 73, 70, 70, rep(0, 40))), path)
  path
}

test_that("STT capabilities own provider-specific local input limits", {
  replicate <- gen_stt_capabilities("replicate")
  openai <- gen_stt_capabilities("openai")
  alias <- gen_stt_capabilities("openai-compatible")

  expect_identical(replicate$service, "replicate")
  expect_identical(replicate$max_local_file_bytes, 256L * 1024L)
  expect_identical(openai$service, "openai")
  expect_identical(openai$max_local_file_bytes, Inf)
  expect_identical(alias$service, "local-openai")
  expect_identical(alias$max_local_file_bytes, Inf)
  expect_error(gen_stt_capabilities(""), "non-empty provider identifier")
  expect_identical(
    formals(genflow:::.stt_replicate_prepare_input)$max_data_url_bytes,
    quote(.stt_max_local_file_bytes("replicate"))
  )
})

test_that("native token normalization removes only unusable time sentinels", {
  segment <- genflow:::.stt_native_normalize_segment(list(
    offsets = list(from = 0L, to = 1000L),
    text = "segment text",
    tokens = list(
      list(
        id = 1L,
        text = "",
        p = 1,
        t0 = -1,
        t1 = -1,
        t_dtw = -1,
        offsets = list(from = -10L, to = -10L)
      ),
      list(
        id = 2L,
        text = " word ",
        p = 0.9,
        t0 = -1,
        t1 = -1,
        offsets = list(from = -10L, to = -10L)
      ),
      list(
        id = 3L,
        text = "",
        p = 0.8,
        offsets = list(from = 100L, to = 200L)
      ),
      list(
        id = 4L,
        text = "",
        p = 0.7,
        start = 0,
        end = 0
      ),
      list(
        id = 5L,
        text = "",
        p = 0.6,
        t0 = 100L,
        t1 = 50L
      )
    )
  ))

  expect_identical(
    vapply(segment$tokens, `[[`, integer(1), "id"),
    c(2L, 3L, 4L)
  )
  expect_identical(segment$tokens[[1]]$text, "word")
  expect_null(segment$tokens[[1]]$t0)
  expect_null(segment$tokens[[1]]$t1)
  expect_null(segment$tokens[[1]]$offsets)
  expect_identical(
    segment$tokens[[2]]$offsets,
    list(from = 100, to = 200)
  )
  expect_identical(segment$tokens[[3]]$start, 0)
  expect_identical(segment$tokens[[3]]$end, 0)
})

test_that("CrispASR runtime device reports confirmations and fallbacks", {
  confirmed <- genflow:::.stt_crispasr_runtime_device(
    "crispasr_init_gpu_backend: using preferred GPU backend: Vulkan0",
    "vulkan"
  )
  expect_identical(confirmed$native_device_status, "confirmed")
  expect_identical(confirmed$native_device_active, "vulkan")
  expect_identical(confirmed$native_device_label, "Vulkan0")

  fallback <- genflow:::.stt_crispasr_runtime_device(
    paste(
      "crispasr_init_gpu_backend: WARNING:",
      "--gpu-backend 'vulkan' requested but no matching GPU device found,",
      "falling back to auto"
    ),
    "vulkan"
  )
  expect_identical(fallback$native_device_status, "fallback")
  expect_identical(fallback$native_device_active, "auto")

  cpu <- genflow:::.stt_crispasr_runtime_device(character(), "cpu")
  expect_identical(cpu$native_device_status, "confirmed")
  expect_identical(cpu$native_device_active, "cpu")
})

test_that("gen_stt accepts a NULL model and dispatches local OpenAI aliases", {
  audio <- local_stt_audio()
  on.exit(unlink(audio), add = TRUE)
  config_path <- tempfile(fileext = ".json")
  old_config_path <- getOption("genflow.local_config_path")
  options(genflow.local_config_path = config_path)
  on.exit(options(genflow.local_config_path = old_config_path), add = TRUE)

  seen <- NULL
  testthat::local_mocked_bindings(
    .stt_local_openai = function(...) {
      seen <<- list(...)
      list(
        text = "mock server transcript",
        metadata = list(backend = "openai-compatible")
      )
    },
    .package = "genflow"
  )

  console <- capture.output(
    result <- gen_stt(
      audio,
      service = list(service = "openai-compatible"),
      model = list(),
      save_txt = FALSE
    )
  )

  expect_null(attr(result, "class", exact = TRUE))
  expect_true(is.list(result))
  expect_identical(class(result), "list")
  expect_identical(result$status_api, "SUCCESS")
  expect_identical(result$response_value, "mock server transcript")
  expect_identical(result$service, "local-openai")
  expect_identical(result$model, "local-model")
  expect_null(seen$model)
  expect_identical(result$metadata$backend, "openai-compatible")
  expect_match(
    paste(console, collapse = "\n"),
    "[SUCCESS]",
    fixed = TRUE
  )
  expect_match(
    paste(console, collapse = "\n"),
    "local-openai | local-model | Time:",
    fixed = TRUE
  )
  expect_match(
    paste(console, collapse = "\n"),
    "-> Response: mock server transcript...",
    fixed = TRUE
  )
})

test_that("gen_stt validates public scalar controls before dispatch", {
  audio <- local_stt_audio()
  on.exit(unlink(audio), add = TRUE)
  dispatches <- 0L
  testthat::local_mocked_bindings(
    .stt_local_openai = function(timeout_secs, ...) {
      dispatches <<- dispatches + 1L
      list(text = "ok", metadata = list(timeout_secs = timeout_secs))
    },
    .package = "genflow"
  )

  logical_cases <- list(
    save_txt = list(save_txt = NA),
    save_txt_vector = list(save_txt = c(TRUE, FALSE)),
    save_txt_number = list(save_txt = 1),
    convert = list(convert = NA),
    convert_vector = list(convert = c(TRUE, FALSE)),
    diarize = list(diarize = NA),
    diarize_speakers = list(diarize_speakers = NA),
    diarize_speakers_number = list(diarize_speakers = 1),
    diarize_embedder = list(diarize_embedder = NA),
    diarize_embedder_vector = list(diarize_embedder = c(TRUE, FALSE)),
    diarize_embedder_number = list(diarize_embedder = 1),
    timestamps = list(timestamps = 1)
  )
  for (case in logical_cases) {
    expect_error(
      do.call(
        gen_stt,
        c(
          list(audio = audio, service = "local-openai"),
          case
        )
      ),
      "must be TRUE or FALSE"
    )
  }
  expect_error(
    gen_stt(
      audio,
      service = "local-native",
      save_txt = FALSE,
      diarize = FALSE,
      diarize_speakers = TRUE
    ),
    "requires `diarize = TRUE`"
  )
  expect_error(
    gen_stt(
      audio,
      service = "local-openai",
      save_txt = FALSE,
      diarize_speakers = TRUE
    ),
    "available only"
  )

  numeric_cases <- list(
    list(timeout_api = 0),
    list(timeout_api = Inf),
    list(timeout_api = c(1, 2)),
    list(timeout_api = TRUE),
    list(poll_interval = -1),
    list(poll_interval = NA_real_),
    list(max_poll_seconds = "not-a-number"),
    list(max_poll_seconds = c(10, 20))
  )
  for (case in numeric_cases) {
    expect_error(
      do.call(
        gen_stt,
        c(
          list(audio = audio, service = "local-openai", save_txt = FALSE),
          case
        )
      ),
      "positive finite number"
    )
  }
  for (case in list(
    list(timeout_per_audio_minute = -1),
    list(timeout_per_audio_minute = Inf)
  )) {
    expect_error(
      do.call(
        gen_stt,
        c(
          list(audio = audio, service = "local-openai", save_txt = FALSE),
          case
        )
      ),
      "non-negative finite number"
    )
  }
  expect_identical(dispatches, 0L)

  result <- gen_stt(
    audio,
    service = "local-openai",
    save_txt = FALSE,
    convert = FALSE,
    timeout_api = "30",
    poll_interval = "0.25",
    max_poll_seconds = "60"
  )
  expect_identical(result$status_api, "SUCCESS")
  expect_identical(result$metadata$timeout_secs, 30)
  expect_identical(dispatches, 1L)
})

test_that("gen_stt scales timeout for each input file duration", {
  audio <- local_stt_audio()
  on.exit(unlink(audio), add = TRUE)
  timeout_seen <- NA_real_
  testthat::local_mocked_bindings(
    .stt_audio_duration_seconds = function(path) 3600,
    .stt_local_openai = function(timeout_secs, ...) {
      timeout_seen <<- timeout_secs
      list(text = "ok", metadata = list())
    },
    .package = "genflow"
  )

  capture.output(result <- gen_stt(
    audio,
    service = "local-openai",
    save_txt = FALSE,
    timeout_api = 240,
    timeout_per_audio_minute = 60
  ))
  expect_identical(result$status_api, "SUCCESS")
  expect_identical(timeout_seen, 3840)
  expect_identical(
    genflow:::.stt_effective_timeout_seconds(
      base_seconds = 600,
      per_audio_minute = 0,
      duration_seconds = 3600
    ),
    600
  )
})

test_that("gen_stt probes duration once and forwards it to local native STT", {
  audio <- local_stt_audio()
  on.exit(unlink(audio), add = TRUE)
  duration_probes <- 0L
  duration_seen <- NA_real_

  testthat::local_mocked_bindings(
    .stt_audio_duration_seconds = function(path) {
      duration_probes <<- duration_probes + 1L
      321
    },
    .stt_local_native = function(audio_duration_seconds, ...) {
      duration_seen <<- audio_duration_seconds
      list(text = "ok", metadata = list())
    },
    .package = "genflow"
  )

  capture.output(result <- gen_stt(
    audio,
    service = "local-native",
    model = "moss-transcribe-diarize-0.9b-q8_0.gguf",
    save_txt = FALSE
  ))

  expect_identical(result$status_api, "SUCCESS")
  expect_identical(duration_probes, 1L)
  expect_identical(duration_seen, 321)
})

test_that("an unsupported STT service is a structured error when model is NULL", {
  audio <- local_stt_audio()
  on.exit(unlink(audio), add = TRUE)

  for (service in c("does-not-exist", "hf-local")) {
    capture.output(result <- gen_stt(
      audio,
      service = service,
      model = NULL,
      save_txt = FALSE
    ))

    expect_identical(result$status_api, "ERROR")
    expect_match(result$status_msg, "Unsupported STT service")
    expect_identical(result$model, "default")
  }
})

test_that("local OpenAI-compatible STT sends the standard multipart contract", {
  audio <- local_stt_audio()
  on.exit(unlink(audio), add = TRUE)
  seen <- NULL

  fake_request <- function(endpoint, headers, body, timeout_secs) {
    seen <<- list(
      endpoint = endpoint,
      headers = headers,
      body = body,
      timeout_secs = timeout_secs
    )
    list(
      status = 200L,
      text = paste0(
        '{"text":"server transcript","segments":',
        '[{"start":0,"end":1,"speaker":"S01","text":"server transcript"}]}'
      )
    )
  }

  result <- genflow:::.stt_local_openai(
    audio_path = audio,
    model = "OpenMOSS-Team/MOSS-Transcribe-Diarize",
    language = "pt",
    prompt = "Diarize.",
    timeout_secs = 45,
    base_url = "http://127.0.0.1:8000/v1",
    api_key = "local-secret",
    response_format = "verbose_json",
    max_new_tokens = 8192L,
    request = fake_request
  )

  expect_identical(result$text, "server transcript")
  expect_identical(
    seen$endpoint,
    "http://127.0.0.1:8000/v1/audio/transcriptions"
  )
  expect_identical(unname(seen$headers[["Authorization"]]), "Bearer local-secret")
  expect_identical(seen$body$response_format, "verbose_json")
  expect_identical(seen$body$max_new_tokens, 8192L)
  expect_identical(result$metadata$backend, "openai-compatible")
  expect_identical(result$metadata$segments[[1]]$speaker, "S01")
})

test_that("local STT aliases and endpoint validation are deterministic", {
  expect_identical(
    genflow:::.stt_normalize_service("openai-compatible"),
    "local-openai"
  )
  expect_identical(
    genflow:::.stt_local_transcriptions_url("http://localhost:9000"),
    "http://localhost:9000/v1/audio/transcriptions"
  )
  expect_error(genflow:::.stt_local_transcriptions_url("localhost:9000"), "http")
})

test_that("gen_stt canonicalizes native services and preserves MOSS aliases", {
  audio <- local_stt_audio()
  model <- tempfile("moss-model-", fileext = ".gguf")
  writeBin(as.raw(c(1, 2, 3)), model)
  on.exit(unlink(c(audio, model)), add = TRUE)
  seen <- NULL

  testthat::local_mocked_bindings(
    .stt_local_native = function(...) {
      seen <<- list(...)
      list(
        text = "native transcript",
        metadata = list(
          engine = "moss-transcribe",
          backend = "moss-diarize",
          model = model
        )
      )
    },
    .package = "genflow"
  )

  result <- gen_stt(
    audio,
    service = "moss_transcribe_cpp",
    model = model,
    executable = "/opt/moss/bin/moss-transcribe",
    native_device = "vulkan",
    max_new_tokens = 4096L,
    save_txt = FALSE
  )

  expect_identical(result$status_api, "SUCCESS")
  expect_identical(result$service, "local-native")
  expect_identical(result$response_value, "native transcript")
  expect_identical(seen$model, model)
  expect_identical(seen$executable, "/opt/moss/bin/moss-transcribe")
  expect_identical(seen$native_engine, "moss-transcribe")
  expect_identical(seen$native_device, "vulkan")
  expect_identical(seen$max_new_tokens, 4096L)
  expect_true(seen$legacy_service)
  expect_true(seen$convert)
  expect_error(
    gen_stt(
      audio,
      service = "moss-cpp",
      model = model,
      native_engine = "crispasr",
      save_txt = FALSE
    ),
    "compatibility alias"
  )
  expect_identical(
    genflow:::.stt_normalize_service("mosscpp"),
    "local-native"
  )
  expect_identical(
    genflow:::.stt_normalize_service("moss-cpp"),
    "local-native"
  )
  expect_identical(
    genflow:::.stt_normalize_service("native-stt"),
    "local-native"
  )
})

test_that("native engine registry and auto-selection are deterministic", {
  registry <- genflow:::.stt_native_engine_registry()
  expect_named(registry, c("crispasr", "moss-transcribe"))
  expect_identical(
    genflow:::.stt_normalize_native_engine("crisp-asr"),
    "crispasr"
  )
  expect_identical(
    genflow:::.stt_normalize_native_engine("moss-cpp"),
    "moss-transcribe"
  )
  expect_error(
    genflow:::.stt_normalize_native_engine("universal-magic"),
    "`native_engine`"
  )

  config <- genflow:::.genflow_local_config_defaults()
  expect_identical(
    genflow:::.stt_resolve_native_engine(
      model = "auto",
      native_backend = "parakeet",
      config = config
    ),
    "crispasr"
  )
  expect_identical(
    genflow:::.stt_resolve_native_engine(
      executable = "/opt/bin/moss-transcribe",
      config = config
    ),
    "moss-transcribe"
  )
  expect_identical(
    genflow:::.stt_resolve_native_engine(
      model = "downloaded-model-q8_0.gguf",
      config = config
    ),
    "crispasr"
  )
  config$stt_native_executable <- "/opt/bin/moss-transcribe"
  expect_identical(
    genflow:::.stt_resolve_native_engine(
      model = "downloaded-model-q8_0.gguf",
      config = config
    ),
    "crispasr"
  )
})

test_that("native engine overrides do not inherit another engine executable", {
  audio <- local_stt_audio()
  model <- tempfile("moss-model-", fileext = ".gguf")
  writeBin(as.raw(c(1, 2, 3)), model)
  on.exit(unlink(c(audio, model)), add = TRUE)
  withr::local_envvar(c(
    GENFLOW_STT_NATIVE_ENGINE = NA,
    GENFLOW_STT_NATIVE_EXECUTABLE = NA,
    GENFLOW_MOSS_CPP_EXECUTABLE = NA,
    GENFLOW_MOSS_CPP_MODEL = NA,
    GENFLOW_MOSS_CPP_DEVICE = NA
  ))

  config <- genflow:::.genflow_local_config_defaults()
  config$stt_native_engine <- "crispasr"
  config$stt_native_executable <- "/saved/crispasr"
  seen <- character()
  testthat::local_mocked_bindings(
    .genflow_read_local_config = function(...) config,
    .stt_resolve_native_executable = function(engine,
                                              executable = NULL,
                                              config = NULL) {
      seen <<- c(seen, executable)
      file.path(R.home("bin"), "R")
    },
    .stt_local_moss_cpp = function(...) {
      list(
        text = "ok",
        metadata = list(
          engine = "moss-transcribe",
          backend = "moss-diarize",
          model = model
        )
      )
    },
    .package = "genflow"
  )

  genflow:::.stt_local_native(
    audio_path = audio,
    model = model,
    language = NULL,
    prompt = NULL,
    timeout_secs = 10,
    native_engine = "moss-transcribe"
  )
  expect_identical(seen[[1]], "")

  withr::local_envvar(GENFLOW_STT_NATIVE_ENGINE = "moss-transcribe")
  genflow:::.stt_local_native(
    audio_path = audio,
    model = model,
    language = NULL,
    prompt = NULL,
    timeout_secs = 10
  )
  expect_identical(seen[[2]], "")

  genflow:::.stt_local_native(
    audio_path = audio,
    model = model,
    language = NULL,
    prompt = NULL,
    timeout_secs = 10,
    executable = "/explicit/moss-transcribe",
    native_engine = "moss-transcribe"
  )
  expect_identical(seen[[3]], "/explicit/moss-transcribe")
})

test_that("a concrete model does not leak a stale backend into engine selection", {
  audio <- local_stt_audio()
  model <- tempfile("moss-model-", fileext = ".gguf")
  writeBin(as.raw(c(1, 2, 3)), model)
  on.exit(unlink(c(audio, model)), add = TRUE)
  withr::local_envvar(c(
    GENFLOW_STT_NATIVE_ENGINE = NA,
    GENFLOW_STT_NATIVE_EXECUTABLE = NA,
    GENFLOW_STT_NATIVE_BACKEND = NA,
    GENFLOW_MOSS_CPP_EXECUTABLE = NA,
    GENFLOW_MOSS_CPP_MODEL = NA,
    GENFLOW_MOSS_CPP_DEVICE = NA
  ))

  config <- genflow:::.genflow_local_config_defaults()
  config$stt_native_engine <- "auto"
  config$stt_native_executable <- "/saved/moss-transcribe"
  config$stt_native_backend <- "whisper"
  testthat::local_mocked_bindings(
    .genflow_read_local_config = function(...) config,
    .stt_resolve_native_executable = function(...) {
      file.path(R.home("bin"), "R")
    },
    .stt_local_moss_cpp = function(...) {
      list(
        text = "ok",
        metadata = list(
          engine = "moss-transcribe",
          backend = "moss-diarize",
          model = model
        )
      )
    },
    .package = "genflow"
  )

  result <- genflow:::.stt_local_native(
    audio_path = audio,
    model = model,
    language = NULL,
    prompt = NULL,
    timeout_secs = 10
  )

  expect_identical(result$metadata$engine, "moss-transcribe")
  expect_identical(result$metadata$backend, "moss-diarize")
})

test_that("a catalog model does not reuse an auto engine's stale executable", {
  audio <- local_stt_audio()
  on.exit(unlink(audio), add = TRUE)
  withr::local_envvar(c(
    GENFLOW_STT_NATIVE_ENGINE = NA,
    GENFLOW_STT_NATIVE_EXECUTABLE = NA,
    GENFLOW_STT_NATIVE_BACKEND = NA,
    GENFLOW_MOSS_CPP_EXECUTABLE = NA,
    GENFLOW_MOSS_CPP_MODEL = NA,
    GENFLOW_MOSS_CPP_DEVICE = NA
  ))

  config <- genflow:::.genflow_local_config_defaults()
  config$stt_native_engine <- "auto"
  config$stt_native_executable <- "/saved/moss-transcribe"
  seen <- NULL
  testthat::local_mocked_bindings(
    .genflow_read_local_config = function(...) config,
    .stt_resolve_native_executable = function(engine,
                                              executable = NULL,
                                              config = NULL) {
      seen <<- list(engine = engine, executable = executable)
      file.path(R.home("bin"), "R")
    },
    .stt_native_crispasr = function(...) {
      list(
        text = "ok",
        metadata = list(
          engine = "crispasr",
          backend = "granite",
          model = "managed-model.gguf"
        )
      )
    },
    .package = "genflow"
  )

  result <- genflow:::.stt_local_native(
    audio_path = audio,
    model = "managed-model.gguf",
    language = NULL,
    prompt = NULL,
    timeout_secs = 10
  )

  expect_identical(seen$engine, "crispasr")
  expect_identical(seen$executable, "")
  expect_identical(result$metadata$engine, "crispasr")
})

test_that("CrispASR normalizes JSON segments and routes Vulkan controls", {
  audio <- local_stt_audio()
  model <- tempfile("parakeet-model-", fileext = ".gguf")
  writeBin(as.raw(c(1, 2, 3)), model)
  on.exit(unlink(c(audio, model)), add = TRUE)
  seen <- NULL

  fake_runner <- function(command, args, timeout_secs, environment) {
    seen <<- list(
      command = command,
      args = args,
      timeout_secs = timeout_secs,
      environment = environment
    )
    output_base <- args[[match("-of", args) + 1L]]
    jsonlite::write_json(
      list(
        crispasr = list(
          backend = "parakeet",
          model = "parakeet-q4_k.gguf",
          language = "pt"
        ),
        transcription = list(
          list(
            timestamps = list(
              from = "00:00:00,240",
              to = "00:00:10,880"
            ),
            offsets = list(from = 240L, to = 10880L),
            text = "transcricao local",
            words = list(list(text = "transcricao")),
            tokens = list(list(
              id = 1L,
              text = "",
              p = 1,
              t0 = -1,
              t1 = -1,
              t_dtw = -1,
              offsets = list(from = -10L, to = -10L)
            ))
          )
        )
      ),
      paste0(output_base, ".json"),
      auto_unbox = TRUE
    )
    list(
      status = 0L,
      output = paste(
        "crispasr_init_gpu_backend:",
        "using preferred GPU backend: Vulkan0"
      )
    )
  }

  result <- genflow:::.stt_native_crispasr(
    audio_path = audio,
    model = model,
    language = "pt",
    prompt = NULL,
    timeout_secs = 60,
    executable = file.path(R.home("bin"), "R"),
    native_backend = "parakeet",
    native_device = "vulkan",
    max_new_tokens = 2048L,
    runner = fake_runner
  )

  expect_identical(result$text, "transcricao local")
  expect_identical(result$metadata$engine, "crispasr")
  expect_identical(result$metadata$backend, "parakeet")
  expect_identical(result$metadata$requested_backend, "parakeet")
  expect_identical(result$metadata$runtime_backend, "parakeet")
  expect_identical(result$metadata$native_device, "vulkan")
  expect_identical(result$metadata$native_device_status, "confirmed")
  expect_identical(result$metadata$native_device_active, "vulkan")
  expect_identical(result$metadata$segments[[1]]$start, 0.24)
  expect_identical(result$metadata$segments[[1]]$end, 10.88)
  expect_null(result$metadata$segments[[1]]$tokens)
  expect_identical(
    result$metadata$segments[[1]]$words,
    list(list(text = "transcricao"))
  )
  expect_identical(
    result$metadata$segments[[1]]$timestamps$from,
    "00:00:00,240"
  )
  expect_identical(seen$command, file.path(R.home("bin"), "R"))
  expect_true(all(c(
    "-m",
    normalizePath(model, winslash = "/", mustWork = TRUE),
    "--backend",
    "parakeet",
    "-f",
    normalizePath(audio, winslash = "/", mustWork = TRUE),
    "-ojf",
    "-np",
    "--gpu-backend",
    "vulkan",
    "-l",
    "pt",
    "--max-new-tokens",
    "2048"
  ) %in% seen$args))
  expect_length(seen$environment, 0L)
  expect_identical(seen$timeout_secs, 60)

  fallback_runner <- function(...) {
    process <- fake_runner(...)
    process$output <- paste(
      "crispasr_init_gpu_backend: WARNING:",
      "--gpu-backend 'vulkan' requested but no matching GPU device found,",
      "falling back to auto"
    )
    process
  }
  expect_warning(
    fallback <- genflow:::.stt_native_crispasr(
      audio_path = audio,
      model = model,
      language = "pt",
      prompt = NULL,
      timeout_secs = 60,
      executable = file.path(R.home("bin"), "R"),
      native_backend = "parakeet",
      native_device = "vulkan",
      max_new_tokens = 2048L,
      runner = fallback_runner
    ),
    "fell back to automatic backend selection",
    fixed = TRUE
  )
  expect_identical(fallback$metadata$native_device_status, "fallback")
  expect_identical(fallback$metadata$native_device_active, "auto")
})

test_that("CrispASR remote model syntax is explicit and auto-download is bounded", {
  audio <- local_stt_audio()
  on.exit(unlink(audio), add = TRUE)
  seen <- NULL
  fake_runner <- function(command, args, ...) {
    seen <<- args
    output_base <- args[[match("-of", args) + 1L]]
    jsonlite::write_json(
      list(
        crispasr = list(backend = "moss-diarize", model = "moss-q4.gguf"),
        transcription = list(list(
          offsets = list(from = 0L, to = 1000L),
          text = "ok"
        ))
      ),
      paste0(output_base, ".json"),
      auto_unbox = TRUE
    )
    list(status = 0L, output = character())
  }

  references <- c(
    "hf://cstr/moss-diarize-GGUF:moss-q4.gguf",
    "hf://cstr/moss-diarize-GGUF/moss-q4.gguf",
    paste0(
      "https://huggingface.co/cstr/moss-diarize-GGUF/",
      "blob/main/moss-q4.gguf"
    )
  )
  for (reference in references) {
    seen <- NULL
    result <- genflow:::.stt_native_crispasr(
      audio_path = audio,
      audio_duration_seconds = 1,
      model = reference,
      language = NULL,
      prompt = NULL,
      timeout_secs = 10,
      executable = file.path(R.home("bin"), "R"),
      native_backend = "moss-diarize",
      native_device = "cpu",
      runner = fake_runner
    )

    expect_identical(result$text, "ok")
    expect_identical(result$metadata$model_kind, "hf")
    expect_identical(
      result$metadata$model,
      "hf://cstr/moss-diarize-GGUF:moss-q4.gguf"
    )
    expect_true(all(c(
      "-m",
      "auto",
      "--hf-repo",
      "cstr/moss-diarize-GGUF:moss-q4.gguf",
      "--no-gpu"
    ) %in% seen))
  }
  expect_error(
    genflow:::.stt_native_crispasr(
      audio_path = audio,
      model = "auto",
      language = NULL,
      prompt = NULL,
      timeout_secs = 10,
      executable = file.path(R.home("bin"), "R"),
      native_backend = NULL,
      native_device = "auto",
      runner = fake_runner
    ),
    "requires `native_backend`"
  )
  expect_error(
    genflow:::.stt_native_crispasr(
      audio_path = audio,
      model = "hf://owner/repository",
      language = NULL,
      prompt = NULL,
      timeout_secs = 10,
      executable = file.path(R.home("bin"), "R"),
      native_backend = "whisper",
      native_device = "auto",
      runner = fake_runner
    ),
    "OWNER/REPO"
  )
  expect_error(
    genflow:::.stt_native_crispasr(
      audio_path = audio,
      model = "hf://owner/repository/nested/model.gguf",
      language = NULL,
      prompt = NULL,
      timeout_secs = 10,
      executable = file.path(R.home("bin"), "R"),
      native_backend = "whisper",
      native_device = "auto",
      runner = fake_runner
    ),
    "one model filename"
  )
  expect_error(
    genflow:::.stt_native_crispasr(
      audio_path = audio,
      model = "hf://owner/repository/model.gguf?download=true",
      language = NULL,
      prompt = NULL,
      timeout_secs = 10,
      executable = file.path(R.home("bin"), "R"),
      native_backend = "whisper",
      native_device = "auto",
      runner = fake_runner
    ),
    "one model filename"
  )
  expect_error(
    genflow:::.stt_native_crispasr(
      audio_path = audio,
      model = "auto",
      language = NULL,
      prompt = NULL,
      timeout_secs = 10,
      executable = file.path(R.home("bin"), "R"),
      native_backend = "parakeet",
      native_device = "hip",
      runner = fake_runner
    ),
    "use a Vulkan-enabled build"
  )
})

test_that("CrispASR accepts complete timeout output and rejects missing JSON", {
  audio <- local_stt_audio()
  model <- tempfile("whisper-model-", fileext = ".gguf")
  writeBin(as.raw(c(1, 2, 3)), model)
  on.exit(unlink(c(audio, model)), add = TRUE)

  complete <- genflow:::.stt_native_crispasr(
    audio_path = audio,
    model = model,
    language = NULL,
    prompt = NULL,
    timeout_secs = 10,
    executable = file.path(R.home("bin"), "R"),
    native_backend = "whisper",
    native_device = "auto",
    runner = function(command, args, ...) {
      output_base <- args[[match("-of", args) + 1L]]
      jsonlite::write_json(
        list(transcription = list(list(
          offsets = list(from = 0L, to = 1000L),
          text = "complete"
        ))),
        paste0(output_base, ".json"),
        auto_unbox = TRUE
      )
      list(status = 124L, output = "timeout boundary")
    }
  )
  expect_identical(complete$text, "complete")

  expect_error(
    genflow:::.stt_native_crispasr(
      audio_path = audio,
      model = model,
      language = NULL,
      prompt = NULL,
      timeout_secs = 10,
      executable = file.path(R.home("bin"), "R"),
      native_backend = "whisper",
      native_device = "auto",
      runner = function(...) list(status = 0L, output = "no file")
    ),
    "expected JSON result"
  )
  expect_error(
    genflow:::.stt_native_crispasr(
      audio_path = audio,
      model = model,
      language = NULL,
      prompt = NULL,
      timeout_secs = 10,
      executable = file.path(R.home("bin"), "R"),
      native_backend = "whisper",
      native_device = "auto",
      runner = function(...) list(status = 124L, output = "still working")
    ),
    "timed out after 10 seconds"
  )
})

test_that("local-native dispatcher returns canonical engine metadata", {
  audio <- local_stt_audio()
  model <- tempfile("native-model-", fileext = ".gguf")
  config_path <- tempfile(fileext = ".json")
  writeBin(as.raw(c(1, 2, 3)), model)
  on.exit(unlink(c(audio, model, config_path)), add = TRUE)
  old_config_path <- getOption("genflow.local_config_path")
  options(genflow.local_config_path = config_path)
  on.exit(options(genflow.local_config_path = old_config_path), add = TRUE)

  result <- genflow:::.stt_local_native(
    audio_path = audio,
    model = model,
    language = NULL,
    prompt = NULL,
    timeout_secs = 10,
    executable = file.path(R.home("bin"), "R"),
    native_engine = "crispasr",
    native_backend = "whisper",
    native_device = "cpu",
    convert = TRUE,
    runner = function(command, args, ...) {
      output_base <- args[[match("-of", args) + 1L]]
      jsonlite::write_json(
        list(
          crispasr = list(backend = "whisper", model = basename(model)),
          transcription = list(list(
            offsets = list(from = 0L, to = 1000L),
            text = "dispatcher ok"
          ))
        ),
        paste0(output_base, ".json"),
        auto_unbox = TRUE
      )
      list(status = 0L, output = character())
    }
  )

  expect_identical(result$text, "dispatcher ok")
  expect_identical(result$metadata$canonical_service, "local-native")
  expect_identical(result$metadata$transport, "process")
  expect_identical(result$metadata$engine, "crispasr")
  expect_identical(result$metadata$backend, "whisper")
  expect_identical(
    result$metadata$model,
    normalizePath(model, winslash = "/", mustWork = TRUE)
  )

  expect_error(
    genflow:::.stt_local_native(
      audio_path = audio,
      model = model,
      language = NULL,
      prompt = NULL,
      timeout_secs = 10,
      executable = file.path(R.home("bin"), "R"),
      native_engine = "moss-transcribe",
      native_backend = "parakeet",
      native_device = "cpu"
    ),
    "only supports the MOSS architecture"
  )
})

test_that("native MOSS C++ CLI preserves diarized segments and Vulkan routing", {
  audio <- local_stt_audio()
  model <- tempfile("moss-model-", fileext = ".gguf")
  writeBin(as.raw(c(1, 2, 3)), model)
  on.exit(unlink(c(audio, model)), add = TRUE)
  seen <- NULL

  fake_runner <- function(command, args, timeout_secs, environment) {
    seen <<- list(
      command = command,
      args = args,
      timeout_secs = timeout_secs,
      environment = environment
    )
    list(
      status = 0L,
      output = c(
        '{"level":"info","message":"initialized Vulkan"}',
        '{"segments":[',
        '{"start":0,"end":1.25,"speaker":"S01","text":"hello"},',
        '{"start":1.25,"end":2,"speaker":"S02","text":"world"}',
        "]}"
      )
    )
  }

  result <- genflow:::.stt_local_moss_cpp(
    audio_path = audio,
    model = model,
    language = NULL,
    prompt = NULL,
    timeout_secs = 60,
    executable = file.path(R.home("bin"), "R"),
    native_device = "vulkan",
    max_new_tokens = 4096L,
    runner = fake_runner
  )

  expect_identical(result$text, "hello world")
  expect_identical(result$metadata$engine, "moss-transcribe")
  expect_identical(result$metadata$backend, "moss-diarize")
  expect_identical(result$metadata$native_device, "vulkan")
  expect_identical(result$metadata$segments[[1]]$speaker, "S01")
  expect_identical(result$metadata$segments[[2]]$end, 2L)
  expect_identical(
    seen$args,
    c(
      "transcribe",
      normalizePath(model, winslash = "/", mustWork = TRUE),
      normalizePath(audio, winslash = "/", mustWork = TRUE),
      "--format",
      "json",
      "--max-new",
      "4096"
    )
  )
  expect_identical(unname(seen$environment[["MTD_DEVICE"]]), "vulkan")
  expect_identical(seen$timeout_secs, 60)
})

test_that("native MOSS C++ accepts top-level segment arrays and complete timeout output", {
  audio <- local_stt_audio()
  model <- tempfile("moss-model-", fileext = ".gguf")
  writeBin(as.raw(c(1, 2, 3)), model)
  on.exit(unlink(c(audio, model)), add = TRUE)

  result <- genflow:::.stt_local_moss_cpp(
    audio_path = audio,
    model = model,
    language = NULL,
    prompt = NULL,
    timeout_secs = 10,
    executable = file.path(R.home("bin"), "R"),
    native_device = "auto",
    runner = function(...) {
      list(
        status = 124L,
        output = paste0(
          '[{"start":0,"end":1,"speaker":"S01","text":"complete"},',
          '{"start":1,"end":2,"speaker":"S01","text":"result"}]'
        )
      )
    }
  )

  expect_identical(result$text, "complete result")
  expect_identical(result$metadata$native_device, "auto")
  expect_length(result$metadata$segments, 2L)
})

test_that("native MOSS C++ failures identify model binary JSON and timeout causes", {
  audio <- local_stt_audio()
  model <- tempfile("moss-model-", fileext = ".gguf")
  writeBin(as.raw(c(1, 2, 3)), model)
  on.exit(unlink(c(audio, model)), add = TRUE)
  executable <- file.path(R.home("bin"), "R")

  expect_error(
    genflow:::.stt_local_moss_cpp(
      audio_path = audio,
      model = tempfile("missing-model-", fileext = ".gguf"),
      language = NULL,
      prompt = NULL,
      timeout_secs = 10,
      executable = executable
    ),
    "model file not found"
  )
  expect_error(
    genflow:::.stt_local_moss_cpp(
      audio_path = audio,
      model = model,
      language = NULL,
      prompt = NULL,
      timeout_secs = 10,
      executable = tempfile("missing-moss-transcribe-")
    ),
    "executable not found"
  )
  expect_error(
    genflow:::.stt_local_moss_cpp(
      audio_path = audio,
      model = model,
      language = NULL,
      prompt = NULL,
      timeout_secs = 10,
      executable = executable,
      native_device = "directml",
      runner = function(...) stop("runner must not be called")
    ),
    "`native_device`"
  )
  expect_error(
    genflow:::.stt_local_moss_cpp(
      audio_path = audio,
      model = model,
      language = NULL,
      prompt = NULL,
      timeout_secs = 10,
      executable = executable,
      runner = function(...) {
        list(status = 0L, output = "native log without a JSON result")
      }
    ),
    "malformed or missing JSON"
  )
  expect_error(
    genflow:::.stt_local_moss_cpp(
      audio_path = audio,
      model = model,
      language = NULL,
      prompt = NULL,
      timeout_secs = 10,
      executable = executable,
      runner = function(...) {
        list(status = 124L, output = "still working")
      }
    ),
    "timed out after 10 seconds"
  )
})

test_that("native STT quant configuration round-trips and validates env overrides", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)

  config <- gen_local_config(
    stt_native_quant = " Q8_0 ",
    path = path
  )
  expect_identical(config$stt_native_quant, "q8_0")
  expect_identical(
    gen_local_config(path = path)$stt_native_quant,
    "q8_0"
  )

  withr::local_envvar(GENFLOW_STT_NATIVE_QUANT = "Q5_K_M")
  effective <- genflow:::.genflow_local_effective_config(config)
  expect_identical(effective$stt_native_quant, "q5_k_m")

  expect_error(
    gen_local_config(
      stt_native_quant = "../q8_0",
      path = path,
      save = FALSE
    ),
    "`native_quant`"
  )
})

test_that("gen_stt forwards the public native quant argument", {
  audio <- local_stt_audio()
  on.exit(unlink(audio), add = TRUE)
  seen <- NULL
  testthat::local_mocked_bindings(
    .stt_local_native = function(...) {
      seen <<- list(...)
      list(text = "quant plumbing", metadata = list(model = "auto"))
    },
    .package = "genflow"
  )

  capture.output(result <- gen_stt(
    audio,
    service = "local-native",
    model = "auto",
    native_quant = "q8_0",
    save_txt = FALSE
  ))

  expect_identical(result$status_api, "SUCCESS")
  expect_identical(seen$native_quant, "q8_0")
})

test_that("an explicit local-native auto model ignores a saved concrete model", {
  audio <- local_stt_audio()
  config_path <- tempfile(fileext = ".json")
  on.exit(unlink(c(audio, config_path)), add = TRUE)
  old_config_path <- getOption("genflow.local_config_path")
  options(genflow.local_config_path = config_path)
  on.exit(options(genflow.local_config_path = old_config_path), add = TRUE)
  withr::local_envvar(c(
    GENFLOW_STT_NATIVE_ENGINE = NA,
    GENFLOW_STT_NATIVE_EXECUTABLE = NA,
    GENFLOW_STT_NATIVE_MODEL = NA,
    GENFLOW_STT_NATIVE_BACKEND = NA,
    GENFLOW_STT_NATIVE_QUANT = NA,
    GENFLOW_STT_NATIVE_DEVICE = NA,
    GENFLOW_MOSS_CPP_EXECUTABLE = NA,
    GENFLOW_MOSS_CPP_MODEL = NA,
    GENFLOW_MOSS_CPP_DEVICE = NA
  ))

  gen_local_config(
    stt_native_engine = "crispasr",
    stt_native_executable = file.path(R.home("bin"), "R"),
    stt_native_model = "hf://owner/repository:saved-Q8_0.gguf",
    stt_native_backend = "granite-4.1",
    stt_native_quant = "q8_0",
    stt_native_device = "cpu",
    path = config_path
  )

  seen <- NULL
  result <- genflow:::.stt_local_native(
    audio_path = audio,
    model = "auto",
    language = NULL,
    prompt = NULL,
    timeout_secs = 10,
    runner = function(command, args, ...) {
      seen <<- args
      output_base <- args[[match("-of", args) + 1L]]
      jsonlite::write_json(
        list(
          crispasr = list(
            backend = "granite",
            model = "registry-Q8_0.gguf"
          ),
          transcription = list(list(
            offsets = list(from = 0L, to = 1000L),
            text = "registry model"
          ))
        ),
        paste0(output_base, ".json"),
        auto_unbox = TRUE
      )
      list(status = 0L, output = character())
    }
  )

  model_position <- match("-m", seen)
  quant_position <- match("--model-quant", seen)
  expect_identical(seen[[model_position + 1L]], "auto")
  expect_false("--hf-repo" %in% seen)
  expect_false(is.na(quant_position))
  expect_identical(seen[[quant_position + 1L]], "q8_0")
  expect_identical(result$text, "registry model")
  expect_identical(result$metadata$engine, "crispasr")
  expect_identical(result$metadata$model, "auto")
  expect_identical(result$metadata$requested_model, "auto")
  expect_identical(result$metadata$resolution_source, "registry")
  expect_identical(result$metadata$requested_quant, "q8_0")
})

test_that("a concrete local-native model ignores a saved legacy backend", {
  audio <- local_stt_audio()
  model <- tempfile("concrete-model-", fileext = ".gguf")
  config_path <- tempfile(fileext = ".json")
  writeBin(as.raw(c(1, 2, 3)), model)
  on.exit(unlink(c(audio, model, config_path)), add = TRUE)
  old_config_path <- getOption("genflow.local_config_path")
  options(genflow.local_config_path = config_path)
  on.exit(options(genflow.local_config_path = old_config_path), add = TRUE)
  withr::local_envvar(c(
    GENFLOW_STT_NATIVE_ENGINE = NA,
    GENFLOW_STT_NATIVE_EXECUTABLE = NA,
    GENFLOW_STT_NATIVE_MODEL = NA,
    GENFLOW_STT_NATIVE_BACKEND = NA,
    GENFLOW_STT_NATIVE_QUANT = NA,
    GENFLOW_STT_NATIVE_DEVICE = NA
  ))

  gen_local_config(
    stt_native_engine = "crispasr",
    stt_native_executable = file.path(R.home("bin"), "R"),
    stt_native_model = "auto",
    stt_native_backend = "whisper",
    stt_native_quant = "q8_0",
    stt_native_device = "cpu",
    path = config_path
  )

  seen <- NULL
  fake_runner <- function(command, args, ...) {
    seen <<- args
    output_base <- args[[match("-of", args) + 1L]]
    jsonlite::write_json(
      list(
        crispasr = list(
          backend = "granite",
          model = basename(model)
        ),
        transcription = list(list(
          offsets = list(from = 0L, to = 1000L),
          text = "concrete model"
        ))
      ),
      paste0(output_base, ".json"),
      auto_unbox = TRUE
    )
    list(status = 0L, output = character())
  }

  result <- genflow:::.stt_local_native(
    audio_path = audio,
    model = model,
    language = NULL,
    prompt = NULL,
    timeout_secs = 10,
    runner = fake_runner
  )

  expect_false("--backend" %in% seen)
  expect_false("--model-quant" %in% seen)
  expect_identical(result$text, "concrete model")
  expect_identical(result$metadata$backend, "granite")
  expect_null(result$metadata$requested_backend)
  expect_identical(
    result$metadata$model,
    normalizePath(model, winslash = "/", mustWork = TRUE)
  )
  expect_identical(result$metadata$requested_model, model)
  expect_identical(result$metadata$resolution_source, "argument")
})

test_that("CrispASR resolves a catalog basename only from its managed cache", {
  audio <- local_stt_audio()
  cache_dir <- tempfile("genflow-crispasr-cache-")
  working_dir <- tempfile("genflow-crispasr-working-")
  dir.create(cache_dir)
  dir.create(working_dir)
  filename <- "granite-speech-4.1-2b-q8_0.gguf"
  model <- file.path(cache_dir, filename)
  writeBin(as.raw(c(1, 2, 3)), model)
  writeBin(as.raw(c(9, 9, 9)), file.path(working_dir, filename))
  on.exit(
    unlink(c(audio, cache_dir, working_dir), recursive = TRUE),
    add = TRUE
  )
  withr::local_envvar(c(
    CRISPASR_CACHE_DIR = cache_dir,
    CRISPASR_MODELS_DIR = NA
  ))
  withr::local_dir(working_dir)

  seen <- NULL
  result <- genflow:::.stt_native_crispasr(
    audio_path = audio,
    model = filename,
    language = NULL,
    prompt = NULL,
    timeout_secs = 10,
    executable = file.path(R.home("bin"), "R"),
    native_device = "cpu",
    runner = function(command, args, ...) {
      seen <<- args
      output_base <- args[[match("-of", args) + 1L]]
      jsonlite::write_json(
        list(
          crispasr = list(
            backend = "granite",
            model = filename
          ),
          transcription = list(list(
            offsets = list(from = 0L, to = 1000L),
            text = "cached catalog model"
          ))
        ),
        paste0(output_base, ".json"),
        auto_unbox = TRUE
      )
      list(status = 0L, output = character())
    }
  )

  expected_model <- normalizePath(model, winslash = "/", mustWork = TRUE)
  model_position <- match("-m", seen)
  expect_identical(seen[[model_position + 1L]], expected_model)
  expect_identical(result$text, "cached catalog model")
  expect_identical(result$metadata$model, expected_model)
  expect_identical(result$metadata$model_kind, "file")
})

test_that("CrispASR catalog basenames never fall back to the working directory", {
  audio <- local_stt_audio()
  cache_dir <- tempfile("genflow-crispasr-cache-")
  working_dir <- tempfile("genflow-crispasr-working-")
  dir.create(cache_dir)
  dir.create(working_dir)
  filename <- "stale-catalog-model.gguf"
  writeBin(as.raw(c(1, 2, 3)), file.path(working_dir, filename))
  on.exit(
    unlink(c(audio, cache_dir, working_dir), recursive = TRUE),
    add = TRUE
  )
  withr::local_envvar(c(
    CRISPASR_CACHE_DIR = cache_dir,
    CRISPASR_MODELS_DIR = NA
  ))
  withr::local_dir(working_dir)

  expect_error(
    genflow:::.stt_native_crispasr(
      audio_path = audio,
      model = filename,
      language = NULL,
      prompt = NULL,
      timeout_secs = 10,
      executable = file.path(R.home("bin"), "R"),
      native_device = "cpu",
      runner = function(...) {
        fail("The runner must not receive a working-directory model.")
      }
    ),
    "not found in the managed cache"
  )
})
