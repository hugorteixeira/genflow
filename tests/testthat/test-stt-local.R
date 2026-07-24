local_stt_audio <- function() {
  path <- tempfile("genflow-stt-", fileext = ".wav")
  writeBin(as.raw(c(82, 73, 70, 70, rep(0, 40))), path)
  path
}

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

test_that("gen_stt accepts a NULL model and dispatches canonical local aliases", {
  audio <- local_stt_audio()
  on.exit(unlink(audio), add = TRUE)
  config_path <- tempfile(fileext = ".json")
  old_config_path <- getOption("genflow.local_config_path")
  options(genflow.local_config_path = config_path)
  on.exit(options(genflow.local_config_path = old_config_path), add = TRUE)

  seen_revision <- NULL
  testthat::local_mocked_bindings(
    .stt_local_hf = function(...) {
      seen_revision <<- list(...)$revision
      list(
        text = "mock local transcript",
        metadata = list(accelerator = "rocm", device = "cuda:0")
      )
    },
    .package = "genflow"
  )

  console <- capture.output(
    result <- gen_stt(
      audio,
      service = "transformers",
      model = NULL,
      revision = "0123456789abcdef",
      save_txt = FALSE
    )
  )

  expect_null(attr(result, "class", exact = TRUE))
  expect_true(is.list(result))
  expect_identical(class(result), "list")
  expect_identical(result$status_api, "SUCCESS")
  expect_identical(result$response_value, "mock local transcript")
  expect_identical(result$service, "hf-local")
  expect_identical(
    result$model,
    "openai/whisper-large-v3-turbo"
  )
  expect_identical(result$metadata$accelerator, "rocm")
  expect_identical(seen_revision, "0123456789abcdef")
  expect_match(
    paste(console, collapse = "\n"),
    "[SUCCESS]",
    fixed = TRUE
  )
  expect_match(
    paste(console, collapse = "\n"),
    "hf-local | openai/whisper-large-v3-turbo | Time:",
    fixed = TRUE
  )
  expect_match(
    paste(console, collapse = "\n"),
    "-> Response: mock local transcript...",
    fixed = TRUE
  )

  empty_list_model <- gen_stt(
    audio,
    service = list(service = "hf_local"),
    model = list(),
    save_txt = FALSE
  )
  expect_identical(empty_list_model$status_api, "SUCCESS")
  expect_identical(
    empty_list_model$model,
    "openai/whisper-large-v3-turbo"
  )
})

test_that("gen_stt validates public scalar controls before dispatch", {
  audio <- local_stt_audio()
  on.exit(unlink(audio), add = TRUE)
  dispatches <- 0L
  testthat::local_mocked_bindings(
    .stt_local_hf = function(timeout_secs, ...) {
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
    convert_vector = list(convert = c(TRUE, FALSE))
  )
  for (case in logical_cases) {
    expect_error(
      do.call(
        gen_stt,
        c(
          list(audio = audio, service = "hf-local"),
          case
        )
      ),
      "must be TRUE or FALSE"
    )
  }

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
          list(audio = audio, service = "hf-local", save_txt = FALSE),
          case
        )
      ),
      "positive finite number"
    )
  }
  expect_error(
    gen_stt(
      audio,
      service = "hf-local",
      revision = "not a valid revision",
      save_txt = FALSE
    ),
    "`revision` cannot contain whitespace"
  )
  expect_identical(dispatches, 0L)

  result <- gen_stt(
    audio,
    service = "hf-local",
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

test_that("an unsupported STT service is a structured error when model is NULL", {
  audio <- local_stt_audio()
  on.exit(unlink(audio), add = TRUE)

  result <- gen_stt(
    audio,
    service = "does-not-exist",
    model = NULL,
    save_txt = FALSE
  )

  expect_identical(result$status_api, "ERROR")
  expect_match(result$status_msg, "Unsupported STT service")
  expect_identical(result$model, "default")
})

test_that("local Hugging Face bridge selects the known MOSS profile", {
  audio <- local_stt_audio()
  on.exit(unlink(audio), add = TRUE)
  seen <- NULL

  fake_runner <- function(command, args, timeout_secs, environment) {
    seen <<- list(
      command = command,
      args = args,
      timeout = timeout_secs,
      environment = environment
    )
    output <- args[[match("--output", args) + 1L]]
    payload <- list(
      ok = TRUE,
      text = "[0.00][S01]hello[1.00]",
      backend = "transformers",
      profile = "moss",
      model = "OpenMOSS-Team/MOSS-Transcribe-Diarize",
      device = "cuda:0",
      accelerator = "rocm",
      dtype = "bfloat16",
      segments = list(list(
        start = 0,
        end = 1,
        speaker = "S01",
        text = "hello"
      ))
    )
    writeLines(
      jsonlite::toJSON(payload, auto_unbox = TRUE),
      output,
      useBytes = TRUE
    )
    list(status = 0L, output = character())
  }

  result <- genflow:::.stt_local_hf(
    audio_path = audio,
    model = "OpenMOSS-Team/MOSS-Transcribe-Diarize",
    language = "pt",
    prompt = "Use speaker labels.",
    timeout_secs = 90,
    profile = "auto",
    device = "rocm",
    dtype = "auto",
    python = file.path(R.home("bin"), "R"),
    revision = "moss-reviewed-commit",
    trust_remote_code = NULL,
    chunk_length_s = NULL,
    return_timestamps = TRUE,
    max_new_tokens = 4096L,
    runner = fake_runner
  )

  expect_identical(result$text, "[0.00][S01]hello[1.00]")
  expect_identical(result$metadata$accelerator, "rocm")
  expect_identical(result$metadata$segments[[1]]$speaker, "S01")
  expect_identical(seen$timeout, 90)
  expect_identical(
    seen$args[[match("--profile", seen$args) + 1L]],
    "moss"
  )
  expect_identical(
    seen$args[[match("--device", seen$args) + 1L]],
    "rocm"
  )
  expect_true("--trust-remote-code" %in% seen$args)
  expect_identical(
    seen$args[[match("--revision", seen$args) + 1L]],
    "moss-reviewed-commit"
  )
  expect_identical(
    seen$args[[match("--max-new-tokens", seen$args) + 1L]],
    "4096"
  )
})

test_that("generic local Transformers inference does not trust remote code by default", {
  audio <- local_stt_audio()
  on.exit(unlink(audio), add = TRUE)
  seen_args <- NULL
  seen_environment <- NULL

  fake_runner <- function(command, args, timeout_secs, environment) {
    seen_args <<- args
    seen_environment <<- environment
    output <- args[[match("--output", args) + 1L]]
    writeLines(
      '{"ok":true,"text":"hello","profile":"transformers","device":"cpu","accelerator":"cpu","dtype":"float32"}',
      output,
      useBytes = TRUE
    )
    list(status = 0L, output = character())
  }

  result <- genflow:::.stt_local_hf(
    audio_path = audio,
    model = "openai/whisper-small",
    language = NULL,
    prompt = NULL,
    timeout_secs = 30,
    profile = "auto",
    device = "cpu",
    dtype = "fp32",
    python = file.path(R.home("bin"), "R"),
    hf_cache_dir = tempdir(),
    revision = "whisper-reviewed-commit",
    trust_remote_code = NULL,
    chunk_length_s = 20,
    return_timestamps = "word",
    max_new_tokens = NULL,
    runner = fake_runner
  )

  expect_identical(result$text, "hello")
  expect_false("--trust-remote-code" %in% seen_args)
  expect_identical(
    seen_args[[match("--revision", seen_args) + 1L]],
    "whisper-reviewed-commit"
  )
  expect_identical(
    seen_args[[match("--profile", seen_args) + 1L]],
    "transformers"
  )
  expect_identical(
    seen_args[[match("--dtype", seen_args) + 1L]],
    "float32"
  )
  expect_identical(
    seen_args[[match("--return-timestamps", seen_args) + 1L]],
    "word"
  )
  expect_identical(
    unname(seen_environment[["HF_HOME"]]),
    path.expand(tempdir())
  )
})

test_that("local Hugging Face revision precedence supports an explicit opt-out", {
  audio <- local_stt_audio()
  on.exit(unlink(audio), add = TRUE)
  config_path <- tempfile(fileext = ".json")
  on.exit(unlink(config_path), add = TRUE)
  old_config_path <- getOption("genflow.local_config_path")
  old_revision <- Sys.getenv("GENFLOW_HF_REVISION", unset = NA_character_)
  on.exit(options(genflow.local_config_path = old_config_path), add = TRUE)
  on.exit({
    if (is.na(old_revision)) {
      Sys.unsetenv("GENFLOW_HF_REVISION")
    } else {
      Sys.setenv(GENFLOW_HF_REVISION = old_revision)
    }
  }, add = TRUE)
  options(genflow.local_config_path = config_path)
  gen_local_config(hf_revision = "saved-commit")
  Sys.setenv(GENFLOW_HF_REVISION = "environment-commit")

  seen <- list()
  fake_runner <- function(command, args, timeout_secs, environment) {
    seen[[length(seen) + 1L]] <<- args
    output <- args[[match("--output", args) + 1L]]
    writeLines(
      '{"ok":true,"text":"hello","profile":"transformers"}',
      output,
      useBytes = TRUE
    )
    list(status = 0L, output = character())
  }
  run <- function(revision = NULL) {
    genflow:::.stt_local_hf(
      audio_path = audio,
      model = "owner/model",
      language = NULL,
      prompt = NULL,
      timeout_secs = 30,
      profile = "transformers",
      device = "cpu",
      dtype = "float32",
      python = file.path(R.home("bin"), "R"),
      runner = fake_runner,
      revision = revision
    )
  }
  revision_arg <- function(args) {
    position <- match("--revision", args)
    if (is.na(position)) "" else args[[position + 1L]]
  }

  invisible(run("explicit-commit"))
  invisible(run())
  Sys.unsetenv("GENFLOW_HF_REVISION")
  invisible(run())
  invisible(run(""))

  expect_identical(
    vapply(seen, revision_arg, character(1)),
    c(
      "explicit-commit",
      "environment-commit",
      "saved-commit",
      ""
    )
  )
})

test_that("local bridge surfaces structured Python dependency diagnostics", {
  audio <- local_stt_audio()
  on.exit(unlink(audio), add = TRUE)

  fake_runner <- function(command, args, timeout_secs, environment) {
    output <- args[[match("--output", args) + 1L]]
    writeLines(
      paste0(
        '{"ok":false,"error_type":"ModuleNotFoundError",',
        '"error":"No module named transformers",',
        '"hint":"Install PyTorch and Transformers in the selected environment."}'
      ),
      output,
      useBytes = TRUE
    )
    list(status = 1L, output = "python failed")
  }

  expect_error(
    genflow:::.stt_local_hf(
      audio_path = audio,
      model = "openai/whisper-small",
      language = NULL,
      prompt = NULL,
      timeout_secs = 30,
      python = file.path(R.home("bin"), "R"),
      runner = fake_runner
    ),
    "ModuleNotFoundError.*Install PyTorch and Transformers"
  )
})

test_that("MOSS remote code opt-out fails before starting Python", {
  audio <- local_stt_audio()
  on.exit(unlink(audio), add = TRUE)

  expect_error(
    genflow:::.stt_local_hf(
      audio_path = audio,
      model = "OpenMOSS-Team/MOSS-Transcribe-Diarize",
      language = NULL,
      prompt = NULL,
      timeout_secs = 30,
      profile = "moss",
      python = file.path(R.home("bin"), "R"),
      trust_remote_code = FALSE,
      runner = function(...) stop("runner must not be called")
    ),
    "requires `trust_remote_code = TRUE`"
  )
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
    genflow:::.stt_normalize_service("huggingface_local"),
    "hf-local"
  )
  expect_identical(
    genflow:::.stt_normalize_service("openai-compatible"),
    "local-openai"
  )
  expect_identical(
    genflow:::.stt_local_transcriptions_url("http://localhost:9000"),
    "http://localhost:9000/v1/audio/transcriptions"
  )
  expect_identical(genflow:::.stt_validate_device("hip"), "hip")
  expect_identical(genflow:::.stt_validate_dtype("bf16"), "bfloat16")
  expect_error(genflow:::.stt_validate_device("gpu"), "`device`")
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
    "hf://cstr/moss-diarize-GGUF/moss-q4.gguf"
  )
  for (reference in references) {
    seen <- NULL
    result <- genflow:::.stt_native_crispasr(
      audio_path = audio,
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
