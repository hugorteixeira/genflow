test_that("local inference config round-trips and normalizes values", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)

  config <- gen_local_config(
    config = list(
      python = "python3",
      device = "ROCM",
      dtype = "BF16",
      hf_revision = "reviewed-model-commit",
      ollama_base_url = "http://127.0.0.1:11434/",
      stt_native_engine = "CRISP-ASR",
      stt_native_executable = "/opt/crisp/bin/crispasr",
      stt_native_model = "/models/parakeet-q4_k.gguf",
      stt_native_backend = "PARAKEET",
      stt_native_device = "VULKAN"
    ),
    path = path
  )

  expect_true(file.exists(path))
  expect_equal(config$device, "rocm")
  expect_equal(config$dtype, "bfloat16")
  expect_equal(config$hf_revision, "reviewed-model-commit")
  expect_equal(config$ollama_base_url, "http://127.0.0.1:11434")
  expect_equal(config$version, 2L)
  expect_equal(config$stt_native_engine, "crispasr")
  expect_equal(config$stt_native_device, "vulkan")
  expect_equal(config$stt_native_model, "/models/parakeet-q4_k.gguf")
  expect_equal(config$stt_native_backend, "parakeet")
  expect_equal(config$moss_cpp_model, "")
  expect_equal(gen_local_config(path = path), config)
  persisted <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  expect_false(any(startsWith(names(persisted), "moss_cpp_")))
})

test_that("local inference config canonicalizes remote native model references", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)

  config <- gen_local_config(
    stt_native_model = paste0(
      "hf://cstr/granite-speech-4.1-2b-GGUF/",
      "granite-speech-4.1-2b-q4_k.gguf"
    ),
    path = path
  )

  expect_identical(
    config$stt_native_model,
    paste0(
      "hf://cstr/granite-speech-4.1-2b-GGUF:",
      "granite-speech-4.1-2b-q4_k.gguf"
    )
  )
  expect_identical(gen_local_config(path = path), config)
})

test_that("legacy MOSS config migrates once and canonical values can be cleared", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  jsonlite::write_json(
    list(
      version = 1L,
      moss_cpp_executable = "/old/moss-transcribe",
      moss_cpp_model = "/old/moss.gguf",
      moss_cpp_device = "vulkan"
    ),
    path,
    auto_unbox = TRUE
  )

  migrated <- gen_local_config(path = path)
  expect_identical(migrated$version, 2L)
  expect_identical(migrated$stt_native_engine, "moss-transcribe")
  expect_identical(
    migrated$stt_native_executable,
    "/old/moss-transcribe"
  )
  expect_identical(migrated$stt_native_model, "/old/moss.gguf")
  expect_identical(migrated$stt_native_device, "vulkan")
  expect_identical(migrated$moss_cpp_executable, "")
  expect_identical(migrated$moss_cpp_model, "")
  expect_identical(migrated$moss_cpp_device, "auto")

  cleared <- gen_local_config(
    stt_native_engine = "auto",
    stt_native_executable = "",
    stt_native_model = "",
    stt_native_device = "auto",
    path = path
  )
  expect_identical(cleared$stt_native_engine, "auto")
  expect_identical(cleared$stt_native_executable, "")
  expect_identical(cleared$stt_native_model, "")
  expect_identical(gen_local_config(path = path), cleared)
})

test_that("local inference config rejects unknown or invalid settings", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)

  expect_error(
    gen_local_config(unknown = "value", path = path),
    "Unknown local inference setting"
  )
  expect_error(
    gen_local_config(device = "gpu", path = path),
    "`device`"
  )
  expect_error(
    gen_local_config(ollama_base_url = "localhost:11434", path = path),
    "http\\(s\\) URL"
  )
  expect_error(
    gen_local_config(hf_revision = "not a valid revision", path = path),
    "`hf_revision` cannot contain whitespace"
  )
  expect_error(
    gen_local_config(moss_cpp_device = "magic", path = path),
    "`moss_cpp_device`"
  )
})

test_that("environment values take precedence in effective local config", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  old_path <- getOption("genflow.local_config_path")
  old_env <- Sys.getenv("OLLAMA_BASE_URL", unset = NA_character_)
  on.exit(options(genflow.local_config_path = old_path), add = TRUE)
  on.exit({
    if (is.na(old_env)) {
      Sys.unsetenv("OLLAMA_BASE_URL")
    } else {
      Sys.setenv(OLLAMA_BASE_URL = old_env)
    }
  }, add = TRUE)

  options(genflow.local_config_path = path)
  gen_local_config(ollama_base_url = "http://127.0.0.1:11434")
  Sys.setenv(OLLAMA_BASE_URL = "http://127.0.0.1:22434")

  config <- genflow:::.genflow_local_effective_config()
  expect_equal(config$ollama_base_url, "http://127.0.0.1:22434")
})

test_that("canonical native environment values override saved configuration", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  old_path <- getOption("genflow.local_config_path")
  old_env <- Sys.getenv(
    c(
      "GENFLOW_STT_NATIVE_ENGINE",
      "GENFLOW_STT_NATIVE_MODEL",
      "GENFLOW_STT_NATIVE_BACKEND",
      "GENFLOW_STT_NATIVE_DEVICE"
    ),
    unset = NA_character_
  )
  on.exit(options(genflow.local_config_path = old_path), add = TRUE)
  on.exit({
    names(old_env) <- c(
      "GENFLOW_STT_NATIVE_ENGINE",
      "GENFLOW_STT_NATIVE_MODEL",
      "GENFLOW_STT_NATIVE_BACKEND",
      "GENFLOW_STT_NATIVE_DEVICE"
    )
    for (name in names(old_env)) {
      if (is.na(old_env[[name]])) {
        Sys.unsetenv(name)
      } else {
        do.call(Sys.setenv, stats::setNames(list(old_env[[name]]), name))
      }
    }
  }, add = TRUE)

  options(genflow.local_config_path = path)
  gen_local_config(
    stt_native_engine = "moss-transcribe",
    stt_native_model = "/saved/model.gguf",
    stt_native_device = "cpu"
  )
  Sys.setenv(
    GENFLOW_STT_NATIVE_ENGINE = "crispasr",
    GENFLOW_STT_NATIVE_MODEL = "auto",
    GENFLOW_STT_NATIVE_BACKEND = "parakeet",
    GENFLOW_STT_NATIVE_DEVICE = "vulkan"
  )

  config <- genflow:::.genflow_local_effective_config()
  expect_identical(config$stt_native_engine, "crispasr")
  expect_identical(config$stt_native_model, "auto")
  expect_identical(config$stt_native_backend, "parakeet")
  expect_identical(config$stt_native_device, "vulkan")
})

test_that("Hugging Face revision environment overrides the saved pin", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  old_path <- getOption("genflow.local_config_path")
  old_revision <- Sys.getenv("GENFLOW_HF_REVISION", unset = NA_character_)
  on.exit(options(genflow.local_config_path = old_path), add = TRUE)
  on.exit({
    if (is.na(old_revision)) {
      Sys.unsetenv("GENFLOW_HF_REVISION")
    } else {
      Sys.setenv(GENFLOW_HF_REVISION = old_revision)
    }
  }, add = TRUE)

  options(genflow.local_config_path = path)
  gen_local_config(hf_revision = "saved-commit")
  Sys.setenv(GENFLOW_HF_REVISION = "environment-commit")

  config <- genflow:::.genflow_local_effective_config()
  expect_identical(config$hf_revision, "environment-commit")
})

test_that("local diagnostics can run without endpoint probes", {
  config <- genflow:::.genflow_local_config_defaults()
  result <- gen_local_diagnostics(
    config = config,
    check_endpoints = FALSE,
    timeout = 2
  )

  expect_s3_class(result, "data.frame")
  expect_named(result, c("component", "status", "detail"))
  expect_true(all(c(
    "Python",
    "FFmpeg",
    "Hugging Face cache",
    "Native STT CLI",
    "Native STT model",
    "Vulkan"
  ) %in% result$component))
  expect_true(all(result$status %in% c("ok", "warning", "error", "info")))
})

test_that("Python executable resolution preserves virtualenv symlinks", {
  target <- unname(Sys.which("Rscript"))
  skip_if(!nzchar(target), "Rscript is unavailable")

  directory <- tempfile("genflow-executable-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  link <- file.path(directory, "python")
  skip_if_not(file.symlink(target, link), "symbolic links are unavailable")

  expected <- file.path(
    normalizePath(directory, winslash = "/", mustWork = TRUE),
    "python"
  )
  expect_identical(
    genflow:::.genflow_resolve_executable(link),
    expected
  )
  expect_identical(
    genflow:::.stt_resolve_python(python = link),
    expected
  )
  expect_false(identical(expected, normalizePath(link, mustWork = TRUE)))
})

test_that("executable resolution rejects directories and non-executable files", {
  directory <- tempfile("genflow-not-executable-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  regular_file <- file.path(directory, "regular-file")
  writeLines("not executable", regular_file)
  Sys.chmod(regular_file, mode = "0644")

  expect_identical(
    genflow:::.genflow_resolve_executable(directory),
    ""
  )
  if (.Platform$OS.type != "windows") {
    expect_identical(
      genflow:::.genflow_resolve_executable(regular_file),
      ""
    )
  }
})

test_that("Python diagnostics explain a CUDA build selected for ROCm", {
  result <- genflow:::.genflow_python_diagnostic_result(
    python = "/tmp/venv/bin/python",
    payload = list(
      python = "3.11.15",
      transformers = "5.5.4",
      torch = "2.11.0+cu130",
      hip = NULL,
      cuda = "13.0",
      accelerator = FALSE,
      device_count = 0L,
      device = NULL,
      mps = FALSE
    ),
    requested_device = "rocm"
  )

  expect_identical(result$status, "error")
  expect_match(result$detail, "build CUDA 13.0", fixed = TRUE)
  expect_match(result$detail, "ROCm/HIP was requested", fixed = TRUE)
})

test_that("Python diagnostics report the MOSS package only when its profile needs it", {
  payload <- list(
    python = "3.11.15",
    transformers = "5.5.4",
    torch = "2.11.0+cu130",
    hip = NULL,
    cuda = "13.0",
    accelerator = FALSE,
    device_count = 0L,
    device = NULL,
    mps = FALSE,
    moss_transcribe_diarize_error = "No module named 'moss_transcribe_diarize'"
  )

  generic <- genflow:::.genflow_python_diagnostic_result(
    python = "/tmp/python",
    payload = payload,
    requested_device = "cpu",
    require_moss = FALSE
  )
  moss <- genflow:::.genflow_python_diagnostic_result(
    python = "/tmp/python",
    payload = payload,
    requested_device = "cpu",
    require_moss = TRUE
  )

  expect_identical(generic$status, "ok")
  expect_identical(moss$status, "error")
  expect_match(moss$detail, "moss_transcribe_diarize", fixed = TRUE)
  expect_match(moss$detail, "/tmp/python", fixed = TRUE)
  expect_match(moss$detail, "-m pip install", fixed = TRUE)
  expect_match(
    moss$detail,
    "github.com/OpenMOSS/MOSS-Transcribe-Diarize/archive/",
    fixed = TRUE
  )
  expect_match(moss$detail, "Transformers >=5.6.0,<6.0.0", fixed = TRUE)
})

test_that("MOSS diagnostics enforce its Transformers compatibility range", {
  versions <- c("5.5.4", "5.6.0", "5.99.0", "6.0.0")
  expected <- c("error", "ok", "ok", "error")

  for (index in seq_along(versions)) {
    result <- genflow:::.genflow_python_diagnostic_result(
      python = "/tmp/python",
      payload = list(
        python = "3.11.15",
        transformers = versions[[index]],
        torch = "2.11.0",
        hip = NULL,
        cuda = NULL,
        accelerator = FALSE,
        device_count = 0L,
        device = NULL,
        mps = FALSE,
        moss_transcribe_diarize = "0.1.0"
      ),
      requested_device = "cpu",
      require_moss = TRUE
    )

    expect_identical(
      result$status,
      expected[[index]],
      info = paste("Transformers", versions[[index]])
    )
  }
})

test_that("Python diagnostics accept a complete probe that exits at the timeout boundary", {
  skip_on_os("windows")

  python <- tempfile("genflow-python-probe-")
  on.exit(unlink(python), add = TRUE)
  writeLines(
    c(
      "#!/bin/sh",
      "printf '%s\\n' 'a harmless probe warning' >&2",
      paste0(
        "printf '%s\\n' '",
        "{\"python\":\"3.11.15\",\"executable\":\"/tmp/python\",",
        "\"transformers\":\"5.5.4\",\"torch\":\"2.11.0+cu130\",",
        "\"hip\":null,\"cuda\":\"13.0\",\"accelerator\":false,",
        "\"device_count\":0,\"device\":null,\"mps\":false,",
        "\"probe_complete\":true}'"
      ),
      "exit 124"
    ),
    python
  )
  Sys.chmod(python, mode = "0755")

  result <- genflow:::.genflow_python_diagnostic(
    python = python,
    timeout = 3,
    requested_device = "cpu"
  )

  expect_identical(result$status, "ok")
  expect_match(result$detail, "torch 2.11.0+cu130", fixed = TRUE)
  expect_false(grepl("could not complete", result$detail, fixed = TRUE))
})

test_that("Python diagnostics report a timeout without a completion marker", {
  output <- structure(
    c(
      "probe warning",
      "{\"python\":\"3.11.15\",\"torch\":\"2.11.0+cu130\"}"
    ),
    status = 124L
  )

  expect_null(genflow:::.genflow_parse_python_probe_output(output))
})

test_that("native STT diagnostics validate a configured engine and model", {
  skip_on_os("windows")

  directory <- tempfile("genflow-moss-cpp-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  executable <- file.path(directory, "moss-transcribe")
  model <- file.path(directory, "model.gguf")
  writeLines(
    c("#!/bin/sh", "printf '%s\\n' 'moss-transcribe transcribe --help'"),
    executable
  )
  Sys.chmod(executable, mode = "0755")
  writeBin(as.raw(c(0x47, 0x47, 0x55, 0x46)), model)

  config <- genflow:::.genflow_local_config_defaults()
  config$stt_native_engine <- "moss-transcribe"
  config$stt_native_executable <- executable
  config$stt_native_model <- model
  rows <- genflow:::.genflow_native_stt_diagnostics(config, timeout = 2)
  rows <- do.call(rbind, rows)

  expect_identical(
    rows$component,
    c("Native STT CLI", "Native STT model")
  )
  expect_identical(rows$status, c("ok", "ok"))
  expect_match(rows$detail[[1]], "moss-transcribe", fixed = TRUE)
  expect_match(rows$detail[[2]], "model.gguf", fixed = TRUE)
})

test_that("native STT diagnostics explain explicit CrispASR downloads", {
  home_dir <- tempfile("genflow-crispasr-home-")
  dir.create(home_dir)
  on.exit(unlink(home_dir, recursive = TRUE), add = TRUE)
  cache_dir <- tempfile("genflow-crispasr-cache-")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)
  withr::local_envvar(c(
    HOME = home_dir,
    CRISPASR_CACHE_DIR = cache_dir,
    CRISPASR_MODELS_DIR = NA
  ))

  config <- genflow:::.genflow_local_config_defaults()
  config$stt_native_engine <- "crispasr"
  config$stt_native_executable <- file.path(R.home("bin"), "R")
  config$stt_native_model <- "auto"
  config$stt_native_backend <- "parakeet"
  rows <- do.call(
    rbind,
    genflow:::.genflow_native_stt_diagnostics(config, timeout = 2)
  )

  expect_identical(rows$component, c("Native STT CLI", "Native STT model"))
  expect_identical(rows$status[[2]], "info")
  expect_match(rows$detail[[2]], "download and cache", fixed = TRUE)

  config$stt_native_backend <- ""
  missing_backend <- do.call(
    rbind,
    genflow:::.genflow_native_stt_diagnostics(config, timeout = 2)
  )
  expect_identical(missing_backend$status[[2]], "error")
  expect_match(missing_backend$detail[[2]], "requires engine crispasr")

  config$stt_native_backend <- "granite-4.1"
  for (model in c(
    paste0(
      "hf://cstr/granite-speech-4.1-2b-GGUF:",
      "granite-speech-4.1-2b-q4_k.gguf"
    ),
    paste0(
      "hf://cstr/granite-speech-4.1-2b-GGUF/",
      "granite-speech-4.1-2b-q4_k.gguf"
    )
  )) {
    config$stt_native_model <- model
    remote <- do.call(
      rbind,
      genflow:::.genflow_native_stt_diagnostics(config, timeout = 2)
    )
    expect_identical(remote$status[[2]], "info")
    expect_match(
      remote$detail[[2]],
      "granite-speech-4.1-2b-q4_k.gguf",
      fixed = TRUE
    )
    expect_match(remote$detail[[2]], "will be downloaded", fixed = TRUE)
  }

  cached_model <- file.path(
    cache_dir,
    "granite-speech-4.1-2b-q4_k.gguf"
  )
  writeBin(as.raw(c(0x47, 0x47, 0x55, 0x46)), cached_model)
  cached <- do.call(
    rbind,
    genflow:::.genflow_native_stt_diagnostics(config, timeout = 2)
  )
  expect_identical(cached$status[[2]], "ok")
  expect_match(cached$detail[[2]], "Cached model:", fixed = TRUE)
  expect_match(cached$detail[[2]], cache_dir, fixed = TRUE)

  sidecar <- paste0(cached_model, ".src")
  writeLines(
    paste0(
      "https://huggingface.co/another/repository/resolve/main/",
      basename(cached_model)
    ),
    sidecar
  )
  wrong_source <- do.call(
    rbind,
    genflow:::.genflow_native_stt_diagnostics(config, timeout = 2)
  )
  expect_identical(wrong_source$status[[2]], "info")
  expect_match(wrong_source$detail[[2]], "will be downloaded", fixed = TRUE)

  writeLines(
    paste0(
      "https://huggingface.co/cstr/granite-speech-4.1-2b-GGUF/",
      "resolve/main/",
      basename(cached_model)
    ),
    sidecar
  )
  matching_source <- do.call(
    rbind,
    genflow:::.genflow_native_stt_diagnostics(config, timeout = 2)
  )
  expect_identical(matching_source$status[[2]], "ok")

  models_dir <- tempfile("genflow-crispasr-models-")
  dir.create(models_dir)
  on.exit(unlink(models_dir, recursive = TRUE), add = TRUE)
  fallback_model <- file.path(models_dir, basename(cached_model))
  expect_true(file.copy(cached_model, fallback_model))
  expect_true(file.copy(sidecar, paste0(fallback_model, ".src")))
  unlink(c(cached_model, sidecar))
  withr::local_envvar(CRISPASR_MODELS_DIR = models_dir)

  fallback_cached <- do.call(
    rbind,
    genflow:::.genflow_native_stt_diagnostics(config, timeout = 2)
  )
  expect_identical(fallback_cached$status[[2]], "ok")
  expect_match(fallback_cached$detail[[2]], models_dir, fixed = TRUE)

  config$stt_native_model <- "hf://owner/repository"
  missing_file <- do.call(
    rbind,
    genflow:::.genflow_native_stt_diagnostics(config, timeout = 2)
  )
  expect_identical(missing_file$status[[2]], "error")
  expect_match(missing_file$detail[[2]], "one model filename", fixed = TRUE)
})

test_that("native STT diagnostics reject a directory used as the executable", {
  directory <- tempfile("genflow-crispasr-directory-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)

  config <- genflow:::.genflow_local_config_defaults()
  config$stt_native_engine <- "crispasr"
  config$stt_native_executable <- directory
  rows <- do.call(
    rbind,
    genflow:::.genflow_native_stt_diagnostics(config, timeout = 2)
  )

  expect_identical(rows$status[[1]], "error")
  expect_match(rows$detail[[1]], "points to a directory", fixed = TRUE)
  expect_match(rows$detail[[1]], "build/bin/crispasr", fixed = TRUE)
})

test_that("local endpoint diagnostics do not duplicate API path prefixes", {
  expect_identical(
    genflow:::.genflow_endpoint_url(
      "http://127.0.0.1:8080/v1",
      "v1/models"
    ),
    "http://127.0.0.1:8080/v1/models"
  )
  expect_identical(
    genflow:::.genflow_endpoint_url(
      "http://127.0.0.1:8000/v1/audio/transcriptions",
      "v1/models"
    ),
    "http://127.0.0.1:8000/v1/models"
  )
  expect_identical(
    genflow:::.genflow_endpoint_url(
      "http://127.0.0.1:11434/api/tags",
      "api/tags"
    ),
    "http://127.0.0.1:11434/api/tags"
  )
})

test_that("local diagnostics can target one adapter without unrelated warnings", {
  config <- genflow:::.genflow_local_config_defaults()
  row <- genflow:::.genflow_diagnostic_row

  testthat::local_mocked_bindings(
    .genflow_python_diagnostic = function(...) {
      row("Python", "ok", "mock Python")
    },
    .genflow_native_stt_diagnostics = function(...) {
      list(row("Native STT CLI", "ok", "mock native"))
    },
    .genflow_vulkan_diagnostic = function(...) {
      row("Vulkan", "ok", "mock Vulkan")
    },
    .package = "genflow"
  )

  hf <- gen_local_diagnostics(
    config = config,
    check_endpoints = FALSE,
    adapters = "hf-local"
  )
  expect_true("Python" %in% hf$component)
  expect_true("Hugging Face cache" %in% hf$component)
  expect_false("Native STT CLI" %in% hf$component)
  expect_false("Vulkan" %in% hf$component)

  native <- gen_local_diagnostics(
    config = config,
    check_endpoints = FALSE,
    adapters = "local-native"
  )
  expect_false("Python" %in% native$component)
  expect_false("Hugging Face cache" %in% native$component)
  expect_true("Native STT CLI" %in% native$component)
  expect_true("Vulkan" %in% native$component)

  ollama <- gen_local_diagnostics(
    config = config,
    check_endpoints = FALSE,
    adapters = "ollama"
  )
  expect_s3_class(ollama, "data.frame")
  expect_equal(nrow(ollama), 0L)
  expect_error(
    gen_local_diagnostics(config = config, adapters = "unknown-runtime"),
    "Unsupported local diagnostic adapter"
  )
})

test_that("authenticated local endpoint diagnostics forward bearer tokens", {
  config <- genflow:::.genflow_local_config_defaults()
  config$llamacpp_base_url <- "http://127.0.0.1:8080/v1"
  config$stt_server_base_url <-
    "http://127.0.0.1:8000/v1/audio/transcriptions"
  calls <- list()
  withr::local_envvar(c(
    LLAMACPP_API_KEY = "llama-secret",
    LLAMA_CPP_API_KEY = NA,
    GENFLOW_STT_API_KEY = "stt-secret"
  ))

  testthat::local_mocked_bindings(
    .genflow_endpoint_diagnostic = function(component,
                                            base_url,
                                            path,
                                            timeout,
                                            headers = character()) {
      calls[[component]] <<- list(
        base_url = base_url,
        path = path,
        headers = headers
      )
      genflow:::.genflow_diagnostic_row(component, "ok", "mock endpoint")
    },
    .package = "genflow"
  )

  result <- gen_local_diagnostics(
    config = config,
    adapters = c("llamacpp", "local-openai")
  )

  expect_identical(result$component, c("llama.cpp", "Local STT server"))
  expect_identical(
    calls[["llama.cpp"]]$base_url,
    "http://127.0.0.1:8080"
  )
  expect_identical(
    unname(calls[["llama.cpp"]]$headers),
    "Bearer llama-secret"
  )
  expect_identical(
    unname(calls[["Local STT server"]]$headers),
    "Bearer stt-secret"
  )
})
