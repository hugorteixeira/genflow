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
  expect_equal(config$version, 3L)
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
  expect_identical(migrated$version, 3L)
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

test_that("legacy MOSS environment values apply only to the MOSS engine", {
  withr::local_envvar(c(
    GENFLOW_STT_NATIVE_ENGINE = NA,
    GENFLOW_STT_NATIVE_EXECUTABLE = NA,
    GENFLOW_STT_NATIVE_MODEL = NA,
    GENFLOW_STT_NATIVE_DEVICE = NA,
    GENFLOW_MOSS_CPP_EXECUTABLE = "/legacy/moss-transcribe",
    GENFLOW_MOSS_CPP_MODEL = "/legacy/moss.gguf",
    GENFLOW_MOSS_CPP_DEVICE = "vulkan"
  ))

  crisp <- genflow:::.genflow_local_config_defaults()
  crisp$stt_native_engine <- "crispasr"
  crisp$stt_native_executable <- "/current/crispasr"
  crisp$stt_native_model <- "/current/crisp.gguf"
  crisp$stt_native_device <- "cpu"
  crisp_effective <- genflow:::.genflow_local_effective_config(crisp)
  expect_identical(crisp_effective$stt_native_engine, "crispasr")
  expect_identical(
    crisp_effective$stt_native_executable,
    "/current/crispasr"
  )
  expect_identical(crisp_effective$stt_native_model, "/current/crisp.gguf")
  expect_identical(crisp_effective$stt_native_device, "cpu")

  moss <- genflow:::.genflow_local_config_defaults()
  moss$stt_native_engine <- "auto"
  moss_effective <- genflow:::.genflow_local_effective_config(moss)
  expect_identical(moss_effective$stt_native_engine, "moss-transcribe")
  expect_identical(
    moss_effective$stt_native_executable,
    "/legacy/moss-transcribe"
  )
  expect_identical(moss_effective$stt_native_model, "/legacy/moss.gguf")
  expect_identical(moss_effective$stt_native_device, "vulkan")
  expect_identical(
    genflow:::.stt_resolve_native_engine(config = moss),
    "moss-transcribe"
  )
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

  writeLines(
    paste0(
      "https://huggingface.co/cstr/granite-speech-4.1-2b-GGUF/",
      "resolve/",
      strrep("a", 40),
      "/",
      basename(cached_model)
    ),
    sidecar
  )
  pinned_source <- do.call(
    rbind,
    genflow:::.genflow_native_stt_diagnostics(config, timeout = 2)
  )
  expect_identical(pinned_source$status[[2]], "ok")

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

test_that("native STT diagnostics reject unpublished remote quantizations", {
  config <- genflow:::.genflow_local_config_defaults()
  config$stt_native_engine <- "crispasr"
  config$stt_native_executable <- file.path(R.home("bin"), "R")
  config$stt_native_backend <- "granite-4.1"
  config$stt_native_model <- paste0(
    "hf://cstr/granite-speech-4.1-2b-GGUF:",
    "granite-speech-4.1-2b-q8_0.gguf"
  )

  testthat::local_mocked_bindings(
    .genflow_crispasr_hf_metadata = function(repository, timeout = 30) {
      list(
        sha = strrep("a", 40),
        gguf = list(architecture = "granite_speech"),
        siblings = list(list(
          rfilename = "granite-speech-4.1-2b-q4_k.gguf",
          size = 8,
          lfs = list(sha256 = strrep("b", 64), size = 8)
        ))
      )
    },
    .package = "genflow"
  )

  rows <- do.call(
    rbind,
    genflow:::.genflow_native_stt_diagnostics(
      config,
      timeout = 2,
      check_remote = TRUE
    )
  )
  expect_identical(rows$status[[2]], "error")
  expect_match(rows$detail[[2]], "does not exist", fixed = TRUE)
  expect_match(rows$detail[[2]], "q8_0", fixed = TRUE)
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

test_that("CrispASR inventory separates managed models from cache noise", {
  cache_dir <- tempfile("genflow-crispasr-managed-")
  external_dir <- tempfile("genflow-crispasr-external-")
  dir.create(cache_dir)
  dir.create(external_dir)
  on.exit(unlink(c(cache_dir, external_dir), recursive = TRUE), add = TRUE)
  withr::local_envvar(c(
    CRISPASR_CACHE_DIR = cache_dir,
    CRISPASR_MODELS_DIR = NA
  ))

  filename <- "granite-speech-4.1-2b-q4_k.gguf"
  source_url <- paste0(
    "https://huggingface.co/cstr/granite-speech-4.1-2b-GGUF/",
    "resolve/main/",
    filename
  )
  managed <- file.path(cache_dir, filename)
  writeBin(as.raw(1:8), managed)
  writeChar(source_url, paste0(managed, ".src"), eos = NULL)
  writeBin(as.raw(1:4), file.path(cache_dir, paste0(filename, ".part.42")))
  writeBin(raw(), file.path(cache_dir, "empty-q8_0.gguf"))
  writeLines("not a model", file.path(cache_dir, "notes.txt"))

  external <- file.path(external_dir, "parakeet-q8_0.gguf")
  writeBin(as.raw(1:3), external)
  link <- file.path(cache_dir, "linked-q8_0.gguf")
  link_created <- file.symlink(external, link)

  config <- genflow:::.genflow_local_config_defaults()
  config$stt_native_model <- paste0(
    "hf://cstr/granite-speech-4.1-2b-GGUF:",
    filename
  )
  inventory <- genflow:::.genflow_crispasr_inventory(
    config = config,
    cache_dirs = c(cache_dir, external_dir)
  )

  expect_named(inventory, c(
    "path",
    "filename",
    "quant",
    "size_bytes",
    "size",
    "source_url",
    "managed",
    "selected"
  ))
  expect_true(filename %in% inventory$filename)
  expect_false(any(grepl("\\.src$|\\.part\\.", inventory$filename)))
  expect_false("empty-q8_0.gguf" %in% inventory$filename)

  managed_row <- inventory[inventory$filename == filename, , drop = FALSE]
  expect_identical(managed_row$quant, "q4_k")
  expect_identical(managed_row$size_bytes, 8)
  expect_identical(managed_row$source_url, source_url)
  expect_true(managed_row$managed)
  expect_true(managed_row$selected)

  external_row <- inventory[
    inventory$filename == basename(external),
    ,
    drop = FALSE
  ]
  expect_false(external_row$managed)
  if (isTRUE(link_created)) {
    link_row <- inventory[
      inventory$filename == basename(link),
      ,
      drop = FALSE
    ]
    expect_false(link_row$managed)
  }
})

test_that("CrispASR Hugging Face discovery accepts only real compatible files", {
  revision <- strrep("a", 40)
  cstr_metadata <- list(
    sha = revision,
    gguf = list(architecture = "granite_speech"),
    siblings = list(
      list(
        rfilename = "granite-speech-4.1-2b-q4_k.gguf",
        size = 2941043168,
        lfs = list(sha256 = strrep("1", 64), size = 2941043168)
      ),
      list(
        rfilename = "granite-speech-4.1-2b-f16.gguf",
        size = 5581887616,
        lfs = list(sha256 = strrep("2", 64), size = 5581887616)
      ),
      list(rfilename = "README.md", size = 4000)
    )
  )

  artifact <- genflow:::.genflow_crispasr_hf_artifact(
    metadata = cstr_metadata,
    repository = "cstr/granite-speech-4.1-2b-GGUF",
    filename = "granite-speech-4.1-2b-q4_k.gguf",
    backend = "granite-4.1"
  )
  expect_identical(artifact$size_bytes, 2941043168)
  expect_identical(artifact$architecture, "granite_speech")
  expect_match(
    artifact$source_url,
    paste0("/resolve/", revision, "/"),
    fixed = TRUE
  )
  expect_identical(artifact$sha256, strrep("1", 64))

  mutable_metadata <- cstr_metadata
  mutable_metadata$sha <- "main"
  expect_error(
    genflow:::.genflow_crispasr_hf_artifact(
      metadata = mutable_metadata,
      repository = "cstr/granite-speech-4.1-2b-GGUF",
      filename = "granite-speech-4.1-2b-q4_k.gguf",
      backend = "granite-4.1"
    ),
    "immutable repository revision"
  )

  missing_hash <- cstr_metadata
  missing_hash$siblings[[1]]$lfs$sha256 <- ""
  expect_error(
    genflow:::.genflow_crispasr_hf_artifact(
      metadata = missing_hash,
      repository = "cstr/granite-speech-4.1-2b-GGUF",
      filename = "granite-speech-4.1-2b-q4_k.gguf",
      backend = "granite-4.1"
    ),
    "LFS SHA-256"
  )

  expect_error(
    genflow:::.genflow_crispasr_hf_artifact(
      metadata = cstr_metadata,
      repository = "cstr/granite-speech-4.1-2b-GGUF",
      filename = "granite-speech-4.1-2b-q8_0.gguf",
      backend = "granite-4.1"
    ),
    "does not exist"
  )

  ibm_metadata <- list(
    gguf = list(architecture = "granite"),
    siblings = list(list(
      rfilename = "granite-speech-4.1-2b-Q8_0.gguf",
      size = 1956154944
    ))
  )
  expect_error(
    genflow:::.genflow_crispasr_hf_artifact(
      metadata = ibm_metadata,
      repository = "ibm-granite/granite-speech-4.1-2b-GGUF",
      filename = "granite-speech-4.1-2b-Q8_0.gguf",
      backend = "granite-4.1"
    ),
    "requires a monolithic `granite_speech` model",
    fixed = TRUE
  )
  expect_error(
    genflow:::.genflow_crispasr_validate_filename("../model.gguf"),
    "flat"
  )
})

test_that("CrispASR auto preview forwards quant without downloading", {
  cache_dir <- tempfile("genflow-crispasr-preview-")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)
  withr::local_envvar(CRISPASR_CACHE_DIR = cache_dir)
  captured_args <- NULL

  testthat::local_mocked_bindings(
    .genflow_crispasr_cache_executable = function(executable = "") {
      "/mock/crispasr"
    },
    .stt_run_process = function(command,
                                args,
                                timeout_secs,
                                environment = character()) {
      captured_args <<- args
      list(
        status = 0L,
        output = c(
          "crispasr 0.8.21",
          "model:",
          "  requested: auto",
          "  backend:   granite-4.1",
          "  registry:  granite-speech-4.1-2b-q8_0.gguf",
          paste0(
            "  url:       https://huggingface.co/cstr/",
            "granite-speech-4.1-2b-GGUF/resolve/main/",
            "granite-speech-4.1-2b-q8_0.gguf"
          ),
          "  size:      ~2.94 GB",
          "  status:    would download",
          paste0(
            "  path:      ",
            cache_dir,
            "/granite-speech-4.1-2b-q8_0.gguf"
          ),
          "companion:",
          "  registry:  unrelated-codec.gguf",
          "  url:       https://huggingface.co/test/codec/resolve/main/unrelated-codec.gguf",
          paste0(
            "  path:      ",
            cache_dir,
            "/unrelated-codec.gguf"
          )
        )
      )
    },
    .package = "genflow"
  )

  preview <- genflow:::.genflow_crispasr_preview_auto(
    selector = "auto:q8_0",
    backend = "granite-4.1"
  )
  expect_identical(preview$quant, "q8_0")
  expect_identical(
    preview$registry,
    "granite-speech-4.1-2b-q8_0.gguf"
  )
  expect_true("--model-quant" %in% captured_args)
  expect_identical(
    captured_args[[match("--model-quant", captured_args) + 1L]],
    "q8_0"
  )
  expect_true("--dry-run-resolve" %in% captured_args)
  expect_false("--hf-repo" %in% captured_args)
  expect_error(
    genflow:::.genflow_crispasr_preview_auto(
      selector = "auto:q8_0",
      backend = "granite-4.1",
      quant = "q4_k"
    ),
    "Conflicting"
  )
})

test_that("CrispASR resolution validates API siblings and HEAD before download", {
  revision <- strrep("b", 40)
  metadata <- list(
    sha = revision,
    gguf = list(architecture = "granite_speech"),
    siblings = list(list(
      rfilename = "granite-speech-4.1-2b-q4_k.gguf",
      size = 8,
      lfs = list(sha256 = strrep("3", 64), size = 8)
    ))
  )
  preview_file <- "granite-speech-4.1-2b-q4_k.gguf"
  head_calls <- list()

  testthat::local_mocked_bindings(
    .genflow_crispasr_preview_auto = function(selector,
                                              backend,
                                              quant = "",
                                              executable = "") {
      list(
        registry = preview_file,
        url = paste0(
          "https://huggingface.co/cstr/",
          "granite-speech-4.1-2b-GGUF/resolve/main/",
          preview_file
        ),
        path = file.path("/cache", preview_file),
        status = "would download"
      )
    },
    .genflow_crispasr_hf_metadata = function(repository, timeout = 30) {
      metadata
    },
    .genflow_crispasr_hf_head = function(url,
                                         expected_size = NA_real_,
                                         timeout = 30) {
      head_calls[[length(head_calls) + 1L]] <<- list(
        url = url,
        expected_size = expected_size
      )
      list(status = 200L, size_bytes = expected_size)
    },
    .package = "genflow"
  )

  artifact <- genflow:::.genflow_crispasr_resolve_download(
    selector = "auto",
    backend = "granite-4.1"
  )
  expect_identical(artifact$filename, preview_file)
  expect_match(
    artifact$source_url,
    paste0("/resolve/", revision, "/"),
    fixed = TRUE
  )
  expect_length(head_calls, 1L)
  expect_identical(head_calls[[1]]$expected_size, 8)

  preview_file <- "granite-speech-4.1-2b-q8_0.gguf"
  expect_error(
    genflow:::.genflow_crispasr_resolve_download(
      selector = "auto:q8_0",
      backend = "granite-4.1"
    ),
    "does not exist"
  )
  expect_length(head_calls, 1L)
})

test_that("CrispASR download installs atomically and reports progress", {
  cache_dir <- tempfile("genflow-crispasr-download-")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)
  withr::local_envvar(c(
    CRISPASR_CACHE_DIR = cache_dir,
    CRISPASR_MODELS_DIR = NA
  ))

  artifact <- list(
    filename = "test-model-q4_k.gguf",
    source_url = paste0(
      "https://huggingface.co/test/model/resolve/",
      strrep("c", 40),
      "/",
      "test-model-q4_k.gguf"
    ),
    size_bytes = 4,
    sha256 = paste0(
      "9f64a747e1b97f131fabb6b447296c9b6",
      "f0201e79fb3c5356e6c77e89b6a806a"
    )
  )
  fetch_count <- 0L
  updates <- list()
  testthat::local_mocked_bindings(
    .genflow_crispasr_resolve_download = function(selector,
                                                  backend = "",
                                                  quant = "",
                                                  executable = "") {
      artifact
    },
    .genflow_crispasr_fetch = function(url,
                                       destination,
                                       expected_size,
                                       filename,
                                       progress = NULL,
                                       timeout = 3600) {
      fetch_count <<- fetch_count + 1L
      connection <- file(destination, open = "wb")
      on.exit(close(connection), add = TRUE)
      writeBin(as.raw(1:4), connection)
      genflow:::.genflow_crispasr_report_progress(
        progress,
        "downloading",
        filename,
        4,
        expected_size
      )
      invisible(4)
    },
    .package = "genflow"
  )

  result <- genflow:::.genflow_crispasr_download(
    selector = "hf://test/model:test-model-q4_k.gguf",
    backend = "test",
    progress = function(update) {
      updates[[length(updates) + 1L]] <<- update
    }
  )
  expect_false(result$cached)
  expect_true(file.exists(result$path))
  expect_identical(file.info(result$path)$size[[1]], 4)
  expect_identical(
    genflow:::.genflow_crispasr_read_source(result$path),
    artifact$source_url
  )
  expect_false(any(grepl(
    "\\.part\\.",
    list.files(cache_dir, all.files = TRUE)
  )))
  expect_true(all(c(
    "resolving",
    "downloading",
    "verifying",
    "publishing",
    "complete"
  ) %in%
    vapply(updates, `[[`, character(1), "stage")))

  cached <- genflow:::.genflow_crispasr_download(
    selector = "hf://test/model:test-model-q4_k.gguf",
    backend = "test"
  )
  expect_true(cached$cached)
  expect_identical(fetch_count, 1L)

  unlink(paste0(result$path, ".src"))
  recovery_stages <- character()
  recovered <- genflow:::.genflow_crispasr_download(
    selector = "hf://test/model:test-model-q4_k.gguf",
    backend = "test",
    progress = function(update) {
      recovery_stages <<- c(recovery_stages, update$stage)
    }
  )
  expect_true(recovered$cached)
  expect_true(all(c("verifying", "publishing", "complete") %in%
    recovery_stages))
  expect_identical(
    genflow:::.genflow_crispasr_read_source(recovered$path),
    artifact$source_url
  )

  writeLines(
    paste0(
      "https://huggingface.co/test/model/resolve/main/",
      artifact$filename
    ),
    paste0(result$path, ".src")
  )
  migrated <- genflow:::.genflow_crispasr_download(
    selector = "hf://test/model:test-model-q4_k.gguf",
    backend = "test"
  )
  expect_true(migrated$cached)
  expect_identical(
    genflow:::.genflow_crispasr_read_source(migrated$path),
    artifact$source_url
  )

  writeBin(as.raw(4:1), result$path)
  expect_error(
    genflow:::.genflow_crispasr_download(
      selector = "hf://test/model:test-model-q4_k.gguf",
      backend = "test"
    ),
    "SHA-256"
  )
  expect_true(file.exists(paste0(result$path, ".src")))

  artifact$filename <- "wrong-hash-q8_0.gguf"
  artifact$source_url <- paste0(
    "https://huggingface.co/test/model/resolve/",
    strrep("c", 40),
    "/",
    artifact$filename
  )
  artifact$size_bytes <- 4
  artifact$sha256 <- strrep("0", 64)
  expect_error(
    genflow:::.genflow_crispasr_download(
      selector = "hf://test/model:wrong-hash-q8_0.gguf",
      backend = "test"
    ),
    "SHA-256"
  )
  expect_false(file.exists(file.path(cache_dir, artifact$filename)))

  artifact$filename <- "wrong-size-q8_0.gguf"
  artifact$source_url <- paste0(
    "https://huggingface.co/test/model/resolve/",
    strrep("c", 40),
    "/",
    artifact$filename
  )
  artifact$size_bytes <- 5
  artifact$sha256 <- strrep("4", 64)
  expect_error(
    genflow:::.genflow_crispasr_download(
      selector = "hf://test/model:wrong-size-q8_0.gguf",
      backend = "test"
    ),
    "size does not match"
  )
  expect_false(file.exists(file.path(cache_dir, artifact$filename)))
  expect_false(any(grepl(
    "wrong-size.*\\.part\\.",
    list.files(cache_dir, all.files = TRUE)
  )))
})

test_that("CrispASR rolls back a downloaded payload when sidecar install fails", {
  cache_dir <- tempfile("genflow-crispasr-sidecar-rollback-")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)
  withr::local_envvar(c(
    CRISPASR_CACHE_DIR = cache_dir,
    CRISPASR_MODELS_DIR = NA
  ))

  filename <- "rollback-model-q4_k.gguf"
  artifact <- list(
    filename = filename,
    source_url = paste0(
      "https://huggingface.co/test/model/resolve/",
      strrep("d", 40),
      "/",
      filename
    ),
    size_bytes = 4,
    sha256 = paste0(
      "9f64a747e1b97f131fabb6b447296c9b6",
      "f0201e79fb3c5356e6c77e89b6a806a"
    )
  )
  testthat::local_mocked_bindings(
    .genflow_crispasr_resolve_download = function(selector,
                                                  backend = "",
                                                  quant = "",
                                                  executable = "") {
      artifact
    },
    .genflow_crispasr_fetch = function(url,
                                       destination,
                                       expected_size,
                                       filename,
                                       progress = NULL,
                                       timeout = 3600) {
      writeBin(as.raw(1:4), destination)
      invisible(4)
    },
    .genflow_crispasr_write_source = function(path, source_url) {
      stop("simulated sidecar failure", call. = FALSE)
    },
    .package = "genflow"
  )

  expect_error(
    genflow:::.genflow_crispasr_download(
      selector = paste0("hf://test/model:", filename),
      backend = "test"
    ),
    "payload was rolled back"
  )
  expect_false(file.exists(file.path(cache_dir, filename)))
  expect_false(file.exists(file.path(cache_dir, paste0(filename, ".src"))))
  expect_false(any(grepl(
    "\\.part\\.",
    list.files(cache_dir, all.files = TRUE)
  )))
})

test_that("CrispASR throttles byte-level progress callbacks", {
  now <- 0
  updates <- list()
  report <- genflow:::.genflow_crispasr_progress_throttler(
    progress = function(update) {
      updates[[length(updates) + 1L]] <<- update
    },
    filename = "model.gguf",
    bytes_total = 100 * 1024^2,
    clock = function() now
  )

  report(1024)
  now <- 0.1
  report(2 * 1024^2)
  expect_length(updates, 0L)

  report(9 * 1024^2)
  expect_length(updates, 1L)
  now <- 0.2
  report(10 * 1024^2)
  expect_length(updates, 1L)

  now <- 0.4
  report(11 * 1024^2)
  expect_length(updates, 2L)
  report(100 * 1024^2)
  expect_length(updates, 3L)
  expect_identical(updates[[3]]$proportion, 1)
})

test_that("CrispASR cache and Hugging Face credentials use safe defaults", {
  withr::local_envvar(c(
    HF_TOKEN = NA,
    HUGGING_FACE_HUB_TOKEN = NA,
    HUGGINGFACE_API_TOKEN = "api-token"
  ))
  expect_identical(genflow:::.genflow_crispasr_hf_token(), "api-token")

  withr::local_envvar(HF_TOKEN = "preferred-token")
  expect_identical(
    genflow:::.genflow_crispasr_hf_token(),
    "preferred-token"
  )

  withr::local_envvar(CRISPASR_CACHE_DIR = "/")
  expect_error(
    genflow:::.genflow_crispasr_canonical_cache_dir(create = FALSE),
    "root or home"
  )
  withr::local_envvar(CRISPASR_CACHE_DIR = path.expand("~"))
  expect_error(
    genflow:::.genflow_crispasr_canonical_cache_dir(create = FALSE),
    "root or home"
  )
})

test_that("CrispASR cleans only stale payload and sidecar part files", {
  cache_dir <- tempfile("genflow-crispasr-stale-parts-")
  dir.create(cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)
  withr::local_envvar(c(
    CRISPASR_CACHE_DIR = cache_dir,
    CRISPASR_MODELS_DIR = NA
  ))

  filename <- "managed-model-q4_k.gguf"
  dead_pid <- file.path(
    cache_dir,
    paste0(".", filename, ".part.999999.dead")
  )
  live_pid <- file.path(
    cache_dir,
    paste0(".", filename, ".part.4242.live")
  )
  old_legacy <- file.path(cache_dir, paste0(filename, ".part.legacy"))
  old_sidecar <- file.path(
    cache_dir,
    paste0(".", filename, ".src.part.legacy")
  )
  fresh_sidecar <- file.path(
    cache_dir,
    paste0(".", filename, ".src.part.fresh")
  )
  unrelated <- file.path(cache_dir, ".another-model.gguf.part.999999.dead")
  for (path in c(
    dead_pid,
    live_pid,
    old_legacy,
    old_sidecar,
    fresh_sidecar,
    unrelated
  )) {
    writeBin(as.raw(1), path)
  }
  Sys.setFileTime(c(old_legacy, old_sidecar), Sys.time() - 120)

  testthat::local_mocked_bindings(
    .genflow_crispasr_pid_alive = function(pid) identical(pid, 4242L),
    .package = "genflow"
  )
  result <- genflow:::.genflow_crispasr_cleanup_stale_parts(
    cache_dir = cache_dir,
    filename = filename,
    stale_after = 60,
    now = Sys.time()
  )

  expect_false(any(file.exists(c(dead_pid, old_legacy, old_sidecar))))
  expect_true(all(file.exists(c(live_pid, fresh_sidecar, unrelated))))
  expect_setequal(
    basename(result$removed),
    basename(c(dead_pid, old_legacy, old_sidecar))
  )
  expect_setequal(
    basename(result$active),
    basename(c(live_pid, fresh_sidecar))
  )
})

test_that("CrispASR removal is exact and confined to its managed cache", {
  cache_dir <- tempfile("genflow-crispasr-remove-")
  external_dir <- tempfile("genflow-crispasr-remove-external-")
  dir.create(cache_dir)
  dir.create(external_dir)
  on.exit(unlink(c(cache_dir, external_dir), recursive = TRUE), add = TRUE)
  withr::local_envvar(c(
    CRISPASR_CACHE_DIR = cache_dir,
    CRISPASR_MODELS_DIR = NA
  ))

  filename <- "managed-model-q4_k.gguf"
  model <- file.path(cache_dir, filename)
  source_url <- paste0(
    "https://huggingface.co/test/model/resolve/main/",
    filename
  )
  writeBin(as.raw(1:4), model)
  writeChar(source_url, paste0(model, ".src"), eos = NULL)
  external <- file.path(external_dir, "external-q4_k.gguf")
  writeBin(as.raw(1:4), external)

  expect_error(
    genflow:::.genflow_crispasr_remove_model(model, active_model = model),
    "selected"
  )
  expect_error(
    genflow:::.genflow_crispasr_remove_model(
      model,
      active_model = paste0("hf://test/model:", filename)
    ),
    "selected"
  )
  writeChar(
    paste0(
      "https://huggingface.co/test/model/resolve/",
      strrep("a", 40),
      "/",
      filename
    ),
    paste0(model, ".src"),
    eos = NULL
  )
  expect_error(
    genflow:::.genflow_crispasr_remove_model(
      model,
      active_model = paste0("hf://test/model:", filename)
    ),
    "selected"
  )

  active_part <- file.path(cache_dir, paste0(".", filename, ".part.job"))
  writeBin(as.raw(1), active_part)
  expect_error(
    genflow:::.genflow_crispasr_remove_model(model),
    "active download"
  )
  stale_sidecar_part <- file.path(
    cache_dir,
    paste0(".", filename, ".src.part.legacy")
  )
  writeBin(as.raw(1), stale_sidecar_part)
  Sys.setFileTime(
    c(active_part, stale_sidecar_part),
    Sys.time() - 7200
  )

  expect_error(
    genflow:::.genflow_crispasr_remove_model(external),
    "managed CrispASR cache"
  )
  expect_error(
    genflow:::.genflow_crispasr_remove_model(
      file.path(cache_dir, "nested", "..", filename)
    ),
    "absolute flat path"
  )
  expect_true(genflow:::.genflow_crispasr_remove_model(model))
  expect_false(file.exists(model))
  expect_false(file.exists(paste0(model, ".src")))
  expect_false(file.exists(active_part))
  expect_false(file.exists(stale_sidecar_part))
  expect_true(file.exists(external))
})
