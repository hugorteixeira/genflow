local_management_scope <- function(.local_envir = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = .local_envir)
  withr::local_options(list(
    genflow.cache_dir = file.path(root, "objects"),
    genflow.models_dir = file.path(root, "catalog"),
    genflow.local_config_path = file.path(root, "local.json"),
    genflow.providers_path = file.path(root, "providers.json"),
    genflow.renviron_path = file.path(root, ".Renviron")
  ), .local_envir = .local_envir)
  withr::local_envvar(c(CRISPASR_CACHE_DIR = file.path(root, "native"),
    OPENAI_API_KEY = NA, OPENAI_BASE_URL = NA, GOOGLE_API_KEY = NA,
    GEMINI_API_KEY = NA, GENFLOW_MANAGEMENT_TEST_KEY = NA), .local_envir = .local_envir)
  testthat::local_mocked_bindings(
    .genflow_env_source_paths = function(path = NULL)
      c(user_renviron = path %||% getOption("genflow.renviron_path")),
    .genflow_agent_app = function(...) stop("Management must not construct Shiny"),
    .package = "genflow", .env = .local_envir)
  root
}

test_that("public credential reads expose state without returning secret values", {
  root <- local_management_scope()
  secret <- "management-secret-abcdefghij"
  saved <- gen_save_credentials(list(OPENAI_API_KEY = secret), set_session = FALSE)
  state <- gen_credentials("openai")
  key <- state$fields[state$fields$env == "OPENAI_API_KEY", ]
  expect_identical(state$schema_version, 1L)
  expect_identical(state$path, file.path(root, ".Renviron"))
  expect_false(key$active)
  expect_true(key$detected)
  expect_identical(key$source, "user_renviron")
  expect_true("openai" %in% state$missing$provider)
  detected <- gen_detect_credentials("openai", paths = c(saved = state$path))
  expect_false("value" %in% names(detected))
  expect_false(grepl(secret, jsonlite::toJSON(list(state, saved, detected)), fixed = TRUE))
  expect_identical(nrow(gen_detect_credentials("openai", paths = character())), 0L)
  imported <- gen_import_credentials(detected)
  expect_identical(Sys.getenv("OPENAI_API_KEY"), secret)
  expect_length(gen_credentials("openai")$missing$provider, 0L)
  expect_false(grepl(secret, jsonlite::toJSON(imported), fixed = TRUE))
  expect_identical(names(gen_credentials("local-native")$fields), names(state$fields))
  expect_equal(nrow(gen_credentials("local-native")$fields), 0L)
  expect_identical(unique(gen_credentials("llama-cpp")$fields$provider), "llamacpp")
})

test_that("credential writes preserve unrelated data and support explicit clearing", {
  local_management_scope()
  path <- getOption("genflow.renviron_path")
  writeLines(c("# keep", "UNRELATED=keep", "OPENAI_API_KEY=previous"), path)
  saved <- gen_save_credentials(c(OPENAI_API_KEY = "new value # with spaces"))
  expect_identical(Sys.getenv("OPENAI_API_KEY"), "new value # with spaces")
  expect_true(file.exists(saved$backup_path))
  expect_true(all(c("# keep", "UNRELATED=keep") %in% readLines(path)))
  if (.Platform$OS.type != "windows") {
    expect_identical(as.character(file.info(path)$mode), "600")
    expect_identical(as.character(file.info(saved$backup_path)$mode), "600")
  }
  gen_save_credentials(c(OPENAI_API_KEY = ""), backup = FALSE)
  expect_identical(Sys.getenv("OPENAI_API_KEY"), "")
  deleted <- gen_delete_credentials("OPENAI_API_KEY")
  expect_identical(deleted$removed, "OPENAI_API_KEY")
  expect_identical(readLines(path), c("# keep", "UNRELATED=keep"))
  expect_false(dir.exists(paste0(path, ".lock")))
})

test_that("credential imports recheck every source before writing anything", {
  root <- local_management_scope()
  source <- file.path(root, "source.env")
  writeLines(c("OPENAI_API_KEY=first-value", "OPENAI_BASE_URL=http://localhost:9999"), source)
  selected <- gen_detect_credentials("openai", paths = c(source = source))
  writeLines(c("OPENAI_API_KEY=changed-value", "OPENAI_BASE_URL=http://localhost:9999"), source)
  error <- tryCatch(gen_import_credentials(selected), error = identity)
  expect_s3_class(error, "genflow_error")
  expect_identical(gen_diagnostic(error)$code, "management_source_changed")
  expect_false(file.exists(getOption("genflow.renviron_path")))
  expect_identical(Sys.getenv("OPENAI_API_KEY"), "")
  selected <- gen_detect_credentials("openai", paths = c(source = source))
  gen_import_credentials(selected, set_session = FALSE)
  expect_identical(Sys.getenv("OPENAI_API_KEY"), "")
  expect_true(file.exists(getOption("genflow.renviron_path")))
  selected <- rbind(selected, selected[1L, ])
  expect_error(gen_import_credentials(selected), class = "genflow_error")
})

test_that("invalid credential arguments fail without writes and expose canonical diagnostics", {
  local_management_scope()
  invalid <- list(character(), "unnamed", c(UNREGISTERED_SECRET = "secret"),
    list(OPENAI_API_KEY = 4), c(OPENAI_API_KEY = NA_character_),
    c(OPENAI_API_KEY = "a\nb"), c(OPENAI_API_KEY = "a", OPENAI_API_KEY = "b"))
  for (value in invalid) {
    error <- tryCatch(gen_save_credentials(value), error = identity)
    expect_s3_class(error, "genflow_error")
    diagnostic <- gen_diagnostic(error)
    expect_identical(diagnostic$code, "management_argument")
    expect_identical(diagnostic$stage, "management")
    expect_match(diagnostic$path, "^/(vars|values)$")
  }
  expect_error(gen_credentials("unregistered"), class = "genflow_error")
  expect_error(gen_detect_credentials(paths = "unnamed"), class = "genflow_error")
  expect_error(gen_save_credentials(c(OPENAI_API_KEY = "x"), path = character()), class = "genflow_error")
  expect_error(gen_save_credentials(c(OPENAI_API_KEY = "x"), backup = NA), class = "genflow_error")
  expect_false(file.exists(getOption("genflow.renviron_path")))
})

test_that("custom provider credential metadata uses the existing public registry", {
  local_management_scope()
  set_provider_openai_compat("management-test", base_url = "http://127.0.0.1:9999",
    api_key_env = "GENFLOW_MANAGEMENT_TEST_KEY", api_key_required = TRUE)
  expect_true("management-test" %in% list_providers()$id)
  expect_true(list_providers(include_builtin = FALSE)$catalog_update)
  builtins <- list_providers(include_custom = FALSE)
  expect_true(builtins$catalog_update[builtins$id == "openai"])
  expect_false(builtins$catalog_update[builtins$id == "voicegain"])
  expect_type(list_providers(FALSE, FALSE)$catalog_update, "logical")
  fields <- gen_credentials("management-test")$fields
  expect_identical(fields$env, "GENFLOW_MANAGEMENT_TEST_KEY")
  gen_save_credentials(c(GENFLOW_MANAGEMENT_TEST_KEY = "custom-secret-value"))
  expect_true(gen_credentials("management-test")$fields$active)
  expect_equal(nrow(gen_credentials("management-test")$missing), 0L)
  gen_delete_credentials("GENFLOW_MANAGEMENT_TEST_KEY")
  rm_provider("management-test")
  expect_false("GENFLOW_MANAGEMENT_TEST_KEY" %in% gen_credentials()$fields$env)
})

test_that("current-session imports and short secrets remain safe", {
  local_management_scope()
  gen_save_credentials(c(OPENAI_API_KEY = "abc"))
  selected <- gen_detect_credentials("openai", paths = character())
  expect_identical(selected$source, "current_session")
  expect_identical(selected$masked, "********")
  gen_import_credentials(selected)
  expect_identical(Sys.getenv("OPENAI_API_KEY"), "abc")
  Sys.setenv(OPENAI_API_KEY = "rotated")
  expect_error(gen_import_credentials(selected), class = "genflow_error")
  expect_identical(Sys.getenv("OPENAI_API_KEY"), "rotated")
})

test_that("favorite publication failures preserve data and release their lock", {
  local_management_scope()
  models <- data.frame(service = "openai", model = "test-model")
  gen_model_favorites(models)
  path <- file.path(getOption("genflow.models_dir"), "favorites.rds")
  before <- readRDS(path)
  testthat::local_mocked_bindings(.genflow_atomic_save_rds = function(...)
    stop("simulated publication failure"), .package = "genflow")
  expect_error(gen_model_favorites(models[FALSE, ]), class = "genflow_error")
  expect_identical(readRDS(path), before)
  expect_false(dir.exists(paste0(path, ".lock")))
})

test_that("catalog and favorites share canonical data without display truncation", {
  local_management_scope()
  directory <- getOption("genflow.models_dir")
  expect_named(gen_models(), c("service", "model", "type", "pricing", "description", "source_file"))
  expect_false(dir.exists(directory))
  dir.create(directory)
  description <- paste(rep("full description", 100L), collapse = " ")
  write.csv(data.frame(provider = "huggingface", id = "owner/model", type = "Audio",
    description = description), file.path(directory, "hf.csv"), row.names = FALSE)
  expect_output(catalog <- gen_models(), NA)
  expect_identical(catalog$service, "hf")
  expect_identical(catalog$description, description)
  expect_identical(gen_models("huggingface"), catalog)
  favorites <- gen_model_favorites(data.frame(service = "huggingface", model = "owner/model"))
  expect_identical(favorites$service, "hf")
  expect_identical(favorites$type, "Audio")
  path <- file.path(directory, "favorites.rds")
  expect_identical(readRDS(path), favorites)
  before <- readBin(path, "raw", n = file.info(path)$size)
  expect_identical(gen_model_favorites(), favorites)
  gen_model_favorites(favorites[FALSE, ], save = FALSE)
  expect_identical(readBin(path, "raw", n = file.info(path)$size), before)
  expect_error(gen_model_favorites(data.frame(service = "hf", model = 1)), class = "genflow_error")
  expect_error(gen_model_favorites(favorites, catalog = list()), class = "genflow_error")
  expect_identical(readBin(path, "raw", n = file.info(path)$size), before)
  gen_model_favorites(favorites[FALSE, ])
  expect_equal(nrow(gen_model_favorites()), 0L)
  expect_false(dir.exists(paste0(path, ".lock")))
})

test_that("native inventory and local inspection do not load models or require a UI", {
  root <- local_management_scope()
  cache <- Sys.getenv("CRISPASR_CACHE_DIR")
  expect_equal(nrow(gen_native_models(cache_dirs = cache)), 0L)
  dir.create(cache)
  model <- file.path(cache, "tiny-q8_0.gguf")
  writeBin(charToRaw("GGUF"), model)
  writeLines("https://huggingface.co/owner/repo/resolve/main/tiny-q8_0.gguf", paste0(model, ".src"))
  inventory <- gen_native_models(cache_dirs = cache)
  expect_identical(inventory$repository, "owner/repo")
  expect_identical(inventory$size, "4 B")
  expect_identical(gen_native_models(config = gen_local_config(), cache_dirs = cache), inventory)
  expect_true(inventory$managed)
  expect_identical(gen_native_models(cache_dirs = cache, managed_only = TRUE), inventory)
  checked <- gen_native_model_check(model)
  expect_identical(checked$size_bytes, 4)
  expect_identical(checked$sha256, unclass(as.character(openssl::sha256(charToRaw("GGUF")))))
  expect_true(gen_native_model_download(model)$cached)
  expect_identical(gen_native_model_reference("hf download hf://owner/repo/tiny-q8_0.gguf"),
    "hf://owner/repo:tiny-q8_0.gguf")
  expect_identical(gen_native_model_reference("https://huggingface.co/owner/repo/blob/main/tiny-q8_0.gguf"),
    "hf://owner/repo:tiny-q8_0.gguf")
  expect_error(gen_native_model_reference("https://example.com/model.gguf"), class = "genflow_error")
  expect_error(gen_native_download_status(list()), class = "genflow_error")
  if (.Platform$OS.type != "windows") {
    link <- file.path(root, "linked.gguf")
    expect_true(file.symlink(model, link))
    expect_error(gen_native_model_download(link), class = "genflow_error")
    expect_error(gen_native_model_check(link), class = "genflow_error")
    expect_error(gen_native_model_remove(link), class = "genflow_error")
  }
  expect_true(file.exists(model))
})

test_that("remote checks resolve metadata without starting downloads", {
  local_management_scope()
  resolved <- NULL
  testthat::local_mocked_bindings(
    .genflow_crispasr_resolve_download = function(selector, backend, quant, executable) {
      resolved <<- list(selector, backend, quant, executable)
      list(filename = "tiny.gguf", revision = strrep("a", 40), size_bytes = 4, sha256 = strrep("b", 64))
    },
    .genflow_crispasr_download = function(...) stop("Unexpected download"),
    .genflow_native_download_job_start = function(...) stop("Unexpected worker"),
    .package = "genflow")
  value <- gen_native_model_check("hf://owner/repo:tiny.gguf")
  expect_identical(resolved, list("hf://owner/repo:tiny.gguf", "", "", ""))
  expect_identical(value$revision, strrep("a", 40))
  expect_false(dir.exists(Sys.getenv("CRISPASR_CACHE_DIR")))
  expect_error(gen_native_model_download("auto", background = TRUE, progress = function(x) x), class = "genflow_error")
})

test_that("public status rereads a worker that finishes between status and liveness", {
  local_management_scope()
  reads <- 0L
  job <- structure(list(id = "fake", dir = tempdir(), status_path = tempfile()),
    class = "genflow_native_download_job")
  testthat::local_mocked_bindings(
    .genflow_native_download_job_read = function(job) {
      reads <<- reads + 1L
      list(state = if (reads == 1L) "running" else "complete")
    }, .genflow_native_download_job_alive = function(job) FALSE, .package = "genflow")
  status <- gen_native_download_status(job)
  expect_identical(status$state, "complete")
  expect_false(status$running)
  expect_identical(reads, 2L)
})

test_that("Shiny credential operations consume the public API without retaining detected secrets", {
  local_management_scope()
  saved <- gen_save_credentials
  imported <- gen_import_credentials
  deleted <- gen_delete_credentials
  calls <- c(save = 0L, import = 0L, delete = 0L)
  testthat::local_mocked_bindings(
    gen_save_credentials = function(...) { calls[["save"]] <<- calls[["save"]] + 1L; saved(...) },
    gen_import_credentials = function(...) { calls[["import"]] <<- calls[["import"]] + 1L; imported(...) },
    gen_delete_credentials = function(...) { calls[["delete"]] <<- calls[["delete"]] + 1L; deleted(...) },
    .genflow_crispasr_inventory = function(...) genflow:::.genflow_crispasr_empty_inventory(),
    .package = "genflow")
  shiny::testServer(genflow:::server, {
    session$flushReact()
    session$setInputs(credentials_provider = "openai", credentials_edit = 1L)
    session$setInputs(credential_value_OPENAI_API_KEY = "shiny-secret-value", credentials_save = 1L)
    expect_identical(calls[["save"]], 1L)
    expect_identical(Sys.getenv("OPENAI_API_KEY"), "shiny-secret-value")
    Sys.unsetenv("OPENAI_API_KEY")
    session$setInputs(credentials_import = 1L)
    selected <- reactiveValuesToList(credentials_state)$import_values
    expect_s3_class(selected, "data.frame")
    expect_false("value" %in% names(selected))
    expect_false(grepl("shiny-secret-value", jsonlite::toJSON(selected), fixed = TRUE))
    session$setInputs(credentials_import_confirm = 1L)
    expect_identical(calls[["import"]], 1L)
    expect_identical(Sys.getenv("OPENAI_API_KEY"), "shiny-secret-value")
    session$setInputs(credentials_delete = 1L, credentials_delete_confirm = 1L)
    expect_identical(calls[["delete"]], 1L)
    expect_identical(Sys.getenv("OPENAI_API_KEY"), "")
  })
})

test_that("public removal protects saved configurations, setups, agents and caller references", {
  root <- local_management_scope()
  cache <- Sys.getenv("CRISPASR_CACHE_DIR")
  dir.create(cache)
  model <- file.path(cache, "protected.gguf")
  writeBin(charToRaw("GGUF"), model)
  gen_local_config(stt_native_model = model)
  expect_error(gen_native_model_remove(model, config = list(stt_native_model = "")), class = "genflow_error")
  gen_local_config(stt_native_model = "")
  setup <- set_setup("setup - descriptive name", "local-native", model, type = "audio", assign = FALSE)
  expect_error(gen_native_model_remove(model), class = "genflow_error")
  set_agent("agent - descriptive name", setup, assign = FALSE)
  rm_setup("setup - descriptive name", force = TRUE)
  expect_error(gen_native_model_remove(model), class = "genflow_error")
  rm_agent("agent - descriptive name")
  expect_error(gen_native_model_remove(model, protected_models = "protected.gguf"), class = "genflow_error")
  part <- file.path(cache, paste0(".protected.gguf.part.", Sys.getpid(), ".test"))
  writeLines("active", part)
  expect_error(gen_native_model_remove(model), class = "genflow_error")
  expect_true(file.exists(part))
  unlink(part)
  # A damaged saved object must block deletion, not be silently skipped.
  broken <- file.path(getOption("genflow.cache_dir"), "setups", "broken.rds")
  writeLines("not RDS", broken)
  expect_error(gen_native_model_remove(model), class = "genflow_error")
  expect_true(file.exists(model))
  unlink(broken)
  expect_true(gen_native_model_remove(model))
  expect_false(file.exists(model))
})

test_that("public background jobs run in another process and preserve completed output", {
  root <- local_management_scope()
  model <- file.path(root, "tiny.gguf")
  writeBin(charToRaw("GGUF"), model)
  job <- gen_native_model_download(model, background = TRUE)
  on.exit({
    if (dir.exists(job$dir)) {
      try(gen_native_download_cancel(job), silent = TRUE)
      try(gen_native_download_cleanup(job), silent = TRUE)
    }
  }, add = TRUE)
  expect_s3_class(job, "genflow_native_download_job")
  deadline <- Sys.time() + 15
  repeat {
    status <- gen_native_download_status(job)
    if (!status$running || Sys.time() >= deadline) break
    Sys.sleep(0.05)
  }
  expect_false(status$running)
  expect_identical(status$state, "complete")
  expect_identical(status$result$path, normalizePath(model))
  expect_true(jsonlite::validate(jsonlite::toJSON(status, auto_unbox = TRUE)))
  expect_false("process" %in% names(status))
  expect_identical(gen_native_download_cancel(job)$state, "complete")
  expect_true(gen_native_download_cleanup(job))
  expect_false(dir.exists(job$dir))
  expect_true(file.exists(model))
})

test_that("public download failures never expose raw worker stderr", {
  local_management_scope()
  job <- structure(list(id = "fake", dir = tempdir(), status_path = tempfile()),
    class = "genflow_native_download_job")
  testthat::local_mocked_bindings(
    .genflow_native_download_job_read = function(job) list(state = "error", stage = "resolving",
      message = "Authorization: Bearer private-token", error = list(stderr = "private-token")),
    .genflow_native_download_job_alive = function(job) FALSE, .package = "genflow")
  status <- suppressMessages(gen_native_download_status(job))
  expect_identical(status$state, "error")
  expect_identical(status$error$diagnostic_version, 1L)
  expect_identical(status$error$stage, "download")
  expect_true(nzchar(status$error$reference))
  expect_false(grepl("private-token", jsonlite::toJSON(status), fixed = TRUE))
})


test_that("loading credentials updates only registered fields without interpolation or writes", {
  root <- local_management_scope()
  path <- getOption("genflow.renviron_path")
  withr::local_envvar(c(OPENAI_API_KEY = "inherited", GENFLOW_MANAGEMENT_TEST_KEY = "previous", UNRELATED = "keep"))
  expect_length(gen_load_credentials()$loaded, 0L)
  set_provider_openai_compat("management-test", base_url = "http://127.0.0.1:9999",
    api_key_env = "GENFLOW_MANAGEMENT_TEST_KEY", api_key_required = TRUE)
  writeLines(c("# preserve", "UNRELATED=must-not-load", "OPENAI_API_KEY=old"), path)
  literal <- "literal-${UNRELATED}-$(must-not-run)-`literal`-credential"
  gen_save_credentials(list(OPENAI_API_KEY = literal, GENFLOW_MANAGEMENT_TEST_KEY = ""), set_session = FALSE)
  before <- readLines(path)
  result <- gen_load_credentials()
  expect_setequal(result$loaded, c("OPENAI_API_KEY", "GENFLOW_MANAGEMENT_TEST_KEY"))
  expect_identical(Sys.getenv("OPENAI_API_KEY"), literal)
  expect_identical(Sys.getenv("GENFLOW_MANAGEMENT_TEST_KEY"), "")
  expect_identical(Sys.getenv("UNRELATED"), "keep")
  expect_identical(readLines(path), before)
  expect_false(grepl(literal, jsonlite::toJSON(result), fixed = TRUE))
  expect_error(gen_load_credentials(character()), class = "genflow_error")
})
