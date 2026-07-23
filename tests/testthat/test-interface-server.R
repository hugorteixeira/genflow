interface_test_scope <- function() {
  root <- tempfile("genflow-interface-")
  dir.create(root)
  old_options <- options(
    genflow.cache_dir = file.path(root, "cache"),
    genflow.local_config_path = file.path(root, "local-inference.json")
  )
  old_xdg <- Sys.getenv("XDG_DATA_HOME", unset = NA_character_)
  Sys.setenv(XDG_DATA_HOME = file.path(root, "data"))

  function() {
    options(old_options)
    if (is.na(old_xdg)) {
      Sys.unsetenv("XDG_DATA_HOME")
    } else {
      Sys.setenv(XDG_DATA_HOME = old_xdg)
    }
    unlink(root, recursive = TRUE)
  }
}

test_that("static interface input and output ids are unique", {
  html <- htmltools::renderTags(genflow:::.app_ui())$html
  matches <- regmatches(
    html,
    gregexpr(
      "(?<![-[:alnum:]_])id=\"[^\"]+\"",
      html,
      perl = TRUE
    )
  )[[1]]
  ids <- sub('^id="', "", matches)
  ids <- sub('"$', "", ids)

  expect_length(ids, length(unique(ids)))
  expect_true(all(c(
    "main_tabs",
    "models_update_provider",
    "local_config_save",
    "local_diagnostics_run"
  ) %in% ids))
})

test_that("model catalogs tolerate heterogeneous columns and invalid rows", {
  restore <- interface_test_scope()
  on.exit(restore(), add = TRUE)
  directory <- tempfile("genflow-models-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)

  write.csv(
    data.frame(
      service = c("openai", NA_character_),
      model = c("model-a", NA_character_),
      provider_only = c("a", "invalid")
    ),
    file.path(directory, "openai.csv"),
    row.names = FALSE
  )
  write.csv(
    data.frame(
      service = "huggingface",
      model = "model-b",
      another_provider_column = 1
    ),
    file.path(directory, "hf.csv"),
    row.names = FALSE
  )

  catalog <- genflow:::.load_models_catalog(directory)
  expect_identical(
    names(catalog),
    c("service", "model", "type", "pricing", "description", "source_file")
  )
  expect_identical(catalog$model, c("model-b", "model-a"))
  expect_identical(catalog$service, c("hf", "openai"))
  expect_false(anyNA(genflow:::.model_service_choices(catalog)))
  expect_false(anyNA(genflow:::.model_model_choices(catalog, "hf")))

  favorites <- data.frame(
    service = "openai",
    model = "model-a",
    type = NA_character_
  )
  normalized <- genflow:::.normalize_favorites(favorites, data.frame())
  expect_identical(normalized$type, "")
})

test_that("server starts and NULL form inputs do not terminate observers", {
  restore <- interface_test_scope()
  on.exit(restore(), add = TRUE)

  shiny::testServer(genflow:::server, {
    session$flushReact()

    expect_type(output$models_status, "character")
    expect_type(output$models_summary, "character")
    expect_type(output$local_config_status, "character")
    expect_type(output$models_table, "character")
    diagnostics_table <- output$local_diagnostics_table
    expect_type(diagnostics_table, "character")
    expect_match(diagnostics_table, '"autoWidth":false', fixed = TRUE)
    expect_match(diagnostics_table, '"width":"62%"', fixed = TRUE)

    caught <- character()
    withCallingHandlers(
      {
        session$setInputs(setup_save = 1)
        session$flushReact()
        session$setInputs(content_save = 1)
        session$flushReact()
        session$setInputs(models_update_all = 1)
        session$flushReact()
        session$setInputs(models_update_selected = 1)
        session$flushReact()
        session$setInputs(setup_extra_action = list())
        session$flushReact()
      },
      warning = function(w) {
        caught <<- c(caught, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    expect_length(caught, 0L)
  })
})

test_that("initial local config updates never read reactive state implicitly", {
  config <- genflow:::.genflow_local_config_defaults()
  local_state <- shiny::reactiveValues(config = config)

  expect_error(
    local_state$config,
    "outside of reactive consumer",
    fixed = TRUE
  )

  initial_config <- shiny::isolate(local_state$config)
  session <- shiny::MockShinySession$new()
  on.exit(session$close(), add = TRUE)

  expect_no_error(
    genflow:::.update_local_config_inputs(session, initial_config)
  )
  expect_error(
    genflow:::.update_local_config_inputs(session),
    'argument "config" is missing',
    fixed = TRUE
  )
})

test_that("first real Shiny flush initializes local configuration", {
  restore <- interface_test_scope()
  on.exit(restore(), add = TRUE)

  shiny:::initCurrentAppState(genflow:::genflow_agent_app)
  on.exit(shiny:::clearCurrentAppState(), add = TRUE)

  websocket <- new.env(parent = emptyenv())
  websocket$request <- list(HTTP_SHINY_SERVER_CREDENTIALS = NULL)
  websocket$send <- function(message) invisible(NULL)

  session <- shiny:::ShinySession$new(websocket)
  on.exit(session$wsClosed(), add = TRUE)

  shiny::withReactiveDomain(session, {
    genflow:::server(session$input, session$output, session)
  })
  shiny:::flushReact()

  expect_no_error(session$flushOutput())
})

test_that("local inference save preserves uninitialized fields and diagnostics are mockable", {
  restore <- interface_test_scope()
  on.exit(restore(), add = TRUE)
  config_path <- getOption("genflow.local_config_path")
  config <- genflow:::.genflow_local_config_defaults()
  config$python <- "/custom/python"
  config$hf_stt_model <- "owner/custom-stt"
  config$hf_revision <- "reviewed-model-commit"
  config$ollama_base_url <- "http://127.0.0.1:22434"
  config$stt_native_engine <- "crispasr"
  config$stt_native_executable <- "/custom/crispasr"
  config$stt_native_model <- "/models/whisper.gguf"
  config$stt_native_backend <- "whisper"
  config$stt_native_device <- "vulkan"
  genflow:::.genflow_write_local_config(config, config_path)

  diagnostics_args <- NULL
  testthat::local_mocked_bindings(
    gen_local_diagnostics = function(...) {
      diagnostics_args <<- list(...)
      data.frame(
        component = "mock-backend",
        status = "ok",
        detail = "mocked without network access",
        stringsAsFactors = FALSE
      )
    },
    .package = "genflow"
  )

  shiny::testServer(genflow:::server, {
    session$flushReact()
    session$setInputs(local_config_save = 1)
    session$flushReact()

    saved <- genflow:::.genflow_read_local_config(config_path)
    expect_identical(saved$python, config$python)
    expect_identical(saved$hf_stt_model, config$hf_stt_model)
    expect_identical(saved$hf_revision, config$hf_revision)
    expect_identical(saved$ollama_base_url, config$ollama_base_url)
    expect_identical(saved$stt_native_engine, config$stt_native_engine)
    expect_identical(saved$stt_native_executable, config$stt_native_executable)
    expect_identical(saved$stt_native_model, config$stt_native_model)
    expect_identical(saved$stt_native_backend, config$stt_native_backend)
    expect_identical(saved$stt_native_device, config$stt_native_device)

    session$setInputs(
      local_hf_revision = "ui-selected-commit",
      local_config_save = 2
    )
    session$flushReact()
    saved <- genflow:::.genflow_read_local_config(config_path)
    expect_identical(saved$hf_revision, "ui-selected-commit")

    session$setInputs(
      local_stt_native_engine = "moss-transcribe",
      local_stt_native_executable = "/custom/moss-transcribe",
      local_stt_native_model = "/models/moss.gguf",
      local_stt_native_backend = "",
      local_stt_native_device = "cpu",
      local_config_save = 3
    )
    session$flushReact()
    saved <- genflow:::.genflow_read_local_config(config_path)
    expect_identical(saved$stt_native_engine, "moss-transcribe")
    expect_identical(saved$stt_native_executable, "/custom/moss-transcribe")
    expect_identical(saved$stt_native_model, "/models/moss.gguf")
    expect_identical(saved$stt_native_backend, "")
    expect_identical(saved$stt_native_device, "cpu")

    session$setInputs(local_diagnostics_run = 1)
    session$flushReact()
    state <- reactiveValuesToList(local_state)
    expect_identical(state$diagnostics$component, "mock-backend")
    expect_match(state$status, "Ollama check completed", fixed = TRUE)
    expect_identical(diagnostics_args$timeout, 10)
    expect_identical(diagnostics_args$adapters, "ollama")
  })
})

test_that("launcher smoke passes the app without opening a browser", {
  seen <- NULL
  testthat::local_mocked_bindings(
    runApp = function(app, launch.browser, host, ...) {
      seen <<- list(
        app = app,
        launch.browser = launch.browser,
        host = host,
        dots = list(...)
      )
      invisible("started")
    },
    .package = "shiny"
  )

  result <- gen_interface(
    launch.browser = FALSE,
    host = "localhost",
    port = 43210
  )

  expect_identical(result, "started")
  expect_s3_class(seen$app, "shiny.appobj")
  expect_false(seen$launch.browser)
  expect_identical(seen$host, "localhost")
  expect_identical(seen$dots$port, 43210)
})
