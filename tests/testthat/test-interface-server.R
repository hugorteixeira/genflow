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
  config$stt_native_quant <- "q8_0"
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
    expect_identical(saved$stt_native_quant, config$stt_native_quant)
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
      local_stt_native_quant = "",
      local_stt_native_device = "cpu",
      local_config_save = 3
    )
    session$flushReact()
    saved <- genflow:::.genflow_read_local_config(config_path)
    expect_identical(saved$stt_native_engine, "moss-transcribe")
    expect_identical(saved$stt_native_executable, "/custom/moss-transcribe")
    expect_identical(saved$stt_native_model, "/models/moss.gguf")
    expect_identical(saved$stt_native_backend, "")
    expect_identical(saved$stt_native_quant, "")
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

test_that("Native STT model manager refreshes, selects, downloads, and deletes safely", {
  restore <- interface_test_scope()
  on.exit(restore(), add = TRUE)

  inventory_calls <- 0L
  download_args <- NULL
  download_status_reads <- 0L
  removed_args <- NULL
  downloaded <- FALSE
  inventory <- data.frame(
    path = c("/cache/granite-q4_k.gguf", "/models/granite-q8_0.gguf"),
    filename = c("granite-q4_k.gguf", "granite-q8_0.gguf"),
    quant = c("q4_k", "q8_0"),
    size_bytes = c(1024, 2048),
    size = c("1 KB", "2 KB"),
    source_url = c(
      "https://huggingface.co/owner/repo/q4",
      "https://huggingface.co/owner/repo/q8"
    ),
    managed = c(TRUE, FALSE),
    selected = c(FALSE, FALSE),
    stringsAsFactors = FALSE
  )

  testthat::local_mocked_bindings(
    .genflow_crispasr_inventory = function(config = NULL) {
      inventory_calls <<- inventory_calls + 1L
      rows <- inventory
      if (isTRUE(downloaded)) {
        rows <- rbind(
          rows,
          data.frame(
            path = "/cache/granite-q8_0.gguf",
            filename = "granite-q8_0.gguf",
            quant = "q8_0",
            size_bytes = 2048,
            size = "2 KB",
            source_url = "https://huggingface.co/owner/repo/q8",
            managed = TRUE,
            selected = FALSE,
            stringsAsFactors = FALSE
          )
        )
      }
      configured <- as.character(config$stt_native_model %||% "")[1]
      rows$selected <- rows$path == configured
      rows
    },
    .genflow_native_download_job_start = function(selector,
                                                  backend = "",
                                                  quant = "",
                                                  executable = "") {
      download_args <<- list(
        selector = selector,
        backend = backend,
        quant = quant,
        executable = executable
      )
      downloaded <<- TRUE
      structure(
        list(
          id = "mock-download",
          stderr_path = tempfile(),
          process = structure(list(), class = "process")
        ),
        class = "genflow_native_download_job"
      )
    },
    .genflow_native_download_job_read = function(job) {
      download_status_reads <<- download_status_reads + 1L
      if (download_status_reads == 1L) {
        return(list(
          state = "queued",
          stage = "queued",
          message = "queued"
        ))
      }
      list(
        state = "complete",
        stage = "complete",
        message = "ready",
        proportion = 1,
        result = list(
          path = "/cache/granite-q8_0.gguf",
          filename = "granite-q8_0.gguf",
          source_url = "https://huggingface.co/owner/repo/q8",
          cached = FALSE,
          size_bytes = 2048
        )
      )
    },
    .genflow_native_download_job_alive = function(job) FALSE,
    .genflow_native_download_job_cleanup = function(job) TRUE,
    .genflow_crispasr_remove_model = function(path, active_model = "") {
      removed_args <<- list(path = path, active_model = active_model)
      TRUE
    },
    .package = "genflow"
  )

  shiny::testServer(genflow:::server, {
    session$flushReact()
    state <- reactiveValuesToList(local_state)
    expect_equal(nrow(state$native_models), 2L)
    expect_gte(inventory_calls, 1L)
    expect_identical(
      state$native_models$filename,
      c("granite-q4_k.gguf", "granite-q8_0.gguf")
    )
    expect_match(output$local_stt_models_summary, "2 files", fixed = TRUE)

    session$setInputs(
      local_stt_models_table_rows_selected = 1L,
      local_stt_model_use = 1L
    )
    session$flushReact()
    state <- reactiveValuesToList(local_state)
    expect_match(state$native_model_status, "Click Save", fixed = TRUE)
    expect_true(state$native_models$selected[[1]])

    session$setInputs(
      local_stt_native_engine = "crispasr",
      local_stt_native_model = "auto",
      local_stt_native_backend = "granite-4.1",
      local_stt_native_quant = "q8_0",
      local_stt_native_executable = "/opt/crispasr",
      local_stt_model_download = 1L
    )
    session$flushReact()
    expect_identical(download_args$selector, "auto")
    expect_identical(download_args$backend, "granite-4.1")
    expect_identical(download_args$quant, "q8_0")
    expect_identical(download_args$executable, "/opt/crispasr")
    state <- reactiveValuesToList(local_state)
    expect_match(state$native_model_status, "Downloaded:", fixed = TRUE)
    expect_match(state$native_model_status, "Click Save", fixed = TRUE)
    expect_identical(
      state$native_models$path[state$native_models$selected],
      "/cache/granite-q8_0.gguf"
    )

    session$setInputs(
      local_stt_models_table_rows_selected = 1L,
      local_stt_model_delete = 1L
    )
    session$flushReact()
    state <- reactiveValuesToList(local_state)
    expect_identical(
      state$native_delete_path,
      "/cache/granite-q4_k.gguf"
    )

    session$setInputs(local_stt_model_delete_confirm = 1L)
    session$flushReact()
    expect_identical(removed_args$path, "/cache/granite-q4_k.gguf")
    expect_identical(removed_args$active_model, "auto")
    state <- reactiveValuesToList(local_state)
    expect_null(state$native_delete_path)
    expect_match(state$native_model_status, "Deleted", fixed = TRUE)

    session$setInputs(
      local_stt_models_table_rows_selected = 2L,
      local_stt_model_delete = 2L
    )
    session$flushReact()
    state <- reactiveValuesToList(local_state)
    expect_match(state$native_model_status, "outside the managed", fixed = TRUE)
  })
})

test_that("Native STT processes a completed job before starting another", {
  restore <- interface_test_scope()
  on.exit(restore(), add = TRUE)

  alive <- TRUE
  starts <- 0L
  cleanups <- 0L
  result_path <- "/cache/completed-before-next.gguf"

  testthat::local_mocked_bindings(
    .genflow_crispasr_inventory = function(config = NULL) {
      genflow:::.genflow_crispasr_empty_inventory()
    },
    .genflow_native_download_job_start = function(selector,
                                                  backend = "",
                                                  quant = "",
                                                  executable = "") {
      starts <<- starts + 1L
      structure(
        list(
          id = "completed-before-next",
          stderr_path = tempfile(),
          process = structure(list(), class = "process")
        ),
        class = "genflow_native_download_job"
      )
    },
    .genflow_native_download_job_read = function(job) {
      list(
        state = "complete",
        stage = "complete",
        message = "ready",
        result = list(
          path = result_path,
          filename = basename(result_path),
          source_url = "",
          cached = FALSE,
          size_bytes = 4096
        )
      )
    },
    .genflow_native_download_job_alive = function(job) alive,
    .genflow_native_download_job_cleanup = function(job) {
      cleanups <<- cleanups + 1L
      TRUE
    },
    .package = "genflow"
  )

  shiny::testServer(genflow:::server, {
    session$flushReact()
    session$setInputs(
      local_stt_native_engine = "crispasr",
      local_stt_native_model = "auto",
      local_stt_native_backend = "granite-4.1",
      local_stt_model_download = 1L
    )
    session$flushReact()
    expect_identical(starts, 1L)

    alive <<- FALSE
    session$setInputs(local_stt_model_download = 2L)
    session$flushReact()

    state <- reactiveValuesToList(local_state)
    expect_identical(starts, 1L)
    expect_identical(cleanups, 1L)
    expect_match(state$native_model_status, "Downloaded:", fixed = TRUE)
    expect_identical(
      state$native_models$path[state$native_models$selected],
      result_path
    )
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
