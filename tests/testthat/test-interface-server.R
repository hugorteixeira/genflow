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
  retired_services <- c(
    "hf-local",
    "hf_local",
    "huggingface-local",
    "huggingface_local",
    "transformers"
  )
  for (service in retired_services) {
    write.csv(
      data.frame(
        service = service,
        model = paste0("legacy/", service)
      ),
      file.path(directory, paste0(service, ".csv")),
      row.names = FALSE
    )
  }

  catalog <- genflow:::.load_models_catalog(directory)
  expect_identical(
    names(catalog),
    c("service", "model", "type", "pricing", "description", "source_file")
  )
  expect_identical(catalog$model, c("model-b", "model-a"))
  expect_identical(catalog$service, c("hf", "openai"))
  expect_false(any(startsWith(catalog$model, "legacy/")))
  expect_false(any(catalog$service %in% retired_services))
  service_choices <- genflow:::.model_service_choices(catalog)
  expect_false(any(unname(service_choices) %in% retired_services))
  expect_false(anyNA(service_choices))
  expect_false(anyNA(genflow:::.model_model_choices(catalog, "hf")))

  favorites <- data.frame(
    service = "openai",
    model = "model-a",
    type = NA_character_
  )
  normalized <- genflow:::.normalize_favorites(favorites, data.frame())
  expect_identical(normalized$type, "")
})

test_that("provider switch drops models from the previous provider", {
  restore <- interface_test_scope()
  on.exit(restore(), add = TRUE)

  seen <- new.env(parent = emptyenv())
  seen$calls <- list()

  testthat::local_mocked_bindings(
    updateSelectizeInput = function(session, inputId, choices = NULL,
                                    selected = NULL, ...) {
      seen$calls[[length(seen$calls) + 1L]] <- list(
        id = inputId,
        choices = unname(choices),
        selected = selected
      )
    },
    .package = "genflow"
  )

  shiny::testServer(genflow:::server, {
    session$flushReact()
    models_state$catalog <- data.frame(
      service = c("openai", "local-native"),
      model = c("gpt-5", "granite.gguf"),
      type = c("Chat", "Audio"),
      pricing = "",
      description = "",
      source_file = c("openai.csv", "local-native.csv")
    )
    session$flushReact()

    session$setInputs(
      setup_service = "openai",
      setup_model = "gpt-5",
      setup_type = "Chat"
    )
    session$flushReact()
    seen$calls <- list()
    session$setInputs(setup_service = "local-native")
    session$flushReact()

    setup_call <- tail(Filter(
      \(x) identical(x$id, "setup_model"),
      seen$calls
    ), 1)[[1]]
    expect_identical(setup_call$choices, "granite.gguf")
    expect_identical(setup_call$selected, "granite.gguf")

    session$setInputs(
      agent_setup_mode = "custom",
      agent_setup_service = "openai",
      agent_setup_model = "gpt-5",
      agent_setup_type = "Chat"
    )
    session$flushReact()
    seen$calls <- list()
    session$setInputs(agent_setup_service = "local-native")
    session$flushReact()

    agent_call <- tail(Filter(
      \(x) identical(x$id, "agent_setup_model"),
      seen$calls
    ), 1)[[1]]
    expect_identical(agent_call$choices, "granite.gguf")
    expect_identical(agent_call$selected, "granite.gguf")
  })
})

test_that("provider switch clears a previous model when the catalog is empty", {
  restore <- interface_test_scope()
  on.exit(restore(), add = TRUE)

  seen <- new.env(parent = emptyenv())
  seen$calls <- list()
  latest_call <- function(id) {
    calls <- Filter(\(x) identical(x$id, id), seen$calls)
    tail(calls, 1)[[1]]
  }

  testthat::local_mocked_bindings(
    updateSelectizeInput = function(session, inputId, choices = NULL,
                                    selected = NULL, ...) {
      seen$calls[[length(seen$calls) + 1L]] <- list(
        id = inputId,
        choices = unname(choices),
        selected = selected
      )
    },
    .package = "genflow"
  )

  shiny::testServer(genflow:::server, {
    session$flushReact()
    models_state$catalog <- data.frame(
      service = "openai",
      model = "gpt-5",
      type = "Chat",
      pricing = "",
      description = "",
      source_file = "openai.csv"
    )
    session$flushReact()

    session$setInputs(
      setup_service = "openai",
      setup_model = "gpt-5",
      setup_type = "Chat"
    )
    session$flushReact()
    seen$calls <- list()
    session$setInputs(setup_service = "local-native")
    session$flushReact()

    expect_identical(latest_call("setup_model")$choices, character())
    expect_identical(latest_call("setup_model")$selected, "")
    expect_identical(latest_call("setup_type")$choices, character())
    expect_identical(latest_call("setup_type")$selected, "")

    session$setInputs(
      agent_setup_mode = "custom",
      agent_setup_service = "openai",
      agent_setup_model = "gpt-5",
      agent_setup_type = "Chat"
    )
    session$flushReact()
    seen$calls <- list()
    session$setInputs(agent_setup_service = "local-native")
    session$flushReact()

    expect_identical(latest_call("agent_setup_model")$choices, character())
    expect_identical(latest_call("agent_setup_model")$selected, "")
    expect_identical(latest_call("agent_setup_type")$choices, character())
    expect_identical(latest_call("agent_setup_type")$selected, "")
  })
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
    expect_false(any(c(
      "python",
      "hf_stt_model",
      "hf_revision"
    ) %in% names(saved)))
    expect_identical(saved$ollama_base_url, config$ollama_base_url)
    expect_identical(saved$stt_native_engine, config$stt_native_engine)
    expect_identical(saved$stt_native_executable, config$stt_native_executable)
    expect_identical(saved$stt_native_model, config$stt_native_model)
    expect_identical(saved$stt_native_backend, config$stt_native_backend)
    expect_identical(saved$stt_native_quant, config$stt_native_quant)
    expect_identical(saved$stt_native_device, config$stt_native_device)

    session$setInputs(
      local_stt_new_model_reference = paste0(
        "https://huggingface.co/owner/repo/blob/main/",
        "new-model-q4_k.gguf"
      ),
      local_config_save = 2
    )
    session$flushReact()
    saved <- genflow:::.genflow_read_local_config(config_path)
    expect_identical(saved$stt_native_model, config$stt_native_model)

    session$setInputs(
      local_stt_native_engine = "moss-transcribe",
      local_stt_native_device = "cpu",
      local_config_save = 3
    )
    session$flushReact()
    saved <- genflow:::.genflow_read_local_config(config_path)
    expect_identical(saved$stt_native_engine, "moss-transcribe")
    expect_identical(saved$stt_native_executable, "")
    expect_identical(saved$stt_native_model, config$stt_native_model)
    expect_identical(saved$stt_native_backend, config$stt_native_backend)
    expect_identical(saved$stt_native_quant, config$stt_native_quant)
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

test_that("Native STT manager verifies, downloads, and deletes cached models", {
  restore <- interface_test_scope()
  on.exit(restore(), add = TRUE)

  inventory_calls <- 0L
  verify_args <- NULL
  download_args <- NULL
  download_status_reads <- 0L
  removed_args <- NULL
  downloaded <- FALSE
  starts <- 0L
  config <- genflow:::.genflow_local_config_defaults()
  config$stt_native_executable <- "/opt/crispasr"
  genflow:::.genflow_write_local_config(
    config,
    getOption("genflow.local_config_path")
  )
  inventory <- data.frame(
    path = c("/cache/granite-q4_k.gguf", "/models/granite-q8_0.gguf"),
    filename = c("granite-q4_k.gguf", "granite-q8_0.gguf"),
    quant = c("q4_k", "q8_0"),
    size_bytes = c(1024, 2048),
    size = c("1 KB", "2 KB"),
    source_url = c(
      paste0(
        "https://huggingface.co/owner/repo/resolve/main/",
        "granite-q4_k.gguf"
      ),
      paste0(
        "https://huggingface.co/owner/repo/resolve/main/",
        "granite-q8_0.gguf"
      )
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
            source_url = paste0(
              "https://huggingface.co/owner/repo/resolve/main/",
              "granite-q8_0.gguf"
            ),
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
    .genflow_crispasr_resolve_download = function(selector,
                                                  backend = "",
                                                  quant = "",
                                                  executable = "") {
      verify_args <<- list(
        selector = selector,
        backend = backend,
        quant = quant,
        executable = executable
      )
      list(
        filename = "granite-q8_0.gguf",
        size_bytes = 2048,
        revision = strrep("a", 40)
      )
    },
    .genflow_native_download_job_start = function(selector,
                                                  backend = "",
                                                  quant = "",
                                                  executable = "") {
      starts <<- starts + 1L
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
          source_url = paste0(
            "https://huggingface.co/owner/repo/resolve/main/",
            "granite-q8_0.gguf"
          ),
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
    expect_match(output$local_stt_models_summary, "1 file", fixed = TRUE)
    managed <- managed_native_models()
    expect_identical(managed$filename, "granite-q4_k.gguf")
    expect_false("granite-q8_0.gguf" %in% managed$filename)
    expect_match(output$local_stt_models_table, "<th>Model", fixed = TRUE)
    expect_match(
      output$local_stt_models_table,
      "<th>Hugging Face",
      fixed = TRUE
    )
    expect_identical(
      .local_native_hf_repository(managed$source_url, managed$filename),
      "owner/repo"
    )
    expect_false(grepl("<th>Location", output$local_stt_models_table, fixed = TRUE))
    expect_false(grepl("<th>State", output$local_stt_models_table, fixed = TRUE))

    session$setInputs(
      local_stt_native_engine = "crispasr",
      local_stt_new_model_reference = paste(
        "hf download",
        "hf://owner/repo/granite-q8_0.gguf"
      ),
      local_stt_model_verify = 1L
    )
    session$flushReact()
    state <- reactiveValuesToList(local_state)
    expect_identical(
      verify_args$selector,
      "hf://owner/repo:granite-q8_0.gguf"
    )
    expect_identical(verify_args$backend, "")
    expect_identical(verify_args$quant, "")
    expect_identical(starts, 0L)
    expect_identical(
      state$native_verified_reference,
      "hf://owner/repo:granite-q8_0.gguf"
    )
    expect_identical(state$native_verify_status$type, "ok")
    expect_match(
      state$native_verify_status$message,
      "Available: granite-q8_0.gguf",
      fixed = TRUE
    )

    session$setInputs(
      local_stt_new_model_reference =
        "hf download hf://owner/repo/granite-q8_0.gguf"
    )
    session$flushReact()
    state <- reactiveValuesToList(local_state)
    expect_identical(state$native_verified_reference, "")
    expect_null(state$native_verify_status)

    session$setInputs(
      local_stt_model_download = 1L
    )
    session$flushReact()
    expect_identical(starts, 1L)
    expect_identical(
      download_args$selector,
      "hf://owner/repo:granite-q8_0.gguf"
    )
    expect_identical(download_args$backend, "")
    expect_identical(download_args$quant, "")
    expect_identical(download_args$executable, "/opt/crispasr")
    state <- reactiveValuesToList(local_state)
    expect_match(state$native_model_status, "Downloaded:", fixed = TRUE)
    expect_match(
      state$native_model_status,
      "available under Models > Native STT",
      fixed = TRUE
    )
    expect_true("/cache/granite-q8_0.gguf" %in% state$native_models$path)
    expect_false(any(state$native_models$selected))

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
    expect_length(removed_args$active_model, 0L)
    state <- reactiveValuesToList(local_state)
    expect_null(state$native_delete_path)
    expect_match(state$native_model_status, "Deleted", fixed = TRUE)
  })
})

test_that("Native STT Verify never downloads and reports invalid remote models", {
  restore <- interface_test_scope()
  on.exit(restore(), add = TRUE)

  resolve_calls <- 0L
  starts <- 0L
  testthat::local_mocked_bindings(
    .genflow_crispasr_inventory = function(config = NULL) {
      genflow:::.genflow_crispasr_empty_inventory()
    },
    .genflow_crispasr_resolve_download = function(...) {
      resolve_calls <<- resolve_calls + 1L
      stop("The requested model file does not exist.", call. = FALSE)
    },
    .genflow_native_download_job_start = function(...) {
      starts <<- starts + 1L
      stop("download should not start", call. = FALSE)
    },
    .package = "genflow"
  )

  shiny::testServer(genflow:::server, {
    session$flushReact()
    session$setInputs(
      local_stt_native_engine = "crispasr",
      local_stt_new_model_reference = "https://example.com/model.gguf",
      local_stt_model_verify = 1L
    )
    session$flushReact()
    state <- reactiveValuesToList(local_state)
    expect_identical(resolve_calls, 0L)
    expect_identical(starts, 0L)
    expect_identical(state$native_verify_status$type, "error")
    expect_match(
      state$native_verify_status$message,
      "Hugging Face",
      fixed = TRUE
    )

    session$setInputs(
      local_stt_new_model_reference =
        "hf://owner/repo:missing-q4_k.gguf",
      local_stt_model_verify = 2L
    )
    session$flushReact()
    state <- reactiveValuesToList(local_state)
    expect_identical(resolve_calls, 1L)
    expect_identical(starts, 0L)
    expect_identical(state$native_verify_status$type, "error")
    expect_match(
      state$native_verify_status$message,
      "does not exist",
      fixed = TRUE
    )
  })
})

test_that("Native STT processes a completed job before starting another", {
  restore <- interface_test_scope()
  on.exit(restore(), add = TRUE)

  alive <- TRUE
  starts <- 0L
  cleanups <- 0L
  seen_selector <- NULL
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
      seen_selector <<- selector
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
      local_stt_new_model_reference = paste0(
        "https://huggingface.co/owner/repo/blob/main/",
        "completed-before-next.gguf"
      ),
      local_stt_model_download = 1L
    )
    session$flushReact()
    expect_identical(starts, 1L)
    expect_identical(
      seen_selector,
      "hf://owner/repo:completed-before-next.gguf"
    )

    alive <<- FALSE
    session$setInputs(local_stt_model_download = 2L)
    session$flushReact()

    state <- reactiveValuesToList(local_state)
    expect_identical(starts, 1L)
    expect_identical(cleanups, 1L)
    expect_match(state$native_model_status, "Downloaded:", fixed = TRUE)
    expect_true(result_path %in% state$native_models$path)
    expect_false(any(state$native_models$selected))
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
