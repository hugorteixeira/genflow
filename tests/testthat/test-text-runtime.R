text_runtime_env <- function(values) {
  withr::local_envvar(values, .local_envir = parent.frame())
}

test_that("model discovery failures fall through to defaults and later endpoints", {
  text_runtime_env(list(
    GROQ_API_KEY = "test-key",
    GROQ_MODEL = NA,
    CEREBRAS_API_KEY = "test-key",
    CEREBRAS_MODEL = NA,
    TOGETHER_API_KEY = "test-key",
    TOGETHER_MODEL = NA,
    SAMBANOVA_API_KEY = "test-key",
    SAMBA_API_KEY = NA,
    SAMBANOVA_MODEL = NA,
    SAMBA_MODEL = NA,
    ANTHROPIC_API_KEY = "test-key",
    CLAUDE_API_KEY = NA,
    ANTHROPIC_MODEL = NA,
    CLAUDE_MODEL = NA,
    OLLAMA_MODEL = NA,
    LLAMACPP_MODEL = NA,
    LLAMA_CPP_MODEL = NA
  ))

  testthat::local_mocked_bindings(
    GET = function(url, ...) list(url = url),
    status_code = function(response) 500L,
    .package = "httr"
  )

  expect_identical(
    genflow:::.groq_resolve_model(NULL),
    "llama-3.3-70b-versatile"
  )
  expect_identical(
    genflow:::.cerebras_resolve_model(NULL),
    "llama-3.3-70b"
  )
  expect_identical(
    genflow:::.together_resolve_model(NULL),
    "meta-llama/Meta-Llama-3.1-8B-Instruct-Turbo"
  )
  expect_identical(
    genflow:::.sambanova_resolve_model(NULL),
    "Meta-Llama-3.1-8B-Instruct"
  )
  expect_identical(
    genflow:::.anthropic_resolve_model(NULL),
    "claude-3-5-sonnet-latest"
  )
  expect_identical(
    genflow:::.ollama_resolve_model(
      NULL,
      config = genflow:::.genflow_local_config_defaults()
    ),
    "llama3.2"
  )
  expect_identical(
    genflow:::.llamacpp_resolve_model(
      NULL,
      config = genflow:::.genflow_local_config_defaults()
    ),
    "local-model"
  )
})

test_that("generic OpenAI-compatible discovery tries the next endpoint", {
  calls <- character()
  testthat::local_mocked_bindings(
    GET = function(url, ...) {
      calls <<- c(calls, url)
      list(url = url)
    },
    status_code = function(response) {
      if (grepl("first", response$url, fixed = TRUE)) 500L else 200L
    },
    content = function(response, ...) {
      list(data = list(list(id = "fallback-model")))
    },
    .package = "httr"
  )

  model <- genflow:::.openai_compat_resolve_model(
    model = NULL,
    api_key = "",
    api_key_required = FALSE,
    default_model = "default-model",
    base_url = "http://first",
    base_urls = "http://second",
    model_paths = "/v1/models"
  )

  expect_identical(model, "fallback-model")
  expect_identical(
    calls,
    c("http://first/v1/models", "http://second/v1/models")
  )
})

test_that("tool builders are normalized before provider dispatch", {
  builder_calls <- 0L
  received_tools <- NULL
  tool_schema <- list(list(
    type = "function",
    `function` = list(
      name = "lookup",
      description = "Look up a value",
      parameters = list(type = "object", properties = list())
    )
  ))

  testthat::local_mocked_bindings(
    .gen_txt_openai = function(prompt, model, temp_v, reasoning, add_img,
                               tools, my_tools, plugins, timeout_secs) {
      received_tools <<- my_tools
      "ok"
    },
    .package = "genflow"
  )

  result <- gen_txt(
    "hello",
    service = "openai",
    model = "test-model",
    tools = TRUE,
    my_tools = function() {
      builder_calls <<- builder_calls + 1L
      tool_schema
    },
    null_repeat = FALSE,
    persist = FALSE,
    directory = tempdir()
  )

  expect_identical(result$status_api, "SUCCESS")
  expect_identical(builder_calls, 1L)
  expect_identical(received_tools, tool_schema)
  expect_false(genflow:::.text_tools_contains_function(received_tools))
})

test_that("tool payloads reject nested R closures", {
  expect_error(
    gen_txt(
      "hello",
      service = "openai",
      model = "test-model",
      tools = list(list(
        type = "function",
        handler = function() "not serializable"
      )),
      null_repeat = FALSE,
      persist = FALSE,
      directory = tempdir()
    ),
    "contains an R function"
  )
})

test_that("Hugging Face receives its provider default model", {
  received_model <- NULL
  testthat::local_mocked_bindings(
    .gen_txt_hf = function(prompt, model, temp_v, reasoning, add_img,
                           tools, my_tools, plugins, timeout_secs) {
      received_model <<- model
      "ok"
    },
    .package = "genflow"
  )

  result <- gen_txt(
    "hello",
    service = "hf",
    null_repeat = FALSE,
    persist = FALSE,
    directory = tempdir()
  )

  expect_identical(received_model, genflow:::.HF_TEXT_DEFAULT_MODEL)
  expect_identical(result$model, genflow:::.HF_TEXT_DEFAULT_MODEL)
  expect_identical(result$status_api, "SUCCESS")
})

test_that("Hugging Face loading responses retry and remain structured errors", {
  attempts <- 0L
  waits <- numeric()
  testthat::local_mocked_bindings(
    .gen_txt_hf = function(...) {
      attempts <<- attempts + 1L
      if (attempts == 1L) {
        "HF_MODEL_LOADING: warming up"
      } else {
        "ready"
      }
    },
    .text_retry_sleep = function(seconds) {
      waits <<- c(waits, seconds)
    },
    .package = "genflow"
  )

  result <- gen_txt(
    "hello",
    service = "hf",
    null_repeat = TRUE,
    persist = FALSE,
    directory = tempdir()
  )
  expect_identical(result$status_api, "SUCCESS")
  expect_identical(result$response_value, "ready")
  expect_identical(attempts, 2L)
  expect_identical(waits, 2)

  testthat::local_mocked_bindings(
    .gen_txt_hf = function(...) "HF_MODEL_LOADING: still warming",
    .package = "genflow"
  )
  loading <- gen_txt(
    "hello",
    service = "hf",
    null_repeat = FALSE,
    persist = FALSE,
    directory = tempdir()
  )
  expect_identical(loading$status_api, "ERROR")
  expect_match(loading$status_msg, "^HF_MODEL_LOADING:")
})

test_that("ellipsis arguments are rejected instead of ignored", {
  expect_error(
    gen_txt(
      "hello",
      service = "openai",
      model = "test-model",
      unsupported_provider_argument = 1,
      persist = FALSE,
      directory = tempdir()
    ),
    "`...` is reserved",
    fixed = TRUE
  )

  agent <- structure(
    list(
      context = "hello",
      service = "openai",
      model = "test-model",
      persist = FALSE,
      directory = tempdir()
    ),
    class = "genflow_agent"
  )
  expect_error(
    gen_txt(agent, unsupported_provider_argument = 1),
    "Unsupported `gen_txt()` override(s)",
    fixed = TRUE
  )
})

test_that("local text settings follow argument, environment, config, default precedence", {
  config <- genflow:::.genflow_local_config_defaults()
  config$ollama_base_url <- "http://config-ollama:11434"
  config$ollama_model <- "config-ollama-model"
  config$llamacpp_base_url <- "http://config-llama:8080"
  config$llamacpp_model <- "config-llama-model"

  text_runtime_env(list(
    OLLAMA_BASE_URL = "http://env-ollama:11434",
    OLLAMA_MODEL = "env-ollama-model",
    LLAMACPP_BASE_URL = "http://env-llama:8080",
    LLAMA_CPP_BASE_URL = NA,
    LLAMACPP_MODEL = "env-llama-model",
    LLAMA_CPP_MODEL = NA
  ))

  expect_identical(
    genflow:::.ollama_base_url("http://argument-ollama:11434", config),
    "http://argument-ollama:11434"
  )
  expect_identical(
    genflow:::.ollama_base_url(config = config),
    "http://env-ollama:11434"
  )
  expect_identical(
    genflow:::.ollama_resolve_model("argument-ollama-model", config = config),
    "argument-ollama-model"
  )
  expect_identical(
    genflow:::.ollama_resolve_model(NULL, config = config),
    "env-ollama-model"
  )
  expect_identical(
    genflow:::.llamacpp_base_url("http://argument-llama:8080/v1", config),
    "http://argument-llama:8080"
  )
  expect_identical(
    genflow:::.llamacpp_base_url(config = config),
    "http://env-llama:8080"
  )
  expect_identical(
    genflow:::.llamacpp_resolve_model("argument-llama-model", config = config),
    "argument-llama-model"
  )
  expect_identical(
    genflow:::.llamacpp_resolve_model(NULL, config = config),
    "env-llama-model"
  )

  text_runtime_env(list(
    OLLAMA_BASE_URL = NA,
    OLLAMA_MODEL = NA,
    LLAMACPP_BASE_URL = NA,
    LLAMA_CPP_BASE_URL = NA,
    LLAMACPP_MODEL = NA,
    LLAMA_CPP_MODEL = NA
  ))
  expect_identical(
    genflow:::.ollama_base_url(config = config),
    "http://config-ollama:11434"
  )
  expect_identical(
    genflow:::.ollama_resolve_model(NULL, config = config),
    "config-ollama-model"
  )
  expect_identical(
    genflow:::.llamacpp_base_url(config = config),
    "http://config-llama:8080"
  )
  expect_identical(
    genflow:::.llamacpp_resolve_model(NULL, config = config),
    "config-llama-model"
  )
})

test_that("explicit local base URL reaches the selected adapter", {
  received_base_url <- NULL
  testthat::local_mocked_bindings(
    .gen_txt_ollama = function(prompt, model, temp_v, reasoning, add_img,
                               tools, my_tools, plugins, timeout_secs,
                               base_url) {
      received_base_url <<- base_url
      "ok"
    },
    .package = "genflow"
  )

  result <- gen_txt(
    "hello",
    service = "ollama",
    model = "local-test-model",
    base_url = "http://127.0.0.1:22434/",
    null_repeat = FALSE,
    persist = FALSE,
    directory = tempdir()
  )

  expect_identical(result$status_api, "SUCCESS")
  expect_identical(received_base_url, "http://127.0.0.1:22434/")
})
