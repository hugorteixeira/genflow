test_that("Gemini key and model aliases have deterministic precedence", {
  withr::local_envvar(c(
    GOOGLE_API_KEY = "google-key",
    GEMINI_API_KEY = "gemini-key",
    GEMINI_MODEL = "models/gemini-configured",
    GOOGLE_MODEL = "gemini-google-model"
  ))

  expect_identical(genflow:::.gemini_api_key(), "google-key")
  expect_identical(
    genflow:::.gemini_resolve_model(NULL),
    "gemini-configured"
  )
  expect_identical(
    genflow:::.gemini_resolve_model("models/gemini-explicit"),
    "gemini-explicit"
  )

  withr::local_envvar(c(
    GOOGLE_API_KEY = NA,
    GEMINI_MODEL = NA
  ))
  expect_identical(genflow:::.gemini_api_key(), "gemini-key")
  expect_identical(
    genflow:::.gemini_resolve_model(NULL),
    "gemini-google-model"
  )
})

test_that("inline image encoding rejects missing files and directories", {
  expect_error(
    genflow:::.encode_image(file.path(tempdir(), "missing-image.png")),
    "readable image file",
    fixed = TRUE
  )
  expect_error(
    genflow:::.encode_image(tempdir()),
    "readable image file",
    fixed = TRUE
  )
})

test_that("Gemini runtime sends the documented generateContent request", {
  withr::local_envvar(c(
    GOOGLE_API_KEY = "test-key",
    GEMINI_API_KEY = NA
  ))
  captured <- NULL
  tool_schema <- list(list(
    type = "function",
    `function` = list(
      name = "lookup",
      description = "Look up an item",
      parameters = list(
        type = "object",
        properties = list(id = list(type = "string"))
      )
    )
  ))

  testthat::local_mocked_bindings(
    POST = function(url, ..., body, encode, config) {
      captured <<- list(url = url, body = body, encode = encode)
      structure(list(), class = "gemini-test-response")
    },
    http_status = function(response) {
      list(category = "Success", reason = "OK")
    },
    content = function(response, ...) {
      list(candidates = list(list(
        content = list(parts = list(
          list(text = "hello"),
          list(text = " world")
        ))
      )))
    },
    .package = "httr"
  )

  value <- genflow:::.gen_txt_gemini(
    prompt = "hello",
    model = "models/gemini-test",
    temp_v = 0.25,
    reasoning = NULL,
    add_img = NULL,
    tools = TRUE,
    my_tools = tool_schema,
    plugins = NULL,
    timeout_secs = 5
  )

  expect_identical(value, "hello world")
  expect_match(
    captured$url,
    "/v1beta/models/gemini-test:generateContent$"
  )
  expect_identical(captured$encode, "json")
  expect_identical(
    captured$body$contents[[1]]$parts[[1]]$text,
    "hello"
  )
  expect_identical(
    captured$body$tools[[1]]$functionDeclarations[[1]]$name,
    "lookup"
  )
})

test_that("Gemini function calls and filters preserve the runtime contract", {
  withr::local_envvar(GOOGLE_API_KEY = "test-key")
  payload <- list(candidates = list(list(
    content = list(parts = list(list(
      functionCall = list(name = "lookup", args = list(id = "1"))
    )))
  )))
  testthat::local_mocked_bindings(
    POST = function(...) structure(list(), class = "gemini-test-response"),
    http_status = function(response) {
      list(category = "Success", reason = "OK")
    },
    content = function(response, ...) payload,
    .package = "httr"
  )
  expect_identical(
    genflow:::.gen_txt_gemini(
      "hello", "gemini-test", 1, NULL, NULL,
      tools = FALSE, timeout_secs = 5
    ),
    payload
  )

  blocked <- list(
    promptFeedback = list(blockReason = "SAFETY"),
    candidates = list()
  )
  testthat::local_mocked_bindings(
    content = function(response, ...) blocked,
    .package = "httr"
  )
  expect_match(
    genflow:::.gen_txt_gemini(
      "hello", "gemini-test", 1, NULL, NULL,
      tools = FALSE, timeout_secs = 5
    ),
    "^CONTENT_FILTERED:"
  )
})

test_that("gen_txt dispatches Gemini with a provider model default", {
  withr::local_envvar(c(
    GEMINI_MODEL = NA,
    GOOGLE_MODEL = NA
  ))
  received <- NULL
  testthat::local_mocked_bindings(
    .gen_txt_gemini = function(prompt, model, temp_v, reasoning, add_img,
                               tools, my_tools, plugins, timeout_secs) {
      received <<- list(model = model, tools = my_tools)
      "ok"
    },
    .package = "genflow"
  )

  result <- gen_txt(
    "hello",
    service = "gemini",
    null_repeat = FALSE,
    persist = FALSE,
    directory = tempdir()
  )
  expect_identical(received$model, genflow:::.GEMINI_TEXT_DEFAULT_MODEL)
  expect_identical(result$status_api, "SUCCESS")
  expect_identical(result$model, genflow:::.GEMINI_TEXT_DEFAULT_MODEL)
})

test_that("Gemini catalog follows pagination and does not put keys in URLs", {
  withr::local_envvar(c(
    GOOGLE_API_KEY = "google-key",
    GEMINI_API_KEY = "gemini-key"
  ))
  calls <- list()
  testthat::local_mocked_bindings(
    GET = function(url, ..., query) {
      calls[[length(calls) + 1L]] <<- list(url = url, query = query)
      structure(
        list(page = length(calls)),
        class = "gemini-catalog-response"
      )
    },
    status_code = function(response) 200L,
    content = function(response, as, ...) {
      if (identical(as, "parsed")) {
        if (identical(response$page, 1L)) {
          return(list(
            models = list(list(
              name = "models/gemini-chat",
              description = "Chat model",
              supportedGenerationMethods = list("generateContent")
            )),
            nextPageToken = "page-2"
          ))
        }
        return(list(models = list(list(
          name = "models/text-embedding-test",
          supportedGenerationMethods = list("embedContent")
        ))))
      }
      ""
    },
    .package = "httr"
  )

  directory <- file.path(tempdir(), paste0("gemini-catalog-", Sys.getpid()))
  unlink(directory, recursive = TRUE)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  result <- genflow:::.update_models_gemini(directory, verbose = FALSE)

  expect_equal(nrow(result), 2L)
  expect_setequal(result$type, c("Chat", "Embedding"))
  expect_identical(length(calls), 2L)
  expect_false(any(grepl("google-key|gemini-key", vapply(
    calls,
    `[[`,
    character(1),
    "url"
  ))))
  expect_identical(calls[[2]]$query$pageToken, "page-2")
  expect_true(file.exists(file.path(directory, "gemini.csv")))
})
