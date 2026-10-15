test_that("registered compatible providers preserve their actual HTTP routes and results", {
  endpoints <- c(
    groq = "https://api.groq.com/openai/v1/chat/completions",
    cerebras = "https://api.cerebras.ai/v1/chat/completions",
    together = "https://api.together.xyz/v1/chat/completions",
    sambanova = "https://api.sambanova.ai/v1/chat/completions",
    nebius = "https://api.studio.nebius.ai/v1/chat/completions",
    deepseek = "https://api.deepseek.com/chat/completions",
    perplexity = "https://api.perplexity.ai/chat/completions",
    fireworks = "https://api.fireworks.ai/inference/v1/chat/completions",
    deepinfra = "https://api.deepinfra.com/v1/openai/chat/completions",
    hyperbolic = "https://api.hyperbolic.xyz/v1/chat/completions",
    openai = "https://api.openai.com/v1/chat/completions",
    openrouter = "https://openrouter.ai/api/v1/chat/completions",
    hf = "https://router.huggingface.co/v1/chat/completions"
  )
  env <- stats::setNames(rep("fixture-key", length(endpoints)), paste0(toupper(names(endpoints)), "_API_KEY"))
  env <- c(env, HUGGINGFACE_API_TOKEN = "fixture-key",
           stats::setNames(rep(NA_character_, 10), paste0(toupper(names(endpoints)[1:10]), "_BASE_URL")))
  withr::local_envvar(env)
  seen <- NULL
  reply <- list(id = "request-1", model = "served-model", usage = list(prompt_tokens = 19L, completion_tokens = 7L),
    choices = list(list(finish_reason = "length", message = list(content = "API_ERRORR: an example in the generated article"))))
  testthat::local_mocked_bindings(
    POST = function(url, ..., body, encode, config) {
      seen <<- list(url = url, body = jsonlite::fromJSON(body, simplifyVector = FALSE))
      structure(list(status_code = 200L, headers = list(`content-type` = "application/json"),
                     content = charToRaw(jsonlite::toJSON(reply, auto_unbox = TRUE))), class = "response")
    }, .package = "httr")
  for (id in names(endpoints)) {
    result <- gen_txt("prompt", service = id, model = "explicit-model", persist = FALSE, null_repeat = FALSE)
    expect_identical(seen$url, unname(endpoints[[id]]), info = id)
    expect_identical(seen$body$messages[[1]]$content, "prompt", info = id)
    expect_identical(result$status_api, "SUCCESS", info = id)
    expect_identical(result$response_value, reply$choices[[1]]$message$content, info = id)
    expect_identical(result$usage$source, "provider", info = id)
    expect_equal(result$usage$input_tokens, 19, info = id)
    expect_identical(result$provider_request_id, "request-1", info = id)
    expect_identical(result$provider_model, "served-model", info = id)
    expect_true(result$truncated, info = id)
  }
})

test_that("OpenAI sampling restrictions are applied to the outbound request", {
  withr::local_envvar(OPENAI_API_KEY = "fixture-key")
  seen <- NULL
  testthat::local_mocked_bindings(POST = function(url, ..., body, encode, config) {
    seen <<- jsonlite::fromJSON(body, simplifyVector = FALSE)
    payload <- if (grepl("responses$", url)) list(output_text = "done", status = "completed") else
      list(choices = list(list(message = list(content = "done"))))
    structure(list(status_code = 200L, headers = list(`content-type` = "application/json"),
      content = charToRaw(jsonlite::toJSON(payload, auto_unbox = TRUE))), class = "response")
  }, .package = "httr")
  result <- gen_txt("prompt", model = "gpt-5-mini", temp = 0.2, persist = FALSE)
  expect_identical(result$status_api, "SUCCESS")
  expect_null(seen$temperature)
  result <- gen_txt("prompt", model = "gpt-5.2", reasoning = "none", temp = 0.2, persist = FALSE)
  expect_identical(result$status_api, "SUCCESS")
  expect_equal(seen$temperature, 0.2)
  gen_txt("prompt", model = "gpt-5.2", reasoning = "high", temp = 0.2, persist = FALSE)
  expect_null(seen$temperature)
})

test_that("chat and Responses refusals and empty output do not become successes", {
  refused <- genflow:::.genflow_chat_result(list(choices = list(list(message = list(refusal = "refused")))))
  expect_identical(refused$error$code, "CONTENT_FILTERED")
  response <- genflow:::.genflow_responses_result(list(output = list(list(content = list(list(type = "refusal"))))))
  expect_identical(response$error$code, "CONTENT_FILTERED")
  empty <- genflow:::.genflow_responses_result(list(status = "completed", output = list()))
  expect_identical(empty$error$code, "EMPTY_OR_NULL_RESPONSE")
})

test_that("OpenAI output caps reach both endpoints through direct, agent and batch calls", {
  withr::local_envvar(OPENAI_API_KEY = "fixture-key")
  seen <- list()
  testthat::local_mocked_bindings(POST = function(url, ..., body, encode, config) {
    seen[[length(seen) + 1L]] <<- list(url = url, body = jsonlite::fromJSON(body, simplifyVector = FALSE))
    payload <- if (grepl("responses$", url)) list(output_text = "short", status = "completed") else
      list(choices = list(list(message = list(content = "short"))))
    structure(list(status_code = 200L, headers = list(`content-type` = "application/json"),
      content = charToRaw(jsonlite::toJSON(payload, auto_unbox = TRUE))), class = "response")
  }, .package = "httr")
  setup <- set_setup("capped", service = "openai", model = "gpt-5-mini", type = "Chat",
    reasoning = "minimal", max_output_tokens = 512L, save = FALSE, assign = FALSE)
  agent <- set_agent("capped", setup = setup, content = list(context = "Be brief."), save = FALSE, assign = FALSE)
  direct <- gen_txt("Be brief.", model = "gpt-5-mini", max_output_tokens = 256L, persist = FALSE)
  via_agent <- gen_txt(agent, persist = FALSE)
  batch <- gen_batch_agent(agent, qty = 1, workers = 1, persist = FALSE, verbose = FALSE, always_fix_errors = FALSE)
  expect_identical(direct$status_api, "SUCCESS")
  expect_identical(via_agent$status_api, "SUCCESS")
  expect_identical(batch[[1]]$status_api, "SUCCESS")
  expect_length(seen, 3L)
  expect_equal(seen[[1]]$body$max_completion_tokens, 256L)
  expect_null(seen[[1]]$body$max_output_tokens)
  for (i in 2:3) {
    expect_match(seen[[i]]$url, "/responses$")
    expect_equal(seen[[i]]$body$max_output_tokens, 512L)
    expect_null(seen[[i]]$body$max_completion_tokens)
    expect_identical(seen[[i]]$body$reasoning$effort, "minimal")
  }
  expect_true(gen_capabilities("openai")$adapter_features[["max_output_tokens"]])
  expect_false(gen_capabilities("openrouter")$adapter_features[["max_output_tokens"]])
  for (value in list(0, -1, 1.5, Inf, NA_real_, "512", c(1, 2))) {
    expect_error(gen_txt("prompt", max_output_tokens = value, persist = FALSE), "positive integer")
  }
  expect_error(gen_txt("prompt", service = "openrouter", max_output_tokens = 512, persist = FALSE), "only by service")
  expect_length(seen, 3L)
})

test_that("HF non-chat catalog entries match executable adapter routes", {
  model <- function(id, task, provider) list(id = id, pipeline_tag = task,
    inferenceProviderMapping = list(list(provider = provider, status = "live")))
  catalog <- genflow:::.hf_models_to_catalog(list(
    model("image-external", "text-to-image", "fal-ai"),
    model("image-supported", "text-to-image", "hf-inference"),
    model("stt-supported", "automatic-speech-recognition", "hf-inference"),
    model("unsupported-video", "text-to-video", "hf-inference"),
    model("unsupported-tts", "text-to-speech", "hf-inference"),
    model("chat-supported", "text-generation", "other-provider")
  ))
  expect_setequal(catalog$model, c("image-supported", "stt-supported", "chat-supported"))
})

test_that("capability and credential descriptions derive from executable adapters", {
  providers <- genflow:::.genflow_provider_registry()
  ns <- asNamespace("genflow")
  for (spec in providers) {
    for (fn in c(spec$text_function, spec$model_resolver, spec$catalog_function)) {
      expect_true(exists(fn, ns, inherits = FALSE), info = fn)
    }
    caps <- genflow:::gen_capabilities(spec$id)
    expect_false(caps$modalities[["video"]])
    expect_identical(caps$model_support, "unverified")
  }
  expect_false(genflow:::gen_capabilities("ollama")$adapter_features[["tools"]])
  expect_false(genflow:::gen_capabilities("openai", "gpt-5-mini")$sampling_temperature)
})
