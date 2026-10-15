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
