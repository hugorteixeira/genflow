test_that("OpenAI catalog HTTP errors preserve canonical diagnostics and the old catalog", {
  td <- tempfile("catalog-diagnostics-"); dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  withr::local_envvar(c(OPENAI_API_KEY = "catalog-test-key"))
  local_mocked_bindings(.genflow_list_custom_provider_configs = function() list(), .package = "genflow")
  status <- 401L; calls <- 0L
  local_mocked_bindings(GET = function(...) {
    calls <<- calls + 1L
    structure(list(status_code = status, headers = list(`content-type` = "application/json"),
      content = charToRaw('{"error":{"message":"Incorrect API key: secret-fixture","code":"invalid_api_key"}}')),
      class = "response")
  }, .package = "httr")
  path <- file.path(td, "openai.csv"); writeLines("previous-catalog", path)
  keys <- c(`401` = "provider_unauthorized", `403` = "provider_forbidden", `404` = "provider_not_found",
    `429` = "provider_rate_limit", `500` = "provider_unavailable", `400` = "provider_request_rejected")
  for (value in names(keys)) {
    status <- as.integer(value); calls <- 0L
    logs <- capture.output(error <- tryCatch(
      gen_update_models("openai", directory = td, verbose = FALSE, fail_on_error = TRUE),
      error = identity), type = "message")
    expect_s3_class(error, "genflow_error")
    diagnostic <- gen_diagnostic(error)
    expect_identical(diagnostic$message_key, unname(keys[[value]]))
    expect_identical(diagnostic$source, "genflow")
    expect_identical(diagnostic$category, "provider")
    expect_identical(diagnostic$stage, "management")
    expect_identical(diagnostic$outcome, "failed")
    expect_identical(diagnostic$params$status_code, status)
    expect_identical(diagnostic$retryable, status %in% c(429L,500L))
    expect_identical(calls, 1L)
    expect_identical(readLines(path), "previous-catalog")
    expect_false(any(grepl("secret-fixture", c(logs, jsonlite::toJSON(diagnostic, auto_unbox = TRUE)), fixed = TRUE)))
  }
})
