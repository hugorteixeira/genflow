test_that("voting counts each ballot and rejects errors and unknown candidates", {
  ballots <- c("option 1", "option 2", "option 2")
  capture.output(vector <- gen_vote(ballots, trigger = "option", return_winner = "result"))
  capture.output(listed <- gen_vote(as.list(ballots), trigger = "option", return_winner = "result"))
  expect_identical(vector, listed)
  expect_identical(vector$winner, "2")
  expect_identical(vector$valid_votes, 3L)
  capture.output(filtered <- gen_vote(list(
    list(response_value = "option 2", status_api = "ERROR"), "option 99", "option 1"
  ), underlying_list = c("one", "two"), trigger = "option", return_winner = "result"))
  expect_identical(filtered$winner, "1")
  expect_identical(filtered$valid_votes, 1L)
  capture.output(tied <- gen_vote(c("option 2", "option 1"), trigger = "option", return_winner = "result"))
  expect_null(tied$winner)
  expect_identical(tied$status, "tie")
  capture.output(quorum <- gen_vote("option 1", trigger = "option", min_votes = 2, return_winner = "result"))
  expect_identical(quorum$status, "insufficient_votes")
  expect_null(quorum$winner)
})

test_that("agent overrides replace fields and direct and batch requests agree", {
  testthat::local_mocked_bindings(.gen_txt_openai = function(prompt, ...) prompt, .package = "genflow")
  agent <- set_agent("parity", setup = list(service = "openai", model = "fixture", type = "Chat"),
                     content = list(context = "old", add = "plain input"),
                     context = "new", save = FALSE, assign = FALSE)
  expect_false(anyDuplicated(names(agent)) > 0L)
  expect_identical(agent$context, "new")
  direct <- gen_txt(agent, persist = FALSE)
  batch <- gen_batch_agent(agent, qty = 1, workers = 1, persist = FALSE,
                           verbose = FALSE, always_fix_errors = FALSE)
  expect_identical(batch[[1]]$response_value, direct$response_value)
  without_add <- gen_txt(agent, add = NULL, persist = FALSE)
  expect_identical(without_add$response_value, "new")
})

test_that("batch forwards the complete image request", {
  seen <- NULL
  output <- tempfile(fileext = ".png")
  writeBin(as.raw(1:8), output)
  withr::defer(unlink(output))
  testthat::local_mocked_bindings(.gen_img_replicate = function(prompt, model, temp, steps, h, y,
      directory, label_sanitized, model_version, replicate_input, poll_interval, max_poll_seconds) {
    seen <<- list(model_version, replicate_input, poll_interval, max_poll_seconds)
    output
  }, .package = "genflow")
  agent <- set_agent("image", setup = list(service = "replicate", model = "owner/model", type = "Image",
    model_version = "pinned", replicate_input = list(seed = 7), poll_interval = 9, max_poll_seconds = 71),
    content = list(context = "scene"), save = FALSE, assign = FALSE)
  batch <- gen_batch_agent(agent, qty = 1, workers = 1, persist = FALSE, verbose = FALSE, always_fix_errors = FALSE)
  expect_identical(batch[[1]]$status_api, "SUCCESS")
  expect_identical(seen, list("pinned", list(seed = 7), 9, 71))
  expect_identical(batch[[1]]$persistence$status, "temporary")
})

test_that("cache identity changes with bytes at the same input path", {
  input <- tempfile(fileext = ".txt")
  withr::defer(unlink(input))
  key <- function() genflow:::.batch_cache_make_key("agent", 1, "test", input,
    one_item_each = NULL, add_img = NULL, directory = "texts", directory_img = "images")
  writeLines("one", input)
  first <- key()
  writeLines("two", input)
  expect_false(identical(first, key()))
})

test_that("content publication is unique, contained and no longer label-dependent", {
  directory <- tempfile()
  dir.create(directory)
  withr::defer(unlink(directory, recursive = TRUE))
  testthat::local_mocked_bindings(.gen_txt_openai = function(...) "generated", .package = "genflow")
  withr::local_envvar(genflow_SKIP_PERSIST_LOG = "1")
  capture.output(first <- gen_txt("private context", label = "../pedido_dir_mkt", service = "openai",
    model = "fixture", directory = directory, res_context = FALSE))
  capture.output(second <- gen_txt("private context", label = "pedido_dir_mkt", service = "openai",
    model = "fixture", directory = directory, res_context = FALSE))
  expect_identical(dirname(first$saved_file), normalizePath(directory))
  expect_false(identical(first$saved_file, second$saved_file))
  expect_identical(readLines(second$saved_file), "generated")
  expect_identical(first$persistence$status, "saved")
  expect_match(first$persistence$sha256, "^[a-f0-9]{64}$")
})

test_that("inference success survives persistence errors without claiming saved success", {
  calls <- 0L
  testthat::local_mocked_bindings(
    .gen_txt_openai = function(...) { calls <<- calls + 1L; "valuable response" },
    .save_response = function(...) stop("disk full"), .package = "genflow")
  result <- gen_txt("prompt", model = "fixture", directory = tempdir())
  expect_identical(calls, 1L)
  expect_identical(result$response_value, "valuable response")
  expect_identical(result$inference_status, "SUCCESS")
  expect_identical(result$status_api, "ERROR")
  expect_identical(result$error$stage, "persistence")
  expect_false(result$error$retryable)
})

test_that("empty responses exhaust bounded retries as structured failures", {
  calls <- 0L
  delays <- numeric()
  testthat::local_mocked_bindings(.gen_txt_openai = function(...) {
    calls <<- calls + 1L
    NULL
  }, .text_retry_sleep = function(seconds) delays <<- c(delays, seconds), .package = "genflow")
  result <- gen_txt("prompt", model = "fixture", persist = FALSE, retry_delays = c(1, 2))
  expect_identical(calls, 3L)
  expect_identical(delays, c(1, 2))
  expect_identical(result$status_api, "ERROR")
  expect_identical(result$error$message, "EMPTY_OR_NULL_RESPONSE")
})

test_that("successful text that looks like an error remains content", {
  testthat::local_mocked_bindings(.gen_txt_openai = function(...) {
    genflow:::.genflow_provider_success("API_ERRORR: a literal in an article")
  }, .package = "genflow")
  result <- gen_txt("prompt", model = "fixture", persist = FALSE)
  expect_identical(result$status_api, "SUCCESS")
  expect_identical(result$response_value, "API_ERRORR: a literal in an article")
})

test_that("saved configuration rejects nested secrets and executable objects", {
  directory <- tempfile()
  withr::local_options(genflow.cache_dir = directory)
  withr::defer(unlink(directory, recursive = TRUE))
  expect_error(set_setup("secret", service = "openai", model = "fixture",
                         nested = list(api_key = "FAKE-KEY"), assign = FALSE), "Secret field")
  expect_error(set_setup("closure", service = "openai", model = "fixture",
                         callback = function() NULL, assign = FALSE), "Non-portable")
  expect_length(list.files(directory, recursive = TRUE), 0L)
})

test_that("TTS artifacts remain distinct and failed publication preserves generated bytes", {
  directory <- tempfile()
  dir.create(directory)
  withr::defer(unlink(directory, recursive = TRUE))
  source_paths <- character()
  testthat::local_mocked_bindings(.tts_openai = function(...) {
    path <- tempfile(fileext = ".mp3")
    source_paths <<- c(source_paths, path)
    writeBin(c(charToRaw("ID3"), as.raw(rep(0, 32))), path)
    path
  }, .package = "genflow")
  withr::defer(unlink(source_paths))
  capture.output(results <- lapply(1:3, function(i) gen_tts("hello", label = "same", directory = directory)))
  paths <- vapply(results, `[[`, character(1), "saved_file")
  expect_length(unique(paths), 3L)
  expect_true(all(file.exists(paths)))
  expect_false(any(file.exists(source_paths)))
  capture.output(temporary <- gen_tts("hello", persist = FALSE))
  withr::defer(unlink(temporary$response_value))
  expect_identical(temporary$persistence$status, "temporary")
  expect_true(is.na(temporary$saved_file))
  expect_true(file.exists(temporary$response_value))
  testthat::local_mocked_bindings(.genflow_copy_artifact = function(...) stop("disk full"), .package = "genflow")
  capture.output(failed <- gen_tts("hello", directory = directory))
  expect_identical(failed$status_api, "ERROR")
  expect_identical(failed$inference_status, "SUCCESS")
  expect_identical(failed$error$stage, "persistence")
  expect_true(file.exists(failed$response_value))
})

test_that("failed export does not replace an existing bundle or include legacy secrets", {
  directory <- tempfile()
  dir.create(directory)
  withr::defer(unlink(directory, recursive = TRUE))
  withr::local_options(genflow.cache_dir = file.path(directory, "cache"))
  path <- file.path(directory, "bundle.zip")
  writeBin(charToRaw("existing archive"), path)
  setup <- set_setup("legacy", service = "openai", model = "fixture", save = FALSE, assign = FALSE)
  setup$api_key <- "FAKE-KEY"
  target <- genflow:::.genflow_setup_path("legacy")
  saveRDS(setup, target)
  expect_error(gen_export_bundle(path, overwrite = TRUE, include_models = FALSE, quiet = TRUE), "Secret field")
  expect_identical(readBin(path, "raw", n = 100), charToRaw("existing archive"))
})

test_that("batch configuration snapshots survive disk edits and are restored on return", {
  directory <- tempfile()
  dir.create(directory)
  withr::defer(unlink(directory, recursive = TRUE))
  withr::local_options(genflow.local_config_path = file.path(directory, "local.json"), genflow.runtime_snapshot = NULL)
  gen_local_config(ollama_base_url = "http://first", save = TRUE)
  seen <- character()
  testthat::local_mocked_bindings(.gen_txt_openai = function(...) {
    seen <<- c(seen, gen_local_config()$ollama_base_url)
    config <- genflow:::.genflow_local_config_defaults()
    config$ollama_base_url <- "http://second"
    genflow:::.genflow_write_local_config(config)
    "done"
  }, .package = "genflow")
  agent <- set_agent("snapshot", setup = list(service = "openai", model = "fixture", type = "Chat"),
    content = list(context = "prompt"), save = FALSE, assign = FALSE)
  gen_batch_agent(agent, qty = 2, workers = 1, persist = FALSE, verbose = FALSE, always_fix_errors = FALSE)
  expect_identical(seen, c("http://first", "http://first"))
  expect_null(getOption("genflow.runtime_snapshot"))
  expect_identical(gen_local_config()$ollama_base_url, "http://second")
})

test_that("portable configurations also reject executable attributes", {
  value <- structure(list(service = "openai"), hidden = new.env())
  expect_error(genflow:::.genflow_validate_portable(value), "Non-portable")
  expect_error(genflow:::.genflow_validate_portable(c(api_key = "FAKE-KEY")), "Secret field")
  expect_silent(genflow:::.genflow_validate_portable(structure(list(service = "openai"), class = "genflow_setup")))
})

test_that("automatic batch recovery does not repeat successful paid inference after disk failure", {
  root <- tempfile()
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE))
  withr::local_options(genflow.cache_dir = file.path(root, "cache"))
  calls <- 0L
  testthat::local_mocked_bindings(
    .gen_txt_openai = function(...) { calls <<- calls + 1L; "generated" },
    .save_response = function(...) stop("disk full"), .package = "genflow")
  agent <- set_agent("publication-failure", setup = list(service = "openai", model = "fixture", type = "Chat"),
    content = list(context = "prompt"), save = FALSE, assign = FALSE)
  run <- function() gen_batch_agent(agent, qty = 1, workers = 1, persist = TRUE,
    directory = root, directory_img = root, verbose = FALSE)
  first <- run()
  second <- run()
  expect_identical(calls, 1L)
  expect_identical(first[[1]]$status_api, "ERROR")
  expect_identical(second[[1]]$response_value, "generated")
  expect_identical(second[[1]]$error$stage, "persistence")
  expect_identical(second$combined_stats$executed_indices, integer())
})

test_that("saved agent layers clear explicit NULL fields and preserve caller-owned identity", {
  agent <- set_agent("owner", setup = list(service = "openai", model = "fixture", context = "old", add = "old"),
    content = list(context = "new", add = NULL, name = "wrong", sname = "wrong"),
    context = NULL, save = FALSE, assign = FALSE)
  expect_identical(agent$name, "owner")
  expect_null(agent$sname)
  expect_null(agent$context)
  expect_null(agent$add)
  expect_identical(agent$model, "fixture")
})
