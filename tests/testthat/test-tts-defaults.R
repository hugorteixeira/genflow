test_that("speech defaults are shared by current calls, forms and portable agents", {
  withr::local_options(list(genflow.models_dir = withr::local_tempdir(), genflow.runtime_snapshot = NULL))
  flags <- c("reference_consent", "spoken_disclaimer", "marking_responsibility")
  current <- list(reference_consent = TRUE, spoken_disclaimer = FALSE, marking_responsibility = TRUE)
  previous <- lapply(current, `!`)
  for (version in 2:4) {
    defaults <- if (version == 4L) current else previous
    args <- list(text = "Olá!", service = "local-native")
    checked <- gen_validate_call("gen_tts", args, version)
    form <- gen_block_configuration("gen_tts", "local-native", version = version)
    agent <- gen_validate_agent_definition(list(operation = "audio.speak", contract_version = version,
      service = "local-native", parameters = list()))
    expect_true(checked$valid); expect_true(form$valid); expect_true(agent$valid)
    expect_identical(checked$args[flags], defaults)
    expect_identical(lapply(form$arguments$properties[flags], `[[`, "default"), defaults)
    expect_identical(agent$body$parameters[flags], defaults)
    pending <- gen_validate_call("gen_tts", args, version, bound = "reference_audio")
    expect_identical(pending$valid, version == 4L)
    # Prepared historical values must still pass the public execution boundary.
    for (service in c("local-native", "openai", "replicate")) {
      old <- gen_validate_call("gen_tts", list(text = "Olá!", service = service), version)
      expect_true(old$valid)
      expect_true(gen_validate_call("gen_tts", old$args, 4L)$valid, info = paste(version, service))
    }
  }
  explicit <- c(list(text = "Olá!", service = "local-native"), previous)
  expect_identical(gen_validate_call("gen_tts", explicit, 4L)$args[flags], previous)
  expect_false(gen_validate_call("gen_tts", explicit, 4L, bound = "reference_audio")$valid)
  explicit$spoken_disclaimer <- FALSE
  expect_true("tts_marking_required" %in% vapply(gen_validate_call("gen_tts", explicit, 4L)$errors, `[[`, "", "code"))
  for (key in flags) for (value in list(NULL, "true", 1)) {
    args <- list(text = "Olá!", service = "local-native"); args[key] <- list(value)
    expect_false(gen_validate_call("gen_tts", args, 4L)$valid, info = key)
  }
})

test_that("native runtime uses new defaults and respects explicit and frozen choices", {
  withr::local_options(list(genflow.models_dir = withr::local_tempdir(), genflow.runtime_snapshot = NULL))
  f <- native_tts_fixture()
  run <- function(...) {
    result <- gen_tts("Olá!", service = "local-native", format = "wav", persist = FALSE, ...)
    expect_identical(result$status_api, "SUCCESS")
    on.exit(unlink(result$response_value))
    unlist(jsonlite::read_json(f$record)$args)
  }
  args <- run()
  expect_true(all(c("--no-spoken-disclaimer", "--accept-marking-responsibility") %in% args))
  expect_false("--i-have-rights" %in% args)
  args <- run(spoken_disclaimer = TRUE, marking_responsibility = FALSE, reference_consent = FALSE)
  expect_false(any(c("--no-spoken-disclaimer", "--accept-marking-responsibility") %in% args))
  reference <- gen_tts("Referência", service = "local-native", format = "wav", persist = FALSE)$response_value
  withr::defer(unlink(reference))
  args <- run(reference_audio = reference)
  expect_true(all(c("--voice", "--i-have-rights", "--no-spoken-disclaimer", "--accept-marking-responsibility") %in% args))
  unlink(f$record)
  expect_error(run(reference_audio = reference, reference_consent = FALSE), class = "genflow_validation_error")
  expect_error(run(marking_responsibility = FALSE), class = "genflow_validation_error")
  expect_false(file.exists(f$record))
  for (version in 2:3) {
    old <- gen_validate_call("gen_tts", list(text = "Olá!", service = "local-native", format = "wav", persist = FALSE), version)
    result <- do.call(gen_tts, old$args)
    expect_identical(result$status_api, "SUCCESS"); unlink(result$response_value)
    expect_false("--no-spoken-disclaimer" %in% unlist(jsonlite::read_json(f$record)$args))
  }
})
