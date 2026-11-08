configuration_scope <- function(env = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = env)
  withr::local_options(list(genflow.models_dir = root, genflow.runtime_snapshot = NULL), .local_envir = env)
  root
}
configuration_profile <- function(...) list(type = "object", properties = list(...), additionalProperties = FALSE)

test_that("form restrictions and offline validation agree for the current speech contract", {
  configuration_scope()
  for (version in 4L) {
    form <- gen_block_configuration("gen_tts", "local-native", "voice.gguf", version)
    expect_true(form$valid)
    expect_false(isTRUE(form$arguments$properties$instructions[["x-ui"]]$disabled))
    for (key in c("voice", "speed")) expect_true(form$arguments$properties[[key]][["x-ui"]]$disabled)
    expect_setequal(unlist(form$arguments$properties$format$enum), c("mp3", "wav", "opus", "aac"))
    bad <- gen_block_configuration("gen_tts", "local-native", "voice.gguf", version, list(instructions = "Voz jovem", speed = 2, format = "pcm"))
    expect_false(bad$valid); expect_length(bad$errors, 2)
    expect_true(all(vapply(bad$errors, function(e) e$contract_version == version, logical(1))))
    expect_true(all(vapply(bad$errors, function(e) e$code == "tts_native_parameter", logical(1))))
    a <- gen_validate_agent_definition(list(operation = "audio.speak", contract_version = version, service = "local-native", model = "voice.gguf", parameters = list(instructions = 42)))
    expect_equal(a$errors[[1]]$contract_version, version)
    expect_equal(a$errors[[1]]$field, "Instruções para a voz")
    remote <- gen_block_configuration("gen_tts", "openai", version = version)
    expect_null(remote$arguments$properties$instructions[["x-ui"]]$disabled)
    expect_true(remote$arguments$properties$native_device[["x-ui"]]$disabled)
  }
  for (version in 1:3) expect_error(gen_block_configuration("gen_tts", "local-native", version=version),class="genflow_validation_error")
})

test_that("resolving is explicit, portable and does not replace a frozen profile", {
  root <- configuration_scope()
  schema <- configuration_profile(model_options = list(type = "object", additionalProperties = FALSE,
    properties = list(cfg = list(type = "number", minimum = 0, maximum = 10, default = 2))))
  gen_save_parameter_schema("local-native", schema, block = "gen_tts")
  unresolved <- gen_block_configuration("gen_tts", "local-native", "a.gguf", 4)
  expect_null(unresolved$parameter_schema)
  first <- gen_block_configuration("gen_tts", "local-native", "a.gguf", 4, resolve = TRUE)
  expect_equal(first$parameter_schema$revisions$service, 1)
  expect_equal(first$arguments$properties$model_options$properties$cfg$default, 2)
  expect_false(first$arguments$properties$model_options[["x-ui"]]$hidden)
  expect_equal(first$arguments$properties$timeout_api$default, unresolved$arguments$properties$timeout_api$default)
  schema$properties$model_options$properties$cfg$default <- 1
  gen_save_parameter_schema("local-native", schema, block = "gen_tts", expected_revision = 1)
  pinned <- list(parameter_schema = first$parameter_schema, model_options = list(cfg = 3))
  second <- gen_block_configuration("gen_tts", "local-native", "a.gguf", 4, pinned, resolve = TRUE)
  expect_identical(second$parameter_schema, first$parameter_schema)
  expect_equal(second$arguments$properties$model_options$properties$cfg$default, 2)
  withr::local_options(list(genflow.models_dir = tempfile()))
  expect_identical(gen_block_configuration("gen_tts", "local-native", "a.gguf", 4, pinned)$parameter_schema, first$parameter_schema)
  wrong <- gen_block_configuration("gen_tts", "local-native", "b.gguf", 4, pinned)
  expect_false(wrong$valid)
  expect_true(any(vapply(wrong$errors, function(e) e$code == "parameter_schema_mismatch", logical(1))))
  tampered <- pinned; tampered$parameter_schema$schema$properties$model_options$properties$cfg$default <- 9
  expect_false(gen_block_configuration("gen_tts", "local-native", "a.gguf", 4, tampered)$valid)
  expect_equal(pinned$model_options$cfg, 3)
})

test_that("profiles narrow controls without enabling adapter features or reserved fields", {
  configuration_scope()
  schema <- configuration_profile(instructions = list(type = c("string", "null"), default = NULL),
    speed = list(type = "number", minimum = 0, maximum = 10),
    model_options = list(type = "object", additionalProperties = FALSE, properties = list(
      cfg = list(type = "number", minimum = -100), fictional = list(type = "string"))))
  gen_save_parameter_schema("local-native", schema, block = "gen_tts")
  f <- gen_block_configuration("gen_tts", "local-native", "voice", 4, resolve = TRUE)
  expect_false(isTRUE(f$arguments$properties$instructions[["x-ui"]]$disabled))
  expect_equal(f$arguments$properties$speed$minimum, 0.25)
  expect_equal(f$arguments$properties$speed$maximum, 4)
  expect_equal(f$arguments$properties$model_options$properties$cfg$minimum, 0)
  expect_true(f$arguments$properties$model_options$properties$fictional[["x-ui"]]$disabled)
  bad <- gen_block_configuration("gen_tts", "local-native", "voice", 4, list(parameter_schema = f$parameter_schema, model_options = list(cfg = -1, fictional = "x"), instructions = 42))
  expect_false(bad$valid); expect_gte(length(bad$errors), 3)
  for (e in bad$errors) expect_true(gen_validate_schema(e, gen_error_catalog()$schema)$valid)
})

test_that("generic profiles work on all generators and service restrictions are shared", {
  configuration_scope()
  cases <- list(c("gen_txt", "openai", 2), c("gen_img", "openai", 3), c("gen_stt", "openai", 2),
    c("gen_tts", "openai", 4), c("gen_sfx", "elevenlabs", 2), c("gen_music", "acestep", 2))
  for (case in cases) {
    field <- if (case[1] == "gen_img") "max_poll_seconds" else "timeout_api"
    profile <- configuration_profile(); profile$properties[[field]] <- list(type = "number", maximum = 900)
    gen_save_parameter_schema(case[2], profile, block = case[1])
    form <- gen_block_configuration(case[1], case[2], version = as.integer(case[3]), resolve = TRUE)
    expect_equal(form$arguments$properties[[field]]$maximum, 900)
    expect_equal(form$parameter_schema$block, case[1])
    expect_true(form$arguments$properties$service[["x-ui"]]$hidden)
  }
  txt <- gen_block_configuration("gen_txt", "openrouter", version = 2)
  expect_true(txt$arguments$properties$max_output_tokens[["x-ui"]]$disabled)
  expect_true(txt$arguments$properties$base_url[["x-ui"]]$disabled)
  music <- gen_block_configuration("gen_music", "elevenlabs", version = 2)
  expect_setequal(unlist(music$arguments$properties$format$enum), c("mp3", "opus"))
  expect_true(music$arguments$properties$seed[["x-ui"]]$disabled)
  lyric <- gen_block_configuration("gen_music", "elevenlabs", version = 2, parameters = list(instrumental = FALSE, lyrics = "Olá"))
  expect_equal(lyric$arguments$properties$duration_seconds$maximum, 120)
  expect_null(lyric$arguments$properties$seed[["x-ui"]]$disabled)
  expect_equal(gen_block_configuration("gen_music", "acestep", version = 2)$arguments$properties$duration_seconds$minimum, 10)
})


test_that("malformed snapshots and reserved fields remain diagnostics, not UI crashes", {
  configuration_scope()
  for (profile in list("bad", 1, list(schema = "bad"), list())) {
    f <- gen_block_configuration("gen_tts", "local-native", "voice", 4, list(parameter_schema = profile))
    expect_false(f$valid)
    expect_identical(f$parameter_schema, profile)
    expect_true(f$arguments$properties$model_options[["x-ui"]]$disabled)
  }
  f <- gen_block_configuration("gen_tts", "local-native", "voice", 4, list(service = "openai", text = "secret", persist = FALSE))
  expect_length(Filter(function(e) e$code == "backend_owned_argument", f$errors), 3)
  expect_false(grepl("secret", jsonlite::toJSON(f, auto_unbox = TRUE, null = "null"), fixed = TRUE))
  pending <- gen_validate_call("gen_music", list(service = "elevenlabs", instrumental = FALSE, seed = 3), 2, bound = c("prompt", "lyrics"))
  expect_true(pending$valid)
})
