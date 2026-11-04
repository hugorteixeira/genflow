test_that("native voice descriptions travel literally without changing speech labels", {
  f <- native_tts_fixture()
  text <- 'Olá "Mimi"!'
  description <- 'Voz jovem; $(touch impossible), suave e alegre.'
  for (style in list(description, NULL, "", " \n\t ")) {
    result <- gen_tts(text, add = "Vamos brincar?", instructions = style,
      service = "local-native", format = "wav", persist = FALSE)
    expect_identical(result$status_api, "SUCCESS"); unlink(result$response_value)
    args <- unlist(jsonlite::read_json(f$record)$args)
    spoken <- paste(text, "Vamos brincar?")
    expected <- if (identical(style, description)) paste0("(", description, ") ", spoken) else spoken
    expect_identical(args[match("--tts", args) + 1L], expected)
    expect_false(grepl("Voz jovem", result$label, fixed = TRUE))
  }
})

test_that("native input limit counts description and added text before inference", {
  f <- native_tts_fixture()
  text <- strrep("á", 4089)
  args <- list(text = text, add = "!", instructions = "Oi", service = "local-native")
  expect_true(gen_validate_call("gen_tts", args, 4)$valid) # 4089 + 2 + 5 = 4096
  args$instructions <- "Olá"
  bad <- gen_validate_call("gen_tts", args, 4)
  expect_false(bad$valid)
  expect_identical(bad$errors[[1]]$code, "too_long")
  expect_match(bad$errors[[1]]$path, "/instructions$")
  expect_error(do.call(gen_tts, c(args, list(persist = FALSE))), class = "genflow_validation_error")
  expect_false(file.exists(f$record))
  args$text <- strrep("a", 4097); args$add <- NULL
  expect_match(gen_validate_call("gen_tts", args, 4)$errors[[1]]$path, "/text$")
})

test_that("voice description profiles project metadata, choices and frozen defaults", {
  root <- withr::local_tempdir()
  withr::local_options(list(genflow.models_dir = root, genflow.runtime_snapshot = NULL))
  styles <- list(NULL, "Voz feminina e jovem", "Voz masculina e suave")
  schema <- list(type = "object", additionalProperties = FALSE, properties = list(
    instructions = list(type = c("string", "null"), title = "Estilo da personagem",
      description = "Descrições da história", enum = styles, default = styles[[2]]),
    voice = list(type = c("string", "null"), enum = list(NULL, "mimi"))))
  gen_save_parameter_schema("local-native", schema, model = "voice.gguf", block = "gen_tts")
  form <- gen_block_configuration("gen_tts", "local-native", "voice.gguf", 4, resolve = TRUE)
  expect_true(form$valid)
  prop <- form$arguments$properties$instructions
  expect_identical(prop$title, "Estilo da personagem")
  expect_identical(prop$description, "Descrições da história")
  expect_identical(prop$enum, styles)
  expect_false(isTRUE(prop[["x-ui"]]$disabled))
  expect_identical(form$arguments$properties$voice$enum, list(NULL))
  expect_true(form$arguments$properties$voice[["x-ui"]]$disabled)
  args <- list(text = "Olá", service = "local-native", model = "voice.gguf", parameter_schema = form$parameter_schema)
  expect_identical(gen_validate_call("gen_tts", args, 4)$args$instructions, styles[[2]])
  expect_null(gen_validate_call("gen_tts", c(args, list(instructions = NULL)), 4)$args$instructions)
  expect_false(gen_validate_call("gen_tts", c(args, list(instructions = "Outra voz")), 4)$valid)
  expect_false(gen_validate_call("gen_tts", c(args, list(voice = "mimi")), 4)$valid)
  schema$properties$instructions$default <- styles[[3]]
  gen_save_parameter_schema("local-native", schema, model = "voice.gguf", block = "gen_tts", expected_revision = 1)
  expect_identical(gen_validate_call("gen_tts", args, 4)$args$instructions, styles[[2]])
  schema$properties$instructions <- list(type = "null", enum = list(NULL))
  gen_save_parameter_schema("local-native", schema, model = "voice.gguf", block = "gen_tts", expected_revision = 2)
  restricted <- gen_block_configuration("gen_tts", "local-native", "voice.gguf", 4, resolve = TRUE)
  expect_identical(restricted$arguments$properties$instructions$enum, list(NULL))
})

test_that("direct and agent TTS honor the same description profile", {
  f <- native_tts_fixture()
  withr::local_options(list(genflow.models_dir = f$root, genflow.runtime_snapshot = NULL))
  gen_save_parameter_schema("local-native", list(type = "object", additionalProperties = FALSE,
    properties = list(instructions = list(type = c("string", "null"), default = "Voz jovem e alegre"))), block = "gen_tts")
  profile <- gen_resolve_parameter_schema("local-native", "auto", "gen_tts")
  setup <- set_setup("voice-style", service = "local-native", model = "auto", type = "TTS",
    format = "wav", parameter_schema = profile, save = FALSE, assign = FALSE)
  agent <- set_agent("voice-style", setup = setup, content = list(text = "Olá"), save = FALSE, assign = FALSE)
  calls <- list(
    function() gen_tts("Olá", service = "local-native", format = "wav", parameter_schema = profile, persist = FALSE),
    function() gen_tts(agent, persist = FALSE))
  for (call in calls) {
    result <- call()
    expect_identical(result$status_api, "SUCCESS"); unlink(result$response_value)
    args <- unlist(jsonlite::read_json(f$record)$args)
    expect_identical(args[match("--tts", args) + 1L], "(Voz jovem e alegre) Olá")
    expect_identical(result$parameters$parameter_schema, profile)
  }
  cleared <- gen_tts(agent, instructions = NULL, persist = FALSE)
  expect_identical(cleared$status_api, "SUCCESS"); unlink(cleared$response_value)
  args <- unlist(jsonlite::read_json(f$record)$args)
  expect_identical(args[match("--tts", args) + 1L], "Olá")
})
