parameter_scope <- function(env = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = env)
  withr::local_options(list(genflow.models_dir = file.path(root, "catalog"),
    genflow.providers_path = file.path(root, "providers.json"), genflow.cache_dir = file.path(root, "objects"),
    genflow.local_config_path = file.path(root, "local.json"), genflow.runtime_snapshot = NULL), .local_envir = env)
  root
}
parameter_schema <- function(...) list(type = "object", properties = list(...), additionalProperties = FALSE)
parameter_cfg <- function(default = 2) parameter_schema(model_options = list(type = "object", default = structure(list(), names = character()),
  properties = list(cfg = list(type = "number", minimum = 0, default = default)), additionalProperties = FALSE))
parameter_json <- function(x) jsonlite::parse_json(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", digits = NA), simplifyVector = FALSE)

test_that("parameter revisions reuse catalog storage without rewriting existing files", {
  root <- parameter_scope(); catalog <- getOption("genflow.models_dir"); dir.create(catalog)
  csv <- file.path(catalog, "example.csv"); writeLines("service,model,type,pricing,description\nexample,voice,TTS,,keep", csv)
  favorites <- file.path(catalog, "favorites.rds"); saveRDS(list(keep = TRUE), favorites)
  before <- lapply(c(csv, favorites), function(p) readBin(p, "raw", file.info(p)$size))
  expect_length(gen_parameter_schemas(), 0)
  expect_false(file.exists(file.path(catalog, "parameter-schemas.json")))
  first <- gen_save_parameter_schema("native_stt", parameter_cfg(), block = "gen_tts")
  expect_identical(first$service, "local-native"); expect_equal(first$revision, 1)
  retry <- gen_save_parameter_schema("local-native", parameter_cfg(), block = "gen_tts")
  expect_identical(retry, parameter_json(first))
  second <- gen_save_parameter_schema("local-native", parameter_cfg(1), block = "gen_tts", expected_revision = 1)
  expect_equal(second$revision, 2)
  expect_error(gen_save_parameter_schema("local-native", parameter_cfg(3), block = "gen_tts", expected_revision = 1), class = "genflow_error")
  expect_length(gen_parameter_schemas(history = TRUE), 2)
  expect_equal(gen_parameter_schemas()[[1]]$revision, 2)
  old <- gen_resolve_parameter_schema("local-native", "renamed.gguf", "gen_tts", revisions = list(service = 1))
  current <- gen_resolve_parameter_schema("local-native", "renamed.gguf", "gen_tts")
  expect_equal(gen_validate_parameters(structure(list(), names = character()), old)$values$model_options$cfg, 2)
  expect_equal(gen_validate_parameters(structure(list(), names = character()), current)$values$model_options$cfg, 1)
  expect_error(gen_resolve_parameter_schema("local-native", "renamed.gguf", "gen_tts", revisions = list(service = 9)), class = "genflow_error")
  expect_null(gen_resolve_parameter_schema("local-native", "renamed.gguf", "gen_tts", revisions = list(service = 0, model = 0)))
  expect_identical(lapply(c(csv, favorites), function(p) readBin(p, "raw", file.info(p)$size)), before)
  expect_false(file.exists(file.path(root, "providers.json")))
})

test_that("service/model scopes compose whole properties and preserve explicit values", {
  parameter_scope()
  common <- parameter_schema(endpoint = list(type = "string", default = "https://example.test"),
    mode = list(type = "string", enum = list("small", "large"), default = "small"))
  common$required <- list("endpoint")
  gen_save_parameter_schema("custom", common)
  gen_save_parameter_schema("custom", parameter_schema(mode = list(type = c("string", "null"), default = NULL)), model = "Model-A")
  combined <- gen_resolve_parameter_schema("custom", "Model-A")
  expect_equal(combined$revisions, list(service = 1, model = 1))
  expect_null(combined$schema$properties$mode$enum)
  expect_identical(combined$schema$required, list("endpoint"))
  value <- gen_validate_parameters(list(mode = NULL), combined)
  expect_true(value$valid); expect_null(value$values$mode)
  expect_identical(value$values$endpoint, "https://example.test")
  expect_equal(gen_resolve_parameter_schema("custom", "model-a")$revisions$model, 0)
  expect_false(gen_validate_parameters(list(endpoint = 42, mode = TRUE, surprise = 1), combined)$valid)
  expect_length(gen_validate_parameters(list(endpoint = 42, mode = TRUE, surprise = 1), combined)$errors, 3)
  expect_length(gen_parameter_schemas(service = "custom", model = "Model-A"), 1)
  expect_false(file.exists(genflow:::.genflow_provider_registry_path()))
})

test_that("invalid schemas, defaults and unsafe generator fields fail before saving", {
  parameter_scope()
  invalid <- list(list(type = "object"), parameter_schema(x = list(type = "number", default = "wrong")),
    parameter_schema(x = list(type = "object", additionalProperties = TRUE)),
    parameter_schema(x = list(type = "string", pattern = "[")), parameter_schema(x = list(type = "string", unrecognized = TRUE)))
  for (schema in invalid) expect_error(gen_save_parameter_schema("custom", schema), class = "genflow_validation_error")
  defaults <- parameter_schema(a = list(type = "number", default = "bad"), b = list(type = "boolean", default = "bad"))
  error <- tryCatch(gen_save_parameter_schema("custom", defaults), error = identity)
  expect_length(error$errors, 2)
  for (field in c("service", "model", "prompt", "executable", "base_url", "parameter_schema", "reference_consent")) {
    schema <- parameter_schema(); schema$properties[[field]] <- list(type = "string")
    expect_error(gen_save_parameter_schema("local-native", schema, block = "gen_tts"), class = "genflow_validation_error", info = field)
  }
  for (field in c("prompt", "temperature", "headers", "command", "reference_audio")) {
    schema <- parameter_cfg(); schema$properties$model_options$properties <- stats::setNames(list(list(type = "string")), field)
    expect_error(gen_save_parameter_schema("local-native", schema, block = "gen_tts"), class = "genflow_validation_error", info = field)
  }
  expect_error(gen_save_parameter_schema("custom", parameter_schema(secret = list(type = "string", default = "secret-value"))))
  expect_error(gen_save_parameter_schema("custom", parameter_schema(x = list(type = "number", default = Inf))))
  expect_error(gen_save_parameter_schema("custom", parameter_schema(x = list(type = "string", default = function() 1))))
  expect_length(gen_parameter_schemas(history = TRUE), 0)
})

test_that("snapshots are self-contained and verify identity and content", {
  root <- parameter_scope()
  gen_save_parameter_schema("local-native", parameter_cfg(), block = "gen_tts")
  prepared <- gen_validate_call("gen_tts", list(text = "Olá", service = "local-native", model = "voice.gguf", model_options = list(cfg = 1)), 3)
  expect_true(prepared$valid, info = paste(vapply(prepared$errors, `[[`, "", "message"), collapse = "; "))
  expect_equal(prepared$args$parameter_schema$revisions$service, 1)
  copy <- parameter_json(prepared$args)
  withr::local_options(list(genflow.models_dir = file.path(root, "destination")))
  expect_true(gen_validate_call("gen_tts", copy, 3)$valid)
  expect_false(dir.exists(getOption("genflow.models_dir")))
  changed <- copy; changed$model <- "other.gguf"
  expect_true("parameter_schema_mismatch" %in% vapply(gen_validate_call("gen_tts", changed, 3)$errors, `[[`, "", "code"))
  changed <- copy; changed$parameter_schema$schema$properties$model_options$properties$cfg$default <- 7
  expect_true("parameter_schema_integrity" %in% vapply(gen_validate_call("gen_tts", changed, 3)$errors, `[[`, "", "code"))
  expect_false(gen_validate_call("gen_tts", list(text = "Olá", service = "local-native", model_options = list(cfg = 1)), 3)$valid)
  expect_false(gen_validate_call("gen_tts", copy, 2)$valid)
  # Even a correctly rehashed imported schema cannot introduce transport fields.
  changed <- copy; changed$parameter_schema$schema$properties$executable <- list(type = "string", default = "/bad")
  changed$parameter_schema$sha256 <- genflow:::.gen_parameter_hash(changed$parameter_schema[setdiff(names(changed$parameter_schema), "sha256")])
  expect_true("parameter_schema_reserved" %in% vapply(gen_validate_call("gen_tts", changed, 3)$errors, `[[`, "", "code"))
})

test_that("the normal validator aggregates profile errors and refuses unsupported execution", {
  parameter_scope()
  schema <- parameter_cfg(); schema$properties$model_options$properties$variant <- list(type = "string", enum = list("a", "b"))
  gen_save_parameter_schema("local-native", schema, block = "gen_tts")
  checked <- gen_validate_call("gen_tts", list(text = "", service = "local-native", model_options = list(cfg = "bad", variant = "z", typo = 1)), 3)
  expect_false(checked$valid)
  expect_true(all(c("/args/text", "/args/model_options/cfg", "/args/model_options/variant", "/args/model_options/typo") %in%
    vapply(checked$errors, `[[`, "", "path")))
  gen_save_parameter_schema("openai", parameter_cfg(), block = "gen_tts")
  testthat::local_mocked_bindings(.tts_openai = function(...) stop("SHOULD NOT CALL"), .package = "genflow")
  error <- tryCatch(gen_tts("Olá", service = "openai", model_options = list(cfg = 1)), error = identity)
  expect_s3_class(error, "genflow_validation_error")
  expect_true("model_options_unsupported" %in% vapply(error$errors, `[[`, "", "code"))
  expect_false(grepl("SHOULD NOT CALL", conditionMessage(error)))
})

test_that("bad registries are never silently replaced", {
  parameter_scope()
  gen_save_parameter_schema("custom", parameter_schema(x = list(type = "number")))
  path <- file.path(getOption("genflow.models_dir"), "parameter-schemas.json")
  for (text in c("{broken", '{"schema_version":1,"records":{}}', '{"schema_version":2,"records":[]}')) {
    writeLines(text, path)
    expect_error(gen_parameter_schemas(), class = "genflow_error")
    expect_error(gen_save_parameter_schema("custom", parameter_schema()), class = "genflow_error")
    expect_identical(readLines(path), text)
  }
})

test_that("concurrent creates cannot lose a registered revision", {
  skip_on_os("windows")
  parameter_scope()
  results <- parallel::mclapply(c(1, 2), function(n) tryCatch({
    gen_save_parameter_schema("custom", parameter_schema(x = list(type = "number", default = n)))
    "saved"
  }, error = function(e) e$diagnostic$code), mc.cores = 2)
  expect_setequal(unlist(results), c("saved", "parameter_schema_conflict"))
  expect_length(gen_parameter_schemas(history = TRUE), 1)
})

test_that("prepared portable workflows retain profiles through materialization", {
  root <- parameter_scope()
  gen_save_parameter_schema("local-native", parameter_cfg(), block = "gen_tts")
  agent <- list(operation = "audio.speak", contract_version = 3, service = "local-native", model = "voice.gguf", prompt_version_id = "prompt", parameters = list(model_options = list(cfg = 1)))
  document <- list(format = "genflow-studio-definition", schema_version = 1,
    root = list(kind = "workflow", name = "Voice", body = list(schema_version = 1, nodes = list(
      list(id = "voice", type = "generate", config = list(agent_version_id = "agent")),
      list(id = "out", type = "output", inputs = list(audio = list(node = "voice", port = "candidates")))))),
    resources = list(list(key = "agent", kind = "agent", name = "Voice", body = agent),
      list(key = "prompt", kind = "prompt", name = "Line", body = list(template = "Olá, Mimi!"))))
  report <- gen_validate_structure(document)
  expect_true(report$valid, info = paste(vapply(report$errors, `[[`, "", "message"), collapse = "; "))
  exported <- parameter_json(report$document)
  pinned <- exported$resources[[1]]$body$parameters$parameter_schema
  expect_equal(pinned$revisions$service, 1)
  gen_save_parameter_schema("local-native", parameter_cfg(7), block = "gen_tts", expected_revision = 1)
  withr::local_options(list(genflow.models_dir = file.path(root, "empty")))
  restored <- gen_materialize_structure(exported, prefix = "copy")
  expect_true(restored$valid)
  expect_identical(restored$document$resources[[1]]$body$parameters$parameter_schema, pinned)
  expect_equal(restored$document$resources[[1]]$body$parameters$model_options$cfg, 1)
  expect_false(dir.exists(getOption("genflow.models_dir")))
  exported$resources[[1]]$body$parameters$model_options$cfg <- "invalid"
  bad <- gen_validate_structure(exported)
  expect_false(bad$valid)
  expect_true("/resources/0/body/parameters/model_options/cfg" %in% vapply(bad$errors, `[[`, "", "path"))
})

test_that("provider JSON options and model reasoning flow through direct, agent and batch calls", {
  parameter_scope()
  withr::local_envvar(c(OPENAI_API_KEY = "fixture", genflow_SKIP_PERSIST_LOG = "1"))
  schema <- parameter_schema(temp = list(type = "number", default = 0.3), reasoning = list(type = c("string", "null"), enum = list("low", "ultra", NULL)),
    model_options = list(type = "object", properties = list(top_p = list(type = "number", minimum = 0, maximum = 1, default = 0.4)), additionalProperties = FALSE))
  gen_save_parameter_schema("openai", schema, block = "gen_txt")
  profile <- gen_resolve_parameter_schema("openai", "example-model", "gen_txt")
  seen <- list()
  testthat::local_mocked_bindings(POST = function(url, ..., body) {
    request <- if (is.character(body)) jsonlite::fromJSON(body, simplifyVector = FALSE) else body
    seen[[length(seen) + 1L]] <<- request
    payload <- if (grepl("/responses$", url)) list(output = list(list(type = "message", content = list(list(type = "output_text", text = "ok"))))) else list(choices = list(list(message = list(content = "ok"))))
    structure(list(url = url, status_code = 200L, headers = list(`content-type` = "application/json"),
      content = charToRaw(jsonlite::toJSON(payload, auto_unbox = TRUE))), class = "response")
  }, .package = "httr")
  direct <- gen_txt("Hello", model = "example-model", parameter_schema = profile, model_options = list(top_p = 0.2), persist = FALSE)
  expect_identical(direct$status_api, "SUCCESS")
  expect_equal(seen[[1]]$temperature, 0.3); expect_equal(seen[[1]]$top_p, 0.2)
  setup <- set_setup("schema-example", service = "openai", model = "example-model", type = "Chat", reasoning = "ultra",
    parameter_schema = profile, model_options = list(top_p = 0.5), save = FALSE, assign = FALSE)
  agent <- set_agent("schema-example", setup = setup, content = list(context = "Hello"), save = FALSE, assign = FALSE)
  via_agent <- gen_txt(agent, persist = FALSE)
  via_batch <- gen_batch_agent(agent, qty = 1, workers = 1, persist = FALSE, verbose = FALSE, always_fix_errors = FALSE)
  expect_identical(via_agent$status_api, "SUCCESS"); expect_identical(via_batch[[1]]$status_api, "SUCCESS")
  expect_length(seen, 3)
  for (request in seen[-1]) { expect_equal(request$top_p, 0.5); expect_identical(request$reasoning$effort, "ultra") }
  expect_error(gen_txt("Hello", model = "example-model", reasoning = "bogus", parameter_schema = profile, persist = FALSE), class = "genflow_validation_error")
  explicit <- gen_txt("Hello", model = "example-model", temp = 0.9, parameter_schema = profile, persist = FALSE)
  expect_identical(explicit$status_api, "SUCCESS"); expect_equal(seen[[4]]$temperature, 0.9)
})

test_that("model option fields inherit service options without recursive value merges", {
  parameter_scope()
  common <- parameter_schema(model_options = parameter_schema(top_p = list(type = "number", default = 0.7),
    detail = list(type = "string", enum = list("low", "high"), default = "low")))
  specific <- parameter_schema(model_options = parameter_schema(detail = list(type = "string", enum = list("low", "ultra"), default = "ultra"),
    seed = list(type = "integer", default = 3)))
  gen_save_parameter_schema("openai", common, block = "gen_txt")
  gen_save_parameter_schema("openai", specific, model = "custom-model", block = "gen_txt")
  profile <- gen_resolve_parameter_schema("openai", "custom-model", "gen_txt")
  expect_setequal(names(profile$schema$properties$model_options$properties), c("top_p", "detail", "seed"))
  prepared <- gen_validate_call("gen_txt", list(context = "Hi", service = "openai", model = "custom-model", parameter_schema = profile), 2)
  expect_true(prepared$valid)
  expect_equal(prepared$args$model_options, list(top_p = 0.7, detail = "ultra", seed = 3))
  changed <- prepared$args; changed$model_options$detail <- "high"
  expect_false(gen_validate_call("gen_txt", changed, 2)$valid)
})

test_that("CFG travels through the actual native initialization channel", {
  parameter_scope(); f <- native_tts_fixture()
  withr::local_envvar(c(CRISPASR_VOXCPM2_CFG_VALUE = "99"))
  gen_save_parameter_schema("local-native", parameter_cfg(), block = "gen_tts")
  output <- gen_tts("Olá, Mimi!", service = "local-native", format = "wav", model_options = list(cfg = 1), persist = FALSE)
  expect_identical(output$status_api, "SUCCESS"); withr::defer(unlink(output$response_value))
  expect_equal(output$native$cfg, 1)
  expect_identical(jsonlite::read_json(f$record)$cfg, "1")
  expect_identical(Sys.getenv("CRISPASR_VOXCPM2_CFG_VALUE"), "99")
  expect_equal(output$parameters$parameter_schema$revisions$service, 1)
})

test_that("compatible provider, llama.cpp and Replicate reuse existing transports", {
  parameter_scope()
  set_provider_openai_compat("option-test", base_url = "http://127.0.0.1:1", api_key_required = FALSE,
    default_model = "example", supports_reasoning = TRUE, reasoning_field = "effort")
  schema <- parameter_schema(model_options = parameter_schema(top_p = list(type = "number", minimum = 0, maximum = 1)))
  for (service in c("option-test", "llamacpp", "groq", "openrouter"))
    gen_save_parameter_schema(service, schema, block = "gen_txt")
  withr::local_envvar(c(GROQ_API_KEY = "fixture", OPENROUTER_API_KEY = "fixture"))
  seen <- list()
  testthat::local_mocked_bindings(POST = function(url, ..., body) {
    seen[[length(seen) + 1L]] <<- jsonlite::fromJSON(body, simplifyVector = FALSE)
    payload <- list(choices = list(list(message = list(content = "ok"))))
    structure(list(url = url, status_code = 200L, headers = list(`content-type` = "application/json"),
      content = charToRaw(jsonlite::toJSON(payload, auto_unbox = TRUE))), class = "response")
  }, .package = "httr")
  for (service in c("option-test", "llamacpp", "groq", "openrouter")) {
    result <- gen_txt("Hello", service = service, model = "example", model_options = list(top_p = 0.25), persist = FALSE)
    expect_identical(result$status_api, "SUCCESS", info = service)
    expect_equal(tail(seen, 1)[[1]]$top_p, 0.25)
  }
  image_schema <- parameter_schema(model_options = parameter_schema(style = list(type = "string")))
  gen_save_parameter_schema("replicate", image_schema, model = "example/image", block = "gen_img")
  png <- tempfile(fileext = ".png"); withr::defer(unlink(png))
  grDevices::png(png, width = 32, height = 32); graphics::par(mar = c(0,0,0,0)); graphics::plot.new(); grDevices::dev.off()
  captured <- NULL
  testthat::local_mocked_bindings(.gen_img_replicate = function(replicate_input, ...) {
    captured <<- replicate_input; copy <- tempfile(fileext = ".png"); file.copy(png, copy); copy
  }, .package = "genflow")
  image <- gen_img("Garden", service = "replicate", model = "example/image", model_options = list(style = "soft"), persist = FALSE)
  expect_identical(image$status_api, "SUCCESS"); withr::defer(unlink(image$response_value))
  expect_identical(captured, list(style = "soft"))
  expect_error(gen_img("Garden", service = "replicate", model = "example/image", model_options = list(style = "soft"),
    replicate_input = list(style = "other")), class = "genflow_validation_error")
})

test_that("batch snapshots freeze registry revisions before workers begin", {
  parameter_scope()
  gen_save_parameter_schema("local-native", parameter_cfg(), block = "gen_tts")
  snapshot <- genflow:::.genflow_snapshot_config()
  gen_save_parameter_schema("local-native", parameter_cfg(1), block = "gen_tts", expected_revision = 1)
  expect_equal(gen_resolve_parameter_schema("local-native", "voice", "gen_tts")$revisions$service, 2)
  withr::local_options(list(genflow.runtime_snapshot = snapshot))
  expect_equal(gen_resolve_parameter_schema("local-native", "voice", "gen_tts")$revisions$service, 1)
})


test_that("R schemas serialize singleton arrays and empty objects as JSON Schema", {
  root <- parameter_scope()
  schema <- parameter_schema(mode = list(type = "string", enum = "only"),
    options = parameter_schema())
  schema$required <- "mode"
  saved <- gen_save_parameter_schema("custom", schema)
  expect_identical(saved$schema$required, list("mode"))
  expect_identical(saved$schema$properties$mode$enum, list("only"))
  expect_identical(names(saved$schema$properties$options$properties), character())
  expect_length(gen_parameter_schemas(), 1)
  schema$enum <- list(list(mode = "only"))
  expect_error(gen_save_parameter_schema("custom", schema), class = "genflow_validation_error")
  expect_error(gen_save_parameter_schema("custom", parameter_schema(x = list())), class = "genflow_validation_error")
})

test_that("existing configuration archives carry validated schema history", {
  skip_if(!nzchar(Sys.which("zip")))
  root <- parameter_scope()
  source <- getOption("genflow.models_dir"); destination <- file.path(root, "imported")
  gen_save_parameter_schema("local-native", parameter_cfg(), block = "gen_tts")
  gen_save_parameter_schema("local-native", parameter_cfg(1), block = "gen_tts", expected_revision = 1)
  bundle <- file.path(root, "catalog.zip")
  gen_export_bundle(bundle, include_setups = FALSE, include_agents = FALSE, include_content = FALSE, overwrite = TRUE, quiet = TRUE)
  gen_import_bundle(bundle, models_dir = destination, quiet = TRUE)
  expect_identical(gen_parameter_schemas(directory = destination, history = TRUE), gen_parameter_schemas(history = TRUE))
  writeLines('{"schema_version":2,"records":[]}', file.path(source, "parameter-schemas.json"))
  before <- readBin(bundle, "raw", file.info(bundle)$size)
  expect_error(gen_export_bundle(bundle, include_setups = FALSE, include_agents = FALSE, include_content = FALSE, overwrite = TRUE, quiet = TRUE), class = "genflow_error")
  expect_identical(readBin(bundle, "raw", file.info(bundle)$size), before)
  extracted <- file.path(root, "extracted"); dir.create(extracted)
  utils::unzip(bundle, exdir = extracted)
  registry <- list.files(extracted, pattern = "parameter-schemas.json", recursive = TRUE, full.names = TRUE)
  writeLines('{"schema_version":2,"records":[]}', registry)
  corrupt <- file.path(root, "corrupt.zip")
  withr::with_dir(extracted, utils::zip(corrupt, list.files(), flags = "-r9Xq"))
  before <- readLines(file.path(destination, "parameter-schemas.json"))
  expect_error(gen_import_bundle(corrupt, models_dir = destination, overwrite = TRUE, quiet = TRUE), class = "genflow_error")
  expect_identical(readLines(file.path(destination, "parameter-schemas.json")), before)
})


test_that("new block contracts do not invalidate previously registered profiles", {
  parameter_scope()
  gen_save_parameter_schema("openai", parameter_schema(temp = list(type = "number", default = 0.3)), block = "gen_txt")
  old <- gen_resolve_parameter_schema("openai", "example", "gen_txt")
  catalog <- gen_block_contracts()
  future <- gen_block_contracts("gen_txt", 2)
  future$version <- 99L; future$arguments$properties$temp <- list(type = "string")
  catalog[["gen_txt@99"]] <- future
  original <- gen_block_contracts
  testthat::local_mocked_bindings(gen_block_contracts = function(block = NULL, version = NULL) {
    if (is.null(block)) catalog else original(block, version)
  }, .package = "genflow")
  expect_identical(gen_resolve_parameter_schema("openai", "example", "gen_txt"), old)
  expect_equal(gen_validate_parameters(list(), old)$values$temp, 0.3)
  expect_true(gen_validate_call("gen_txt", list(context = "Hello", service = "openai", model = "example", parameter_schema = old), 2)$valid)
})


test_that("every profiled generator resolves defaults through its existing contract", {
  parameter_scope()
  cases <- list(
    list(block = "gen_txt", version = 2L, args = list(context = "Hello"), field = "temp", value = 0.3),
    list(block = "gen_img", version = 3L, args = list(prompt = "Garden"), field = "steps", value = 20L),
    list(block = "gen_stt", version = 2L, args = list(audio = "audio.wav"), field = "language", value = "pt"),
    list(block = "gen_tts", version = 3L, args = list(text = "Hello"), field = "speed", value = 0.9),
    list(block = "gen_sfx", version = 2L, args = list(prompt = "Wings"), field = "duration_seconds", value = 2),
    list(block = "gen_music", version = 2L, args = list(prompt = "Piano"), field = "duration_seconds", value = 10))
  for (case in cases) {
    base <- gen_validate_call(case$block, case$args, case$version)
    expect_true(base$valid, info = case$block)
    property <- list(type = if (is.character(case$value)) "string" else "number", default = case$value)
    schema <- parameter_schema(); schema$properties <- stats::setNames(list(property), case$field)
    gen_save_parameter_schema(base$args$service, schema, block = case$block)
    args <- case$args
    args$parameter_schema <- gen_resolve_parameter_schema(base$args$service, base$args$model, case$block)
    checked <- gen_validate_call(case$block, args, case$version)
    expect_true(checked$valid, info = case$block)
    expect_equal(checked$args[[case$field]], case$value, info = case$block)
  }
})

test_that("malformed canonical identities and named required arrays fail explicitly", {
  parameter_scope()
  schema <- parameter_cfg(); schema$required <- list(typo = "model_options")
  expect_error(gen_save_parameter_schema("local-native", schema, block = "gen_tts"), class = "genflow_validation_error")
  gen_save_parameter_schema("local-native", parameter_cfg(), block = "gen_tts")
  path <- file.path(getOption("genflow.models_dir"), "parameter-schemas.json")
  content <- jsonlite::read_json(path)
  content$records[[1]]$service <- "native_stt"
  jsonlite::write_json(content, path, auto_unbox = TRUE, null = "null")
  error <- tryCatch(gen_parameter_schemas(), error = identity)
  expect_identical(gen_diagnostic(error)$code, "parameter_registry_corrupt")
})


test_that("STT schema identities normalize aliases without changing engine selection", {
  parameter_scope()
  gen_save_parameter_schema("local-cli", parameter_schema(language = list(type = "string", default = "pt")), block = "gen_stt")
  profile <- gen_resolve_parameter_schema("moss_cpp", block = "gen_stt")
  expect_identical(profile$service, "local-native")
  checked <- gen_validate_call("gen_stt", list(audio = "voice.wav", service = "moss_cpp", parameter_schema = profile), 2)
  expect_true(checked$valid)
  expect_identical(checked$args$service, "moss_cpp")
  expect_identical(checked$args$language, "pt")
})


test_that("STT cache signatures use effective profile values and preserve explicit NULL", {
  parameter_scope()
  schema <- parameter_schema(language = list(type = c("string", "null"), default = "pt"))
  gen_save_parameter_schema("openai", schema, block = "gen_stt")
  profile <- gen_resolve_parameter_schema("openai", block = "gen_stt")
  args <- list(parameter_schema = profile)
  expect_identical(gen_stt_signature(stt_args = args), gen_stt_signature(language = "pt"))
  expect_identical(gen_stt_signature(language = NULL, stt_args = args), gen_stt_signature(language = NULL))
  expect_false(identical(gen_stt_signature(stt_args = args), gen_stt_signature(language = NULL, stt_args = args)))
  expect_identical(gen_stt_signature(language = "en", stt_args = args), gen_stt_signature(language = "en"))
  schema$description <- "A metadata-only revision"
  gen_save_parameter_schema("openai", schema, block = "gen_stt", expected_revision = 1)
  other <- gen_resolve_parameter_schema("openai", block = "gen_stt")
  expect_identical(gen_stt_signature(stt_args = list(parameter_schema = other)), gen_stt_signature(stt_args = args))
  expect_error(gen_stt_signature(language = 42, stt_args = args), class = "genflow_validation_error")
})
