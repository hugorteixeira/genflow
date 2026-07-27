test_that("reasoning settings load canonical and legacy setup fields", {
  canonical <- genflow:::.normalize_setup_reasoning(list(reasoning = "high"))
  expect_true(canonical$enabled)
  expect_equal(canonical$level, "high")

  legacy <- genflow:::.normalize_setup_reasoning(list(
    thinking = TRUE,
    thinking_budget = "low"
  ))
  expect_true(legacy$enabled)
  expect_equal(legacy$level, "low")

  disabled <- genflow:::.normalize_setup_reasoning(list())
  expect_false(disabled$enabled)
  expect_equal(disabled$level, "medium")
})

test_that("OpenRouter online model suffix is split exactly once", {
  base <- genflow:::.openrouter_model_state("openai/gpt-4.1")
  expect_false(base$online)
  expect_equal(base$base, "openai/gpt-4.1")

  online <- genflow:::.openrouter_model_state("openai/gpt-4.1:online")
  expect_true(online$online)
  expect_equal(online$base, "openai/gpt-4.1")
})

test_that("entity selection keeps names out of executable JavaScript", {
  hostile_name <- "x'); alert('stored-xss"
  attributes <- genflow:::.entity_select_attributes(
    "setup_select_trigger",
    hostile_name
  )
  node <- do.call(
    htmltools::div,
    c(list("Visible label"), attributes)
  )
  rendered <- htmltools::renderTags(node)$html

  expect_identical(attributes[["data-name"]], hostile_name)
  expect_false(grepl(hostile_name, attributes$onclick, fixed = TRUE))
  expect_match(attributes$onclick, "this.dataset.name", fixed = TRUE)
  expect_match(rendered, "data-name=\"x&#39;\\); alert\\(&#39;stored-xss\"")
})

test_that("app exposes the local inference configuration surface", {
  html <- htmltools::renderTags(genflow:::.app_ui())$html
  expect_match(html, "data-value=\"Local\"", fixed = TRUE)
  expect_false(grepl("Local inference", html, fixed = TRUE))
  expect_false(grepl("local_python", html, fixed = TRUE))
  expect_false(grepl("local_device", html, fixed = TRUE))
  expect_false(grepl("local_dtype", html, fixed = TRUE))
  expect_false(grepl("local_hf_stt_model", html, fixed = TRUE))
  expect_false(grepl("local_hf_revision", html, fixed = TRUE))
  expect_match(html, "local_stt_native_engine", fixed = TRUE)
  expect_false(grepl("local_stt_native_executable", html, fixed = TRUE))
  expect_false(grepl("local_stt_native_model", html, fixed = TRUE))
  expect_false(grepl("local_stt_native_backend", html, fixed = TRUE))
  expect_false(grepl("local_stt_native_quant", html, fixed = TRUE))
  expect_match(html, "local_stt_new_model_reference", fixed = TRUE)
  expect_match(html, "local_stt_model_verify", fixed = TRUE)
  expect_match(
    html,
    "hf download hf://owner/repository/model.gguf",
    fixed = TRUE
  )
  expect_match(html, "/blob/main/model.gguf", fixed = TRUE)
  expect_match(html, "local_stt_native_device", fixed = TRUE)
  expect_match(html, "local_stt_models_table", fixed = TRUE)
  expect_match(html, "local_stt_model_download", fixed = TRUE)
  expect_match(html, "local_stt_download_progress_ui", fixed = TRUE)
  expect_match(html, "local_stt_new_model_status_ui", fixed = TRUE)
  expect_match(html, "local_stt_model_delete", fixed = TRUE)
  expect_match(html, "Downloaded models", fixed = TRUE)
  expect_match(html, "Hugging Face model link", fixed = TRUE)
  expect_match(html, ">Download<", fixed = TRUE)
  expect_match(html, ">Verify<", fixed = TRUE)
  expect_match(html, "Delete selected", fixed = TRUE)
  expect_match(html, "Verify checks the exact remote file", fixed = TRUE)
  expect_lt(
    regexpr("Hugging Face model link", html, fixed = TRUE)[[1]],
    regexpr("Downloaded models", html, fixed = TRUE)[[1]]
  )
  expect_false(grepl("local_stt_models_refresh", html, fixed = TRUE))
  expect_false(grepl("local_stt_model_use", html, fixed = TRUE))
  expect_false(grepl("Download current model", html, fixed = TRUE))
  expect_false(grepl("Requested quantization", html, fixed = TRUE))
  expect_false(grepl("local_stt_server_model", html, fixed = TRUE))
  expect_false(grepl("__new__", html, fixed = TRUE))
  expect_match(html, "local_adapter_tabs", fixed = TRUE)
  expect_match(
    genflow:::.theme_css,
    ".gf-local-shell > .tabbable > .tab-content { padding: 32px 24px 24px; }",
    fixed = TRUE
  )
  expect_match(
    genflow:::.theme_css,
    ".gf-local-shell > .tabbable > .tab-content { padding: 26px 16px 18px; }",
    fixed = TRUE
  )
  expect_match(
    genflow:::.theme_css,
    ".gf-local-shell > .tabbable > .nav > li > a",
    fixed = TRUE
  )
  expect_match(
    genflow:::.theme_css,
    ".gf-local-shell > .tabbable > .nav > li > a.active",
    fixed = TRUE
  )
  expect_match(html, "CrispASR \\(multiple GGUF families\\)")
  expect_match(html, "moss-transcribe.cpp \\(MOSS only\\)")
  expect_match(html, "service = \"local-native\"", fixed = TRUE)
  expect_false(grepl("local_moss_cpp_", html, fixed = TRUE))
  expect_false(grepl(">Runtime<", html, fixed = TRUE))
  expect_false(grepl(">Native STT runtime<", html, fixed = TRUE))
  expect_false(grepl("This adapter uses PyTorch", html, fixed = TRUE))
  expect_false(grepl("openai/whisper-large-v3-turbo", html, fixed = TRUE))
  expect_false(grepl("openai/whisper-tiny", html, fixed = TRUE))
  expect_match(html, "Advanced settings", fixed = TRUE)
  expect_match(html, "local_diagnostics_run", fixed = TRUE)
  expect_match(html, "gf-diagnostics-table", fixed = TRUE)
  expect_match(
    genflow:::.theme_css,
    ".gf-diagnostics-table table.dataTable { table-layout: fixed; }",
    fixed = TRUE
  )
  expect_match(
    genflow:::.theme_css,
    paste0(
      "\\.gf-local-model-table table\\.dataTable th,\\s*",
      "\\.gf-local-model-table table\\.dataTable td\\s*\\{",
      "[^}]*font-size:\\s*0\\.78rem;",
      "[^}]*line-height:\\s*1\\.25;",
      "[^}]*\\}"
    ),
    perl = TRUE
  )
  expect_match(genflow:::.theme_css, "overflow-wrap: anywhere", fixed = TRUE)
})

test_that("Native STT helpers expose managed downloads and exact HF references", {
  inventory <- data.frame(
    path = c("/cache/granite-q4_k.gguf", "/models/granite-q8_0.gguf"),
    filename = c("granite-q4_k.gguf", "granite-q8_0.gguf"),
    quant = c("q4_k", "q8_0"),
    size_bytes = c(1024, 2048),
    size = c("", "2 KB"),
    source_url = c("", ""),
    managed = c(TRUE, FALSE),
    selected = c(FALSE, TRUE),
    stringsAsFactors = FALSE
  )

  normalized <- genflow:::.local_native_inventory_normalize(inventory)
  expect_identical(normalized$size, c("1.00 KB", "2 KB"))

  managed <- genflow:::.local_native_managed_inventory(normalized)
  expect_identical(managed$path, "/cache/granite-q4_k.gguf")
  expect_identical(managed$filename, "granite-q4_k.gguf")

  filename <- "model-q8_0.gguf"
  expect_identical(
    genflow:::.local_native_hf_repository(
      paste0(
        "https://huggingface.co/owner/repository/resolve/main/",
        filename
      ),
      filename
    ),
    "owner/repository"
  )
  expect_identical(
    genflow:::.local_native_hf_repository(
      paste0(
        "https://huggingface.co/owner/repository/resolve/",
        strrep("a", 40),
        "/",
        filename
      ),
      filename
    ),
    "owner/repository"
  )
  expect_identical(
    genflow:::.local_native_hf_repository("", filename),
    "\u2014"
  )
  expect_identical(
    genflow:::.local_native_hf_repository(
      paste0("https://example.com/owner/repository/", filename),
      filename
    ),
    "\u2014"
  )
  expect_identical(
    genflow:::.local_native_hf_repository(
      paste0(
        "https://huggingface.co/owner/repository/resolve/main/",
        filename
      ),
      "different-model-q8_0.gguf"
    ),
    "\u2014"
  )

  expect_identical(
    genflow:::.local_native_hf_selector(
      "hf://owner/repo:model-q8_0.gguf"
    ),
    "hf://owner/repo:model-q8_0.gguf"
  )
  expect_identical(
    genflow:::.local_native_hf_selector(
      "https://huggingface.co/owner/repo/blob/main/model-q8_0.gguf"
    ),
    "hf://owner/repo:model-q8_0.gguf"
  )
  command <- paste(
    "hf download",
    "hf://handy-computer/whisper-large-v3-gguf/whisper-large-v3-Q8_0.gguf"
  )
  expect_identical(
    genflow:::.local_native_hf_reference_input(command),
    "hf://handy-computer/whisper-large-v3-gguf/whisper-large-v3-Q8_0.gguf"
  )
  expect_identical(
    genflow:::.local_native_hf_selector(command),
    paste0(
      "hf://handy-computer/whisper-large-v3-gguf:",
      "whisper-large-v3-Q8_0.gguf"
    )
  )
  expect_identical(
    genflow:::.local_native_hf_selector(
      "hf download 'hf://owner/repo/model-q8_0.gguf'"
    ),
    "hf://owner/repo:model-q8_0.gguf"
  )
  expect_error(
    genflow:::.local_native_hf_selector("https://example.com/model.gguf"),
    "Hugging Face"
  )
  expect_error(
    genflow:::.local_native_hf_selector(""),
    "Enter a Hugging Face model reference",
    fixed = TRUE
  )
})

test_that("Native STT deletion discovers models referenced by saved state", {
  config <- genflow:::.genflow_local_config_defaults()
  config$stt_native_model <- "configured.gguf"

  setups <- list(
    native = list(service = "local-native", model = "setup.gguf"),
    remote = list(service = "hf", model = "remote/model"),
    automatic = list(service = "local-native", model = "auto")
  )
  agents <- list(
    native = list(service = "moss-cpp", model = "agent.gguf"),
    remote = list(service = "openai", model = "gpt-5")
  )
  referenced <- genflow:::.local_native_referenced_models(
    config = config,
    setup_names = names(setups),
    agent_names = names(agents),
    setup_reader = function(name) setups[[name]],
    agent_reader = function(name) agents[[name]]
  )

  expect_setequal(
    referenced,
    c("configured.gguf", "setup.gguf", "agent.gguf")
  )
})

test_that("Hugging Face remains the remote provider label only", {
  expect_identical(genflow:::.model_label("hf"), "Hugging Face")
  expect_identical(get_provider("huggingface")$label, "Hugging Face")
  expect_false("hf-local" %in% genflow:::.model_provider_ids())
})

test_that("retired local Hugging Face ids cannot return through saved state", {
  retired <- c(
    "hf-local",
    "hf_local",
    "huggingface-local",
    "huggingface_local",
    "transformers"
  )
  favorites <- data.frame(
    service = c(retired, "hf"),
    model = c(paste0("legacy/", retired), "remote/model"),
    type = "Audio",
    stringsAsFactors = FALSE
  )

  normalized <- genflow:::.normalize_favorites(favorites, data.frame())
  expect_identical(normalized$service, "hf")
  expect_identical(normalized$model, "remote/model")

  directory <- tempfile("genflow-retired-favorites-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  saveRDS(favorites, genflow:::.favorites_path(directory))
  loaded <- genflow:::.load_favorites(directory)
  expect_identical(loaded$service, "hf")
  expect_identical(loaded$model, "remote/model")

  for (service in retired) {
    expect_error(
      genflow:::.genflow_normalize_custom_provider_config(list(
        id = service,
        base_urls = "http://127.0.0.1:8000"
      )),
      "retired"
    )
  }
})

test_that("interface binds locally unless remote exposure is explicit", {
  expect_equal(
    genflow:::.genflow_validate_interface_host("localhost"),
    "localhost"
  )
  expect_error(
    genflow:::.genflow_validate_interface_host("0.0.0.0"),
    "Refusing to expose"
  )
  expect_warning(
    expect_equal(
      genflow:::.genflow_validate_interface_host(
        "0.0.0.0",
        allow_remote = TRUE
      ),
      "0.0.0.0"
    ),
    "does not provide authentication"
  )
})
