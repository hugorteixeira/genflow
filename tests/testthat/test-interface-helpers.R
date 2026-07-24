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
  expect_match(html, "Local inference", fixed = TRUE)
  expect_match(html, "local_hf_stt_model", fixed = TRUE)
  expect_match(html, "local_hf_revision", fixed = TRUE)
  expect_match(html, "local_stt_native_engine", fixed = TRUE)
  expect_match(html, "local_stt_native_executable", fixed = TRUE)
  expect_match(html, "local_stt_native_model", fixed = TRUE)
  expect_match(html, "hf://owner/repo/model.gguf", fixed = TRUE)
  expect_match(html, "local_stt_native_backend", fixed = TRUE)
  expect_match(html, "local_stt_native_quant", fixed = TRUE)
  expect_match(html, "local_stt_native_device", fixed = TRUE)
  expect_match(html, "local_stt_models_table", fixed = TRUE)
  expect_match(html, "local_stt_models_refresh", fixed = TRUE)
  expect_match(html, "local_stt_model_use", fixed = TRUE)
  expect_match(html, "local_stt_model_download", fixed = TRUE)
  expect_match(html, "local_stt_download_progress_ui", fixed = TRUE)
  expect_match(html, "local_stt_model_delete", fixed = TRUE)
  expect_match(html, "Downloaded models", fixed = TRUE)
  expect_match(html, "Download current model", fixed = TRUE)
  expect_match(html, "Choose selected", fixed = TRUE)
  expect_match(html, "Delete selected", fixed = TRUE)
  expect_match(html, "Requested quantization", fixed = TRUE)
  expect_match(html, "Availability is verified before download", fixed = TRUE)
  expect_lt(
    regexpr("Download current model", html, fixed = TRUE)[[1]],
    regexpr("Downloaded models", html, fixed = TRUE)[[1]]
  )
  expect_false(grepl(">Use selected<", html, fixed = TRUE))
  expect_match(html, "or an omitted model uses this selection", fixed = TRUE)
  expect_match(
    html,
    "does not silently substitute another quantization",
    fixed = TRUE
  )
  expect_match(html, "local_adapter_tabs", fixed = TRUE)
  expect_match(html, "CrispASR \\(multiple GGUF families\\)")
  expect_match(html, "moss-transcribe.cpp \\(MOSS only\\)")
  expect_match(html, "service = \"local-native\"", fixed = TRUE)
  expect_false(grepl("local_moss_cpp_", html, fixed = TRUE))
  expect_false(grepl(">Runtime<", html, fixed = TRUE))
  expect_false(grepl(">Native STT runtime<", html, fixed = TRUE))
  expect_match(html, "This adapter uses PyTorch", fixed = TRUE)
  expect_match(html, "openai/whisper-large-v3-turbo", fixed = TRUE)
  expect_match(html, "openai/whisper-tiny", fixed = TRUE)
  expect_match(html, "Advanced settings", fixed = TRUE)
  expect_match(html, "local_diagnostics_run", fixed = TRUE)
  expect_match(html, "gf-diagnostics-table", fixed = TRUE)
  expect_match(
    genflow:::.theme_css,
    ".gf-diagnostics-table table.dataTable { table-layout: fixed; }",
    fixed = TRUE
  )
  expect_match(genflow:::.theme_css, "overflow-wrap: anywhere", fixed = TRUE)
})

test_that("Native STT model choices combine automatic, cached, and custom values", {
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

  choices <- genflow:::.local_native_model_choices(
    normalized,
    "hf://owner/repo:model-q8_0.gguf"
  )
  expect_identical(
    unname(choices[[1]]),
    "auto"
  )
  expect_true(all(inventory$path %in% unname(choices)))
  expect_true("hf://owner/repo:model-q8_0.gguf" %in% unname(choices))

  backend_choices <- genflow:::.local_native_backend_choices()
  expect_identical(unname(backend_choices[[1]]), "")
})

test_that("Hugging Face STT presets stay simple and preserve custom models", {
  presets <- genflow:::.local_hf_stt_model_choices()
  expect_identical(
    unname(presets[["Whisper Large v3 Turbo (recommended)"]]),
    "openai/whisper-large-v3-turbo"
  )
  expect_identical(
    unname(presets[["Whisper Tiny (smoke test)"]]),
    "openai/whisper-tiny"
  )

  custom <- genflow:::.local_hf_stt_model_choices("owner/custom-asr")
  expect_true("owner/custom-asr" %in% unname(custom))
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
