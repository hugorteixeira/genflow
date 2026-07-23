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
  expect_match(html, "local_stt_native_device", fixed = TRUE)
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
