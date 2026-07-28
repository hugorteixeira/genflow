test_that("agent argument preparation rejects silent override mistakes", {
  agent <- structure(
    list(context = "saved", service = "openai"),
    class = "genflow_agent"
  )
  target <- alist(context = , service = "openai", temp = 1, ... = )

  expect_error(
    genflow:::.genflow_prepare_agent_args(
      agent,
      list(typo = 2),
      target
    ),
    "Unsupported agent override(s): typo",
    fixed = TRUE
  )
  expect_error(
    genflow:::.genflow_prepare_agent_args(
      agent,
      structure(list(1), names = ""),
      target
    ),
    "must be named",
    fixed = TRUE
  )

  prepared <- genflow:::.genflow_prepare_agent_args(
    agent,
    list(context_override = "replacement"),
    target,
    required = "context",
    override_aliases = c(context_override = "context")
  )
  expect_identical(prepared$context, "replacement")
})

test_that("image agents reuse content context and support explicit override", {
  testthat::local_mocked_bindings(
    gen_img.default = function(prompt,
                               service = "hf",
                               model = NULL,
                               add = NULL,
                               ...) {
      list(prompt = prompt, service = service, model = model, add = add)
    },
    .package = "genflow"
  )
  agent <- structure(
    list(context = "saved image prompt", service = "hf"),
    class = "genflow_agent"
  )

  saved <- genflow:::gen_img.genflow_agent(agent)
  replaced <- genflow:::gen_img.genflow_agent(
    agent,
    prompt_override = "new image prompt"
  )
  expect_identical(saved$prompt, "saved image prompt")
  expect_identical(replaced$prompt, "new image prompt")
  expect_error(
    genflow:::gen_img.genflow_agent(agent, typo = TRUE),
    "Unsupported `gen_img()` override(s): typo",
    fixed = TRUE
  )
})

test_that("TTS agents reuse context and STT uses only current runtime fields", {
  testthat::local_mocked_bindings(
    gen_tts.default = function(text, service = "openai", ...) {
      list(text = text, service = service)
    },
    gen_stt.default = function(audio,
                               service = "openai",
                               native_engine = NULL,
                               native_backend = NULL,
                               diarize_speakers = FALSE,
                               diarize_embedder = TRUE,
                               ...) {
      list(
        audio = audio,
        service = service,
        native_engine = native_engine,
        native_backend = native_backend,
        diarize_speakers = diarize_speakers,
        diarize_embedder = diarize_embedder
      )
    },
    .package = "genflow"
  )
  tts_agent <- structure(
    list(context = "saved speech", service = "openai"),
    class = "genflow_agent"
  )
  stt_agent <- structure(
    list(
      audio = "saved.wav",
      service = "local-openai"
    ),
    class = "genflow_agent"
  )

  expect_identical(
    genflow:::gen_tts.genflow_agent(tts_agent)$text,
    "saved speech"
  )
  expect_identical(
    genflow:::gen_tts.genflow_agent(
      tts_agent,
      text_override = "replacement speech"
    )$text,
    "replacement speech"
  )
  expect_identical(
    genflow:::gen_stt.genflow_agent(
      stt_agent,
      audio_override = "replacement.wav"
    )[c("audio", "service")],
    list(audio = "replacement.wav", service = "local-openai")
  )
  expect_error(
    genflow:::gen_stt.genflow_agent(
      stt_agent,
      revision = "retired-python-bridge-field"
    ),
    "Unsupported `gen_stt()` override(s): revision.",
    fixed = TRUE
  )

  native_agent <- structure(
    list(
      audio = "meeting.wav",
      service = "local-native",
      native_engine = "crispasr",
      native_backend = "parakeet",
      diarize_speakers = TRUE,
      diarize_embedder = TRUE
    ),
    class = "genflow_agent"
  )
  expect_identical(
    genflow:::gen_stt.genflow_agent(
      native_agent,
      native_backend = "whisper",
      diarize_embedder = FALSE
    )[c(
      "service",
      "native_engine",
      "native_backend",
      "diarize_speakers",
      "diarize_embedder"
    )],
    list(
      service = "local-native",
      native_engine = "crispasr",
      native_backend = "whisper",
      diarize_speakers = TRUE,
      diarize_embedder = FALSE
    )
  )
})

test_that("gen_batch_agent rejects qty values that overflow R integers", {
  agent <- structure(
    list(name = "test-agent", type = "text"),
    class = "genflow_agent"
  )

  expect_error(
    gen_batch_agent(agent, qty = 1e20),
    "`qty` must be a positive integer",
    fixed = TRUE
  )
})
