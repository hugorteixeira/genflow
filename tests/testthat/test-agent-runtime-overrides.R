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

test_that("TTS agents reuse content context and STT accepts an audio override", {
  testthat::local_mocked_bindings(
    gen_tts.default = function(text, service = "openai", ...) {
      list(text = text, service = service)
    },
    gen_stt.default = function(audio,
                               service = "openai",
                               revision = NULL,
                               native_engine = NULL,
                               native_backend = NULL,
                               ...) {
      list(
        audio = audio,
        service = service,
        revision = revision,
        native_engine = native_engine,
        native_backend = native_backend
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
      service = "hf-local",
      revision = "saved-commit"
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
      audio_override = "replacement.wav",
      revision = "override-commit"
    )[c("audio", "revision")],
    list(audio = "replacement.wav", revision = "override-commit")
  )

  native_agent <- structure(
    list(
      audio = "meeting.wav",
      service = "local-native",
      native_engine = "crispasr",
      native_backend = "parakeet"
    ),
    class = "genflow_agent"
  )
  expect_identical(
    genflow:::gen_stt.genflow_agent(
      native_agent,
      native_backend = "whisper"
    )[c("service", "native_engine", "native_backend")],
    list(
      service = "local-native",
      native_engine = "crispasr",
      native_backend = "whisper"
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
