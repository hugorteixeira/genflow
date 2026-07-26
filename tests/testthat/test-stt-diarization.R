stt_diarized_segments_fixture <- function() {
  list(
    list(
      timestamps = list(
        from = "00:00:00,240",
        to = "00:00:01,250"
      ),
      offsets = list(from = 240L, to = 1250L),
      speaker = "(Speaker 1) ",
      text = "Welcome everyone."
    ),
    list(
      timestamps = list(
        from = "00:00:01,250",
        to = "00:00:02,900"
      ),
      offsets = list(from = 1250L, to = 2900L),
      speaker = "(Speaker 2) ",
      text = "Thanks. Let us begin."
    ),
    list(
      timestamps = list(
        from = "00:00:03,100",
        to = "00:00:04,400"
      ),
      offsets = list(from = 3100L, to = 4400L),
      speaker = "(Speaker 1) ",
      text = "The first item is ready."
    )
  )
}

stt_plain_segment_fixture <- function() {
  list(
    list(
      timestamps = list(
        from = "00:00:00,000",
        to = "00:00:02,000"
      ),
      offsets = list(from = 0L, to = 2000L),
      text = "A transcript without speaker information."
    )
  )
}

stt_zero_based_diarized_segments_fixture <- function() {
  segments <- stt_diarized_segments_fixture()
  segments[[1]]$speaker <- "(speaker 0) "
  segments[[2]]$speaker <- "(speaker 1) "
  segments[[3]]$speaker <- "(speaker 0) "
  segments
}

stt_diarization_audio_fixture <- function() {
  path <- tempfile("genflow-diarization-", fileext = ".wav")
  writeBin(as.raw(c(82, 73, 70, 70, rep(0, 40))), path)
  path
}

test_that("CrispASR speaker labels normalize to stable Sxx labels", {
  expect_identical(
    genflow:::.stt_normalize_speaker_label("(Speaker 1) "),
    "S01"
  )
  expect_identical(
    genflow:::.stt_normalize_speaker_label("(speaker 2)"),
    "S02"
  )
  expect_identical(
    genflow:::.stt_normalize_speaker_label(" S01 "),
    "S01"
  )
  expect_identical(
    genflow:::.stt_normalize_speaker_label("Speaker 1"),
    "S01"
  )
  expect_identical(
    genflow:::.stt_normalize_speaker_label(1L),
    "S01"
  )
  expect_identical(
    genflow:::.stt_normalize_speaker_label("Host"),
    "Host"
  )
  expect_identical(
    genflow:::.stt_normalize_speaker_label(""),
    ""
  )
  expect_identical(
    genflow:::.stt_normalize_speaker_label(NULL),
    ""
  )
})

test_that("zero-based native speaker sets rebase to public one-based labels", {
  normalized <- genflow:::.stt_normalize_native_payload(list(
    transcription = stt_zero_based_diarized_segments_fixture()
  ))

  expect_identical(
    vapply(normalized$metadata$segments, `[[`, character(1), "speaker"),
    c("S01", "S02", "S01")
  )
  expect_identical(
    vapply(normalized$metadata$segments, `[[`, character(1), "speaker_raw"),
    c("(speaker 0)", "(speaker 1)", "(speaker 0)")
  )
})

test_that("MOSS backend inference uses artifacts and recorded sources, not folders", {
  expect_identical(
    genflow:::.stt_crispasr_backend_from_model(
      "/models/moss-diarize/granite-speech-q8_0.gguf"
    ),
    ""
  )
  expect_identical(
    genflow:::.stt_crispasr_backend_from_model(
      "/models/native/model-q8_0.gguf",
      source = paste0(
        "hf://OpenMOSS-Team/MOSS-Transcribe-Diarize-GGUF:",
        "model-q8_0.gguf"
      )
    ),
    "moss-diarize"
  )
})

test_that("Granite Plus native speaker attribution is identified narrowly", {
  expect_true(genflow:::.stt_crispasr_has_native_speaker_attribution(
    "granite-speech-4.1-2b-plus-f16.gguf"
  ))
  expect_true(genflow:::.stt_crispasr_has_native_speaker_attribution(
    "auto",
    backend = "granite-4.1-plus"
  ))
  expect_true(genflow:::.stt_crispasr_has_native_speaker_attribution(
    "renamed-model.gguf",
    source = paste0(
      "https://huggingface.co/cstr/",
      "granite-speech-4.1-2b-plus-GGUF/resolve/main/renamed-model.gguf"
    )
  ))
  expect_false(genflow:::.stt_crispasr_has_native_speaker_attribution(
    "granite-speech-4.1-2b-f16.gguf",
    backend = "granite-4.1"
  ))
  expect_false(genflow:::.stt_crispasr_has_native_speaker_attribution(
    "whisper-large-v3.gguf",
    backend = "whisper"
  ))
})

test_that("diarized transcripts render one timed speaker turn per line", {
  rendered <- genflow:::.stt_render_diarized_transcript(
    stt_diarized_segments_fixture(),
    fallback_text = "Welcome everyone. Thanks. Let us begin."
  )

  expect_identical(
    rendered,
    paste(
      "[00:00:00.240 --> 00:00:01.250] [S01] Welcome everyone.",
      "[00:00:01.250 --> 00:00:02.900] [S02] Thanks. Let us begin.",
      "[00:00:03.100 --> 00:00:04.400] [S01] The first item is ready.",
      sep = "\n"
    )
  )
})

test_that("diarized transcripts merge adjacent speaker segments without timestamps", {
  segments <- append(
    stt_diarized_segments_fixture(),
    list(list(
      offsets = list(from = 1250L, to = 1750L),
      speaker = "(Speaker 1) ",
      text = "Still speaking."
    )),
    after = 1L
  )
  rendered <- genflow:::.stt_render_diarized_transcript(
    segments,
    include_timestamps = FALSE
  )

  expect_identical(
    rendered,
    paste(
      "[S01] Welcome everyone. Still speaking.",
      "[S02] Thanks. Let us begin.",
      "[S01] The first item is ready.",
      sep = "\n"
    )
  )
})

test_that("non-diarized transcripts retain their plain-text fallback", {
  fallback <- "A transcript without speaker information."

  expect_identical(
    genflow:::.stt_render_diarized_transcript(
      stt_plain_segment_fixture(),
      fallback_text = fallback
    ),
    fallback
  )
  expect_identical(
    genflow:::.stt_render_diarized_transcript(
      list(),
      fallback_text = fallback
    ),
    fallback
  )
})

test_that("diarization summaries count distinct normalized speakers", {
  summary <- genflow:::.stt_diarization_summary(
    stt_diarized_segments_fixture()
  )

  expect_true(summary$has_diarization)
  expect_identical(summary$speaker_count, 2L)
  expect_identical(summary$segment_count, 3L)
  expect_identical(summary$speakers, c("S01", "S02"))
})

test_that("diarization summaries distinguish segments without speakers", {
  summary <- genflow:::.stt_diarization_summary(
    stt_plain_segment_fixture()
  )

  expect_false(summary$has_diarization)
  expect_identical(summary$speaker_count, 0L)
  expect_identical(summary$segment_count, 1L)
  expect_identical(summary$speakers, character())
})

test_that("CrispASR runs MOSS Diarize as one speaker-continuous input", {
  audio <- stt_diarization_audio_fixture()
  model <- tempfile(
    "moss-transcribe-diarize-0.9b-q8_0-",
    fileext = ".gguf"
  )
  writeBin(as.raw(c(1, 2, 3)), model)
  on.exit(unlink(c(audio, model)), add = TRUE)
  seen <- NULL

  result <- genflow:::.stt_native_crispasr(
    audio_path = audio,
    model = model,
    language = NULL,
    prompt = NULL,
    timeout_secs = 10,
    executable = file.path(R.home("bin"), "R"),
    native_device = "cpu",
    runner = function(command, args, timeout_secs, environment) {
      seen <<- list(
        command = command,
        args = args,
        timeout_secs = timeout_secs,
        environment = environment
      )
      output_base <- args[[match("-of", args) + 1L]]
      jsonlite::write_json(
        list(
          crispasr = list(
            backend = "moss-diarize",
            model = basename(model)
          ),
          transcription = stt_diarized_segments_fixture()
        ),
        paste0(output_base, ".json"),
        auto_unbox = TRUE
      )
      list(status = 0L, output = character())
    }
  )

  chunk_position <- match("--chunk-seconds", seen$args)
  backend_position <- match("--backend", seen$args)
  expect_false(is.na(chunk_position))
  expect_identical(seen$args[[chunk_position + 1L]], "0")
  expect_false(is.na(backend_position))
  expect_identical(seen$args[[backend_position + 1L]], "moss-diarize")
  expect_identical(result$metadata$backend, "moss-diarize")
  expect_identical(result$metadata$inferred_backend, "moss-diarize")
  expect_null(result$metadata$requested_backend)
  expect_identical(result$metadata$external_chunk_seconds, 0L)
  expect_identical(
    vapply(
      result$metadata$segments,
      `[[`,
      character(1),
      "speaker"
    ),
    c("S01", "S02", "S01")
  )
  expect_identical(
    result$metadata$segments[[1]]$speaker_raw,
    "(Speaker 1)"
  )

  expect_warning(
    conflicted <- genflow:::.stt_native_crispasr(
      audio_path = audio,
      model = model,
      language = NULL,
      prompt = NULL,
      timeout_secs = 10,
      executable = file.path(R.home("bin"), "R"),
      native_backend = "granite-4.1",
      native_device = "cpu",
      runner = function(command, args, timeout_secs, environment) {
        output_base <- args[[match("-of", args) + 1L]]
        jsonlite::write_json(
          list(
            crispasr = list(
              backend = "moss-diarize",
              model = basename(model)
            ),
            transcription = stt_diarized_segments_fixture()
          ),
          paste0(output_base, ".json"),
          auto_unbox = TRUE
        )
        list(status = 0L, output = character())
      }
    ),
    "conflicts with requested backend"
  )
  expect_identical(conflicted$metadata$backend, "moss-diarize")
  expect_identical(conflicted$metadata$requested_backend, "granite-4.1")
})

test_that("CrispASR activates Granite Plus speaker attribution on request", {
  audio <- stt_diarization_audio_fixture()
  model <- tempfile(
    "granite-speech-4.1-2b-plus-f16-",
    fileext = ".gguf"
  )
  writeBin(as.raw(c(1, 2, 3)), model)
  on.exit(unlink(c(audio, model)), add = TRUE)

  run_probe <- function(diarize) {
    seen <- NULL
    result <- genflow:::.stt_native_crispasr(
      audio_path = audio,
      model = model,
      language = "en",
      prompt = NULL,
      timeout_secs = 10,
      executable = file.path(R.home("bin"), "R"),
      native_backend = "granite-4.1",
      native_device = "cpu",
      diarize = diarize,
      runner = function(command, args, timeout_secs, environment) {
        seen <<- args
        output_base <- args[[match("-of", args) + 1L]]
        jsonlite::write_json(
          list(
            crispasr = list(
              backend = "granite",
              model = basename(model)
            ),
            transcription = stt_zero_based_diarized_segments_fixture()
          ),
          paste0(output_base, ".json"),
          auto_unbox = TRUE
        )
        list(status = 0L, output = character())
      }
    )
    list(args = seen, result = result)
  }

  enabled <- run_probe(TRUE)
  disabled <- run_probe(FALSE)

  expect_true("--diarize" %in% enabled$args)
  expect_false("--diarize" %in% disabled$args)
  expect_true(enabled$result$metadata$native_speaker_attribution)
  expect_null(disabled$result$metadata$native_speaker_attribution)
  expect_identical(
    vapply(
      enabled$result$metadata$segments,
      `[[`,
      character(1),
      "speaker"
    ),
    c("S01", "S02", "S01")
  )
})

test_that("gen_stt forwards its diarize control to the native adapter", {
  audio <- stt_diarization_audio_fixture()
  on.exit(unlink(audio), add = TRUE)
  seen <- logical()

  testthat::local_mocked_bindings(
    .stt_local_native = function(diarize, ...) {
      seen <<- c(seen, diarize)
      list(text = "Plain transcript.", metadata = list())
    },
    .package = "genflow"
  )

  capture.output(result <- gen_stt(
    audio,
    service = "local-native",
    model = "granite-speech-4.1-2b-plus-f16.gguf",
    save_txt = FALSE,
    diarize = FALSE
  ))
  capture.output(gen_stt(
    audio,
    service = "local-native",
    model = "granite-speech-4.1-2b-plus-f16.gguf",
    save_txt = FALSE,
    diarize = TRUE
  ))

  expect_identical(seen, c(FALSE, TRUE))
  expect_identical(result$response_value, "Plain transcript.")
})

test_that("gen_stt saves readable diarization and a structured JSON sidecar", {
  audio <- stt_diarization_audio_fixture()
  directory <- tempfile("genflow-diarization-output-")
  dir.create(directory)
  on.exit(unlink(c(audio, directory), recursive = TRUE), add = TRUE)
  normalized <- genflow:::.stt_normalize_native_payload(list(
    crispasr = list(
      backend = "moss-diarize",
      model = "moss-transcribe-diarize-0.9b-q8_0.gguf"
    ),
    transcription = stt_diarized_segments_fixture()
  ))
  normalized$metadata$model <- "moss-transcribe-diarize-0.9b-q8_0.gguf"

  testthat::local_mocked_bindings(
    .stt_local_native = function(...) normalized,
    .package = "genflow"
  )

  console <- capture.output(result <- gen_stt(
    audio,
    service = "local-native",
    model = "moss-transcribe-diarize-0.9b-q8_0.gguf",
    directory = directory
  ))

  expected_diarized <- genflow:::.stt_render_diarized_transcript(
    normalized$metadata$segments,
    normalized$text,
    include_timestamps = FALSE
  )
  expect_identical(
    result$response_value,
    paste(
      "Welcome everyone.",
      "Thanks. Let us begin.",
      "The first item is ready."
    )
  )
  expect_identical(
    result$model,
    "moss-transcribe-diarize-0.9b-q8_0.gguf"
  )
  expect_identical(result$diarized_transcript, expected_diarized)
  expect_true(file.exists(result$saved_file))
  expect_identical(
    paste(readLines(result$saved_file, warn = FALSE), collapse = "\n"),
    expected_diarized
  )
  expect_true(file.exists(result$saved_metadata_file))

  sidecar <- jsonlite::read_json(
    result$saved_metadata_file,
    simplifyVector = FALSE
  )
  expect_identical(sidecar$schema_version, 1L)
  expect_identical(sidecar$response_value, result$response_value)
  expect_identical(sidecar$diarized_transcript, expected_diarized)
  expect_identical(sidecar$metadata$segments[[1]]$speaker, "S01")
  expect_identical(sidecar$metadata$segments[[2]]$speaker, "S02")
  expect_match(
    paste(console, collapse = "\n"),
    "Diarization: 2 speakers (S01, S02) | 3 segments",
    fixed = TRUE
  )
  expect_match(
    paste(console, collapse = "\n"),
    paste0("Metadata: ", basename(result$saved_metadata_file)),
    fixed = TRUE
  )
})

test_that("diarize FALSE keeps plain output even when speaker metadata exists", {
  audio <- stt_diarization_audio_fixture()
  directory <- tempfile("genflow-disabled-diarization-output-")
  dir.create(directory)
  on.exit(unlink(c(audio, directory), recursive = TRUE), add = TRUE)
  normalized <- genflow:::.stt_normalize_native_payload(list(
    crispasr = list(backend = "moss-diarize"),
    transcription = stt_diarized_segments_fixture()
  ))

  testthat::local_mocked_bindings(
    .stt_local_native = function(...) normalized,
    .package = "genflow"
  )

  capture.output(result <- gen_stt(
    audio,
    service = "local-native",
    model = "moss-transcribe-diarize-0.9b-q8_0.gguf",
    directory = directory,
    diarize = FALSE
  ))

  expect_false("diarized_transcript" %in% names(result))
  expect_false("saved_metadata_file" %in% names(result))
  expect_identical(
    paste(readLines(result$saved_file, warn = FALSE), collapse = "\n"),
    result$response_value
  )
  expect_false(file.exists(sub("\\.txt$", ".json", result$saved_file)))
})

test_that("plain STT persistence keeps the existing one-file contract", {
  audio <- stt_diarization_audio_fixture()
  directory <- tempfile("genflow-plain-stt-output-")
  dir.create(directory)
  on.exit(unlink(c(audio, directory), recursive = TRUE), add = TRUE)

  testthat::local_mocked_bindings(
    .stt_local_openai = function(...) {
      list(
        text = "Plain transcript.",
        metadata = list(segments = stt_plain_segment_fixture())
      )
    },
    .package = "genflow"
  )

  capture.output(result <- gen_stt(
    audio,
    service = "local-openai",
    model = "local-model",
    directory = directory
  ))

  expect_identical(result$response_value, "Plain transcript.")
  expect_false("diarized_transcript" %in% names(result))
  expect_false("saved_metadata_file" %in% names(result))
  expect_identical(
    paste(readLines(result$saved_file, warn = FALSE), collapse = "\n"),
    "Plain transcript."
  )
  expect_false(file.exists(sub("\\.txt$", ".json", result$saved_file)))
})
